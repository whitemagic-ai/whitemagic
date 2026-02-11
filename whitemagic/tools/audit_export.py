"""MCP-Native Audit Export — Karmic Trace + Telemetry in Standard Format.
=====================================================================
Exports Whitemagic's internal audit trail (karmic trace, telemetry,
Dharma evaluations, circuit breaker events) in a format compatible
with MCP gateway audit logging expectations.

Standard fields per entry:
  - timestamp (ISO 8601)
  - tool_name
  - agent_id
  - action (invoke / block / fail)
  - duration_ms
  - result_status
  - metadata (dharma evaluation, breaker state, etc.)

Usage:
    from whitemagic.tools.audit_export import export_audit_log

    entries = export_audit_log(limit=100, since="2026-02-07T00:00:00")
"""

import logging
from datetime import datetime
from typing import Any, cast

logger = logging.getLogger(__name__)


def export_audit_log(
    limit: int = 100,
    since: str | None = None,
    tool_filter: str | None = None,
    agent_filter: str | None = None,
    format: str = "mcp",
) -> dict[str, Any]:
    """Export audit log entries in MCP-compatible format.

    Args:
        limit: Max entries to return
        since: ISO timestamp — only entries after this time
        tool_filter: Filter to specific tool name
        agent_filter: Filter to specific agent ID
        format: Output format ('mcp' or 'raw')

    """
    entries: list[dict[str, Any]] = []

    # 1. Gather karmic trace entries (Dharma evaluations)
    try:
        from whitemagic.dharma.rules import get_rules_engine
        engine = get_rules_engine()
        trace = engine.get_karmic_trace(limit=limit)
        for t in trace:
            entry = _normalize_karmic_entry(t)
            if _passes_filters(entry, since, tool_filter, agent_filter):
                entries.append(entry)
    except Exception as e:
        logger.debug(f"Karmic trace unavailable: {e}")

    # 2. Gather telemetry entries (tool call records)
    try:
        from whitemagic.core.monitoring.telemetry import get_telemetry
        tel = get_telemetry()
        summary = tel.get_summary()
        recent_events = summary.get("recent_events", [])
        if isinstance(recent_events, list):
            telemetry_records = recent_events[-limit:]
        else:
            telemetry_records = []
        for record in telemetry_records:
            entry = _normalize_telemetry_entry(record)
            if _passes_filters(entry, since, tool_filter, agent_filter):
                entries.append(entry)
    except Exception as e:
        logger.debug(f"Telemetry unavailable: {e}")

    # 3. Gather circuit breaker events
    try:
        from whitemagic.tools.circuit_breaker import get_breaker_registry
        reg = get_breaker_registry()
        for status in reg.all_status():
            if status.get("state") != "closed":
                entries.append({
                    "timestamp": datetime.now().isoformat(),
                    "tool_name": status.get("tool", "unknown"),
                    "agent_id": "*",
                    "action": "circuit_open",
                    "duration_ms": 0,
                    "result_status": "blocked",
                    "metadata": {
                        "breaker_state": status.get("state"),
                        "failure_count": status.get("failure_count", 0),
                    },
                })
    except Exception as e:
        logger.debug(f"Circuit breaker data unavailable: {e}")

    # 4. Gather rate limiter stats
    try:
        from whitemagic.tools.rate_limiter import get_rate_limiter
        rl_stats = get_rate_limiter().get_stats()
        if rl_stats.get("total_blocked", 0) > 0:
            entries.append({
                "timestamp": datetime.now().isoformat(),
                "tool_name": "*",
                "agent_id": "*",
                "action": "rate_limit_summary",
                "duration_ms": 0,
                "result_status": "info",
                "metadata": rl_stats,
            })
    except Exception:
        pass

    # Sort by timestamp descending
    entries.sort(key=lambda e: e.get("timestamp", ""), reverse=True)
    entries = entries[:limit]

    return cast("dict[str, Any]", _json_safe({
        "status": "success",
        "format": format,
        "entry_count": len(entries),
        "entries": entries,
        "export_timestamp": datetime.now().isoformat(),
    }))


def _json_safe(obj: Any) -> Any:
    """Recursively convert non-JSON-serializable objects to strings."""
    if obj is None or isinstance(obj, (bool, int, float, str)):
        return obj
    if isinstance(obj, dict):
        return {str(k): _json_safe(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_json_safe(v) for v in obj]
    if isinstance(obj, datetime):
        return obj.isoformat()
    return str(obj)


def _normalize_karmic_entry(trace_entry: Any) -> dict[str, Any]:
    """Convert a karmic trace entry to MCP audit format."""
    if isinstance(trace_entry, dict):
        return {
            "timestamp": trace_entry.get("timestamp", datetime.now().isoformat()),
            "tool_name": trace_entry.get("tool", trace_entry.get("action", "unknown")),
            "agent_id": trace_entry.get("agent_id", "system"),
            "action": "dharma_evaluation",
            "duration_ms": 0,
            "result_status": trace_entry.get("result", "unknown"),
            "metadata": {
                "profile": trace_entry.get("profile"),
                "rule": trace_entry.get("rule_name"),
                "action_taken": trace_entry.get("action_taken"),
                "severity": trace_entry.get("severity"),
            },
        }
    return {
        "timestamp": datetime.now().isoformat(),
        "tool_name": "unknown",
        "agent_id": "system",
        "action": "dharma_evaluation",
        "duration_ms": 0,
        "result_status": "unknown",
        "metadata": {"raw": str(trace_entry)[:200]},
    }


def _normalize_telemetry_entry(record: Any) -> dict[str, Any]:
    """Convert a telemetry record to MCP audit format."""
    if isinstance(record, dict):
        return {
            "timestamp": record.get("timestamp", datetime.now().isoformat()),
            "tool_name": record.get("tool_name", "unknown"),
            "agent_id": record.get("agent_id", "default"),
            "action": "invoke",
            "duration_ms": record.get("duration_ms", 0),
            "result_status": record.get("status", "unknown"),
            "metadata": {
                "category": record.get("category"),
                "safety": record.get("safety"),
            },
        }
    return {
        "timestamp": datetime.now().isoformat(),
        "tool_name": "unknown",
        "agent_id": "default",
        "action": "invoke",
        "duration_ms": 0,
        "result_status": "unknown",
        "metadata": {},
    }


def _passes_filters(
    entry: dict[str, Any],
    since: str | None,
    tool_filter: str | None,
    agent_filter: str | None,
) -> bool:
    """Check if an entry passes the given filters."""
    if since and entry.get("timestamp", "") < since:
        return False
    if tool_filter and entry.get("tool_name") != tool_filter:
        return False
    if agent_filter and entry.get("agent_id") != agent_filter:
        return False
    return True
