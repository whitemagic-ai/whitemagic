"""Gnosis Portal — Unified Introspection Across All Subsystems.
=============================================================
Inspired by MandalaOS's Gnosis Portals: standardized, read-only APIs
at every layer boundary allowing introspection into the system's state.

The ``gnosis`` tool aggregates data from every Whitemagic subsystem into
a single, structured snapshot that any AI caller can use to understand
what the system is doing, how it's performing, and whether anything
needs attention.

Subsystem portals:
  - **harmony**   : Harmony Vector snapshot (7 dimensions + composite)
  - **dharma**    : Active profile, rule count, recent Karmic Trace summary
  - **karma**     : Karma Ledger debt summary
  - **circuit**   : Circuit breaker states (any tripped?)
  - **yin_yang**  : Classic Yin/Yang balance report
  - **telemetry** : Tool call counts, error rates (if available)
  - **state**     : WM_STATE_ROOT paths and sizes
  - **broker**    : Redis broker connectivity (if available)
  - **tasks**     : Task queue summary (pending/running/completed)
  - **votes**     : Open voting sessions summary
  - **temporal**  : Temporal Scheduler lane stats
  - **agents**    : Registered agent count and heartbeat status

Usage:
    from whitemagic.tools.gnosis import gnosis_snapshot
    snapshot = gnosis_snapshot()
"""

from __future__ import annotations

import logging
from collections.abc import Callable
from datetime import datetime
from typing import Any, TypeVar, cast

logger = logging.getLogger(__name__)
T = TypeVar("T")


def _safe(fn: Callable[[], T], default: T | None = None) -> T | None:
    """Call fn, return default on any failure."""
    try:
        return fn()
    except Exception:
        return default


def _json_safe(obj: Any) -> Any:
    """Recursively convert non-serializable objects (datetime, etc.) to strings."""
    if isinstance(obj, datetime):
        return obj.isoformat()
    if isinstance(obj, dict):
        return {k: _json_safe(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_json_safe(v) for v in obj]
    if hasattr(obj, "__dict__") and not isinstance(obj, (str, int, float, bool, type(None))):
        return str(obj)
    return obj


def gnosis_snapshot(compact: bool = False) -> dict[str, Any]:
    """Aggregate all subsystem introspection portals into a single snapshot.

    Every value is read-only and JSON-serializable. Subsystems that fail
    to respond are reported as ``null`` — the portal never raises.

    Args:
        compact: If True, return only anomalies/alerts/action items —
                 dramatically fewer tokens for AI consumers.

    """
    if compact:
        return cast("dict[str, Any]", _json_safe(_compact_snapshot()))
    timestamp = datetime.now().isoformat()

    # --- Harmony Vector ---
    harmony = _safe(lambda: _harmony_portal())

    # --- Dharma ---
    dharma = _safe(lambda: _dharma_portal())

    # --- Karma Ledger ---
    karma = _safe(lambda: _karma_portal())

    # --- Circuit Breakers ---
    circuit = _safe(lambda: _circuit_portal())

    # --- Yin/Yang Balance ---
    yin_yang = _safe(lambda: _yin_yang_portal())

    # --- Telemetry ---
    telemetry = _safe(lambda: _telemetry_portal())

    # --- State Paths ---
    state = _safe(lambda: _state_portal())

    # --- Distributed Coordination ---
    broker = _safe(lambda: _broker_portal())
    tasks = _safe(lambda: _tasks_portal())
    votes = _safe(lambda: _votes_portal())

    # --- Temporal Scheduler ---
    temporal = _safe(lambda: _temporal_portal())

    # --- Agent Registry ---
    agents = _safe(lambda: _agents_portal())

    # --- Homeostatic Loop ---
    homeostasis = _safe(lambda: _homeostasis_portal())

    # --- Maturity ---
    maturity = _safe(lambda: _maturity_portal())

    # --- Galactic Map ---
    galactic = _safe(lambda: _galactic_portal())

    # --- Emotion & Drive Core ---
    drives = _safe(lambda: _drives_portal())

    # --- Self-Model / Predictive Introspection ---
    self_model = _safe(lambda: _self_model_portal())

    # --- PRAT Resonance ---
    resonance = _safe(lambda: _resonance_portal())

    # --- Capability Matrix (summary only in full snapshot) ---
    capabilities = _safe(lambda: _capabilities_portal())

    # --- v14.0 Living Graph ---
    living_graph = _safe(lambda: _living_graph_portal())

    return cast("dict[str, Any]", _json_safe({
        "timestamp": timestamp,
        "harmony": harmony,
        "dharma": dharma,
        "karma": karma,
        "circuit_breakers": circuit,
        "yin_yang": yin_yang,
        "telemetry": telemetry,
        "state": state,
        "broker": broker,
        "tasks": tasks,
        "votes": votes,
        "temporal": temporal,
        "agents": agents,
        "homeostasis": homeostasis,
        "maturity": maturity,
        "galactic": galactic,
        "drives": drives,
        "self_model": self_model,
        "resonance": resonance,
        "capabilities": capabilities,
        "living_graph": living_graph,
    }))


def _compact_snapshot() -> dict[str, Any]:
    """Token-efficient snapshot: only anomalies, alerts, and suggested actions.
    Designed for AI agents that need a quick health check without the full portal.
    """
    alerts: list[str] = []
    suggestions: list[str] = []
    status_line = "healthy"

    # Harmony — flag if composite drops below 0.5
    harmony = _safe(lambda: _harmony_portal())
    if harmony:
        score = harmony.get("harmony_score", 1.0)
        if score < 0.5:
            alerts.append(f"harmony_low: {score:.2f}")
            suggestions.append("Run harmony_vector to diagnose dimensions")
            status_line = "degraded"

    # Karma — flag if debt > 5
    karma = _safe(lambda: _karma_portal())
    if karma and karma.get("total_debt", 0) > 5:
        alerts.append(f"karma_debt: {karma['total_debt']}")
        suggestions.append("Run karma_report to audit side-effect mismatches")

    # Circuit breakers — flag any tripped
    circuit = _safe(lambda: _circuit_portal())
    if circuit and circuit.get("any_open"):
        tripped = circuit.get("tripped", [])
        names = [b.get("tool", "?") for b in tripped[:3]]
        alerts.append(f"breakers_open: {', '.join(names)}")
        status_line = "degraded"

    # Homeostasis — flag if loop is not running or recent corrective actions
    # Note: stopped background services are informational on fresh installs,
    # not degraded — they require explicit start and aren't auto-started on pip install.
    homeo = _safe(lambda: _homeostasis_portal())
    if homeo:
        if not homeo.get("running"):
            has_history = homeo.get("total_checks", 0) > 0
            if has_history:
                alerts.append("homeostasis_stopped")
            # else: never started — fresh install, don't penalize
        actions = homeo.get("recent_actions", [])
        if actions:
            alerts.append(f"homeostasis_actions: {len(actions)} recent")

    # Maturity — always report stage (compact)
    maturity = _safe(lambda: _maturity_portal())
    stage = maturity.get("stage", "unknown") if maturity else "unknown"

    # Temporal — flag if not running (only if it was previously started)
    temporal = _safe(lambda: _temporal_portal())
    if temporal and not temporal.get("running"):
        lanes = temporal.get("lanes", {})
        has_history = any(v > 0 for v in lanes.values() if isinstance(v, (int, float))) if isinstance(lanes, dict) else False
        if has_history:
            alerts.append("temporal_scheduler_stopped")

    # Tasks — flag pending tasks
    tasks = _safe(lambda: _tasks_portal())
    if tasks:
        counts = tasks.get("counts", {})
        pending = counts.get("pending", 0)
        if pending > 0:
            alerts.append(f"tasks_pending: {pending}")

    # Votes — flag open sessions
    votes = _safe(lambda: _votes_portal())
    if votes and votes.get("open_sessions", 0) > 0:
        alerts.append(f"votes_open: {votes['open_sessions']}")

    if not alerts:
        suggestions.append("System healthy — no action needed")
    else:
        if status_line == "degraded":
            suggestions.append("Run homeostasis.check to trigger corrective actions")

    # Auto-suggest next actions based on system state
    next_actions = _auto_suggest(alerts, stage, harmony, maturity)

    # ready = True when system is healthy enough for productive work
    ready = status_line == "healthy" or (status_line == "degraded" and len(alerts) <= 2)

    return {
        "timestamp": datetime.now().isoformat(),
        "ready": ready,
        "status": status_line,
        "maturity_stage": stage,
        "alert_count": len(alerts),
        "alerts": alerts,
        "suggestions": suggestions,
        "next_actions": next_actions,
    }


def _auto_suggest(
    alerts: list[str],
    stage: str,
    harmony: dict[str, Any] | None,
    maturity: dict[str, Any] | None,
) -> list[dict[str, Any]]:
    """Generate specific tool call suggestions based on current state."""
    del harmony, maturity
    actions: list[dict[str, Any]] = []

    # If no alerts and system is healthy, suggest productive work
    if not alerts:
        actions.append({"tool": "salience.spotlight", "reason": "Check what deserves attention"})
        if stage in ("SEED", "BICAMERAL"):
            actions.append({"tool": "maturity.assess", "reason": "Work toward next maturity stage"})
        return actions[:3]

    # Suggest based on specific alerts
    for alert in alerts:
        if "harmony_low" in alert:
            actions.append({"tool": "harmony_vector", "reason": "Diagnose which dimensions are degraded"})
        elif "karma_debt" in alert:
            actions.append({"tool": "karma_report", "reason": "Audit side-effect mismatches"})
        elif "breakers_open" in alert:
            actions.append({"tool": "gnosis", "args": {"compact": False}, "reason": "Full snapshot to see breaker details"})
        elif "homeostasis_stopped" in alert:
            actions.append({"tool": "homeostasis.check", "reason": "Manually trigger a health check"})
        elif "tasks_pending" in alert:
            actions.append({"tool": "task.status", "reason": "Check pending task details"})
        elif "votes_open" in alert:
            actions.append({"tool": "vote.list", "reason": "Review open vote sessions"})

    return actions[:5]


# ---------------------------------------------------------------------------
# Individual portals
# ---------------------------------------------------------------------------

def _harmony_portal() -> dict[str, Any]:
    from whitemagic.harmony.vector import read_harmony_fast
    snap = read_harmony_fast()
    return cast("dict[str, Any]", snap.to_dict())


def _dharma_portal() -> dict[str, Any]:
    from whitemagic.dharma.rules import get_rules_engine
    engine = get_rules_engine()
    trace = engine.get_karmic_trace(limit=5)
    return {
        "active_profile": engine.get_profile(),
        "rule_count": len(engine.get_rules()),
        "recent_decisions": len(trace),
        "last_decision": trace[-1] if trace else None,
    }


def _karma_portal() -> dict[str, Any]:
    from whitemagic.dharma.karma_ledger import get_karma_ledger
    ledger = get_karma_ledger()
    return {
        "total_debt": round(ledger.get_debt(), 2),
        "mismatch_rate": round(
            sum(ledger._tool_mismatches.values()) /
            max(sum(ledger._tool_calls.values()), 1), 4,
        ),
    }


def _circuit_portal() -> dict[str, Any]:
    from whitemagic.tools.circuit_breaker import get_breaker_registry
    reg = get_breaker_registry()
    tripped = reg.tripped()
    return {
        "total_breakers": len(reg.all_status()),
        "tripped": tripped,
        "any_open": any(
            b.get("state") == "open" for b in tripped
        ),
    }


def _yin_yang_portal() -> dict[str, Any]:
    from whitemagic.harmony.yin_yang_tracker import get_tracker
    return cast("dict[str, Any]", get_tracker().get_report())


def _telemetry_portal() -> dict[str, Any]:
    from whitemagic.core.monitoring.telemetry import get_telemetry
    t = get_telemetry()
    return cast("dict[str, Any]", t.get_summary())


def _state_portal() -> dict[str, Any]:
    import os

    from whitemagic.config.paths import CACHE_DIR, LOGS_DIR, MEMORY_DIR, WM_ROOT

    def _dir_size(path: Any) -> int:
        total = 0
        try:
            for dirpath, _, filenames in os.walk(path):
                for f in filenames:
                    fp = os.path.join(dirpath, f)
                    try:
                        total += os.path.getsize(fp)
                    except OSError:
                        pass
        except OSError:
            pass
        return total

    return {
        "wm_root": str(WM_ROOT),
        "memory_dir_bytes": _dir_size(MEMORY_DIR),
        "logs_dir_bytes": _dir_size(LOGS_DIR),
        "cache_dir_bytes": _dir_size(CACHE_DIR),
        "tasks_dir_bytes": _dir_size(WM_ROOT / "tasks"),
        "votes_dir_bytes": _dir_size(WM_ROOT / "votes"),
    }


def _broker_portal() -> dict[str, Any]:
    from whitemagic.tools.handlers.broker import handle_broker_status
    result = handle_broker_status()
    return {
        "connected": result.get("connected", False),
        "redis_version": result.get("redis_version"),
        "connected_clients": result.get("connected_clients", 0),
    }


def _tasks_portal() -> dict[str, Any]:
    from whitemagic.tools.handlers.task_dist import handle_task_status
    result = handle_task_status()
    return {
        "total": result.get("total", 0),
        "counts": result.get("counts", {}),
    }


def _votes_portal() -> dict[str, Any]:
    from whitemagic.tools.handlers.voting import handle_vote_list
    result = handle_vote_list(filter_status="open")
    return {
        "open_sessions": result.get("count", 0),
        "sessions": [
            {"id": s["id"], "problem": s.get("problem", "")[:80], "vote_count": s.get("vote_count", 0)}
            for s in result.get("sessions", [])[:5]
        ],
    }


def _temporal_portal() -> dict[str, Any]:
    from whitemagic.core.resonance.temporal_scheduler import get_temporal_scheduler
    scheduler = get_temporal_scheduler()
    stats = scheduler.get_stats()
    return {
        "running": scheduler.is_running,
        "lanes": stats,
    }


def _agents_portal() -> dict[str, Any]:
    try:
        from whitemagic.tools.handlers.agent_registry import handle_agent_list
        result = handle_agent_list()
        return {
            "total": result.get("count", 0),
            "active": result.get("active_count", 0),
        }
    except (ImportError, Exception):
        return {"total": 0, "active": 0, "note": "agent registry not yet initialized"}


def _homeostasis_portal() -> dict[str, Any]:
    from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
    loop = get_homeostatic_loop()
    stats = loop.get_stats()
    return {
        "running": stats.get("running", False),
        "total_checks": stats.get("total_checks", 0),
        "total_actions": stats.get("total_actions", 0),
        "recent_actions": stats.get("recent_actions", [])[:3],
    }


def _maturity_portal() -> dict[str, Any]:
    from whitemagic.core.governance.maturity_gates import get_maturity_engine
    engine = get_maturity_engine()
    report = engine.assess()
    d = report.to_dict()
    return {
        "stage": d.get("current_stage", "unknown"),
        "level": d.get("level", 0),
        "next_stage": d.get("next_stage"),
    }


def _galactic_portal() -> dict[str, Any]:
    """Galactic Map zone counts — live from the database."""
    import sqlite3

    from whitemagic.core.memory.unified import get_unified_memory
    um = get_unified_memory()
    with um.backend.pool.connection() as conn:
        conn.row_factory = sqlite3.Row
        rows = conn.execute("""
            SELECT
              SUM(CASE WHEN galactic_distance < 0.15 THEN 1 ELSE 0 END) as core,
              SUM(CASE WHEN galactic_distance >= 0.15 AND galactic_distance < 0.40 THEN 1 ELSE 0 END) as inner_rim,
              SUM(CASE WHEN galactic_distance >= 0.40 AND galactic_distance < 0.65 THEN 1 ELSE 0 END) as mid_band,
              SUM(CASE WHEN galactic_distance >= 0.65 AND galactic_distance < 0.85 THEN 1 ELSE 0 END) as outer_rim,
              SUM(CASE WHEN galactic_distance >= 0.85 THEN 1 ELSE 0 END) as far_edge,
              SUM(is_protected) as protected_count
            FROM memories
        """).fetchone()
        return {
            "core": rows["core"] or 0,
            "inner_rim": rows["inner_rim"] or 0,
            "mid_band": rows["mid_band"] or 0,
            "outer_rim": rows["outer_rim"] or 0,
            "far_edge": rows["far_edge"] or 0,
            "protected": rows["protected_count"] or 0,
        }


def _drives_portal() -> dict[str, Any]:
    """Emotion & Drive Core — intrinsic motivation snapshot."""
    from whitemagic.core.intelligence.emotion_drive import get_drive_core
    core = get_drive_core()
    snap = core.snapshot()
    return {
        "dominant_drive": snap.dominant_drive,
        "composite": round(snap.composite, 3),
        "trend": snap.trend,
        "curiosity": round(snap.curiosity, 3),
        "satisfaction": round(snap.satisfaction, 3),
        "caution": round(snap.caution, 3),
        "energy": round(snap.energy, 3),
        "social": round(snap.social, 3),
        "event_count": snap.event_count,
    }


def _self_model_portal() -> dict[str, Any]:
    """Self-Model — predictive introspection alerts."""
    from whitemagic.core.intelligence.self_model import get_self_model
    model = get_self_model()
    alerts = model.get_alerts()
    stats = model.get_stats()
    return {
        "tracked_metrics": stats.get("metric_count", 0),
        "total_observations": stats.get("total_observations", 0),
        "alert_count": len(alerts),
        "alerts": [a.to_dict() for a in alerts[:3]],
    }


def _resonance_portal() -> dict[str, Any]:
    """PRAT Resonance — session-level Gana resonance state."""
    from whitemagic.tools.prat_resonance import get_resonance_summary
    return cast("dict[str, Any]", get_resonance_summary())


def _living_graph_portal() -> dict[str, Any]:
    """v14.0 Living Graph — topology, surprise gate, walker stats."""
    result: dict[str, Any] = {}

    # Graph Engine
    try:
        from whitemagic.core.memory.graph_engine import get_graph_engine
        engine = get_graph_engine()
        result["graph_engine"] = engine.get_stats()
    except Exception:
        result["graph_engine"] = None

    # Graph Walker
    try:
        from whitemagic.core.memory.graph_walker import get_graph_walker
        walker = get_graph_walker()
        result["graph_walker"] = walker.get_stats()
    except Exception:
        result["graph_walker"] = None

    # Surprise Gate
    try:
        from whitemagic.core.memory.surprise_gate import get_surprise_gate
        gate = get_surprise_gate()
        result["surprise_gate"] = gate.get_stats()
    except Exception:
        result["surprise_gate"] = None

    # Bridge Synthesizer
    try:
        from whitemagic.core.memory.bridge_synthesizer import get_bridge_synthesizer
        synth = get_bridge_synthesizer()
        result["bridge_synthesizer"] = synth.get_stats()
    except Exception:
        result["bridge_synthesizer"] = None

    return result


def _capabilities_portal() -> dict[str, Any]:
    """Capability Matrix — summary counts for Gnosis (full matrix via capability.matrix tool)."""
    from whitemagic.tools.capability_matrix import (
        ACTIVE_FUSIONS,
        SUBSYSTEMS,
        UNEXPLORED_FUSIONS,
    )
    polyglot_count = sum(1 for s in SUBSYSTEMS if s.get("polyglot"))
    return {
        "total_subsystems": len(SUBSYSTEMS),
        "active_fusions": len(ACTIVE_FUSIONS),
        "unexplored_fusions": len(UNEXPLORED_FUSIONS),
        "polyglot_accelerated": polyglot_count,
        "note": "Call capability.matrix for full details",
    }
