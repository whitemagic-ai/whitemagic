"""Explain This — Pre-Execution Impact Preview.
=============================================
Before running a tool, an AI caller can ask "explain this" to get a
comprehensive preview of what will happen:

  1. **Dharma Evaluation** — will this action be allowed? Which rules fire?
  2. **Resource Estimate** — expected side-effects (reads/writes/deletes)
  3. **Dependency Chain** — prerequisites, suggested follow-ups
  4. **Risk Assessment** — Governor risk level, circuit breaker state
  5. **Karma Forecast** — predicted karma debt impact
  6. **Maturity Gate** — is the system mature enough for this tool?

Usage::

    from whitemagic.tools.explain_this import explain_tool
    preview = explain_tool("delete_memory", filename="old_note.md")
    # Returns a rich dict with all impact dimensions

MCP tool: ``explain_this``
"""

from __future__ import annotations

import logging
from typing import Any

logger = logging.getLogger(__name__)


def explain_tool(tool_name: str, **kwargs: Any) -> dict[str, Any]:
    """Generate a pre-execution impact preview for a tool call.

    This does NOT execute the tool — it only predicts what would happen.
    """
    preview: dict[str, Any] = {
        "tool": tool_name,
        "args": kwargs,
        "status": "preview",
    }

    # 1. Tool metadata
    preview["metadata"] = _get_tool_metadata(tool_name)

    # 2. Dharma evaluation (dry-run)
    preview["dharma"] = _evaluate_dharma(tool_name, kwargs)

    # 3. Resource estimate
    preview["resource_estimate"] = _estimate_resources(tool_name, kwargs)

    # 4. Dependency chain
    preview["dependencies"] = _get_dependency_info(tool_name)

    # 5. Risk assessment
    preview["risk"] = _assess_risk(tool_name, kwargs)

    # 6. Karma forecast
    preview["karma_forecast"] = _forecast_karma(tool_name)

    # 7. Maturity gate check
    preview["maturity"] = _check_maturity(tool_name)

    # 8. Circuit breaker state
    preview["circuit_breaker"] = _check_breaker(tool_name)

    # 9. Overall recommendation
    preview["recommendation"] = _synthesize_recommendation(preview)

    return preview


def _get_tool_metadata(tool_name: str) -> dict[str, Any]:
    """Fetch tool definition metadata."""
    try:
        from whitemagic.tools.registry import get_tool
        tool = get_tool(tool_name)
        if tool:
            return {
                "name": tool.name,
                "category": tool.category.value,
                "safety": tool.safety.value,
                "description": tool.description,
                "risk_level": tool.risk_level,
                "gana": tool.gana,
                "garden": tool.garden,
            }
    except Exception:
        pass
    return {"name": tool_name, "category": "unknown", "safety": "unknown"}


def _evaluate_dharma(tool_name: str, kwargs: dict[str, Any]) -> dict[str, Any]:
    """Dry-run Dharma evaluation without actually enforcing."""
    try:
        from whitemagic.dharma.rules import get_rules_engine
        engine = get_rules_engine()
        action: dict[str, Any] = {
            "tool": tool_name,
            "description": f"tool:{tool_name}",
            "safety": "READ",
            "kwargs": kwargs,
            "dry_run": True,
        }
        result = engine.evaluate(action)
        action_value = result.action.value if hasattr(result.action, "value") else str(result.action)
        allowed = action_value != "block"
        return {
            "allowed": allowed,
            "action": action_value,
            "rules_fired": list(result.triggered_rules),
            "reason": result.explain,
            "profile": getattr(result, "profile", getattr(engine, "_active_profile", "default")),
        }
    except Exception as e:
        return {"allowed": True, "action": "LOG", "rules_fired": [], "note": f"Dharma unavailable: {e}"}


def _estimate_resources(tool_name: str, kwargs: dict[str, Any]) -> dict[str, Any]:
    """Estimate resource impact based on tool safety and category."""
    try:
        from whitemagic.tools.registry import get_tool
        tool = get_tool(tool_name)
        if not tool:
            return {"reads": True, "writes": False, "deletes": False}

        safety = tool.safety.value
        expected_side_effects: list[str] = []
        estimate: dict[str, Any] = {
            "reads": True,  # all tools read something
            "writes": safety in ("write", "delete"),
            "deletes": safety == "delete",
            "category": tool.category.value,
            "expected_side_effects": expected_side_effects,
        }

        # Category-specific estimates
        if tool.category.value == "memory":
            if safety == "write":
                estimate["expected_side_effects"].append("Creates or modifies memory entries")
                estimate["expected_side_effects"].append("Updates galactic distance and retention scores")
            elif safety == "delete":
                estimate["expected_side_effects"].append("Archives memory to FAR_EDGE (no-delete policy)")
        elif tool.category.value == "session":
            if safety == "write":
                estimate["expected_side_effects"].append("Creates or modifies session state")
        elif tool.category.value == "dharma":
            if safety == "write":
                estimate["expected_side_effects"].append("Modifies Dharma enforcement profile")
        elif tool.category.value == "system":
            if safety == "write":
                estimate["expected_side_effects"].append("System-level mutation")

        return estimate
    except Exception:
        return {"reads": True, "writes": False, "deletes": False}


def _get_dependency_info(tool_name: str) -> dict[str, Any]:
    """Query the dependency graph for prerequisites and next steps."""
    try:
        from whitemagic.tools.dependency_graph import get_tool_graph
        graph = get_tool_graph()

        prereqs = graph.prerequisites(tool_name)
        next_steps = graph.next_steps(tool_name)

        return {
            "prerequisites": [
                {"tool": e.get("source", ""), "type": e.get("type", ""), "weight": e.get("weight", 0)}
                for e in prereqs
            ] if prereqs else [],
            "suggested_next": [
                {"tool": e.get("target", ""), "type": e.get("type", ""), "weight": e.get("weight", 0)}
                for e in next_steps
            ] if next_steps else [],
            "has_hard_deps": any(
                e.get("type") == "requires" for e in (prereqs or [])
            ),
        }
    except Exception as e:
        return {"prerequisites": [], "suggested_next": [], "note": f"Graph unavailable: {e}"}


def _assess_risk(tool_name: str, kwargs: dict[str, Any]) -> dict[str, Any]:
    """Combine Governor validation with tool risk level."""
    risk_info: dict[str, Any] = {"level": "unknown", "governor_safe": True}

    # Tool risk level
    try:
        from whitemagic.tools.registry import get_tool
        tool = get_tool(tool_name)
        if tool:
            risk_info["level"] = tool.risk_level
    except Exception:
        pass

    # Governor dry-run
    try:
        from whitemagic.core.governor import get_governor
        gov = get_governor()
        validation = gov.validate_tool_call(tool_name, kwargs)
        risk_info["governor_safe"] = validation.safe
        if not validation.safe:
            risk_info["governor_reason"] = validation.reason
            risk_info["governor_risk"] = validation.risk_level.name
    except ImportError:
        pass

    return risk_info


def _forecast_karma(tool_name: str) -> dict[str, Any]:
    """Predict karma impact based on tool safety and historical ledger data."""
    try:
        from whitemagic.dharma.karma_ledger import get_karma_ledger
        from whitemagic.tools.registry import get_tool

        tool = get_tool(tool_name)
        ledger = get_karma_ledger()
        report = ledger.report()

        # Historical mismatch rate for this tool
        tool_stats = report.get("per_tool", {}).get(tool_name, {})
        historical_mismatch_rate = tool_stats.get("mismatch_rate", 0.0) if tool_stats else 0.0

        forecast = {
            "declared_safety": tool.safety.value if tool else "unknown",
            "current_total_debt": report.get("total_debt", 0.0),
            "historical_mismatch_rate": round(historical_mismatch_rate, 3),
        }

        # Predict debt delta
        if tool and tool.safety.value == "read":
            forecast["predicted_debt_delta"] = 0.0
            forecast["note"] = "READ tools accrue zero debt when behaving correctly"
        elif tool and tool.safety.value == "write":
            forecast["predicted_debt_delta"] = 0.0 if historical_mismatch_rate < 0.1 else 0.2
            if historical_mismatch_rate >= 0.1:
                forecast["note"] = f"Tool has {historical_mismatch_rate:.0%} historical mismatch rate"
        elif tool and tool.safety.value == "delete":
            forecast["predicted_debt_delta"] = 0.0
            forecast["note"] = "DELETE tools redirect to archive (no-delete policy)"

        return forecast
    except Exception as e:
        return {"note": f"Karma forecast unavailable: {e}"}


def _check_maturity(tool_name: str) -> dict[str, Any]:
    """Check if the tool is gated by maturity."""
    try:
        from whitemagic.tools.maturity_check import check_maturity_for_tool
        gate_result = check_maturity_for_tool(tool_name)
        if gate_result is not None:
            return {"gated": True, "reason": gate_result.get("error", "Maturity gate blocked")}
        return {"gated": False}
    except Exception:
        return {"gated": False, "note": "Maturity check unavailable"}


def _check_breaker(tool_name: str) -> dict[str, Any]:
    """Check circuit breaker state for the tool."""
    try:
        from whitemagic.tools.circuit_breaker import get_breaker_registry
        breaker = get_breaker_registry().get(tool_name)
        if breaker is None:
            return {"state": "unknown", "is_open": False}
        status = breaker.status() if hasattr(breaker, "status") else {}
        return {
            "state": status.get("state", "unknown"),
            "failure_count": status.get("recent_failures", 0),
            "is_open": breaker.is_open(),
        }
    except Exception:
        return {"state": "unknown", "is_open": False}


def _synthesize_recommendation(preview: dict[str, Any]) -> dict[str, Any]:
    """Synthesize all dimensions into a go/no-go recommendation."""
    blockers: list[str] = []
    warnings: list[str] = []

    # Dharma block
    dharma = preview.get("dharma", {})
    if not dharma.get("allowed", True):
        blockers.append(f"Dharma: {dharma.get('action', 'BLOCK')} — {dharma.get('reason', 'rule violation')}")

    # Governor block
    risk = preview.get("risk", {})
    if not risk.get("governor_safe", True):
        blockers.append(f"Governor: {risk.get('governor_reason', 'unsafe')}")

    # Circuit breaker
    cb = preview.get("circuit_breaker", {})
    if cb.get("is_open"):
        blockers.append(f"Circuit breaker OPEN (failures: {cb.get('failure_count', '?')})")

    # Maturity gate
    maturity = preview.get("maturity", {})
    if maturity.get("gated"):
        blockers.append(f"Maturity gate: {maturity.get('reason', 'insufficient maturity')}")

    # Risk warnings
    if risk.get("level") in ("DANGEROUS", "FORBIDDEN"):
        warnings.append(f"Risk level: {risk.get('level')}")

    # Karma warnings
    karma = preview.get("karma_forecast", {})
    if karma.get("predicted_debt_delta", 0) > 0:
        warnings.append(f"May accrue karma debt: +{karma['predicted_debt_delta']}")

    # Resource warnings
    resources = preview.get("resource_estimate", {})
    if resources.get("deletes"):
        warnings.append("Involves deletion (redirected to archive)")

    # Missing prerequisites
    deps = preview.get("dependencies", {})
    if deps.get("has_hard_deps"):
        prereq_names = [p["tool"] for p in deps.get("prerequisites", []) if p.get("type") == "requires"]
        if prereq_names:
            warnings.append(f"Hard prerequisites: {', '.join(prereq_names)}")

    if blockers:
        verdict = "BLOCKED"
    elif warnings:
        verdict = "PROCEED_WITH_CAUTION"
    else:
        verdict = "SAFE_TO_PROCEED"

    return {
        "verdict": verdict,
        "blockers": blockers,
        "warnings": warnings,
    }
