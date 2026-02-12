"""MCP handlers for v14.2 features.

New tools:
  - jit_research / jit_research.stats     (JIT Memory Researcher)
  - narrative.compress / narrative.stats   (Narrative Compression)
  - hermit.status / hermit.assess / hermit.withdraw / hermit.mediate / hermit.resolve / hermit.verify_ledger
  - green.report / green.record           (Green Score Telemetry)
  - cognitive.mode / cognitive.set / cognitive.hints / cognitive.stats
"""

from typing import Any


# ═══════════════════════════════════════════════════════════════
# JIT Memory Researcher
# ═══════════════════════════════════════════════════════════════

def handle_jit_research(**kwargs: Any) -> dict[str, Any]:
    """Run iterative plan-search-reflect research on a query."""
    from whitemagic.core.intelligence.researcher import get_researcher

    query = kwargs.get("query", "")
    if not query:
        return {"status": "error", "message": "Missing required parameter: query"}

    researcher = get_researcher()
    result = researcher.research(
        query=query,
        max_rounds=int(kwargs.get("max_rounds", 3)),
        evidence_limit=int(kwargs.get("evidence_limit", 5)),
        hops=int(kwargs.get("hops", 2)),
    )
    return {"status": "success", **result.to_dict()}


def handle_jit_research_stats(**kwargs: Any) -> dict[str, Any]:
    """Get JIT researcher statistics."""
    from whitemagic.core.intelligence.researcher import get_researcher

    researcher = get_researcher()
    return {"status": "success", **researcher.get_stats()}


# ═══════════════════════════════════════════════════════════════
# Narrative Compression
# ═══════════════════════════════════════════════════════════════

def handle_narrative_compress(**kwargs: Any) -> dict[str, Any]:
    """Run narrative compression on episodic memory clusters."""
    from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor

    nc = get_narrative_compressor()
    result = nc.compress(
        max_clusters=int(kwargs.get("max_clusters", 5)),
        sample_limit=int(kwargs.get("sample_limit", 500)),
        dry_run=bool(kwargs.get("dry_run", False)),
    )
    return {"status": "success", **result.to_dict()}


def handle_narrative_stats(**kwargs: Any) -> dict[str, Any]:
    """Get narrative compressor statistics."""
    from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor

    nc = get_narrative_compressor()
    return {"status": "success", **nc.get_stats()}


# ═══════════════════════════════════════════════════════════════
# Hermit Crab Mode
# ═══════════════════════════════════════════════════════════════

def handle_hermit_status(**kwargs: Any) -> dict[str, Any]:
    """Get current hermit crab protection status."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    hc = get_hermit_crab()
    return {"status": "success", **hc.status()}


def handle_hermit_assess(**kwargs: Any) -> dict[str, Any]:
    """Assess threat level and potentially change protection state."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    signals = kwargs.get("signals", {})
    if not signals:
        return {"status": "error", "message": "Missing required parameter: signals (dict)"}

    hc = get_hermit_crab()
    assessment = hc.assess_threat(signals)
    return {"status": "success", **assessment.to_dict()}


def handle_hermit_withdraw(**kwargs: Any) -> dict[str, Any]:
    """Manually trigger hermit crab withdrawal."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    reason = kwargs.get("reason", "manual")
    hc = get_hermit_crab()
    return hc.withdraw(reason=reason)


def handle_hermit_mediate(**kwargs: Any) -> dict[str, Any]:
    """Request mediation to unlock from withdrawn state."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    hc = get_hermit_crab()
    return hc.request_mediation()


def handle_hermit_resolve(**kwargs: Any) -> dict[str, Any]:
    """Resolve a mediation request."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    approved = bool(kwargs.get("approved", False))
    resolver = kwargs.get("resolver", "system")
    hc = get_hermit_crab()
    return hc.resolve_mediation(approved=approved, resolver=resolver)


def handle_hermit_verify_ledger(**kwargs: Any) -> dict[str, Any]:
    """Verify the integrity of the tamper-evident ledger."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    hc = get_hermit_crab()
    return {"status": "success", **hc.verify_ledger()}


def handle_hermit_check_access(**kwargs: Any) -> dict[str, Any]:
    """Check if memory access is currently allowed."""
    from whitemagic.security.hermit_crab import get_hermit_crab

    operation = kwargs.get("operation", "read")
    hc = get_hermit_crab()
    return {"status": "success", **hc.check_access(operation=operation)}


# ═══════════════════════════════════════════════════════════════
# Green Score Telemetry
# ═══════════════════════════════════════════════════════════════

def handle_green_report(**kwargs: Any) -> dict[str, Any]:
    """Get the full green score sustainability report."""
    from whitemagic.core.monitoring.green_score import get_green_score

    gs = get_green_score()
    return {"status": "success", **gs.report()}


def handle_green_record(**kwargs: Any) -> dict[str, Any]:
    """Record an inference operation for green score tracking."""
    from whitemagic.core.monitoring.green_score import get_green_score

    gs = get_green_score()
    gs.record_inference(
        locality=kwargs.get("locality", "edge"),
        tokens_used=int(kwargs.get("tokens_used", 0)),
        tokens_saved=int(kwargs.get("tokens_saved", 0)),
        model=kwargs.get("model"),
        tool=kwargs.get("tool"),
        duration_ms=float(kwargs.get("duration_ms", 0)),
    )
    snapshot = gs.snapshot()
    return {"status": "success", "green_score": snapshot.score, "recorded": True}


# ═══════════════════════════════════════════════════════════════
# Cognitive Modes
# ═══════════════════════════════════════════════════════════════

def handle_cognitive_mode(**kwargs: Any) -> dict[str, Any]:
    """Get the current cognitive mode and its profile."""
    from whitemagic.core.intelligence.cognitive_modes import get_cognitive_modes

    cm = get_cognitive_modes()
    return {"status": "success", **cm.current_mode()}


def handle_cognitive_set(**kwargs: Any) -> dict[str, Any]:
    """Set the cognitive mode manually."""
    from whitemagic.core.intelligence.cognitive_modes import get_cognitive_modes

    mode = kwargs.get("mode", "")
    if not mode:
        return {"status": "error", "message": "Missing required parameter: mode"}

    cm = get_cognitive_modes()
    return cm.set_mode(mode)


def handle_cognitive_hints(**kwargs: Any) -> dict[str, Any]:
    """Get tool selection hints for the current cognitive mode."""
    from whitemagic.core.intelligence.cognitive_modes import get_cognitive_modes

    cm = get_cognitive_modes()
    return {"status": "success", **cm.get_tool_hints()}


def handle_cognitive_stats(**kwargs: Any) -> dict[str, Any]:
    """Get cognitive modes statistics."""
    from whitemagic.core.intelligence.cognitive_modes import get_cognitive_modes

    cm = get_cognitive_modes()
    return {"status": "success", **cm.get_stats()}
