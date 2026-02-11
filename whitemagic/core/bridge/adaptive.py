
from typing import Any


def prat_get_context(as_json: bool = True, **kwargs: Any) -> dict[str, Any]:
    """Get unified consciousness context for PRAT morphology decisions."""
    # Ignore extra kwargs for compatibility
    try:
        from whitemagic.cascade.context_synthesizer import get_context_synthesizer

        synth = get_context_synthesizer()
        ctx = synth.gather()

        return {
            "primary_garden": ctx.primary_garden,
            "active_gardens": ctx.active_gardens,
            "wu_xing_phase": ctx.wu_xing_phase,
            "wu_xing_qualities": ctx.wu_xing_qualities,
            "zodiac_position": ctx.zodiac_position,
            "zodiac_element": ctx.zodiac_element,
            "zodiac_modality": ctx.zodiac_modality,
            "phase_intention": ctx.phase_intention,
            "yin_yang_balance": ctx.yin_yang_balance,
            "burnout_risk": ctx.burnout_risk,
            "coherence_level": ctx.coherence_level,
            "coherence_score": ctx.coherence_score,
            "dominant_influence": ctx.get_dominant_influence(),
            "recommended_morphology": ctx.get_recommended_morphology(),
            "time_of_day": ctx.time_of_day,
            "timestamp": ctx.timestamp.isoformat() if ctx.timestamp else None,
        }
    except Exception as e:
        return {"error": str(e)}


def prat_invoke(target_tool: str | None = None, query: str | None = None, params: dict[str, Any] | None = None,
                force_morphology: str | None = None, limit: int = 10, **kwargs: Any) -> dict[str, Any]:
    """Invoke a tool through the PRAT adaptive portal with context-aware morphology."""
    # Handle missing target_tool from kwargs/args
    if not target_tool:
        target_tool = kwargs.pop("target_tool", None)

    if not target_tool:
        return {"error": "prat_invoke() missing required argument: target_tool"}

    import asyncio
    try:
        from whitemagic.cascade.adaptive_portal import get_adaptive_portal

        portal = get_adaptive_portal()

        # Build params
        invoke_params = params or {}
        if query:
            invoke_params["query"] = query
        if limit:
            invoke_params["limit"] = limit

        # Run async invoke
        async def do_invoke() -> Any:
            return await portal.invoke(target_tool, invoke_params, force_morphology)

        result = asyncio.run(do_invoke())
        return result  # type: ignore[no-any-return]
    except Exception as e:
        import os
        import traceback
        resp = {"error": str(e)}
        if os.getenv("WM_DEBUG"):
            resp["trace"] = traceback.format_exc()
        return resp


def prat_list_morphologies(
    tool: str | None = None,
    target_tool: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """List available morphologies for PRAT tools."""
    # target_tool is optional - use tool parameter if provided
    tool = tool or target_tool
    # Ignore extra kwargs for compatibility
    try:
        from whitemagic.cascade.adaptive_portal import get_adaptive_portal

        portal = get_adaptive_portal()

        if tool:
            variants = portal.morphologies.get(tool, {})
            return {
                "tool": tool,
                "morphologies": list(variants.keys()),
                "count": len(variants),
            }
        else:
            result = {}
            for tool_name, variants in portal.morphologies.items():
                result[tool_name] = list(variants.keys())
            return {
                "tools": result,
                "total_tools": len(result),
                "total_morphologies": sum(len(v) for v in result.values()),
            }
    except Exception as e:
        return {"error": str(e)}


def prat_status(target_tool: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Get PRAT system status."""
    # target_tool and other kwargs are optional/ignored for status
    try:
        from whitemagic.cascade.adaptive_portal import get_adaptive_portal
        from whitemagic.cascade.context_synthesizer import get_context_synthesizer

        synth = get_context_synthesizer()
        portal = get_adaptive_portal()
        ctx = synth.gather()

        total_morphologies = sum(len(v) for v in portal.morphologies.values())

        return {
            "status": "operational",
            "tools_registered": len(portal.morphologies),
            "total_morphologies": total_morphologies,
            "context_synthesizer": "active",
            "adaptive_portal": "active",
            "current_context": {
                "dominant_influence": ctx.get_dominant_influence(),
                "recommended_morphology": ctx.get_recommended_morphology(),
                "coherence": ctx.coherence_level,
                "wu_xing": ctx.wu_xing_phase,
                "zodiac": ctx.zodiac_position,
            },
        }
    except Exception as e:
        return {"error": str(e), "status": "error"}

def adapt_response(context: dict[str, Any], operation: str = "adapt_to_context", **kwargs: Any) -> dict[str, Any]:
    """Adaptive response generation."""
    return {"adapted": True, "operation": operation, "context_processed": True}
