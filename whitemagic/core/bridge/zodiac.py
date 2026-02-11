
from typing import Any, cast


def zodiac_list_cores(**kwargs: Any) -> dict[str, Any]:
    """List all 12 zodiac cores."""
    from whitemagic.zodiac.zodiac_cores import get_zodiac_cores
    cores_obj = get_zodiac_cores()

    result = []
    for sign in ["aries", "taurus", "gemini", "cancer", "leo", "virgo",
                 "libra", "scorpio", "sagittarius", "capricorn", "aquarius", "pisces"]:
        core = cores_obj.get_core(sign)
        if core:
            result.append({
                "name": sign,
                "element": core.element,
                "mode": core.mode,
                "ruler": core.ruler,
                "frequency": core.frequency,
                "activation_count": core.activation_count,
            })

    return {"cores": result}


def zodiac_activate_core(core_name: str, context: dict[str, Any] | None = None, **kwargs: Any) -> dict[str, Any]:
    """Activate a specific zodiac core."""
    from whitemagic.zodiac.zodiac_cores import get_zodiac_cores
    cores = get_zodiac_cores()

    ctx = context or {}
    response = cores.activate_core(core_name, ctx)

    if response:
        return {
            "core": core_name,
            "wisdom": response.wisdom,
            "resonance": response.resonance,
            "transformation": response.transformation_applied,
        }
    return {"error": f"Core not found: {core_name}"}


def zodiac_consult_council(query: str, context: dict[str, Any] | None = None, **kwargs: Any) -> dict[str, Any]:
    """Consult the full zodiac council."""
    from whitemagic.gardens.metal.zodiac import consult_council
    result = consult_council(query, context or {})
    return cast("dict[str, Any]", result)


def zodiac_run_cycle(intention: str | None = None, context: dict[str, Any] | None = None, **kwargs: Any) -> dict[str, Any]:
    """Run one zodiacal cycle."""
    from whitemagic.zodiac.zodiac_round_cycle import run_one_cycle
    ctx = context or {}
    if intention:
        ctx["intention"] = intention
    # Need to run async function in sync wrapper
    import asyncio
    result = asyncio.run(run_one_cycle(ctx))
    return {"result": str(result)}


def manage_zodiac_cores(
    operation: str = "list",
    core_name: str | None = None,
    question: str | None = None,
    context: dict[str, Any] | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Unified zodiac core management."""
    operation = (operation or "list").lower()

    if operation == "list":
        return zodiac_list_cores(**kwargs)
    elif operation == "activate":
        if not core_name:
            return {"error": "core_name required for activate"}
        return zodiac_activate_core(core_name, context, **kwargs)
    elif operation == "council":
        if not question:
            return {"error": "question required for council operation"}
        return zodiac_consult_council(question, context, **kwargs)
    else:
        return {"error": f"Unknown zodiac operation: {operation}"}
