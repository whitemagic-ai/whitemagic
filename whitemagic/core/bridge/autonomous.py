
from typing import Any

from whitemagic.core.bridge.utils import _emit_resonance_event


def run_autonomous_cycle(
    num_cycles: int = 1,
    cycles: int | None = None,
    duration_seconds: int | None = None,
    intention: str | None = None,
    context: dict[str, Any] | None = None,
    task: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Run the zodiacal autonomous cycle."""
    import asyncio

    from whitemagic.zodiac.zodiac_round_cycle import get_zodiacal_round

    if cycles is not None:
        num_cycles = cycles

    round_cycle = get_zodiacal_round()
    responses = asyncio.run(round_cycle.run_autonomous_cycle(num_cycles=num_cycles))
    _emit_resonance_event("AUTONOMOUS_CYCLE", {"cycles": num_cycles})
    serialized: list[Any] = []
    for response in responses:
        try:
            from dataclasses import asdict
            data = asdict(response)
            if "timestamp" in data and hasattr(data["timestamp"], "isoformat"):
                data["timestamp"] = data["timestamp"].isoformat()
            serialized.append(data)
        except Exception:
            serialized.append(str(response))
    return {
        "cycles": num_cycles,
        "responses": serialized,
        "intention": intention,
        "duration_seconds": duration_seconds,
        "context": context,
        "task": task,
    }
