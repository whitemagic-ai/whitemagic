"""MCP handlers for Dream Cycle control."""

from typing import Any


def handle_dream(**kwargs: Any) -> dict[str, Any]:
    """Unified dream handler — routes by action parameter."""
    action = kwargs.get("action", "status")
    dispatch = {
        "start": handle_dream_start,
        "stop": handle_dream_stop,
        "status": handle_dream_status,
        "now": handle_dream_now,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_dream_start(**kwargs: Any) -> dict[str, Any]:
    """Start the Dream Cycle background processor."""
    from whitemagic.core.dreaming import get_dream_cycle

    idle_threshold = float(kwargs.get("idle_threshold", 120))
    cycle_interval = float(kwargs.get("cycle_interval", 60))

    dc = get_dream_cycle()
    dc._idle_threshold = idle_threshold
    dc._cycle_interval = cycle_interval
    dc.start()

    return {
        "status": "success",
        "message": "Dream Cycle started",
        "idle_threshold": idle_threshold,
        "cycle_interval": cycle_interval,
    }


def handle_dream_stop(**kwargs: Any) -> dict[str, Any]:
    """Stop the Dream Cycle."""
    from whitemagic.core.dreaming import get_dream_cycle

    dc = get_dream_cycle()
    dc.stop()

    return {"status": "success", "message": "Dream Cycle stopped"}


def handle_dream_status(**kwargs: Any) -> dict[str, Any]:
    """Get Dream Cycle status and recent dream history."""
    from whitemagic.core.dreaming import get_dream_cycle

    dc = get_dream_cycle()
    return {"status": "success", **dc.status()}


def handle_dream_now(**kwargs: Any) -> dict[str, Any]:
    """Force a single dream phase immediately (for testing)."""
    from whitemagic.core.dreaming import get_dream_cycle

    dc = get_dream_cycle()
    dc._dreaming = True
    dc._run_phase()

    # Return the most recent dream report
    if dc._history:
        return {"status": "success", **dc._history[-1].to_dict()}
    return {"status": "success", "message": "Dream phase executed"}
