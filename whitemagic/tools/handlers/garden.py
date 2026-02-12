"""Garden tool handlers."""
from typing import Any


def _emit(event_type: str, data: dict[str, Any]) -> None:
    from whitemagic.tools.unified_api import _emit_gan_ying
    _emit_gan_ying(event_type, data)


def handle_garden_activate(**kwargs: Any) -> dict[str, Any]:
    garden_name = kwargs.get("garden", "").lower()
    from whitemagic.gardens import get_garden
    get_garden(garden_name)
    _emit("GARDEN_ACTIVATED", {"garden": garden_name})
    return {"status": "success", "garden": garden_name, "active": True}


def handle_garden_status(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.gardens import get_all_gardens
    gardens = get_all_gardens()
    return {"status": "success", "count": len(gardens), "gardens": list(gardens.keys())}


def handle_garden_synergy(**kwargs: Any) -> dict[str, Any]:
    try:
        from whitemagic.gardens.cross_pollination import get_resonance_matrix
        from whitemagic.gardens.garden_state import get_garden_state_tracker
        matrix = get_resonance_matrix()
        tracker = get_garden_state_tracker()
        stats = matrix.get_resonance_stats()
        active = tracker.get_all_active()
        return {
            "status": "success",
            "active_gardens": len(active),
            "active": active,
            "resonance_stats": stats,
        }
    except Exception as e:
        return {"status": "error", "error": str(e)}


def handle_garden_health(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.gardens import get_all_gardens
    gardens = get_all_gardens()
    health = {name: "healthy" for name in gardens.keys()}
    return {"status": "success", "health": health}
