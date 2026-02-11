"""Air Garden - Clarity, movement, and breath.

Holographic Integration:
- X-axis: Slightly logical (clarity over emotion)
- Y-axis: Macro (big-picture perspective)
- Z-axis: Present (breath and immediacy)
- W-axis: Light gravity (gentle uplift)
"""

from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin, init_listeners
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


class AirGarden(BaseGarden, GanYingMixin):
    """Cultivates clarity, movement, and spaciousness."""

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        init_listeners(self)
        self.last_breath: dict[str, Any] | None = None
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Air"})

    def get_name(self) -> str:
        return "air"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(
            x=-0.1,  # Slightly logical (clarity)
            y=0.4,   # Macro perspective
            z=0.0,   # Present-moment awareness
            w=0.1,   # Light uplift
        )

    def breathe(self, note: str = "") -> dict[str, Any]:
        """Record a breath and emit a calm signal."""
        payload = {"note": note}
        self.last_breath = payload
        self.emit(EventType.MINDFULNESS_ACHIEVED, payload)
        return payload

    def clear(self) -> dict[str, Any]:
        """Clear stale state and emit a clarity signal."""
        self.last_breath = None
        payload = {"cleared": True}
        self.emit(EventType.GARDEN_RESONANCE, payload)
        return payload


_instance = None


def get_air_garden() -> AirGarden:
    global _instance
    if _instance is None:
        _instance = AirGarden()
    return _instance


__all__ = ["AirGarden", "get_air_garden"]
