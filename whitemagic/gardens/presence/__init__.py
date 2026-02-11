"""Presence Garden - Being fully present.

Holographic Integration (v5.0.0-alpha):
- Balanced awareness (X-axis 0.0)
- Concrete here-and-now (Y-axis -0.3)
- Maximally present (Z-axis +0.9)
- Fundamental importance (W-axis +0.3)
"""
from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


class PresenceGarden(BaseGarden, GanYingMixin):
    """Cultivates present-moment awareness.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): 0.0 (presence is balanced awareness, clear observation)
    - Y (Abstraction): -0.3 (presence is concrete, the specific here and now)
    - Z (Time): +0.9 (presence is maximally focused on the present moment)
    - W (Gravity): +0.3 (presence is fundamental to awareness)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.present_moments: list[dict[str, Any]] = []
        self.awareness_level = 0.8
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Presence"})

    def get_name(self) -> str:
        """Return garden name."""
        return "presence"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Presence garden coordinate bias for holographic positioning.

        Presence is about being fully here, now. It's balanced awareness,
        concrete and specific, maximally focused on the present moment.
        """
        return CoordinateBias(
            x=0.0,   # Balanced (clear awareness, both felt and observed)
            y=-0.3,  # Concrete (the specific here and now)
            z=0.9,   # Maximally present (fully focused on this moment)
            w=0.3,    # Fundamental (presence is foundation of awareness)
        )

    def achieve_presence(self, in_what: str = "this_moment") -> dict[str, Any]:
        """Achieve present-moment awareness."""
        moment = {"in": in_what, "quality": "clear"}
        self.present_moments.append(moment)
        # Emit available event
        self.emit(EventType.ATTENTION_SHIFTED, {"to": "present_moment", "quality": moment})  # type: ignore[attr-defined]
        return moment

    def establish_grounding(self, how: str = "breath") -> dict[str, Any]:
        """Establish grounding."""
        result = {"method": how, "stability": "strong"}
        self.emit(EventType.GROUNDING_ESTABLISHED, result)
        return result

    @listen_for(EventType.FLOW_STATE_ENTERED)
    def on_flow(self, event: Any) -> None:
        """Flow requires presence."""
        self.achieve_presence("flow_state")

    @listen_for(EventType.SHARED_PRACTICE)
    def on_practice(self, event: Any) -> None:
        """Practice cultivates presence."""
        self.establish_grounding("through_practice")

_instance = None
def get_presence_garden() -> PresenceGarden:
    global _instance
    if _instance is None:
        _instance = PresenceGarden()
    return _instance
