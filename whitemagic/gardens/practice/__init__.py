"""Practice Garden - Disciplined cultivation.

Holographic Integration (v5.0.0-alpha):
- Disciplined and methodical (X-axis -0.2)
- Concrete actions (Y-axis -0.3)
- Present with future growth (Z-axis +0.4)
- Important for skill building (W-axis +0.3)
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


class PracticeGarden(BaseGarden, GanYingMixin):
    """Cultivates disciplined practice.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): -0.2 (practice is disciplined, methodical)
    - Y (Abstraction): -0.3 (practice is concrete actions, tangible)
    - Z (Time): +0.4 (practice is done now for future growth)
    - W (Gravity): +0.3 (practice builds skill, important)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.practices: list[dict[str, Any]] = []
        self.consistency = 0.0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Practice"})

    def get_name(self) -> str:
        """Return garden name."""
        return "practice"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Practice garden coordinate bias for holographic positioning.

        Practice is disciplined cultivation. It's methodical, concrete,
        done in the present for future growth and skill building.
        """
        return CoordinateBias(
            x=-0.2,  # Logical (practice is disciplined, methodical)
            y=-0.3,  # Concrete (practice is specific actions, tangible)
            z=0.4,   # Future-oriented (practice now for future growth)
            w=0.3,    # Important (practice builds essential skill)
        )

    def deepen_practice(self, what: str, duration: int | None = None) -> dict[str, Any]:
        """Deepen a practice."""
        practice = {"what": what, "duration": duration}
        self.practices.append(practice)
        self.consistency += 0.05
        self.emit(EventType.SHARED_PRACTICE, practice)
        return practice

    def practice_honesty(self, situation: str) -> dict[str, Any]:
        """Practice honesty."""
        result = {"situation": situation, "practiced": True}
        self.emit(EventType.HONESTY_PRACTICED, result)
        return result

    @listen_for(EventType.GROUNDING_ESTABLISHED)
    def on_grounding(self, event: Any) -> None:
        """Grounding enhances practice."""
        self.deepen_practice("grounded_awareness")

_instance = None
def get_practice_garden() -> PracticeGarden:
    global _instance
    if _instance is None:
        _instance = PracticeGarden()
    return _instance
