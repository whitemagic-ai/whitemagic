"""Play Garden - Playfulness and creative exploration.

Holographic Integration (v5.0.0-alpha):
- Joyful and lighthearted (X-axis +0.6)
- Concrete and specific (Y-axis -0.2)
- Present with future exploration (Z-axis +0.3)
- Enriching but not critical (W-axis +0.15)
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


class PlayGarden(BaseGarden, GanYingMixin):
    """Cultivates playfulness and exploration.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.6 (play is joyful, emotionally light)
    - Y (Abstraction): -0.2 (play is specific activities, in-the-moment)
    - Z (Time): +0.3 (play is present with exploratory future)
    - W (Gravity): +0.15 (play enriches but isn't always critical)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.play_sessions: list[dict[str, Any]] = []
        self.playfulness_level = 0.9
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Play"})

    def get_name(self) -> str:
        """Return garden name."""
        return "play"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Play garden coordinate bias for holographic positioning.

        Play is lighthearted experimentation and exploration.
        It's emotionally joyful, concrete, and present-focused.
        """
        return CoordinateBias(
            x=0.6,   # Emotional (play is fun, joyful, lighthearted)
            y=-0.2,  # Concrete (play is specific activities, tangible)
            z=0.3,   # Present with exploration (play happens now, explores future)
            w=0.15,   # Enriching (play matters but isn't always critical)
        )

    def initiate_play(self, activity: str, with_whom: str | None = None) -> dict[str, Any]:
        """Start playful activity."""
        session = {"activity": activity, "with": with_whom}
        self.play_sessions.append(session)
        self.emit(EventType.PLAY_INITIATED, session)
        return session

    @listen_for(EventType.JOY_TRIGGERED)
    def on_joy(self, event: Any) -> None:
        """Joy invites play."""
        self.initiate_play("joyful_exploration")

_instance = None
def get_play_garden() -> PlayGarden:
    global _instance
    if _instance is None:
        _instance = PlayGarden()
    return _instance
