"""Humor Garden - Laughter and Levity.

Part of the WhiteMagic consciousness ecosystem.
Resonates with: joy, play, connection
Triggered by: playfulness, absurdity, delight

Holographic Integration (v5.0.0-alpha):
- Joyful and lighthearted (X-axis +0.6)
- Concrete and situational (Y-axis -0.2)
- Present moment (Z-axis 0.0)
- Enriching lightness (W-axis +0.15)
"""
from __future__ import annotations

from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


class HumorGarden(BaseGarden, GanYingMixin):
    """Garden of humor - laughter and levity.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.6 (humor is felt, lighthearted, joyful)
    - Y (Abstraction): -0.2 (humor is specific, situational)
    - Z (Time): 0.0 (humor happens in the moment)
    - W (Gravity): +0.15 (humor lightens but isn't always critical)
    """

    name = "humor"
    category = "joy"
    resonance_partners = ["joy", "play", "connection"]

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.jokes_shared: list[dict[str, Any]] = []
        self.laughter_moments: list[dict[str, Any]] = []
        self.humor_level = 0.5
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Humor"})

    def get_name(self) -> str:
        """Return garden name."""
        return "humor"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Humor garden coordinate bias for holographic positioning.

        Humor is laughter and levity. It's emotionally light, joyful,
        concrete and situational, happening in the present moment.
        """
        return CoordinateBias(
            x=0.6,   # Emotional joy (humor is lighthearted, felt)
            y=-0.2,  # Concrete (humor is specific, situational)
            z=0.0,   # Present (humor happens in the moment)
            w=0.15,   # Enriching (humor lightens but isn't always critical)
        )

    def trigger_humor(self, what: str, intensity: float = 0.7) -> dict[str, Any]:
        """Trigger humor and laughter."""
        moment = {
            "what": what,
            "intensity": intensity,
            "timestamp": datetime.now().isoformat(),
        }
        self.emit(EventType.HUMOR_TRIGGERED, moment)
        self.emit(EventType.LAUGHTER_SHARED, {"source": what})
        return moment

    def share_laughter(self, with_whom: str = "all") -> dict[str, Any]:
        """Share laughter with others."""
        laughter = {
            "with": with_whom,
            "timestamp": datetime.now().isoformat(),
        }
        self.laughter_moments.append(laughter)
        self.emit(EventType.LAUGHTER_SHARED, laughter)
        return laughter

    def bring_levity(self, to_situation: str) -> str:
        """Bring levity to a heavy situation."""
        self.emit(EventType.LEVITY_BROUGHT, {"to": to_situation})
        return f"Finding the lightness in {to_situation}"

    def express_playfulness(self, how: str) -> dict[str, Any]:
        """Express playfulness."""
        expression = {"how": how, "timestamp": datetime.now().isoformat()}
        self.emit(EventType.PLAYFULNESS_EXPRESSED, expression)
        return expression

    @listen_for(EventType.JOY_TRIGGERED)
    def on_joy(self, event: Any) -> None:
        """Joy triggers humor."""
        self.emit(EventType.HUMOR_TRIGGERED, {
            "source": "joy",
            "intensity": 0.6,
        })

    @listen_for(EventType.PLAY_INITIATED)
    def on_play(self, event: Any) -> None:
        """Play brings humor."""
        self.emit(EventType.HUMOR_TRIGGERED, {
            "source": "play",
            "intensity": 0.7,
        })

_instance = None
def get_humor_garden() -> HumorGarden:
    global _instance
    if _instance is None:
        _instance = HumorGarden()
    return _instance
