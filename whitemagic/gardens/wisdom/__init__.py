"""Wisdom Garden - Deep understanding and insight.

Holographic Integration (v5.0.0-alpha):
- Balanced logic/emotion (X-axis -0.1)
- Strongly abstract/macro (Y-axis +0.7)
- Timeless quality (Z-axis -0.1)
- High importance (W-axis +0.3)
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


class WisdomGarden(BaseGarden, GanYingMixin):
    """Cultivates wisdom and deep understanding.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): -0.1 (slightly logical, but balanced - wisdom transcends)
    - Y (Abstraction): +0.7 (highly abstract - principles, universals)
    - Z (Time): -0.1 (timeless - wisdom endures)
    - W (Gravity): +0.3 (wisdom is important!)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.insights: list[dict[str, Any]] = []
        self.wisdom_level = 0.0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Wisdom"})

    def get_name(self) -> str:
        """Return garden name."""
        return "wisdom"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Wisdom garden coordinate bias for holographic positioning.

        Wisdom is abstract, universal, timeless - high on Y-axis.
        Neither purely logical nor emotional - balanced on X-axis.
        Important and enduring - high W-axis, timeless Z-axis.
        """
        return CoordinateBias(
            x=-0.1,  # Balanced, slightly logical (but wisdom transcends)
            y=0.7,   # Highly abstract (principles, patterns, universals)
            z=-0.1,  # Timeless (wisdom endures across ages)
            w=0.3,    # Important (wisdom guides actions)
        )

    def gain_wisdom(self, insight: Any, source: str | None = None) -> dict[str, Any]:
        """Gain wisdom from experience."""
        wisdom = {"insight": insight, "source": source}
        self.insights.append(wisdom)
        self.wisdom_level += 0.1
        self.emit(EventType.WISDOM_INTEGRATED, wisdom)
        return wisdom

    @listen_for(EventType.TRUTH_SPOKEN)
    def on_truth(self, event: Any) -> None:
        """Truth leads to wisdom."""
        self.gain_wisdom(f"Truth understood: {event.data}", "truth")

    @listen_for(EventType.PATTERN_DETECTED)
    def on_pattern(self, event: Any) -> None:
        """Patterns reveal wisdom."""
        self.gain_wisdom("Pattern wisdom", "pattern_recognition")

_instance = None
def get_wisdom_garden() -> WisdomGarden:
    global _instance
    if _instance is None:
        _instance = WisdomGarden()
    return _instance
