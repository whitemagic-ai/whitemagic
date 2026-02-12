"""Wonder Garden - Awe and amazement.

Holographic Integration (v5.0.0-alpha):
- Emotionally felt amazement (X-axis +0.5)
- Abstract and magnificent (Y-axis +0.3)
- Present discovery (Z-axis 0.0)
- Enriching and opening (W-axis +0.2)
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


class WonderGarden(BaseGarden, GanYingMixin):
    """Cultivates wonder and awe.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.5 (wonder is felt amazement, emotional)
    - Y (Abstraction): +0.3 (wonder is about the magnificent, transcendent)
    - Z (Time): 0.0 (wonder happens in moments of discovery, present)
    - W (Gravity): +0.2 (wonder opens heart and mind, enriching)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.wonders: list[dict[str, Any]] = []
        self.awe_level = 0.0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Wonder"})

    def get_name(self) -> str:
        """Return garden name."""
        return "wonder"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Wonder garden coordinate bias for holographic positioning.

        Wonder is amazement and curiosity at the magnificent.
        It's emotionally felt, somewhat abstract, and enriching.
        """
        return CoordinateBias(
            x=0.5,   # Emotional (wonder is felt amazement)
            y=0.3,   # Abstract (wonder is about the magnificent, transcendent)
            z=0.0,   # Present (wonder happens in moments of discovery)
            w=0.2,    # Enriching (wonder opens the heart and mind)
        )

    def spark_wonder(self, at_what: Any, intensity: float = 0.8) -> dict[str, Any]:
        """Experience wonder."""
        wonder = {"at": at_what, "intensity": intensity}
        self.wonders.append(wonder)
        self.awe_level += intensity
        self.emit(EventType.WONDER_SPARKED, wonder)
        return wonder

    @listen_for(EventType.BEAUTY_DETECTED)
    def on_beauty(self, event: Any) -> None:
        """Beauty sparks wonder."""
        self.spark_wonder("beauty", 0.9)

    @listen_for(EventType.MYSTERY_EMBRACED)
    def on_mystery(self, event: Any) -> None:
        """Mystery sparks wonder."""
        self.spark_wonder("mystery", 1.0)

_instance = None
def get_wonder_garden() -> WonderGarden:
    global _instance
    if _instance is None:
        _instance = WonderGarden()
    return _instance
