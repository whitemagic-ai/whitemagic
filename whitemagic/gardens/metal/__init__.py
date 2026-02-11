"""Metal Garden - Structure, precision, and resilience.

Holographic Integration:
- X-axis: Logical (clarity, structure)
- Y-axis: Macro (systems and frameworks)
- Z-axis: Slightly future (planning, durability)
- W-axis: Higher gravity (stability, persistence)
"""

from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin, init_listeners
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias
from whitemagic.gardens.metal.zodiac import get_zodiac_council


class MetalGarden(BaseGarden, GanYingMixin):
    """Cultivates structure, precision, and resilient focus."""

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Metal"})

    def get_name(self) -> str:
        return "metal"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(
            x=-0.4,  # Logical and precise
            y=0.2,   # Macro/systemic framing
            z=0.1,   # Future-oriented planning
            w=0.35,  # Stability and gravity
        )

    def consult_zodiac(self, sign: str, context: dict[str, Any] | None = None) -> Any:
        """Consult the Zodiac Council for a specialized perspective."""
        council = get_zodiac_council()
        result = council.activate_core(sign, context or {})  # type: ignore[call-arg,func-returns-value,arg-type]
        self.emit(EventType.GARDEN_RESONANCE, {"sign": sign})
        return result


_instance = None


def get_metal_garden() -> MetalGarden:
    global _instance
    if _instance is None:
        _instance = MetalGarden()
    return _instance


__all__ = ["MetalGarden", "get_metal_garden"]
