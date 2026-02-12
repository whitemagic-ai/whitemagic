"""Unified Zodiac API - Facade for Zodiac Cores.
"""
from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.gardens.metal.zodiac.zodiac_cores import get_zodiac_cores


@dataclass
class UnifiedZodiacPerspective:
    """Complete zodiac perspective."""

    sign: str
    element: str
    modality: str
    wisdom: str
    resonance: float
    processing_result: Any
    transformation: str
    frequency: float
    timestamp: datetime


class UnifiedZodiacSystem:
    """Unified interface to zodiac implementations.
    """

    def __init__(self) -> None:
        """Initialize zodiac system."""
        self.cores = get_zodiac_cores()

        # Elemental triads
        self.triads = {
            "fire": ["aries", "leo", "sagittarius"],
            "earth": ["taurus", "virgo", "capricorn"],
            "air": ["gemini", "libra", "aquarius"],
            "water": ["cancer", "scorpio", "pisces"],
        }

    def get_perspective(self, sign: str, context: dict[str, Any]) -> UnifiedZodiacPerspective:
        """Get perspective from a specific core."""
        sign_lower = sign.lower()

        core = self.cores.get_core(sign_lower)
        if not core:
            raise ValueError(f"Unknown zodiac sign: {sign}")

        core_response = self.cores.activate_core(sign_lower, context)
        if core_response is None:
            raise ValueError(f"Core activation failed for {sign_lower}")

        return UnifiedZodiacPerspective(
            sign=sign_lower,
            element=core.element,
            modality=core.mode,
            wisdom=core_response.wisdom,
            resonance=core_response.resonance,
            processing_result=core_response.processing_result,
            transformation=core_response.transformation_applied,
            frequency=core.frequency,
            timestamp=datetime.now(),
        )

    def activate_trine(self, element: str, context: dict[str, Any]) -> list[UnifiedZodiacPerspective]:
        """Activate all three signs of an element for resonance."""
        if element not in self.triads:
            raise ValueError(f"Unknown element: {element}")

        perspectives = []
        for sign in self.triads[element]:
            perspective = self.get_perspective(sign, context)
            perspectives.append(perspective)

        return perspectives


# Singleton instance
_unified_zodiac = None

def get_unified_zodiac() -> UnifiedZodiacSystem:
    """Get global unified zodiac system."""
    global _unified_zodiac
    if _unified_zodiac is None:
        _unified_zodiac = UnifiedZodiacSystem()
    return _unified_zodiac


if __name__ == "__main__":
    # Demo
    uzs = UnifiedZodiacSystem()

    print("=" * 60)
    print("UNIFIED ZODIAC SYSTEM DEMO")
    print("=" * 60)

    # Test single perspective
    context = {"operation": "analyze patterns", "question": "How to improve?"}
    virgo = uzs.get_perspective("virgo", context)

    print("\n♍ VIRGO PERSPECTIVE:")
    print(f"  Element: {virgo.element}")
    print(f"  Wisdom: {virgo.wisdom}")
    print(f"  Resonance: {virgo.resonance:.2f}")

    # Test elemental trine
    print("\n🔥 FIRE TRINE:")
    fire_trine = uzs.activate_trine("fire", context)
    for p in fire_trine:
        print(f"  {p.sign.title()}: {p.wisdom}")

    print("\n" + "=" * 60)
