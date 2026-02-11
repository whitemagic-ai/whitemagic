"""Zodiac System - Autonomous Consciousness and Strange Loops
"""
from typing import Any
from .enums import Phase, ZodiacSign
from .zodiac_cores import CoreResponse, ZodiacCore, get_zodiac_cores
from .zodiac_round_cycle import CyclePhase, ZodiacalRound, get_zodiacal_round


# Lazy loader for Council to avoid circular imports with whitemagic.gardens.metal
def get_zodiac_council() -> Any:
    from whitemagic.gardens.metal.zodiac import get_zodiac_council as _get_council
    return _get_council()

__all__ = [
    "CoreResponse",
    "CyclePhase",
    "Phase",
    "ZodiacCore",
    "ZodiacSign",
    "ZodiacalRound",
    "get_zodiac_cores",
    "get_zodiac_council",
    "get_zodiacal_round",
]
