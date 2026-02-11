"""Engine Framework — 28 Engines × 28 Gardens × 28 Grimoire Pages (Leap 7d).

Each cognitive engine in WhiteMagic maps 1:1 to:
- A Lunar Mansion (1-28)
- A Garden directory
- A Grimoire page
- A Gana class
- A Wu Xing element (via quadrant)

The Engine base class defines the contract that all engines follow:
- read from the StateBoard (Data Sea)
- produce actionable output
- emit events to the EventRing
"""

from whitemagic.core.engines.registry import (
    ENGINE_REGISTRY,
    EngineEntry,
    get_engine_entry,
    get_engines_by_quadrant,
    get_engines_by_status,
    get_engine_stats,
)

__all__ = [
    "ENGINE_REGISTRY",
    "EngineEntry",
    "get_engine_entry",
    "get_engines_by_quadrant",
    "get_engines_by_status",
    "get_engine_stats",
]
