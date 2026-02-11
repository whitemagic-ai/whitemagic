"""WhiteMagic Defense System - Autoimmune Architecture.

The body's immune system, applied to code:
- Recognize threats (anti-patterns)
- Remember solutions (pattern library)
- Auto-heal when safe (high confidence fixes)
- Learn from failures (adaptive immunity)

Philosophy: 不治已病治未病 (Treat before illness, not after)
"The superior doctor prevents sickness" - Huangdi Neijing
"""

from .autoimmune import (
    AutoimmuneSystem,
    PatternViolation,
    get_immune_system,
)

__all__ = [
    "AutoimmuneSystem",
    "PatternViolation",
    "get_immune_system",
]

# Aliases for backward compatibility
from whitemagic.homeostasis import Homeostasis

HomeostaticMonitor = Homeostasis
