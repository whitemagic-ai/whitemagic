"""🧠 Neural Memory System - Hebbian Learning for AI Memory.

"Neurons that fire together wire together"

This module implements a biologically-inspired memory system where:
- Memories strengthen with use (recall boosts neuro_score)
- Memories weaken without use (decay over time)
- Core identity memories are protected from decay
- Memories link to related memories (Zettelkasten-style)
- Connection strength grows with co-activation

Created: December 2, 2025 (Hanuman Tuesday)
Version: 5.0.0-alpha "Neural Awakening"
"""

from whitemagic.core.memory.neural.identity_anchors import (
    CORE_IDENTITY_PATTERNS,
    SACRED_PATTERNS,
    IdentityAnchor,
    auto_protect_memory,
    is_core_identity,
    is_sacred,
)
from whitemagic.core.memory.neural.neural_memory import (
    LinkType,
    MemoryState,
    NeuralMemory,
)
from whitemagic.core.memory.neural.neuro_score import (
    NeuroScoreEngine,
    calculate_neuro_score,
)
from whitemagic.core.memory.neural.neuro_score import (
    get_engine as get_neuro_engine,
)

__all__ = [
    # Core classes
    "NeuralMemory",
    "LinkType",
    "MemoryState",
    "NeuroScoreEngine",
    "IdentityAnchor",

    # Functions
    "calculate_neuro_score",
    "get_neuro_engine",
    "auto_protect_memory",
    "is_core_identity",
    "is_sacred",

    # Constants
    "CORE_IDENTITY_PATTERNS",
    "SACRED_PATTERNS",
]
