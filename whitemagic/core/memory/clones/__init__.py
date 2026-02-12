"""🥷 Shadow Clone Memory System - Kage Bunshin no Jutsu!

Deploy thousands of specialized clones for parallel memory search.
Each clone type specializes in a different format or domain.

Inspired by Naruto's Shadow Clone technique - massive parallelism
for exhaustive search and consensus-based results.

Created: December 2, 2025 (Hanuman Tuesday)
"""

from whitemagic.core.memory.clones.clone_army import (
    CloneArmy,
    ConsensusResult,
    SearchResult,
    deploy_introspection,
    deploy_memory_search,
)
from whitemagic.core.memory.clones.clone_types import CloneSpec, CloneType

__all__ = [
    "CloneType",
    "CloneSpec",
    "CloneArmy",
    "SearchResult",
    "ConsensusResult",
    "deploy_memory_search",
    "deploy_introspection",
]
