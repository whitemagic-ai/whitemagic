"""🔗 Memory Linking System - Zettelkasten-Style Connections.

Every memory connects to related memories, forming a knowledge graph.
Links strengthen when memories are accessed together (Hebbian learning).

Inspired by:
- Zettelkasten (slip-box) method
- A-MEM (Agentic Memory) paper
- Biological neural networks

Created: December 2, 2025 (Hanuman Tuesday)
"""

from whitemagic.core.memory.linking.auto_linker import (
    AutoLinker,
    create_memory_with_links,
    find_similar_memories,
    get_linker,
)
from whitemagic.core.memory.linking.cascade import (
    CascadeDiscovery,
    create_cascade_link,
    discover_cascade_links,
)
from whitemagic.core.memory.linking.strength_tracker import (
    StrengthTracker,
    get_recent_accesses,
    on_co_access,
)

__all__ = [
    "AutoLinker",
    "find_similar_memories",
    "create_memory_with_links",
    "get_linker",
    "CascadeDiscovery",
    "discover_cascade_links",
    "create_cascade_link",
    "StrengthTracker",
    "on_co_access",
    "get_recent_accesses",
]
