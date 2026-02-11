"""🕸️ Graph Engine - The Neural Nexus
Connects the Python NeuralMemory system to the Rust Graph Core.
Created: December 4, 2025.
"""

import logging

from whitemagic.core.memory.neural.rust_bridge import (
    build_memory_graph,
    parallel_traverse,
)
from whitemagic.core.memory.unified_types import Memory

logger = logging.getLogger(__name__)

class GraphEngine:
    """Manages the graph structure of memories using Rust acceleration.
    """

    def __init__(self) -> None:
        self._graph_cache: dict[str, list[tuple[str, float]]] = {}
        self._is_dirty = True

    def build_graph(self, memories: list[Memory], force: bool = False) -> dict[str, list[tuple[str, float]]]:
        """Build the full memory graph.
        """
        if not force and not self._is_dirty and self._graph_cache:
            return self._graph_cache

        logger.info(f"Building memory graph for {len(memories)} memories...")

        # Convert to tuples for Rust
        # Memory content might be Any, cast to str
        memory_tuples = [
            (m.id, str(m.content), list(m.tags))
            for m in memories
        ]

        # Build graph using Rust Bridge
        # Returns: {source_id: [(target_id, weight), ...]}
        self._graph_cache = build_memory_graph(
            memory_tuples,
            min_similarity=0.2,
            max_links=15,
        )

        self._is_dirty = False
        logger.info(f"Graph built with {len(self._graph_cache)} nodes")
        return self._graph_cache

    def get_cluster(self, start_id: str, memories: list[Memory], max_depth: int = 3) -> set[str]:
        """Get a cluster of related memories around a start node.
        """
        memory_tuples = [
            (m.id, str(m.content), list(m.tags))
            for m in memories
        ]

        return parallel_traverse(
            memory_tuples,
            start_id,
            max_depth=max_depth,
            min_similarity=0.3,
        )

    def invalidate(self) -> None:
        """Mark graph as dirty (needs rebuild)."""
        self._is_dirty = True

# Singleton
_graph_engine = None

def get_graph_engine() -> GraphEngine:
    global _graph_engine
    if _graph_engine is None:
        _graph_engine = GraphEngine()
    return _graph_engine
