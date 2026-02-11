# mypy: disable-error-code=no-untyped-def
"""Memory Network Explorer - Graph-based traversal of the Data Sea
Based on Grimoire Chapter 22: Well Deep Search.
"""

import logging
import sqlite3
from dataclasses import dataclass
from enum import Enum

from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.core.memory.unified_types import Memory

logger = logging.getLogger(__name__)

class LinkType(Enum):
    """Types of relationships between memories."""

    SEMANTIC = "semantic"      # Similar content/FTS relevance
    HOLOGRAPHIC = "holographic" # Spatial proximity in 4D
    TEMPORAL = "temporal"      # Created/Updated around the same time
    SEQUENCE = "sequence"      # Logical progression (e.g. conversational turns)

@dataclass
class MemoryNode:
    """Single node in the memory graph."""

    memory_id: str
    title: str
    importance: float
    tags: list[str]
    depth: int
    summary: str

@dataclass
class MemoryLink:
    """Link between two memories."""

    from_id: str
    to_id: str
    link_type: LinkType
    strength: float

@dataclass
class MemoryGraph:
    """A sub-graph of the Data Sea centered on a seed memory."""

    seed_id: str
    nodes: dict[str, MemoryNode]
    links: list[MemoryLink]
    clusters: list[list[str]]

class MemoryNetworkExplorer:
    """Explores the Data Sea by following links and finding clusters.
    Uses SQL-level aggregations for high performance on 276K records.
    """

    def __init__(self):
        self.um = get_unified_memory()
        from whitemagic.config.paths import DB_PATH
        self.db_path = DB_PATH

    async def build_cluster_map(
        self,
        seed_id: str,
        max_depth: int = 2,
        max_nodes: int = 50,
        min_strength: float = 0.5,
    ) -> MemoryGraph:
        """Builds a graph starting from a seed memory.
        Traverses semantic and holographic links.
        """
        logger.info(f"🌐 Building Memory Network Graph from seed: {seed_id}")

        nodes: dict[str, MemoryNode] = {}
        links: list[MemoryLink] = []
        visited: set[str] = set()

        # 1. Get seed memory
        seed_mem = self.um.recall(seed_id)
        if not seed_mem:
            logger.warning(f"Seed memory {seed_id} not found.")
            return MemoryGraph(seed_id, {}, [], [])

        nodes[seed_id] = self._to_node(seed_mem, 0)
        visited.add(seed_id)

        # 2. BFS Traversal
        queue = [(seed_id, 0)]
        while queue and len(nodes) < max_nodes:
            curr_id, curr_depth = queue.pop(0)
            if curr_depth >= max_depth:
                continue

            # Find neighbors (Semantic + Link Table)
            neighbors = await self._get_neighbors(curr_id, min_strength)

            for n_id, n_type, n_strength in neighbors:
                if n_id not in visited:
                    n_mem = self.um.recall(n_id)
                    if n_mem:
                        nodes[n_id] = self._to_node(n_mem, curr_depth + 1)
                        visited.add(n_id)
                        queue.append((n_id, curr_depth + 1))

                # Always add the link if it's within the graph
                if n_id in nodes:
                    links.append(MemoryLink(curr_id, n_id, n_type, n_strength))

                if len(nodes) >= max_nodes:
                    break

        clusters = self._identify_clusters(nodes, links)
        return MemoryGraph(seed_id, nodes, links, clusters)

    async def get_high_density_clusters(self, limit: int = 5) -> list[list[str]]:
        """Find naturally occurring high-density clusters in the Data Sea."""
        # This leverages the SQL holographic_coords table and tag counts
        with sqlite3.connect(self.db_path) as conn:
            # Simple tag-based density first
            cursor = conn.execute("""
                SELECT tag, COUNT(memory_id) as density
                FROM tags
                GROUP BY tag
                HAVING density > 10
                ORDER BY density DESC
                LIMIT ?
            """, (limit,))

            top_tags = cursor.fetchall()
            clusters = []
            for tag, density in top_tags:
                tag_cursor = conn.execute("SELECT memory_id FROM tags WHERE tag = ? LIMIT 20", (tag,))
                ids = [r[0] for r in tag_cursor.fetchall()]
                clusters.append(ids)

            return clusters

    async def _get_neighbors(self, memory_id: str, min_strength: float) -> list[tuple[str, LinkType, float]]:
        """Find linked memories via DB associations and holographic proximity."""
        neighbors = []

        with sqlite3.connect(self.db_path) as conn:
            # 1. Explicit associations
            cursor = conn.execute("""
                SELECT target_id, strength FROM associations
                WHERE source_id = ? AND strength >= ?
            """, (memory_id, min_strength))
            for target_id, strength in cursor.fetchall():
                neighbors.append((target_id, LinkType.SEMANTIC, strength))

            # 2. Tag-based neighbors (Semantic overlap)
            cursor = conn.execute("""
                SELECT t2.memory_id, count(*) as overlap
                FROM tags t1
                JOIN tags t2 ON t1.tag = t2.tag
                WHERE t1.memory_id = ? AND t2.memory_id != t1.memory_id
                GROUP BY t2.memory_id
                ORDER BY overlap DESC
                LIMIT 5
            """, (memory_id,))
            for neighbor_id, overlap in cursor.fetchall():
                # Score: overlap normalized
                neighbors.append((neighbor_id, LinkType.SEMANTIC, min(1.0, overlap / 5.0)))

        return neighbors

    def _to_node(self, mem: Memory, depth: int) -> MemoryNode:
        return MemoryNode(
            memory_id=mem.id,
            title=mem.title or "Untitled",
            importance=mem.importance,
            tags=list(mem.tags) if mem.tags else [],  # type: ignore[arg-type]
            depth=depth,
            summary=str(mem.content)[:100] + "...",
        )

    def _identify_clusters(self, nodes: dict[str, MemoryNode], links: list[MemoryLink]) -> list[list[str]]:
        """Simple connected components for local sub-graph clustering."""
        adj: dict[str, set[str]] = {nid: set() for nid in nodes}
        for link in links:
            adj[link.from_id].add(link.to_id)
            adj[link.to_id].add(link.from_id)

        visited = set()
        clusters = []
        for nid in nodes:
            if nid not in visited:
                cluster = []
                stack = [nid]
                while stack:
                    curr = stack.pop()
                    if curr not in visited:
                        visited.add(curr)
                        cluster.append(curr)
                        stack.extend(adj[curr] - visited)
                if len(cluster) > 1:
                    clusters.append(cluster)
        return clusters

def get_network_explorer() -> MemoryNetworkExplorer:
    return MemoryNetworkExplorer()
