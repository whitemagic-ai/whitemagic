"""Graph Walker — Multi-Hop Weighted Traversal of the Association Graph.
====================================================================
The #1 gap identified by 4 independent research teams: WhiteMagic has
19M associations that are created but NEVER walked. All retrieval is
stateless single-hop. This module changes that.

The GraphWalker loads neighbors from the associations table, computes
transition probabilities from four signals (semantic similarity, galactic
gravity, recency, and staleness), and performs k-hop weighted random walks
from anchor nodes.

Usage:
    from whitemagic.core.memory.graph_walker import get_graph_walker
    walker = get_graph_walker()

    # Walk 2 hops from a seed memory
    paths = walker.walk(seed_ids=["abc123"], hops=2, top_k=5)

    # Hybrid recall: anchor search + graph expansion
    results = walker.hybrid_recall(query="memory consolidation", hops=2)
"""

from __future__ import annotations

import logging
import sqlite3
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class WalkPath:
    """A single traversal path through the association graph."""

    nodes: list[str]  # memory IDs in traversal order
    edge_weights: list[float]  # strength of each edge traversed
    relation_types: list[str]  # relation_type of each edge
    total_score: float = 0.0  # cumulative transition probability
    depth: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "nodes": self.nodes,
            "edge_weights": [round(w, 4) for w in self.edge_weights],
            "relation_types": self.relation_types,
            "total_score": round(self.total_score, 4),
            "depth": self.depth,
        }


@dataclass
class WalkResult:
    """Result of a graph walk operation."""

    seed_ids: list[str]
    hops: int
    paths_explored: int = 0
    unique_nodes_visited: int = 0
    paths: list[WalkPath] = field(default_factory=list)
    duration_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "seed_ids": self.seed_ids,
            "hops": self.hops,
            "paths_explored": self.paths_explored,
            "unique_nodes_visited": self.unique_nodes_visited,
            "duration_ms": round(self.duration_ms, 1),
            "paths": [p.to_dict() for p in self.paths],
        }

    def discovered_ids(self) -> set[str]:
        """All unique memory IDs discovered (excluding seeds)."""
        seeds = set(self.seed_ids)
        all_nodes: set[str] = set()
        for path in self.paths:
            all_nodes.update(path.nodes)
        return all_nodes - seeds


@dataclass
class Neighbor:
    """A neighbor node in the association graph."""

    memory_id: str
    strength: float
    direction: str
    relation_type: str
    edge_type: str
    traversal_count: int
    created_at: str | None
    last_traversed_at: str | None


class GraphWalker:
    """Multi-hop weighted graph traversal engine.

    Transition probability for edge (u → v):
        P(v|u) ∝ Strength × Gravity^α × Recency × (1 - Staleness)^β

    Where:
        - Strength: association edge weight [0, 1]
        - Gravity: 1 / (1 + galactic_distance_v), attracting toward CORE
        - Recency: 1 / (1 + days_since_edge_creation)
        - Staleness: traversal_count / max_traversals (penalize over-walked paths)
    """

    def __init__(
        self,
        gravity_alpha: float = 0.5,
        staleness_beta: float = 0.3,
        max_paths_per_hop: int = 10,
        min_edge_strength: float = 0.05,
    ) -> None:
        self._gravity_alpha = gravity_alpha
        self._staleness_beta = staleness_beta
        self._max_paths = max_paths_per_hop
        self._min_strength = min_edge_strength
        self._lock = threading.Lock()
        self._total_walks = 0
        self._total_nodes_visited = 0

    # ------------------------------------------------------------------
    # Neighbor loading
    # ------------------------------------------------------------------

    def _get_neighbors(self, memory_id: str, pool: Any) -> list[Neighbor]:
        """Load all outgoing association edges for a memory."""
        try:
            with pool.connection() as conn:
                conn.row_factory = sqlite3.Row
                rows = conn.execute(
                    """SELECT target_id, strength,
                              COALESCE(direction, 'undirected') as direction,
                              COALESCE(relation_type, 'associated_with') as relation_type,
                              COALESCE(edge_type, 'semantic') as edge_type,
                              COALESCE(traversal_count, 0) as traversal_count,
                              created_at, last_traversed_at
                       FROM associations
                       WHERE source_id = ? AND strength >= ?
                       ORDER BY strength DESC
                       LIMIT 50""",
                    (memory_id, self._min_strength),
                ).fetchall()
                return [
                    Neighbor(
                        memory_id=row["target_id"],
                        strength=row["strength"],
                        direction=row["direction"],
                        relation_type=row["relation_type"],
                        edge_type=row["edge_type"],
                        traversal_count=row["traversal_count"],
                        created_at=row["created_at"],
                        last_traversed_at=row["last_traversed_at"],
                    )
                    for row in rows
                ]
        except Exception as e:
            logger.debug(f"GraphWalker: failed to load neighbors for {memory_id}: {e}")
            return []

    def _get_galactic_distance(self, memory_id: str, pool: Any) -> float:
        """Get galactic distance for gravity calculation."""
        try:
            with pool.connection() as conn:
                row = conn.execute(
                    "SELECT galactic_distance FROM memories WHERE id = ?",
                    (memory_id,),
                ).fetchone()
                if row:
                    return row[0] or 0.5
        except Exception:
            pass
        return 0.5

    # ------------------------------------------------------------------
    # Transition probability
    # ------------------------------------------------------------------

    def _transition_score(
        self,
        neighbor: Neighbor,
        target_galactic_dist: float,
        max_traversals: int,
    ) -> float:
        """Compute transition probability for an edge.

        P(v|u) ∝ Strength × Gravity^α × Recency × (1 - Staleness)^β
        """
        # Strength: raw edge weight
        strength = max(0.001, neighbor.strength)

        # Gravity: attract toward galactic CORE
        gravity = 1.0 / (1.0 + target_galactic_dist)

        # Recency: favor recently created edges
        recency = 1.0
        if neighbor.created_at:
            try:
                created = datetime.fromisoformat(neighbor.created_at)
                days_old = max(0.0, (datetime.now() - created).total_seconds() / 86400.0)
                recency = 1.0 / (1.0 + days_old * 0.01)  # gentle decay
            except Exception:
                pass

        # Staleness: penalize over-traversed paths (encourage exploration)
        staleness = 0.0
        if max_traversals > 0 and neighbor.traversal_count > 0:
            staleness = min(1.0, neighbor.traversal_count / max(1, max_traversals))

        score = (
            strength
            * (gravity ** self._gravity_alpha)
            * recency
            * ((1.0 - staleness) ** self._staleness_beta)
        )
        return float(max(0.0001, score))

    # ------------------------------------------------------------------
    # Walk
    # ------------------------------------------------------------------

    def walk(
        self,
        seed_ids: list[str],
        hops: int = 2,
        top_k: int = 5,
        allowed_relations: set[str] | None = None,
        allowed_directions: set[str] | None = None,
    ) -> WalkResult:
        """Perform weighted random walk from seed nodes.

        Args:
            seed_ids: Starting memory IDs for the walk.
            hops: Number of hops to traverse.
            top_k: Return the top-K highest-scoring paths.
            allowed_relations: If set, only traverse these relation types.
            allowed_directions: If set, only traverse these directions.

        Returns:
            WalkResult with discovered paths and nodes.
        """
        start = time.perf_counter()
        result = WalkResult(seed_ids=seed_ids, hops=hops)

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            pool = um.backend.pool
        except Exception as e:
            logger.error(f"GraphWalker: could not access memory system: {e}")
            result.duration_ms = (time.perf_counter() - start) * 1000
            return result

        visited: set[str] = set()
        all_paths: list[WalkPath] = []

        # Get max traversal count for staleness normalization
        max_traversals = self._get_max_traversals(pool)

        # Initialize frontier with seed nodes
        frontier: list[WalkPath] = []
        for sid in seed_ids:
            frontier.append(WalkPath(
                nodes=[sid],
                edge_weights=[],
                relation_types=[],
                total_score=1.0,
                depth=0,
            ))
            visited.add(sid)

        for hop in range(hops):
            next_frontier: list[WalkPath] = []

            for path in frontier:
                current_id = path.nodes[-1]
                neighbors = self._get_neighbors(current_id, pool)

                if not neighbors:
                    continue

                # Filter by allowed relations/directions
                if allowed_relations:
                    neighbors = [n for n in neighbors if n.relation_type in allowed_relations]
                if allowed_directions:
                    neighbors = [n for n in neighbors if n.direction in allowed_directions]

                if not neighbors:
                    continue

                # Compute transition scores
                scored: list[tuple[Neighbor, float]] = []
                for n in neighbors:
                    gdist = self._get_galactic_distance(n.memory_id, pool)
                    score = self._transition_score(n, gdist, max_traversals)
                    scored.append((n, score))

                # Normalize to probabilities
                total_score = sum(s for _, s in scored)
                if total_score <= 0:
                    continue

                # Select top neighbors (weighted, not pure random)
                scored.sort(key=lambda x: x[1], reverse=True)
                selected = scored[:self._max_paths]

                for neighbor, score in selected:
                    prob = score / total_score
                    new_path = WalkPath(
                        nodes=path.nodes + [neighbor.memory_id],
                        edge_weights=path.edge_weights + [neighbor.strength],
                        relation_types=path.relation_types + [neighbor.relation_type],
                        total_score=path.total_score * prob,
                        depth=hop + 1,
                    )
                    next_frontier.append(new_path)
                    visited.add(neighbor.memory_id)
                    result.paths_explored += 1

                    # Record traversal for staleness tracking
                    self._record_traversal(current_id, neighbor.memory_id, pool)

            # Keep best paths for next hop
            next_frontier.sort(key=lambda p: p.total_score, reverse=True)
            frontier = next_frontier[:self._max_paths * len(seed_ids)]
            all_paths.extend(frontier)

        # Select top-K paths by score
        all_paths.sort(key=lambda p: p.total_score, reverse=True)
        result.paths = all_paths[:top_k]
        result.unique_nodes_visited = len(visited)

        elapsed = (time.perf_counter() - start) * 1000
        result.duration_ms = elapsed

        with self._lock:
            self._total_walks += 1
            self._total_nodes_visited += len(visited)

        logger.info(
            f"🔍 Graph walk: {len(seed_ids)} seeds × {hops} hops → "
            f"{len(visited)} nodes, {result.paths_explored} edges traversed ({elapsed:.0f}ms)",
        )
        return result

    # ------------------------------------------------------------------
    # Hybrid recall: anchor search + graph expansion
    # ------------------------------------------------------------------

    def hybrid_recall(
        self,
        query: str,
        hops: int = 2,
        anchor_limit: int = 5,
        walk_top_k: int = 10,
        final_limit: int = 10,
    ) -> list[dict[str, Any]]:
        """Anchor search + graph walk expansion.

        1. Find anchor memories via hybrid search (BM25 + embedding)
        2. Walk the association graph from anchors
        3. Hydrate discovered memories
        4. Return ranked results with reasoning paths

        Args:
            query: Search query text.
            hops: Graph walk depth.
            anchor_limit: Number of anchor results from initial search.
            walk_top_k: Top-K paths to keep from graph walk.
            final_limit: Maximum results to return.

        Returns:
            List of dicts with memory data + walk metadata.
        """
        start = time.perf_counter()

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
        except Exception as e:
            logger.error(f"hybrid_recall: could not access memory system: {e}")
            return []

        # Step 1: Anchor search
        anchors = um.search_hybrid(query=query, limit=anchor_limit)
        if not anchors:
            anchors = um.search(query=query, limit=anchor_limit)
        if not anchors:
            return []

        anchor_ids = [m.id for m in anchors]
        # Step 2: Graph walk from anchors
        walk_result = self.walk(
            seed_ids=anchor_ids,
            hops=hops,
            top_k=walk_top_k,
        )

        # Step 3: Collect all discovered node IDs
        discovered_ids = walk_result.discovered_ids()

        # Step 4: Hydrate discovered memories
        discovered_map: dict[str, Any] = {}
        for mid in discovered_ids:
            try:
                mem = um.recall(mid)
                if mem:
                    discovered_map[mid] = mem
            except Exception:
                pass

        # Step 5: Build result set — anchors first, then graph-discovered
        results: list[dict[str, Any]] = []
        seen: set[str] = set()

        # Anchors (direct search hits)
        for mem in anchors:
            results.append({
                "memory_id": mem.id,
                "title": mem.title,
                "content": str(mem.content)[:500],
                "importance": mem.importance,
                "source": "anchor",
                "rrf_score": mem.metadata.get("rrf_score", 0.0),
                "walk_paths": [],
            })
            seen.add(mem.id)

        # Graph-discovered (with reasoning paths)
        # Score by: number of paths reaching this node × path scores
        node_scores: dict[str, float] = {}
        node_paths: dict[str, list[dict]] = {}
        for path in walk_result.paths:
            for node_id in path.nodes:
                if node_id not in seen and node_id in discovered_map:
                    node_scores[node_id] = node_scores.get(node_id, 0.0) + path.total_score
                    if node_id not in node_paths:
                        node_paths[node_id] = []
                    node_paths[node_id].append(path.to_dict())

        # Sort graph-discovered by aggregate score
        sorted_discovered = sorted(node_scores.keys(), key=lambda x: node_scores[x], reverse=True)

        for mid in sorted_discovered:
            if len(results) >= final_limit:
                break
            if mid in seen:
                continue
            mem = discovered_map.get(mid)
            if not mem:
                continue

            results.append({
                "memory_id": mem.id,
                "title": mem.title,
                "content": str(mem.content)[:500],
                "importance": mem.importance,
                "source": "graph_walk",
                "graph_score": round(node_scores[mid], 4),
                "walk_paths": node_paths.get(mid, [])[:3],  # top 3 paths
            })
            seen.add(mid)

        elapsed = (time.perf_counter() - start) * 1000
        logger.info(
            f"🧠 Hybrid recall: '{query[:50]}' → {len(anchors)} anchors + "
            f"{len(discovered_ids)} graph-discovered = {len(results)} results ({elapsed:.0f}ms)",
        )
        return results

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _get_max_traversals(self, pool: Any) -> int:
        """Get the maximum traversal_count for staleness normalization."""
        try:
            with pool.connection() as conn:
                row = conn.execute(
                    "SELECT MAX(COALESCE(traversal_count, 0)) FROM associations",
                ).fetchone()
                if row and row[0]:
                    return int(row[0])
        except Exception:
            pass
        return 10  # default

    def _record_traversal(self, source_id: str, target_id: str, pool: Any) -> None:
        """Record that an edge was traversed (for staleness tracking)."""
        try:
            with pool.connection() as conn:
                conn.execute(
                    """UPDATE associations
                       SET traversal_count = COALESCE(traversal_count, 0) + 1,
                           last_traversed_at = ?
                       WHERE source_id = ? AND target_id = ?""",
                    (datetime.now().isoformat(), source_id, target_id),
                )
        except Exception:
            pass  # non-critical

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_walks": self._total_walks,
                "total_nodes_visited": self._total_nodes_visited,
                "gravity_alpha": self._gravity_alpha,
                "staleness_beta": self._staleness_beta,
                "min_edge_strength": self._min_strength,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_walker: GraphWalker | None = None
_walker_lock = threading.Lock()


def get_graph_walker(**kwargs: Any) -> GraphWalker:
    """Get the global GraphWalker singleton."""
    global _walker
    if _walker is None:
        with _walker_lock:
            if _walker is None:
                _walker = GraphWalker(**kwargs)
    return _walker
