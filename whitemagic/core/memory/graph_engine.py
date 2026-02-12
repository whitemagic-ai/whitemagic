"""Graph Topology Engine — Centrality, Communities, and Bridge Detection.
=====================================================================
Foundation for both dream cycle governance (echo chamber detection via
eigenvector centrality) and graph walker gravity (PageRank for topological
weight). Uses networkx when available, with pure-Python fallbacks.

Overlap E synthesis: Compute centrality ONCE, use in both graph walking
(gravity signal) and dream governance (echo chamber detection).

Usage:
    from whitemagic.core.memory.graph_engine import get_graph_engine
    engine = get_graph_engine()

    # Build graph from associations + KG
    engine.rebuild()

    # Get centrality scores
    centrality = engine.eigenvector_centrality()

    # Detect bridge nodes
    bridges = engine.find_bridge_nodes(top_n=10)

    # Detect communities
    communities = engine.detect_communities()

    # Echo chamber detection (compare T_now vs T_prev)
    echo = engine.detect_echo_chambers(sigma_threshold=2.0)
"""

from __future__ import annotations

import logging
import sqlite3
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, cast

logger = logging.getLogger(__name__)

# Try networkx — optional dependency with graceful fallback
try:
    import networkx as nx  # type: ignore[import-untyped]
    _NX_AVAILABLE = True
except ImportError:
    nx = None  # type: ignore[assignment]
    _NX_AVAILABLE = False


@dataclass
class CentralitySnapshot:
    """Point-in-time centrality measurements for drift detection."""

    timestamp: str
    eigenvector: dict[str, float] = field(default_factory=dict)
    betweenness: dict[str, float] = field(default_factory=dict)
    pagerank: dict[str, float] = field(default_factory=dict)
    node_count: int = 0
    edge_count: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "timestamp": self.timestamp,
            "node_count": self.node_count,
            "edge_count": self.edge_count,
            "top_eigenvector": dict(sorted(
                self.eigenvector.items(), key=lambda x: x[1], reverse=True,
            )[:10]),
            "top_pagerank": dict(sorted(
                self.pagerank.items(), key=lambda x: x[1], reverse=True,
            )[:10]),
        }


@dataclass
class EchoChamber:
    """A detected echo chamber — node with centrality spike without new data."""

    node_id: str
    current_centrality: float
    previous_centrality: float
    spike_ratio: float
    has_new_data: bool

    def to_dict(self) -> dict[str, Any]:
        return {
            "node_id": self.node_id,
            "current_centrality": round(self.current_centrality, 6),
            "previous_centrality": round(self.previous_centrality, 6),
            "spike_ratio": round(self.spike_ratio, 2),
            "has_new_data": self.has_new_data,
        }


@dataclass
class Community:
    """A detected community of tightly-connected memories."""

    community_id: int
    member_ids: list[str]
    size: int
    internal_edges: int
    avg_strength: float
    theme_tags: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "community_id": self.community_id,
            "size": self.size,
            "internal_edges": self.internal_edges,
            "avg_strength": round(self.avg_strength, 4),
            "theme_tags": self.theme_tags[:5],
            "sample_members": self.member_ids[:5],
        }


class GraphEngine:
    """Graph topology engine backed by networkx.

    Builds a weighted DiGraph from the associations table, computes
    centrality metrics, detects communities, and identifies bridge nodes.
    """

    def __init__(self, cache_ttl_seconds: float = 300.0) -> None:
        self._graph: Any = None  # nx.DiGraph or None
        self._lock = threading.Lock()
        self._cache_ttl = cache_ttl_seconds
        self._last_build: float = 0.0
        self._previous_snapshot: CentralitySnapshot | None = None
        self._current_snapshot: CentralitySnapshot | None = None
        self._total_rebuilds = 0

    @property
    def available(self) -> bool:
        return _NX_AVAILABLE

    @property
    def graph(self) -> Any:
        """Get the current graph, rebuilding if stale."""
        if self._graph is None or (time.time() - self._last_build > self._cache_ttl):
            self.rebuild()
        return self._graph

    # ------------------------------------------------------------------
    # Graph construction
    # ------------------------------------------------------------------

    def rebuild(self, sample_limit: int = 50000) -> dict[str, Any]:
        """Build/rebuild the graph from association table.

        Args:
            sample_limit: Max edges to load (for performance on 19M associations).

        Returns:
            Build stats dict.
        """
        if not _NX_AVAILABLE:
            logger.debug("GraphEngine: networkx not available")
            return {"status": "unavailable", "reason": "networkx not installed"}

        start = time.perf_counter()

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            pool = um.backend.pool
        except Exception as e:
            logger.error(f"GraphEngine: could not access memory system: {e}")
            return {"status": "error", "message": str(e)}

        G = nx.DiGraph()

        try:
            with pool.connection() as conn:
                conn.row_factory = sqlite3.Row
                # Load edges with v14 schema columns
                rows = conn.execute(
                    """SELECT source_id, target_id, strength,
                              COALESCE(direction, 'undirected') as direction,
                              COALESCE(relation_type, 'associated_with') as relation_type,
                              COALESCE(edge_type, 'semantic') as edge_type
                       FROM associations
                       WHERE strength >= 0.05
                       ORDER BY strength DESC
                       LIMIT ?""",
                    (sample_limit,),
                ).fetchall()

                for row in rows:
                    G.add_edge(
                        row["source_id"],
                        row["target_id"],
                        weight=row["strength"],
                        direction=row["direction"],
                        relation_type=row["relation_type"],
                        edge_type=row["edge_type"],
                    )

                # Also load KG relations if available
                try:
                    kg_rows = conn.execute(
                        """SELECT source, target, relation, confidence
                           FROM knowledge_graph
                           WHERE confidence >= 0.3
                           LIMIT ?""",
                        (sample_limit // 10,),
                    ).fetchall()
                    for row in kg_rows:
                        G.add_edge(
                            row["source"],
                            row["target"],
                            weight=row["confidence"],
                            relation_type=row["relation"],
                            edge_type="knowledge_graph",
                            direction="forward",
                        )
                except Exception:
                    pass  # KG table may not exist

        except Exception as e:
            logger.error(f"GraphEngine: failed to load edges: {e}")
            return {"status": "error", "message": str(e)}

        with self._lock:
            self._graph = G
            self._last_build = time.time()
            self._total_rebuilds += 1

        elapsed = (time.perf_counter() - start) * 1000

        stats = {
            "status": "success",
            "nodes": G.number_of_nodes(),
            "edges": G.number_of_edges(),
            "duration_ms": round(elapsed, 1),
            "rebuilds": self._total_rebuilds,
        }

        logger.info(
            f"🕸️ Graph rebuilt: {stats['nodes']} nodes, {stats['edges']} edges ({elapsed:.0f}ms)",
        )
        return stats

    # ------------------------------------------------------------------
    # Centrality metrics
    # ------------------------------------------------------------------

    def eigenvector_centrality(self, max_iter: int = 100) -> dict[str, float]:
        """Compute eigenvector centrality (used for echo chamber detection)."""
        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return {}
        try:
            # Use undirected view for eigenvector centrality
            return cast(
                dict[str, float],
                nx.eigenvector_centrality_numpy(G.to_undirected(), max_iter=max_iter),
            )
        except Exception:
            try:
                return cast(
                    dict[str, float],
                    nx.eigenvector_centrality(G.to_undirected(), max_iter=max_iter, tol=1e-4),
                )
            except Exception as e:
                logger.debug(f"Eigenvector centrality failed: {e}")
                return {}

    def betweenness_centrality(self, k: int | None = 500) -> dict[str, float]:
        """Compute betweenness centrality (approximate for large graphs)."""
        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return {}
        try:
            n = G.number_of_nodes()
            sample_k = min(k, n) if k and n > k else None
            return cast(dict[str, float], nx.betweenness_centrality(G, k=sample_k, weight="weight"))
        except Exception as e:
            logger.debug(f"Betweenness centrality failed: {e}")
            return {}

    def pagerank(self, alpha: float = 0.85) -> dict[str, float]:
        """Compute PageRank (used for walker gravity)."""
        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return {}
        try:
            return cast(dict[str, float], nx.pagerank(G, alpha=alpha, weight="weight"))
        except Exception as e:
            logger.debug(f"PageRank failed: {e}")
            return {}

    def centrality_snapshot(self) -> CentralitySnapshot:
        """Compute and store a full centrality snapshot.

        Stores as T_prev for later echo chamber comparison.
        """
        snap = CentralitySnapshot(
            timestamp=datetime.now().isoformat(),
        )

        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return snap

        snap.node_count = G.number_of_nodes()
        snap.edge_count = G.number_of_edges()
        snap.eigenvector = self.eigenvector_centrality()
        snap.pagerank = self.pagerank()
        # Betweenness is expensive — only compute for moderate graphs
        if snap.node_count <= 10000:
            snap.betweenness = self.betweenness_centrality()

        with self._lock:
            self._previous_snapshot = self._current_snapshot
            self._current_snapshot = snap

        return snap

    # ------------------------------------------------------------------
    # Bridge detection
    # ------------------------------------------------------------------

    def bridging_centrality(self, node_id: str) -> float:
        """Compute bridging centrality for a single node.

        bridging_centrality(v) = betweenness(v) × bridging_coefficient(v)
        where bridging_coefficient(v) = 1/degree(v) / Σ(1/degree(u)) for u ∈ N(v)
        """
        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return 0.0

        try:
            UG = G.to_undirected()
            if node_id not in UG:
                return 0.0

            degree_v = UG.degree(node_id)
            if degree_v == 0:
                return 0.0

            neighbors = list(UG.neighbors(node_id))
            if not neighbors:
                return 0.0

            # Bridging coefficient
            inv_degree_v = 1.0 / degree_v
            sum_inv_degree = sum(1.0 / max(1, UG.degree(u)) for u in neighbors)
            if sum_inv_degree == 0:
                return 0.0
            bridging_coeff = inv_degree_v / sum_inv_degree

            # Betweenness for this node
            between = self.betweenness_centrality()
            bc = between.get(node_id, 0.0)

            return float(bc * bridging_coeff)
        except Exception:
            return 0.0

    def find_bridge_nodes(self, top_n: int = 10) -> list[dict[str, Any]]:
        """Find top bridge nodes by bridging centrality.

        Bridge nodes connect otherwise disconnected communities — the most
        interesting nodes for serendipity discovery.
        """
        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return []

        try:
            between = self.betweenness_centrality()
            if not between:
                return []

            UG = G.to_undirected()

            bridges: list[dict[str, Any]] = []
            # Only compute bridging for top-betweenness nodes (efficiency)
            top_between = sorted(between.items(), key=lambda x: x[1], reverse=True)[:top_n * 3]

            for node_id, bc in top_between:
                degree = UG.degree(node_id)
                if degree < 2:
                    continue

                neighbors = list(UG.neighbors(node_id))
                inv_degree = 1.0 / degree
                sum_inv = sum(1.0 / max(1, UG.degree(u)) for u in neighbors)
                bridging_coeff = inv_degree / sum_inv if sum_inv > 0 else 0.0
                bridging_score = bc * bridging_coeff

                bridges.append({
                    "node_id": node_id,
                    "bridging_centrality": round(bridging_score, 6),
                    "betweenness": round(bc, 6),
                    "bridging_coefficient": round(bridging_coeff, 4),
                    "degree": degree,
                })

            bridges.sort(key=lambda x: x["bridging_centrality"], reverse=True)
            return bridges[:top_n]
        except Exception as e:
            logger.debug(f"Bridge detection failed: {e}")
            return []

    # ------------------------------------------------------------------
    # Community detection
    # ------------------------------------------------------------------

    def detect_communities(self, resolution: float = 1.0) -> list[Community]:
        """Detect communities using greedy modularity.

        Falls back to connected components if community detection fails.
        """
        G = self.graph
        if G is None or not _NX_AVAILABLE:
            return []

        try:
            UG = G.to_undirected()

            # Try Louvain first (requires networkx >= 3.1)
            communities_list: list[set[str]] = []
            try:
                communities_gen = nx.community.louvain_communities(
                    UG, weight="weight", resolution=resolution,
                )
                communities_list = [set(c) for c in communities_gen]
            except (AttributeError, Exception):
                # Fallback: greedy modularity
                try:
                    communities_gen = nx.community.greedy_modularity_communities(
                        UG, weight="weight",
                    )
                    communities_list = [set(c) for c in communities_gen]
                except Exception:
                    # Final fallback: connected components
                    communities_list = [
                        set(c) for c in nx.connected_components(UG)
                        if len(c) >= 2
                    ]

            # Build Community objects
            result: list[Community] = []
            for idx, members in enumerate(communities_list):
                if len(members) < 2:
                    continue

                # Count internal edges and average strength
                internal_edges = 0
                total_strength = 0.0
                for u in members:
                    for v in UG.neighbors(u):
                        if v in members:
                            internal_edges += 1
                            total_strength += UG[u][v].get("weight", 0.5)

                internal_edges //= 2  # undirected, counted twice
                avg_strength = total_strength / max(1, internal_edges * 2)

                # Get theme tags from members
                theme_tags = self._get_member_tags(list(members)[:20])

                result.append(Community(
                    community_id=idx,
                    member_ids=sorted(members),
                    size=len(members),
                    internal_edges=internal_edges,
                    avg_strength=avg_strength,
                    theme_tags=theme_tags,
                ))

            result.sort(key=lambda c: c.size, reverse=True)
            return result
        except Exception as e:
            logger.debug(f"Community detection failed: {e}")
            return []

    def _get_member_tags(self, member_ids: list[str]) -> list[str]:
        """Get most common tags for a set of memory IDs."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            tag_counts: dict[str, int] = {}
            with um.backend.pool.connection() as conn:
                placeholders = ",".join("?" * len(member_ids))
                rows = conn.execute(
                    f"SELECT tag, COUNT(*) as cnt FROM tags WHERE memory_id IN ({placeholders}) GROUP BY tag ORDER BY cnt DESC LIMIT 5",
                    member_ids,
                ).fetchall()
                for row in rows:
                    tag_counts[row[0]] = row[1]
            return sorted(tag_counts.keys(), key=lambda t: tag_counts[t], reverse=True)[:5]
        except Exception:
            return []

    # ------------------------------------------------------------------
    # Echo chamber detection
    # ------------------------------------------------------------------

    def detect_echo_chambers(
        self,
        sigma_threshold: float = 2.0,
    ) -> list[EchoChamber]:
        """Detect echo chambers by comparing centrality snapshots.

        A node is an echo chamber candidate if:
        1. Its eigenvector centrality spiked > sigma_threshold standard deviations
        2. WITHOUT corresponding new data (no new edges in the last cycle)

        Args:
            sigma_threshold: Standard deviation threshold for centrality spike.

        Returns:
            List of detected echo chambers.
        """
        with self._lock:
            prev = self._previous_snapshot
            curr = self._current_snapshot

        if prev is None or curr is None:
            # Need at least 2 snapshots
            return []

        if not prev.eigenvector or not curr.eigenvector:
            return []

        # Compute mean and std of centrality changes
        common_nodes = set(prev.eigenvector.keys()) & set(curr.eigenvector.keys())
        if len(common_nodes) < 10:
            return []

        deltas = []
        for nid in common_nodes:
            delta = curr.eigenvector[nid] - prev.eigenvector[nid]
            deltas.append(delta)

        if not deltas:
            return []

        mean_delta = sum(deltas) / len(deltas)
        variance = sum((d - mean_delta) ** 2 for d in deltas) / len(deltas)
        std_delta = variance ** 0.5

        if std_delta < 1e-10:
            return []  # No variation

        # Find nodes with spikes
        echo_chambers: list[EchoChamber] = []
        new_edges = self._get_recent_edge_nodes()

        for nid in common_nodes:
            delta = curr.eigenvector[nid] - prev.eigenvector[nid]
            z_score = (delta - mean_delta) / std_delta

            if z_score > sigma_threshold:
                has_new_data = nid in new_edges
                if not has_new_data:
                    echo_chambers.append(EchoChamber(
                        node_id=nid,
                        current_centrality=curr.eigenvector[nid],
                        previous_centrality=prev.eigenvector[nid],
                        spike_ratio=z_score,
                        has_new_data=False,
                    ))

        echo_chambers.sort(key=lambda e: e.spike_ratio, reverse=True)
        return echo_chambers

    def _get_recent_edge_nodes(self, hours: int = 24) -> set[str]:
        """Get node IDs that received new edges recently."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            with um.backend.pool.connection() as conn:
                cutoff = datetime.now().isoformat()[:10]  # today's date prefix
                rows = conn.execute(
                    """SELECT DISTINCT source_id FROM associations
                       WHERE created_at >= ?
                       UNION
                       SELECT DISTINCT target_id FROM associations
                       WHERE created_at >= ?""",
                    (cutoff, cutoff),
                ).fetchall()
                return {row[0] for row in rows}
        except Exception:
            return set()

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        G = self._graph
        stats: dict[str, Any] = {
            "available": _NX_AVAILABLE,
            "total_rebuilds": self._total_rebuilds,
            "has_previous_snapshot": self._previous_snapshot is not None,
            "has_current_snapshot": self._current_snapshot is not None,
        }
        if G is not None:
            stats["nodes"] = G.number_of_nodes()
            stats["edges"] = G.number_of_edges()
            stats["cache_age_seconds"] = round(time.time() - self._last_build, 1)
        return stats

    def summary(self) -> dict[str, Any]:
        """Rich summary for Gnosis portal."""
        stats = self.get_stats()
        if self._current_snapshot:
            stats["snapshot"] = self._current_snapshot.to_dict()
        return stats


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_engine: GraphEngine | None = None
_engine_lock = threading.Lock()


def get_graph_engine(**kwargs: Any) -> GraphEngine:
    """Get the global GraphEngine singleton."""
    global _engine
    if _engine is None:
        with _engine_lock:
            if _engine is None:
                _engine = GraphEngine(**kwargs)
    return _engine
