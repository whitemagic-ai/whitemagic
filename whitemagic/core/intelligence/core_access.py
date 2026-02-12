"""CoreAccessLayer — Unified Read Interface to the Holographic Galactic Core.
==========================================================================
Provides every insight engine a clean, shared API to query the full richness
of the Data Sea: constellations, associations, temporal patterns, holographic
neighbors, and hybrid recall (vector + graph RRF).

This is the *connective tissue* that lets PredictiveEngine, KaizenEngine,
SerendipityEngine, and EmergenceEngine tap the same data structures without
duplicating SQL queries or importing each other's internals.

Usage:
    from whitemagic.core.intelligence.core_access import get_core_access
    cal = get_core_access()

    # Constellation context for a set of tags or coordinates
    context = cal.query_constellation_context(tags=["rust", "acceleration"])

    # N-hop association subgraph from seed memories
    subgraph = cal.query_association_subgraph(["mem_abc123"], depth=2)

    # Time-bucketed activity metrics
    activity = cal.query_temporal_activity(time_window="7d")

    # KNN in 5D holographic space
    neighbors = cal.query_holographic_neighbors(coords=(0.1, -0.3, 0.2, 0.8, 0.6), k=20)

    # Hybrid recall: embedding similarity + association graph walk
    results = cal.hybrid_recall("memory consolidation architecture", k=10)
"""

from __future__ import annotations

import logging
import sqlite3
import threading
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data structures returned by the access layer
# ---------------------------------------------------------------------------

@dataclass
class ConstellationContext:
    """Constellation query result."""

    name: str
    size: int
    centroid: tuple[float, float, float, float, float]
    dominant_tags: list[str]
    zone: str
    distance: float = 0.0  # Distance from query point (0 if tag-matched)

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "size": self.size,
            "centroid": {"x": self.centroid[0], "y": self.centroid[1],
                         "z": self.centroid[2], "w": self.centroid[3],
                         "v": self.centroid[4]},
            "dominant_tags": self.dominant_tags,
            "zone": self.zone,
            "distance": round(self.distance, 3),
        }


@dataclass
class AssociationNode:
    """A node in the association subgraph."""

    memory_id: str
    title: str | None = None
    strength: float = 0.0
    depth: int = 0  # Hop distance from seed

    def to_dict(self) -> dict[str, Any]:
        return {
            "memory_id": self.memory_id,
            "title": self.title,
            "strength": round(self.strength, 3),
            "depth": self.depth,
        }


@dataclass
class TemporalBucket:
    """One time bucket of activity metrics."""

    period: str  # ISO date string for the bucket
    memories_created: int = 0
    memories_accessed: int = 0
    associations_created: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "period": self.period,
            "memories_created": self.memories_created,
            "memories_accessed": self.memories_accessed,
            "associations_created": self.associations_created,
        }


@dataclass
class HolographicNeighbor:
    """A memory found via holographic KNN."""

    memory_id: str
    title: str | None = None
    distance: float = 0.0
    coords: tuple[float, float, float, float, float] | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "memory_id": self.memory_id,
            "title": self.title,
            "distance": round(self.distance, 3),
        }


@dataclass
class HybridResult:
    """A result from hybrid recall (vector + graph fusion)."""

    memory_id: str
    title: str | None = None
    content_preview: str = ""
    score: float = 0.0
    sources: list[str] = field(default_factory=list)  # "vector", "graph", "both"

    def to_dict(self) -> dict[str, Any]:
        return {
            "memory_id": self.memory_id,
            "title": self.title,
            "content_preview": self.content_preview[:200],
            "score": round(self.score, 4),
            "sources": self.sources,
        }


# ---------------------------------------------------------------------------
# Core Access Layer
# ---------------------------------------------------------------------------

class CoreAccessLayer:
    """Unified read interface to the holographic galactic core.

    Delegates to existing subsystems:
    - ConstellationDetector for constellation queries
    - SQLiteBackend for association graph + temporal queries
    - HolographicMemory for 5D KNN
    - EmbeddingEngine for semantic search
    - Association graph traversal for multi-hop reasoning
    """

    def __init__(self) -> None:
        self._conn: sqlite3.Connection | None = None
        self._lock = threading.Lock()

    def _get_conn(self) -> sqlite3.Connection:
        """Lazy-init a read-only SQLite connection to the hot DB."""
        if self._conn is None:
            from whitemagic.config.paths import DB_PATH
            self._conn = sqlite3.connect(str(DB_PATH), timeout=30, check_same_thread=False)
            self._conn.row_factory = sqlite3.Row
            try:
                self._conn.execute("PRAGMA journal_mode = WAL")
                self._conn.execute("PRAGMA synchronous = NORMAL")
                self._conn.execute("PRAGMA busy_timeout = 30000")
            except Exception:
                pass
        return self._conn

    # ------------------------------------------------------------------
    # 1. Constellation Queries
    # ------------------------------------------------------------------

    def query_constellation_context(
        self,
        tags: list[str] | None = None,
        coords: tuple[float, float, float, float, float] | None = None,
        k: int = 5,
    ) -> list[ConstellationContext]:
        """Find constellations relevant to given tags or coordinates.

        If tags are provided, matches constellations whose dominant_tags
        overlap with the query tags. If coords are provided, finds the
        k nearest constellations by 5D distance. Both can be combined.
        """
        try:
            from whitemagic.core.memory.constellations import get_constellation_detector
            detector = get_constellation_detector()
            # Use TTL-cached detection (avoids re-running detect on every query)
            cached = detector.get_cached_or_detect(sample_limit=10000)
            report = cached.to_dict() if cached else None
        except Exception as e:
            logger.debug(f"Constellation query failed: {e}")
            return []

        if not report or not report.get("constellations"):
            return []

        raw_constellations = report["constellations"]
        results: list[ConstellationContext] = []

        for c in raw_constellations:
            centroid_dict = c.get("centroid", {})
            centroid = (
                centroid_dict.get("x", 0), centroid_dict.get("y", 0),
                centroid_dict.get("z", 0), centroid_dict.get("w", 0),
                centroid_dict.get("v", 0),
            )

            # Tag matching
            tag_score = 0.0
            if tags:
                overlap = set(tags) & set(c.get("dominant_tags", []))
                tag_score = len(overlap) / max(1, len(tags))

            # Coordinate distance
            coord_dist = float("inf")
            if coords:
                import math
                coord_dist = math.sqrt(sum(
                    (a - b) ** 2 for a, b in zip(coords, centroid)
                ))

            # Include if matches tags or is spatially close
            if tag_score > 0 or (coords and coord_dist < 2.0):
                results.append(ConstellationContext(
                    name=c["name"],
                    size=c.get("size", 0),
                    centroid=centroid,
                    dominant_tags=c.get("dominant_tags", []),
                    zone=c.get("zone", "unknown"),
                    distance=coord_dist if coords else 0.0,
                ))

        # Sort: tag matches first (by overlap), then by distance
        results.sort(key=lambda r: (-len(set(tags or []) & set(r.dominant_tags)), r.distance))
        return results[:k]

    def get_all_constellations(self) -> list[ConstellationContext]:
        """Return all detected constellations (TTL-cached)."""
        try:
            from whitemagic.core.memory.constellations import get_constellation_detector
            detector = get_constellation_detector()
            cached = detector.get_cached_or_detect(sample_limit=10000)
            report = cached.to_dict() if cached else None
        except Exception:
            return []

        if not report or not report.get("constellations"):
            return []

        results = []
        for c in report["constellations"]:
            cd = c.get("centroid", {})
            results.append(ConstellationContext(
                name=c["name"],
                size=c.get("size", 0),
                centroid=(cd.get("x", 0), cd.get("y", 0), cd.get("z", 0),
                          cd.get("w", 0), cd.get("v", 0)),
                dominant_tags=c.get("dominant_tags", []),
                zone=c.get("zone", "unknown"),
            ))
        return results

    # ------------------------------------------------------------------
    # 2. Association Graph Traversal
    # ------------------------------------------------------------------

    def query_association_subgraph(
        self,
        seed_ids: list[str],
        depth: int = 2,
        min_strength: float = 0.3,
        max_nodes: int = 50,
    ) -> list[AssociationNode]:
        """Walk N-hop association graph from seed memories.

        Returns nodes reachable within `depth` hops, filtered by
        minimum association strength. BFS traversal.

        Uses Rust rusqlite accelerator when available (single call,
        no Python↔SQLite round trips per hop). Falls back to Python BFS.
        """
        # --- Rust-accelerated path ---
        try:
            import whitemagic_rs
            from whitemagic.config.paths import DB_PATH
            walk_nodes = whitemagic_rs.association_walk(
                str(DB_PATH), seed_ids, depth, min_strength, max_nodes,
            )
            nodes = [
                AssociationNode(
                    memory_id=n.memory_id,
                    title=n.title or None,
                    strength=n.strength,
                    depth=n.depth,
                )
                for n in walk_nodes
            ]
            logger.debug("Association walk: Rust path (%d nodes)", len(nodes))
            return nodes
        except Exception:
            pass

        # --- Python fallback ---
        conn = self._get_conn()
        visited: dict[str, AssociationNode] = {}
        frontier = list(seed_ids)
        current_depth = 0

        # Add seeds
        for sid in seed_ids:
            try:
                row = conn.execute(
                    "SELECT id, title FROM memories WHERE id = ?", (sid,)
                ).fetchone()
                title = row["title"] if row else None
            except Exception:
                title = None
            visited[sid] = AssociationNode(
                memory_id=sid, title=title, strength=1.0, depth=0,
            )

        while current_depth < depth and frontier and len(visited) < max_nodes:
            current_depth += 1
            next_frontier = []

            for mem_id in frontier:
                try:
                    rows = conn.execute("""
                        SELECT target_id as neighbor_id, strength FROM associations
                        WHERE source_id = ? AND strength >= ?
                        UNION
                        SELECT source_id as neighbor_id, strength FROM associations
                        WHERE target_id = ? AND strength >= ?
                        ORDER BY strength DESC
                        LIMIT 20
                    """, (mem_id, min_strength, mem_id, min_strength)).fetchall()
                except Exception:
                    continue

                for row in rows:
                    nid = row["neighbor_id"]
                    if nid not in visited and len(visited) < max_nodes:
                        try:
                            mem_row = conn.execute(
                                "SELECT title FROM memories WHERE id = ?", (nid,)
                            ).fetchone()
                            title = mem_row["title"] if mem_row else None
                        except Exception:
                            title = None

                        visited[nid] = AssociationNode(
                            memory_id=nid,
                            title=title,
                            strength=row["strength"],
                            depth=current_depth,
                        )
                        next_frontier.append(nid)

            frontier = next_frontier

        # Update traversal tracking for edges we actually walked
        self._record_traversals(visited, conn)

        # Return sorted by depth then strength
        nodes = list(visited.values())
        nodes.sort(key=lambda n: (n.depth, -n.strength))
        return nodes

    def _record_traversals(
        self, visited: dict[str, AssociationNode], conn: sqlite3.Connection,
    ) -> None:
        """Update last_traversed_at and traversal_count for walked edges."""
        now = datetime.now().isoformat()
        try:
            for node in visited.values():
                if node.depth == 0:
                    continue  # Skip seeds
                conn.execute("""
                    UPDATE associations
                    SET last_traversed_at = ?,
                        traversal_count = COALESCE(traversal_count, 0) + 1
                    WHERE (source_id = ? OR target_id = ?)
                    AND strength >= 0.3
                """, (now, node.memory_id, node.memory_id))
            conn.commit()
        except Exception:
            pass  # Non-critical — don't break the query

    def get_association_stats(self) -> dict[str, Any]:
        """Get association graph statistics."""
        conn = self._get_conn()
        try:
            row = conn.execute("""
                SELECT COUNT(*) as total,
                       AVG(strength) as avg_strength,
                       MIN(strength) as min_strength,
                       MAX(strength) as max_strength
                FROM associations
            """).fetchone()
            return {
                "total_associations": row["total"],
                "avg_strength": round(row["avg_strength"] or 0, 3),
                "min_strength": round(row["min_strength"] or 0, 3),
                "max_strength": round(row["max_strength"] or 0, 3),
            }
        except Exception:
            return {"total_associations": 0}

    def find_broken_associations(self, limit: int = 20) -> list[dict[str, Any]]:
        """Find high-strength associations where one end has drifted to FAR_EDGE."""
        conn = self._get_conn()
        try:
            rows = conn.execute("""
                SELECT a.source_id, a.target_id, a.strength,
                       m1.galactic_distance as src_distance,
                       m2.galactic_distance as tgt_distance,
                       m1.title as src_title, m2.title as tgt_title
                FROM associations a
                JOIN memories m1 ON a.source_id = m1.id
                JOIN memories m2 ON a.target_id = m2.id
                WHERE a.strength > 0.5
                AND (m1.galactic_distance > 0.85 OR m2.galactic_distance > 0.85)
                ORDER BY a.strength DESC
                LIMIT ?
            """, (limit,)).fetchall()
            return [dict(r) for r in rows]
        except Exception:
            return []

    # ------------------------------------------------------------------
    # 3. Temporal Activity Queries
    # ------------------------------------------------------------------

    def query_temporal_activity(
        self,
        time_window: str = "7d",
        bucket: str = "1d",
    ) -> list[TemporalBucket]:
        """Time-bucketed memory creation and access metrics.

        Args:
            time_window: How far back to look ("7d", "30d", "90d")
            bucket: Bucket granularity ("1d", "7d")

        Returns list of TemporalBucket objects.
        """
        conn = self._get_conn()

        # Parse window
        days = int(time_window.rstrip("d"))
        bucket_days = int(bucket.rstrip("d"))

        threshold = (datetime.now() - timedelta(days=days)).isoformat()

        try:
            # Memory creation by date
            rows = conn.execute("""
                SELECT DATE(created_at) as day, COUNT(*) as cnt
                FROM memories
                WHERE created_at > ?
                GROUP BY day ORDER BY day
            """, (threshold,)).fetchall()

            creation_by_day: dict[str, int] = {r["day"]: r["cnt"] for r in rows}

            # Memory access by date
            access_rows = conn.execute("""
                SELECT DATE(accessed_at) as day, COUNT(*) as cnt
                FROM memories
                WHERE accessed_at > ? AND accessed_at IS NOT NULL
                GROUP BY day ORDER BY day
            """, (threshold,)).fetchall()

            access_by_day: dict[str, int] = {r["day"]: r["cnt"] for r in access_rows}
        except Exception:
            return []

        # Build buckets
        buckets: list[TemporalBucket] = []
        current = datetime.now() - timedelta(days=days)
        end = datetime.now()

        while current < end:
            bucket_end = current + timedelta(days=bucket_days)
            created = 0
            accessed = 0

            d = current
            while d < bucket_end and d <= end:
                day_str = d.strftime("%Y-%m-%d")
                created += creation_by_day.get(day_str, 0)
                accessed += access_by_day.get(day_str, 0)
                d += timedelta(days=1)

            buckets.append(TemporalBucket(
                period=current.strftime("%Y-%m-%d"),
                memories_created=created,
                memories_accessed=accessed,
            ))
            current = bucket_end

        return buckets

    def get_velocity_metrics(self) -> dict[str, float]:
        """Calculate memory creation velocity metrics."""
        conn = self._get_conn()
        try:
            row = conn.execute("""
                SELECT
                    COUNT(*) as total,
                    COUNT(CASE WHEN created_at > datetime('now', '-1 days') THEN 1 END) as last_1d,
                    COUNT(CASE WHEN created_at > datetime('now', '-7 days') THEN 1 END) as last_7d,
                    COUNT(CASE WHEN created_at > datetime('now', '-30 days') THEN 1 END) as last_30d
                FROM memories
            """).fetchone()

            last_7d = row["last_7d"]
            last_30d = row["last_30d"]
            acceleration = (last_7d / 7) / max(0.01, last_30d / 30) if last_30d > 0 else 1.0

            return {
                "total": row["total"],
                "last_1d": row["last_1d"],
                "last_7d": last_7d,
                "last_30d": last_30d,
                "daily_avg_7d": round(last_7d / 7, 1),
                "daily_avg_30d": round(last_30d / 30, 1),
                "acceleration": round(acceleration, 2),  # >1 = accelerating
            }
        except Exception:
            return {"total": 0}

    # ------------------------------------------------------------------
    # 4. Holographic Neighbor Queries
    # ------------------------------------------------------------------

    def query_holographic_neighbors(
        self,
        coords: tuple[float, float, float, float, float],
        k: int = 20,
        include_cold: bool = False,
    ) -> list[HolographicNeighbor]:
        """KNN in 5D holographic space.

        Uses Zig SIMD accelerated distance when available, falls back
        to SQL-based brute force.
        """
        conn = self._get_conn()
        x, y, z, w, v = coords

        try:
            # SQL-based brute force with distance calculation
            rows = conn.execute("""
                SELECT hc.memory_id,
                       m.title,
                       hc.x, hc.y, hc.z, hc.w, hc.v,
                       ((hc.x - ?) * (hc.x - ?) +
                        (hc.y - ?) * (hc.y - ?) +
                        (hc.z - ?) * (hc.z - ?) +
                        (hc.w - ?) * (hc.w - ?) +
                        (hc.v - ?) * (hc.v - ?)) as dist_sq
                FROM holographic_coords hc
                JOIN memories m ON hc.memory_id = m.id
                WHERE hc.x IS NOT NULL
                ORDER BY dist_sq ASC
                LIMIT ?
            """, (x, x, y, y, z, z, w, w, v, v, k)).fetchall()

            import math
            return [
                HolographicNeighbor(
                    memory_id=r["memory_id"],
                    title=r["title"],
                    distance=math.sqrt(max(0, r["dist_sq"])),
                    coords=(r["x"], r["y"], r["z"], r["w"], r["v"]),
                )
                for r in rows
            ]
        except Exception as e:
            logger.debug(f"Holographic neighbor query failed: {e}")
            return []

    # ------------------------------------------------------------------
    # 5. Hybrid Recall (Vector + Graph RRF)
    # ------------------------------------------------------------------

    def hybrid_recall(
        self,
        query: str,
        k: int = 10,
        vector_weight: float = 0.6,
        graph_weight: float = 0.4,
        graph_depth: int = 2,
        include_cold: bool = False,
    ) -> list[HybridResult]:
        """Reciprocal Rank Fusion: embedding similarity + association graph walk.

        1. Get top-K from embedding search (vector channel)
        2. For top vector results, walk their association graph (graph channel)
        3. Fuse via RRF: score = w_v / (k_rrf + rank_v) + w_g / (k_rrf + rank_g)
        4. Return unified ranked results
        """
        k_rrf = 60  # RRF constant (standard value from literature)
        vector_results: list[tuple[str, float]] = []  # (memory_id, similarity)
        graph_results: list[tuple[str, float]] = []    # (memory_id, strength)

        # --- Vector channel ---
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            vec_hits = engine.search_similar(query, limit=k * 2, include_cold=include_cold)
            for hit in vec_hits:
                mid = hit.get("memory_id") or hit.get("id", "")
                sim = hit.get("similarity", 0.0)
                vector_results.append((mid, sim))
        except Exception as e:
            logger.debug(f"Vector channel failed: {e}")

        # --- Graph channel ---
        # Use top vector results as seeds, then walk associations
        if vector_results:
            seed_ids = [mid for mid, _ in vector_results[:5]]
            graph_nodes = self.query_association_subgraph(
                seed_ids, depth=graph_depth, min_strength=0.3, max_nodes=k * 3,
            )
            for node in graph_nodes:
                if node.depth > 0:  # Skip seeds themselves
                    graph_results.append((node.memory_id, node.strength))

        # --- Reciprocal Rank Fusion ---
        # Try Rust-accelerated RRF first, fall back to Python
        vec_ids = [mid for mid, _ in vector_results]
        graph_ids_sorted = [mid for mid, _ in sorted(graph_results, key=lambda x: -x[1])]

        scored: list[tuple[str, float, list[str]]] = []
        try:
            import whitemagic_rs
            rrf_results = whitemagic_rs.rrf_fuse(
                vec_ids, graph_ids_sorted,
                vector_weight, graph_weight, k_rrf, k,
            )
            for r in rrf_results:
                sources = []
                if r.from_vector:
                    sources.append("vector")
                if r.from_graph:
                    sources.append("graph")
                scored.append((r.memory_id, r.score, sources))
            logger.debug("RRF: Rust-accelerated path (%d results)", len(scored))
        except Exception:
            # Python fallback
            vec_rank: dict[str, int] = {}
            for rank, mid in enumerate(vec_ids):
                vec_rank[mid] = rank + 1

            graph_rank: dict[str, int] = {}
            for rank, mid in enumerate(graph_ids_sorted):
                graph_rank[mid] = rank + 1

            all_ids = set(vec_rank.keys()) | set(graph_rank.keys())

            for mid in all_ids:
                score = 0.0
                src_list: list[str] = []
                if mid in vec_rank:
                    score += vector_weight / (k_rrf + vec_rank[mid])
                    src_list.append("vector")
                if mid in graph_rank:
                    score += graph_weight / (k_rrf + graph_rank[mid])
                    src_list.append("graph")
                scored.append((mid, score, src_list))

            scored.sort(key=lambda x: -x[1])

        # Fetch titles and previews for top results
        conn = self._get_conn()
        results: list[HybridResult] = []
        for mid, score, sources in scored[:k]:
            title = None
            preview = ""
            try:
                row = conn.execute(
                    "SELECT title, SUBSTR(content, 1, 200) as preview FROM memories WHERE id = ?",
                    (mid,),
                ).fetchone()
                if row:
                    title = row["title"]
                    preview = row["preview"] or ""
            except Exception:
                pass

            results.append(HybridResult(
                memory_id=mid,
                title=title,
                content_preview=preview,
                score=score,
                sources=sources,
            ))

        return results

    # ------------------------------------------------------------------
    # 6. Cross-Constellation Analysis
    # ------------------------------------------------------------------

    def find_constellation_bridges(self, limit: int = 10) -> list[dict[str, Any]]:
        """Find memories that sit between two constellations.

        These bridge memories potentially link disparate knowledge domains
        and are high-value for serendipitous insight generation.
        """
        constellations = self.get_all_constellations()
        if len(constellations) < 2:
            return []

        # Build set of member IDs per constellation
        try:
            from whitemagic.core.memory.constellations import get_constellation_detector
            detector = get_constellation_detector()
            report = detector.get_last_report()
            if not report:
                return []
        except Exception:
            return []

        constellation_members: dict[str, set[str]] = {}
        for c in report.get("constellations", []):
            constellation_members[c["name"]] = set(c.get("member_ids", []))

        # Find memories that appear in associations linking two different constellations
        conn = self._get_conn()
        bridges: list[dict[str, Any]] = []

        try:
            names = list(constellation_members.keys())
            for i in range(len(names)):
                for j in range(i + 1, len(names)):
                    c1_ids = constellation_members[names[i]]
                    c2_ids = constellation_members[names[j]]

                    # Find associations that cross between constellations
                    for sid in list(c1_ids)[:50]:  # Sample to avoid O(n^2) explosion
                        row = conn.execute("""
                            SELECT target_id, strength FROM associations
                            WHERE source_id = ? AND strength > 0.4
                            LIMIT 10
                        """, (sid,)).fetchall()

                        for r in row:
                            if r["target_id"] in c2_ids:
                                bridges.append({
                                    "source_id": sid,
                                    "target_id": r["target_id"],
                                    "strength": r["strength"],
                                    "constellation_1": names[i],
                                    "constellation_2": names[j],
                                })

                    if len(bridges) >= limit:
                        break
                if len(bridges) >= limit:
                    break
        except Exception as e:
            logger.debug(f"Bridge search failed: {e}")

        return bridges[:limit]

    def get_constellation_drift(self, window_days: int = 7) -> list[dict[str, Any]]:
        """Get drift vectors for all tracked constellations over a time window.

        Returns dicts with name, current_centroid, drift_vector, drift_magnitude.
        Sorted by magnitude descending (biggest movers first).
        """
        try:
            from whitemagic.core.memory.constellations import get_constellation_detector
            detector = get_constellation_detector()
            return detector.get_drift_vectors(window_days=window_days)
        except Exception as e:
            logger.debug(f"Drift query failed: {e}")
            return []

    def find_association_orphans(self, min_gravity: float = 0.6, limit: int = 20) -> list[dict[str, Any]]:
        """Find high-gravity memories with few or no associations.

        These are isolated knowledge nodes that should be connected.
        """
        conn = self._get_conn()
        try:
            rows = conn.execute("""
                SELECT m.id, m.title, h.w as gravity,
                       (SELECT COUNT(*) FROM associations
                        WHERE source_id = m.id OR target_id = m.id) as assoc_count
                FROM memories m
                JOIN holographic_coords h ON m.id = h.memory_id
                WHERE h.w > ?
                HAVING assoc_count < 3
                ORDER BY h.w DESC
                LIMIT ?
            """, (min_gravity, limit)).fetchall()
            return [dict(r) for r in rows]
        except Exception:
            return []


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_instance: CoreAccessLayer | None = None
_instance_lock = threading.Lock()


def get_core_access() -> CoreAccessLayer:
    """Get or create the global CoreAccessLayer singleton."""
    global _instance
    with _instance_lock:
        if _instance is None:
            _instance = CoreAccessLayer()
        return _instance
