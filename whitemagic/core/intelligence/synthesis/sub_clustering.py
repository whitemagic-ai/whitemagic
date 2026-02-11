"""SubClusteringEngine - Subdivide large memory clusters.

Refines large clusters into granular topics using 4D coordinates
and quadrant-based segmentation.
"""

import sqlite3
from dataclasses import dataclass
from typing import Any


@dataclass
class SubCluster:
    """A sub-cluster within a larger cluster."""

    id: str
    parent_cluster: str
    quadrant: str  # e.g., "strategic_logical"
    memory_ids: list[str]
    centroid: tuple[float, float, float, float]
    size: int

class SubClusteringEngine:
    """Subdivide large memory clusters."""

    QUADRANTS = [
        ("strategic", "logical", lambda x, y: y >= 0 and x < 0),
        ("strategic", "emotional", lambda x, y: y >= 0 and x >= 0),
        ("detail", "logical", lambda x, y: y < 0 and x < 0),
        ("detail", "emotional", lambda x, y: y < 0 and x >= 0),
    ]

    def __init__(self, db_path: str | None = None) -> None:
        from whitemagic.config.paths import DB_PATH
        self.db_path = db_path or str(DB_PATH)
        try:
            from pathlib import Path
            Path(self.db_path).expanduser().resolve().parent.mkdir(parents=True, exist_ok=True)
        except Exception:
            pass
        self._conn: sqlite3.Connection | None = None

    def _get_conn(self) -> sqlite3.Connection:
        conn = self._conn
        if conn is None:
            conn = sqlite3.connect(self.db_path, timeout=30, check_same_thread=False)
            conn.row_factory = sqlite3.Row
            try:
                conn.execute("PRAGMA journal_mode = WAL")
                conn.execute("PRAGMA synchronous = NORMAL")
                conn.execute("PRAGMA foreign_keys = ON")
                conn.execute("PRAGMA busy_timeout = 5000")
            except Exception:
                pass
            self._conn = conn
        return conn

    def find_large_clusters(self, threshold: int = 20) -> list[tuple[str, int]]:
        """Find clusters larger than threshold."""
        conn = self._get_conn()
        cur = conn.cursor()

        # Cluster by rounding coordinates
        cur.execute("""
            SELECT
                CAST(ROUND(x * 5) AS TEXT) || '_' || CAST(ROUND(y * 5) AS TEXT) as cluster_id,
                COUNT(*) as size
            FROM holographic_coords
            GROUP BY cluster_id
            HAVING size > ?
            ORDER BY size DESC
        """, (threshold,))

        return [(r["cluster_id"], r["size"]) for r in cur.fetchall()]

    def subdivide_cluster(self, cluster_id: str) -> list[SubCluster]:
        """Subdivide a single cluster into quadrants."""
        conn = self._get_conn()
        cur = conn.cursor()

        # Parse cluster coordinates
        parts = cluster_id.split("_")
        cx, cy = float(parts[0]) / 5, float(parts[1]) / 5

        # Find all memories in this cluster
        cur.execute("""
            SELECT memory_id, x, y, z, w
            FROM holographic_coords
            WHERE ROUND(x * 5) = ? AND ROUND(y * 5) = ?
        """, (int(float(parts[0])), int(float(parts[1]))))

        memories = cur.fetchall()

        # Subdivide by quadrant relative to cluster center
        sub_clusters = []

        for y_name, x_name, condition in self.QUADRANTS:
            quadrant_mems = [m for m in memories if condition(m["x"] - cx, m["y"] - cy)]

            if quadrant_mems:
                avg_x = sum(m["x"] for m in quadrant_mems) / len(quadrant_mems)
                avg_y = sum(m["y"] for m in quadrant_mems) / len(quadrant_mems)
                avg_z = sum(m["z"] for m in quadrant_mems) / len(quadrant_mems)
                avg_w = sum(m["w"] for m in quadrant_mems) / len(quadrant_mems)

                sub_clusters.append(SubCluster(
                    id=f"{cluster_id}_{y_name}_{x_name}",
                    parent_cluster=cluster_id,
                    quadrant=f"{y_name}_{x_name}",
                    memory_ids=[m["memory_id"] for m in quadrant_mems],
                    centroid=(avg_x, avg_y, avg_z, avg_w),
                    size=len(quadrant_mems),
                ))

        return sub_clusters

    def subdivide_large_clusters(self, threshold: int = 20,
                                  dry_run: bool = False) -> dict[str, list[SubCluster]]:
        """Subdivide all large clusters."""
        large = self.find_large_clusters(threshold)
        results = {}

        for cluster_id, size in large:
            sub_clusters = self.subdivide_cluster(cluster_id)
            results[cluster_id] = sub_clusters

            if not dry_run:
                # Could create summary memories for each sub-cluster here
                pass

        return results

    def get_cluster_stats(self) -> dict[str, Any]:
        """Get statistics about current clustering."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT
                CAST(ROUND(x * 5) AS TEXT) || '_' || CAST(ROUND(y * 5) AS TEXT) as cluster_id,
                COUNT(*) as size,
                AVG(w) as avg_gravity
            FROM holographic_coords
            GROUP BY cluster_id
        """)

        clusters = cur.fetchall()

        sizes = [c["size"] for c in clusters]

        return {
            "total_clusters": len(clusters),
            "total_memories": sum(sizes),
            "avg_cluster_size": sum(sizes) / len(clusters) if clusters else 0,
            "max_cluster_size": max(sizes) if sizes else 0,
            "min_cluster_size": min(sizes) if sizes else 0,
            "large_clusters": len([s for s in sizes if s > 20]),
        }

# Global instance
_sub_clustering_engine: SubClusteringEngine | None = None

def get_sub_clustering_engine() -> SubClusteringEngine:
    global _sub_clustering_engine
    if _sub_clustering_engine is None:
        _sub_clustering_engine = SubClusteringEngine()
    return _sub_clustering_engine
