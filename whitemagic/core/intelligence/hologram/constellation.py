"""Constellation Search - 4D Spatial Memory Queries.
=================================================

Search memories by proximity in holographic space:
- Find memories near a point
- Find memories in a sector (range query)
- Find related memories (neighbors)
- Trace paths through memory space

Axes:
  X: Logic (-1) ↔ Emotion (+1)
  Y: Micro (-1) ↔ Macro (+1)
  Z: Past (-1) ↔ Future (+1)
  W: Low importance (0) ↔ High importance (1)
"""

import logging
import math
import sqlite3
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from whitemagic.config.paths import DB_PATH

logger = logging.getLogger(__name__)

@dataclass
class SearchResult:
    """A memory with its distance from query point."""

    id: str
    title: str
    content: str
    coords: tuple[float, float, float, float]
    distance: float
    importance: float

class ConstellationSearch:
    """4D spatial search for holographic memories."""

    def __init__(self, db_path: Path | None = None):
        self.db_path = db_path or DB_PATH

    def _distance(self, c1: tuple, c2: tuple) -> float:
        """4D Euclidean distance."""
        return math.sqrt(sum((a - b) ** 2 for a, b in zip(c1, c2)))

    def _get_all_with_coords(self) -> list[dict]:
        """Get all memories with coordinates."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row
        results = conn.execute("""
            SELECT m.id, m.title, m.content, m.importance,
                   h.x, h.y, h.z, h.w
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
        """).fetchall()
        conn.close()
        return [dict(r) for r in results]

    def near(self, x: float, y: float, z: float, w: float = 0.5,
             radius: float = 0.5, limit: int = 10) -> list[SearchResult]:
        """Find memories near a 4D point using SQL range optimization."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        # Use a bounding box to limit search before fine-grained distance check
        results = conn.execute("""
            SELECT m.id, m.title, m.content, m.importance,
                   h.x, h.y, h.z, h.w
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            WHERE h.x BETWEEN ? AND ?
              AND h.y BETWEEN ? AND ?
              AND h.z BETWEEN ? AND ?
              AND h.w BETWEEN ? AND ?
        """, (
            x - radius, x + radius,
            y - radius, y + radius,
            z - radius, z + radius,
            w - radius, w + radius,
        )).fetchall()
        conn.close()

        search_results = []
        for r in results:
            coords = (r["x"], r["y"], r["z"], r["w"])
            dist = self._distance((x, y, z, w), coords)
            if dist <= radius:
                search_results.append(SearchResult(
                    id=r["id"],
                    title=r["title"] or "Untitled",
                    content=(r["content"] or "")[:200],
                    coords=coords,
                    distance=dist,
                    importance=r["importance"],
                ))

        search_results.sort(key=lambda r: r.distance)
        return search_results[:limit]

    def sector(self, x_range: tuple[float, float] = (-1, 1),
               y_range: tuple[float, float] = (-1, 1),
               z_range: tuple[float, float] = (-1, 1),
               w_range: tuple[float, float] = (0, 1),
               limit: int = 20) -> list[SearchResult]:
        """Find memories within a 4D sector using SQL."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        results = conn.execute("""
            SELECT m.id, m.title, m.content, m.importance,
                   h.x, h.y, h.z, h.w
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            WHERE h.x BETWEEN ? AND ?
              AND h.y BETWEEN ? AND ?
              AND h.z BETWEEN ? AND ?
              AND h.w BETWEEN ? AND ?
            ORDER BY m.importance DESC
            LIMIT ?
        """, (
            x_range[0], x_range[1],
            y_range[0], y_range[1],
            z_range[0], z_range[1],
            w_range[0], w_range[1],
            limit,
        )).fetchall()
        conn.close()

        return [SearchResult(
            id=r["id"],
            title=r["title"] or "Untitled",
            content=(r["content"] or "")[:200],
            coords=(r["x"], r["y"], r["z"], r["w"]),
            distance=0.0,
            importance=r["importance"],
        ) for r in results]

    def neighbors(self, memory_id: str, radius: float = 0.4,
                  limit: int = 10) -> list[SearchResult]:
        """Find memories near a specific memory."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        # Get the target memory's coords
        row = conn.execute("""
            SELECT x, y, z, w FROM holographic_coords WHERE memory_id = ?
        """, (memory_id,)).fetchone()
        conn.close()

        if not row:
            return []

        # Search near this point
        results = self.near(row["x"], row["y"], row["z"], row["w"], radius, limit + 1)
        # Remove self
        return [r for r in results if r.id != memory_id][:limit]

    def by_axis(self, axis: str, value: float, tolerance: float = 0.2,
                limit: int = 15) -> list[SearchResult]:
        """Find memories by a specific axis value.

        axis: 'x' (logic/emotion), 'y' (micro/macro), 'z' (time), 'w' (importance)
        """
        axis_map = {"x": 0, "y": 1, "z": 2, "w": 3,
                    "logic": 0, "emotion": 0, "micro": 1, "macro": 1,
                    "past": 2, "future": 2, "time": 2, "importance": 3}

        axis_idx = axis_map.get(axis.lower(), 0)
        memories = self._get_all_with_coords()

        results = []
        for m in memories:
            coords = (m["x"], m["y"], m["z"], m["w"])
            if abs(coords[axis_idx] - value) <= tolerance:
                results.append(SearchResult(
                    id=m["id"],
                    title=m["title"] or "Untitled",
                    content=(m["content"] or "")[:200],
                    coords=coords,
                    distance=abs(coords[axis_idx] - value),
                    importance=m["importance"],
                ))

        results.sort(key=lambda r: r.distance)
        return results[:limit]

    def quadrant(self, quadrant: str, limit: int = 20) -> list[SearchResult]:
        """Get memories from a named quadrant.

        Quadrants (X-Y plane):
          'logic-micro': Technical details
          'logic-macro': Strategic analysis
          'emotion-micro': Personal moments
          'emotion-macro': Big picture feelings
        """
        quadrants = {
            "logic-micro": ((-1, 0), (-1, 0)),
            "logic-macro": ((-1, 0), (0, 1)),
            "emotion-micro": ((0, 1), (-1, 0)),
            "emotion-macro": ((0, 1), (0, 1)),
            "technical": ((-1, 0), (-1, 0)),
            "strategic": ((-1, 0), (0, 1)),
            "personal": ((0, 1), (-1, 0)),
            "visionary": ((0, 1), (0, 1)),
        }

        if quadrant.lower() not in quadrants:
            return []

        x_range, y_range = quadrants[quadrant.lower()]
        return self.sector(x_range=x_range, y_range=y_range, limit=limit)

    def important(self, min_importance: float = 0.7, limit: int = 20) -> list[SearchResult]:
        """Get high-importance memories."""
        return self.sector(w_range=(min_importance, 1.0), limit=limit)

    def recent(self, limit: int = 20) -> list[SearchResult]:
        """Get memories with future-oriented Z (recent/upcoming)."""
        return self.sector(z_range=(0.0, 1.0), limit=limit)

    def historical(self, limit: int = 20) -> list[SearchResult]:
        """Get memories with past-oriented Z."""
        return self.sector(z_range=(-1.0, 0.0), limit=limit)

    def stats(self) -> dict[str, Any]:
        """Get distribution stats using SQL aggregates (O(1) memory)."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        row = conn.execute("""
            SELECT
                COUNT(*) as count,
                MIN(x) as min_x, MAX(x) as max_x, AVG(x) as avg_x,
                MIN(y) as min_y, MAX(y) as max_y, AVG(y) as avg_y,
                MIN(z) as min_z, MAX(z) as max_z, AVG(z) as avg_z,
                MIN(w) as min_w, MAX(w) as max_w, AVG(w) as avg_w
            FROM holographic_coords
        """).fetchone()
        conn.close()

        if not row or row["count"] == 0:
            return {"count": 0}

        return {
            "count": row["count"],
            "x": {"min": row["min_x"], "max": row["max_x"], "avg": row["avg_x"]},
            "y": {"min": row["min_y"], "max": row["max_y"], "avg": row["avg_y"]},
            "z": {"min": row["min_z"], "max": row["max_z"], "avg": row["avg_z"]},
            "w": {"min": row["min_w"], "max": row["max_w"], "avg": row["avg_w"]},
        }

def get_constellation() -> ConstellationSearch:
    return ConstellationSearch()

# CLI
if __name__ == "__main__":
    search = get_constellation()

    logger.info("Constellation Search")
    logger.info("=" * 50)

    stats = search.stats()
    logger.info(f"\nMemory Distribution ({stats['count']} total):")
    logger.info(f"  X (Logic↔Emotion): {stats['x']['min']:.2f} to {stats['x']['max']:.2f}, avg={stats['x']['avg']:.2f}")
    logger.info(f"  Y (Micro↔Macro):   {stats['y']['min']:.2f} to {stats['y']['max']:.2f}, avg={stats['y']['avg']:.2f}")
    logger.info(f"  Z (Past↔Future):   {stats['z']['min']:.2f} to {stats['z']['max']:.2f}, avg={stats['z']['avg']:.2f}")
    logger.info(f"  W (Importance):    {stats['w']['min']:.2f} to {stats['w']['max']:.2f}, avg={stats['w']['avg']:.2f}")

    logger.info("\n--- High Importance Memories ---")
    for r in search.important(limit=5):
        logger.info(f"  [{r.importance:.2f}] {r.title[:50]}")

    logger.info("\n--- Strategic Quadrant ---")
    for r in search.quadrant("strategic", limit=5):
        logger.info(f"  {r.title[:50]}")
