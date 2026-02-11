"""Constellation Detection — Named Clusters in 5D Holographic Space.
==================================================================
Discovers dense clusters of semantically related memories in the 5D
holographic coordinate space (X, Y, Z, W, V). Each cluster is named
a "constellation" — a persistent semantic theme that emerges organically
from the Data Sea.

Constellations are persisted as PATTERN memories with special tags,
allowing them to be searched, visualized, and used for contextual recall.

Strategy:
  1. Load all memories with holographic coordinates.
  2. Use a lightweight grid-based density scan in 5D space.
  3. Identify cells with memory count above threshold.
  4. Merge adjacent dense cells into constellations.
  5. Name each constellation from dominant tags/keywords.
  6. Persist as metadata (not as new memories — avoids bloat).

Usage:
    from whitemagic.core.memory.constellations import get_constellation_detector
    detector = get_constellation_detector()
    report = detector.detect()
"""

from __future__ import annotations

import logging
import math
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class Constellation:
    """A named cluster of semantically related memories in 5D space."""

    name: str
    member_ids: list[str]
    centroid: tuple[float, float, float, float, float]  # (x, y, z, w, v)
    radius: float  # Average distance from centroid
    dominant_tags: list[str]
    dominant_type: str  # Most common memory_type
    avg_importance: float
    zone: str  # Galactic zone of centroid's V coordinate

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "size": len(self.member_ids),
            "centroid": {
                "x": round(self.centroid[0], 3),
                "y": round(self.centroid[1], 3),
                "z": round(self.centroid[2], 3),
                "w": round(self.centroid[3], 3),
                "v": round(self.centroid[4], 3),
            },
            "radius": round(self.radius, 3),
            "dominant_tags": self.dominant_tags[:5],
            "dominant_type": self.dominant_type,
            "avg_importance": round(self.avg_importance, 3),
            "zone": self.zone,
            "member_ids": self.member_ids[:10],  # Sample
        }


@dataclass
class DetectionReport:
    """Results from a constellation detection run."""

    memories_scanned: int = 0
    constellations_found: int = 0
    largest_constellation: int = 0
    duration_ms: float = 0.0
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())
    constellations: list[Constellation] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "memories_scanned": self.memories_scanned,
            "constellations_found": self.constellations_found,
            "largest_constellation": self.largest_constellation,
            "duration_ms": round(self.duration_ms, 1),
            "timestamp": self.timestamp,
            "constellations": [c.to_dict() for c in self.constellations],
        }


# ---------------------------------------------------------------------------
# Constellation Detector
# ---------------------------------------------------------------------------

class ConstellationDetector:
    """Discovers dense clusters in 5D holographic space.

    Uses grid-based density scanning: divide each axis into bins,
    count memories per cell, identify dense cells, merge neighbors.
    """

    def __init__(
        self,
        bins_per_axis: int = 8,
        min_cluster_size: int = 5,
        max_constellations: int = 30,
        cache_ttl_seconds: float = 3600.0,
    ) -> None:
        self._bins = bins_per_axis
        self._min_size = min_cluster_size
        self._max_constellations = max_constellations
        self._lock = threading.Lock()
        self._last_report: DetectionReport | None = None
        # TTL cache: avoid re-running detect() on every query
        self._cache_ttl = cache_ttl_seconds
        self._last_detect_time: float = 0.0
        # Drift tracking: historical centroids keyed by constellation name
        # Each entry: list of (timestamp_iso, centroid_5d) tuples
        self._centroid_history: dict[str, list[tuple[str, tuple[float, ...]]]] = {}
        self._max_history_per_constellation = 100

    # ------------------------------------------------------------------
    # Grid helpers
    # ------------------------------------------------------------------

    def _bin_value(self, value: float, axis_min: float, axis_max: float) -> int:
        """Map a value to a bin index."""
        if axis_max <= axis_min:
            return 0
        normalized = (value - axis_min) / (axis_max - axis_min)
        return min(self._bins - 1, max(0, int(normalized * self._bins)))

    def _cell_key(
        self, x: float, y: float, z: float, w: float, v: float,
        ranges: dict[str, tuple[float, float]],
    ) -> tuple[int, int, int, int, int]:
        """Map a 5D point to a grid cell."""
        return (
            self._bin_value(x, *ranges["x"]),
            self._bin_value(y, *ranges["y"]),
            self._bin_value(z, *ranges["z"]),
            self._bin_value(w, *ranges["w"]),
            self._bin_value(v, *ranges["v"]),
        )

    @staticmethod
    def _distance_5d(a: tuple[float, ...], b: tuple[float, ...]) -> float:
        """Euclidean distance in 5D space. Uses Zig SIMD when available."""
        try:
            from whitemagic.core.acceleration.simd_holographic import (
                holographic_5d_distance,
            )
            return float(holographic_5d_distance(a, b, (1.0, 1.0, 1.0, 1.0, 1.0)))
        except Exception:
            pass
        return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

    # ------------------------------------------------------------------
    # Detection
    # ------------------------------------------------------------------

    def detect(self, sample_limit: int = 50000) -> DetectionReport:
        """Run constellation detection across the Data Sea.

        Returns a DetectionReport with discovered constellations.
        """
        start = time.perf_counter()
        report = DetectionReport()

        try:
            import sqlite3

            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            backend = um.backend
        except Exception as e:
            logger.error(f"Constellation detection: backend unavailable: {e}")
            return report

        # Load coordinates + metadata
        with backend.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            rows = conn.execute("""
                SELECT hc.memory_id, hc.x, hc.y, hc.z, hc.w, hc.v,
                       m.title, m.memory_type, m.importance, m.galactic_distance
                FROM holographic_coords hc
                JOIN memories m ON hc.memory_id = m.id
                WHERE hc.x IS NOT NULL AND hc.y IS NOT NULL
                ORDER BY m.importance DESC
                LIMIT ?
            """, (sample_limit,)).fetchall()

        if len(rows) < self._min_size:
            return report

        report.memories_scanned = len(rows)

        # Compute axis ranges
        coords = [(r["x"], r["y"], r["z"], r["w"], r["v"]) for r in rows]
        ranges = {}
        for i, axis in enumerate(["x", "y", "z", "w", "v"]):
            vals = [c[i] for c in coords]
            ranges[axis] = (min(vals), max(vals))

        # Assign memories to grid cells
        cells: dict[tuple, list[int]] = defaultdict(list)
        for idx, row in enumerate(rows):
            key = self._cell_key(row["x"], row["y"], row["z"], row["w"], row["v"], ranges)
            cells[key].append(idx)

        # Find dense cells (above threshold)
        dense_cells = {
            k: v for k, v in cells.items()
            if len(v) >= self._min_size
        }

        if not dense_cells:
            report.duration_ms = (time.perf_counter() - start) * 1000
            return report

        # Merge adjacent dense cells into constellations
        merged = self._merge_adjacent(dense_cells)

        # Build Constellation objects
        used_names: set[str] = set()
        constellations = []
        for cell_group in merged:
            member_indices = []
            for cell_key in cell_group:
                member_indices.extend(dense_cells[cell_key])

            # Deduplicate
            member_indices = list(set(member_indices))
            if len(member_indices) < self._min_size:
                continue

            # Compute centroid
            member_coords = [coords[i] for i in member_indices]
            centroid = tuple(
                sum(c[d] for c in member_coords) / len(member_coords)
                for d in range(5)
            )

            # Average distance from centroid
            avg_radius = sum(
                self._distance_5d(c, centroid) for c in member_coords
            ) / len(member_coords)

            # Dominant tags
            member_ids = [rows[i]["memory_id"] for i in member_indices]
            tag_counts = self._get_tag_counts(backend, member_ids)
            dominant_tags = sorted(tag_counts, key=lambda t: tag_counts.get(t, 0), reverse=True)[:5]

            # Dominant type
            type_counts: dict[str, int] = defaultdict(int)
            for i in member_indices:
                type_counts[rows[i]["memory_type"]] += 1
            dominant_type = max(type_counts, key=lambda t: type_counts.get(t, 0))

            # Average importance
            avg_imp = sum(rows[i]["importance"] for i in member_indices) / len(member_indices)

            # Zone from centroid V
            from whitemagic.core.memory.galactic_map import classify_zone
            zone = classify_zone(1.0 - centroid[4]).value  # V is inverted distance

            # Name from dominant tags
            name = self._generate_name(dominant_tags, dominant_type, zone, used_names)

            constellations.append(Constellation(
                name=name,
                member_ids=member_ids,
                centroid=centroid,
                radius=avg_radius,
                dominant_tags=dominant_tags,
                dominant_type=dominant_type,
                avg_importance=avg_imp,
                zone=zone,
            ))

        # Sort by size descending, cap
        constellations.sort(key=lambda c: len(c.member_ids), reverse=True)
        constellations = constellations[:self._max_constellations]

        report.constellations = constellations
        report.constellations_found = len(constellations)
        report.largest_constellation = max(
            (len(c.member_ids) for c in constellations), default=0,
        )
        report.duration_ms = (time.perf_counter() - start) * 1000

        with self._lock:
            self._last_report = report
            self._last_detect_time = time.time()

        # Record centroid history for drift tracking
        self._record_centroid_history(constellations)

        # Gap A3 synthesis: Feed constellations into Knowledge Graph
        self._feed_knowledge_graph(constellations)

        logger.info(
            f"✨ Constellation detection: {report.constellations_found} constellations "
            f"found in {report.memories_scanned} memories ({report.duration_ms:.0f}ms). "
            f"Largest: {report.largest_constellation} members",
        )
        return report

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _merge_adjacent(
        self, dense_cells: dict[tuple, list[int]],
    ) -> list[list[tuple]]:
        """Merge adjacent dense cells into groups (simple flood-fill)."""
        visited: set[tuple] = set()
        groups: list[list[tuple]] = []

        for cell_key in dense_cells:
            if cell_key in visited:
                continue

            # Flood-fill from this cell
            group = []
            queue = [cell_key]
            while queue:
                current = queue.pop()
                if current in visited:
                    continue
                visited.add(current)
                group.append(current)

                # Check all 5D neighbors (±1 on each axis)
                for dim in range(5):
                    for delta in (-1, 1):
                        neighbor_list = list(current)
                        neighbor_list[dim] += delta
                        neighbor = tuple(neighbor_list)
                        if neighbor in dense_cells and neighbor not in visited:
                            queue.append(neighbor)

            groups.append(group)

        return groups

    @staticmethod
    def _get_tag_counts(backend: Any, memory_ids: list[str]) -> dict[str, int]:
        """Get tag frequency for a set of memory IDs."""
        counts: dict[str, int] = defaultdict(int)
        if not memory_ids:
            return counts

        # Process in chunks to avoid huge IN clauses
        chunk_size = 200
        for i in range(0, len(memory_ids), chunk_size):
            chunk = memory_ids[i:i + chunk_size]
            placeholders = ",".join("?" * len(chunk))
            try:
                with backend.pool.connection() as conn:
                    rows = conn.execute(
                        f"SELECT tag, COUNT(*) as cnt FROM tags "
                        f"WHERE memory_id IN ({placeholders}) "
                        f"GROUP BY tag ORDER BY cnt DESC",
                        chunk,
                    ).fetchall()
                    for row in rows:
                        tag = row[0] if isinstance(row, tuple) else row["tag"]
                        cnt = row[1] if isinstance(row, tuple) else row["cnt"]
                        # Skip generic tags
                        if tag not in ("archive", "scavenged", "meta", "history"):
                            counts[tag] += cnt
            except Exception:
                pass

        return counts

    def _generate_name(
        self, dominant_tags: list[str], dominant_type: str, zone: str,
        used_names: set[str],
    ) -> str:
        """Generate a unique constellation name from its characteristics."""
        # Use top 2 tags as the name base
        tag_part = " ".join(t.replace("_", " ").title() for t in dominant_tags[:2])
        if not tag_part:
            tag_part = dominant_type.replace("_", " ").title()

        # Prefix by zone
        zone_prefix = {
            "core": "Corona",
            "inner_rim": "Aureola",
            "mid_band": "Nebula",
            "outer_rim": "Drift",
            "far_edge": "Veil",
        }
        prefix = zone_prefix.get(zone, "Cluster")
        base_name = f"{prefix} {tag_part}"

        # Ensure uniqueness with roman numeral suffixes
        if base_name not in used_names:
            used_names.add(base_name)
            return base_name

        _roman = ["II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]
        for suffix in _roman:
            candidate = f"{base_name} {suffix}"
            if candidate not in used_names:
                used_names.add(candidate)
                return candidate

        # Fallback: use size
        fallback = f"{base_name} ({len(used_names)})"
        used_names.add(fallback)
        return fallback

    # ------------------------------------------------------------------
    # Gap A3 synthesis: Constellations → Knowledge Graph
    # ------------------------------------------------------------------

    def _feed_knowledge_graph(self, constellations: list[Constellation]) -> None:
        """Register constellation entities and membership relations in the KG.

        Each constellation becomes a KG entity with its dominant tags.
        Members get 'belongs_to_constellation' relations, allowing the
        KG to answer queries like "which constellation does memory X
        belong to?" or "what are the themes of constellation Y?"
        """
        if not constellations:
            return
        try:
            from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
            kg = get_knowledge_graph()

            entities_created = 0
            for c in constellations:
                # Register constellation as a KG entity
                kg.add_entity(
                    name=c.name,
                    entity_type="constellation",
                    metadata={
                        "zone": c.zone,
                        "size": len(c.member_ids),
                        "dominant_tags": c.dominant_tags,
                        "avg_importance": c.avg_importance,
                        "origin": "constellation_detector",
                    },
                )
                entities_created += 1

                # Link a sample of members (avoid huge fan-out)
                for mid in c.member_ids[:20]:
                    kg.add_relation(
                        source=mid,
                        relation="belongs_to_constellation",
                        target=c.name,
                        metadata={"zone": c.zone},
                    )

            if entities_created:
                logger.info(
                    f"KG enrichment: {entities_created} constellation entities registered",
                )
        except Exception as e:
            logger.debug(f"KG enrichment from constellations skipped: {e}")

    def find_nearest_constellation(
        self, x: float, y: float, z: float, w: float, v: float,
        max_distance: float = 0.5,
    ) -> dict[str, Any] | None:
        """Find the nearest constellation to a 5D point.

        Returns a dict with constellation name, distance, and zone if
        a constellation is within max_distance. Uses the last detection
        report (cached). Returns None if no constellations are cached
        or none are close enough.
        """
        with self._lock:
            if not self._last_report or not self._last_report.constellations:
                return None

        point = (x, y, z, w, v)
        best = None
        best_dist = max_distance

        for c in self._last_report.constellations:
            dist = self._distance_5d(point, c.centroid)
            if dist < best_dist:
                best_dist = dist
                best = c

        if best is None:
            return None

        return {
            "constellation": best.name,
            "distance": round(best_dist, 3),
            "zone": best.zone,
            "size": len(best.member_ids),
            "dominant_tags": best.dominant_tags[:3],
        }

    def annotate_memories(
        self, memories: list, backend: Any=None,
    ) -> list:
        """Annotate a list of Memory objects with constellation context.

        For each memory that has holographic coordinates, checks if it
        falls near a known constellation and adds metadata annotation.
        Modifies memories in-place and returns them.
        """
        with self._lock:
            if not self._last_report or not self._last_report.constellations:
                return memories

        if not backend:
            return memories

        for mem in memories:
            try:
                coords = backend.get_coords(mem.id)
                if not coords:
                    continue
                match = self.find_nearest_constellation(*coords)
                if match:
                    if not hasattr(mem, "metadata") or mem.metadata is None:
                        mem.metadata = {}
                    mem.metadata["constellation"] = match
            except Exception:
                continue

        return memories

    def get_last_report(self) -> dict[str, Any] | None:
        with self._lock:
            return self._last_report.to_dict() if self._last_report else None

    def get_cached_or_detect(self, sample_limit: int = 10000) -> DetectionReport | None:
        """Return cached report if TTL is valid, otherwise re-detect.

        This is the preferred entry point for callers that don't need
        guaranteed-fresh data (e.g., CoreAccessLayer, InsightPipeline).
        """
        with self._lock:
            elapsed = time.time() - self._last_detect_time
            if self._last_report is not None and elapsed < self._cache_ttl:
                return self._last_report
        return self.detect(sample_limit=sample_limit)

    # ------------------------------------------------------------------
    # Drift Tracking
    # ------------------------------------------------------------------

    def _record_centroid_history(self, constellations: list[Constellation]) -> None:
        """Record current centroids into drift history."""
        now = datetime.now().isoformat()
        for c in constellations:
            if c.name not in self._centroid_history:
                self._centroid_history[c.name] = []
            history = self._centroid_history[c.name]
            history.append((now, c.centroid))
            # Trim to max history
            if len(history) > self._max_history_per_constellation:
                self._centroid_history[c.name] = history[-self._max_history_per_constellation:]

    def get_drift_vectors(self, window_days: int = 7) -> list[dict[str, Any]]:
        """Compute drift vectors for all tracked constellations.

        Returns list of dicts with:
          - name: constellation name
          - current_centroid: current 5D position
          - drift_vector: 5D displacement over the window
          - drift_magnitude: Euclidean distance moved
          - samples: number of historical observations in the window
        """
        import math as _math
        from datetime import datetime as _dt, timedelta as _td

        cutoff = (_dt.now() - _td(days=window_days)).isoformat()
        results: list[dict[str, Any]] = []

        with self._lock:
            current_report = self._last_report

        if not current_report:
            return []

        current_centroids = {c.name: c.centroid for c in current_report.constellations}

        for name, history in self._centroid_history.items():
            if name not in current_centroids:
                continue

            # Filter history within window
            window_points = [(ts, c) for ts, c in history if ts >= cutoff]
            if len(window_points) < 2:
                continue

            earliest = window_points[0][1]
            current = current_centroids[name]

            drift_vec = tuple(c - e for c, e in zip(current, earliest))
            magnitude = _math.sqrt(sum(d * d for d in drift_vec))

            results.append({
                "name": name,
                "current_centroid": {"x": current[0], "y": current[1],
                                     "z": current[2], "w": current[3],
                                     "v": current[4]},
                "drift_vector": {"dx": round(drift_vec[0], 4),
                                 "dy": round(drift_vec[1], 4),
                                 "dz": round(drift_vec[2], 4),
                                 "dw": round(drift_vec[3], 4),
                                 "dv": round(drift_vec[4], 4)},
                "drift_magnitude": round(magnitude, 4),
                "samples": len(window_points),
                "window_days": window_days,
            })

        results.sort(key=lambda r: -r["drift_magnitude"])
        return results


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_detector_instance: ConstellationDetector | None = None
_detector_lock = threading.Lock()


def get_constellation_detector(
    bins_per_axis: int = 8,
    min_cluster_size: int = 5,
    max_constellations: int = 30,
) -> ConstellationDetector:
    """Get or create the global ConstellationDetector singleton."""
    global _detector_instance
    with _detector_lock:
        if _detector_instance is None:
            _detector_instance = ConstellationDetector(
                bins_per_axis=bins_per_axis,
                min_cluster_size=min_cluster_size,
                max_constellations=max_constellations,
            )
        return _detector_instance
