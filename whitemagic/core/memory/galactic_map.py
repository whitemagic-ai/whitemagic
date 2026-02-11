"""Galactic Map - Memory Lifecycle as Stellar Rotation.
====================================================
Every memory lives in a galaxy. Active, vital memories orbit close to the
galactic core (distance ≈ 0.0). As they age, fade, or lose relevance, they
spiral outward toward the galactic edge (distance → 1.0). But no star is
ever extinguished — it simply drifts to the far rim, still visible to anyone
who looks far enough.

The Galactic Map computes each memory's position (galactic_distance) from
its multi-signal retention score, then persists that position to the DB.

Zones:
  0.00 - 0.15  CORE       — Hot, frequently accessed, high salience
  0.15 - 0.40  INNER RIM  — Stable, important, well-connected
  0.40 - 0.65  MID BAND   — Moderate relevance, occasional recall
  0.65 - 0.85  OUTER RIM  — Fading, rarely accessed, low emotional weight
  0.85 - 1.00  FAR EDGE   — Deep archive, minimal neuro_score, but never lost

Usage:
    from whitemagic.core.memory.galactic_map import (
        get_galactic_map, GalacticMap
    )

    gmap = get_galactic_map()
    report = gmap.full_sweep()        # Score + position every memory
    print(report.zone_counts)         # {'CORE': 42, 'INNER_RIM': 130, ...}

    # Or for a single memory:
    distance = gmap.compute_distance(memory)
"""

import logging
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from whitemagic.core.memory.unified_types import Memory

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Galactic Zones
# ---------------------------------------------------------------------------

class GalacticZone(Enum):
    """Named zones radiating outward from the galactic core."""

    CORE = "core"              # 0.00 - 0.15
    INNER_RIM = "inner_rim"    # 0.15 - 0.40
    MID_BAND = "mid_band"      # 0.40 - 0.65
    OUTER_RIM = "outer_rim"    # 0.65 - 0.85
    FAR_EDGE = "far_edge"      # 0.85 - 1.00


def classify_zone(distance: float) -> GalacticZone:
    """Map a galactic distance to its named zone."""
    if distance < 0.15:
        return GalacticZone.CORE
    elif distance < 0.40:
        return GalacticZone.INNER_RIM
    elif distance < 0.65:
        return GalacticZone.MID_BAND
    elif distance < 0.85:
        return GalacticZone.OUTER_RIM
    else:
        return GalacticZone.FAR_EDGE


# ---------------------------------------------------------------------------
# Sweep Report
# ---------------------------------------------------------------------------

@dataclass
class GalacticSweepReport:
    """Results from a full galactic map sweep."""

    total_memories: int = 0
    memories_updated: int = 0
    zone_counts: dict[str, int] = field(default_factory=dict)
    avg_retention: float = 0.0
    avg_distance: float = 0.0
    core_count: int = 0
    edge_count: int = 0
    protected_count: int = 0
    sweep_duration_ms: float = 0.0
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        return {
            "total_memories": self.total_memories,
            "memories_updated": self.memories_updated,
            "zone_counts": self.zone_counts,
            "avg_retention": round(self.avg_retention, 4),
            "avg_distance": round(self.avg_distance, 4),
            "core_count": self.core_count,
            "edge_count": self.edge_count,
            "protected_count": self.protected_count,
            "sweep_duration_ms": round(self.sweep_duration_ms, 2),
            "timestamp": self.timestamp.isoformat(),
        }


# ---------------------------------------------------------------------------
# Galactic Map Engine
# ---------------------------------------------------------------------------

class GalacticMap:
    """Computes and persists galactic distance for every memory.

    Galactic distance is the inverse of retention score:
      distance = 1.0 - retention_score

    Protected/core-identity/sacred memories are pinned to the core
    regardless of their computed score.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._total_sweeps = 0

    # ------------------------------------------------------------------
    # Distance computation
    # ------------------------------------------------------------------

    def compute_distance(self, mem: Memory, retention_score: float | None = None) -> float:
        """Compute galactic distance for a single memory.

        If retention_score is not provided, uses a quick heuristic from
        the memory's own fields. For precise results, pass the score
        from the RetentionEngine.
        """
        # Protected memories are always at the core
        if mem.is_protected or mem.is_core_identity or mem.is_sacred or mem.is_pinned:
            return 0.0

        if retention_score is not None:
            score = retention_score
        else:
            score = self._quick_retention_estimate(mem)

        # Distance is the inverse of retention
        distance = 1.0 - max(0.0, min(1.0, score))
        return round(max(0.0, min(1.0, distance)), 4)

    def _quick_retention_estimate(self, mem: Memory) -> float:
        """Fast heuristic retention estimate from memory fields alone.
        Used when the full RetentionEngine is not invoked.
        """
        # Weighted average of available signals
        signals = [
            (mem.importance, 1.0),
            (mem.neuro_score, 0.9),
            (abs(mem.emotional_valence), 0.6),
            (min(1.0, mem.recall_count / 20.0), 0.5),
        ]

        total_weight = sum(w for _, w in signals)
        weighted_sum = sum(v * w for v, w in signals)
        return weighted_sum / total_weight if total_weight > 0 else 0.5

    # ------------------------------------------------------------------
    # Full sweep
    # ------------------------------------------------------------------

    def full_sweep(self, batch_size: int = 500) -> GalacticSweepReport:
        """Sweep ALL memories in the database:
          1. Load in batches
          2. Evaluate retention with the RetentionEngine
          3. Compute galactic distance
          4. Batch-update the DB
          5. Return a report.

        This is designed to be run from the slow-lane of the
        TemporalScheduler or as a periodic maintenance task.
        """
        start = time.perf_counter()
        report = GalacticSweepReport()

        # Get retention engine
        try:
            from whitemagic.core.memory.mindful_forgetting import get_retention_engine
            retention = get_retention_engine()
        except Exception as exc:
            logger.error(f"GalacticMap sweep: RetentionEngine unavailable: {exc}")
            return report

        # Get backend
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            backend = um.backend
        except Exception as exc:
            logger.error(f"GalacticMap sweep: UnifiedMemory unavailable: {exc}")
            return report

        # Fetch all memories in batches
        all_memories = backend.list_recent(limit=50000)
        report.total_memories = len(all_memories)

        zone_counts: dict[str, int] = {z.value: 0 for z in GalacticZone}
        total_retention = 0.0
        total_distance = 0.0
        updates: list[tuple[str, float, float]] = []

        # Try Rust accelerated batch scoring for large sweeps
        used_rust = False
        if len(all_memories) > 100:
            try:
                from whitemagic.optimization.rust_accelerators import (
                    galactic_batch_score,
                    rust_available,
                )
                if rust_available():
                    mem_dicts = []
                    for mem in all_memories:
                        mem_dicts.append({
                            "id": mem.id,
                            "importance": getattr(mem, "importance", 0.5),
                            "neuro_score": getattr(mem, "neuro_score", 0.5),
                            "emotional_valence": getattr(mem, "emotional_valence", 0.0),
                            "recall_count": getattr(mem, "recall_count", 0),
                            "is_protected": bool(getattr(mem, "is_protected", False)),
                            "is_core_identity": bool(getattr(mem, "is_core_identity", False)),
                            "is_sacred": bool(getattr(mem, "is_sacred", False)),
                            "is_pinned": bool(getattr(mem, "is_pinned", False)),
                            "memory_type_weight": getattr(mem, "importance", 0.5),
                            "richness": min(1.0, len(str(mem.content)) / 2000.0) if mem.content else 0.3,
                            "activity": min(1.0, getattr(mem, "recall_count", 0) / 20.0),
                            "recency": 0.5,
                            "emotion": abs(getattr(mem, "emotional_valence", 0.0)),
                            "protection": 1.0 if any([
                                getattr(mem, "is_protected", False),
                                getattr(mem, "is_core_identity", False),
                                getattr(mem, "is_sacred", False),
                                getattr(mem, "is_pinned", False),
                            ]) else 0.0,
                        })

                    results = galactic_batch_score(mem_dicts, quick=False)
                    result_map = {r["id"]: r for r in results}

                    for mem in all_memories:
                        r = result_map.get(mem.id)
                        if not r:
                            continue
                        ret_score = r["retention_score"]
                        distance = r["galactic_distance"]
                        zone = classify_zone(distance)
                        zone_counts[zone.value] += 1
                        total_retention += ret_score
                        total_distance += distance
                        if mem.is_protected or mem.is_core_identity or mem.is_sacred or mem.is_pinned:
                            report.protected_count += 1
                        if zone == GalacticZone.CORE:
                            report.core_count += 1
                        elif zone == GalacticZone.FAR_EDGE:
                            report.edge_count += 1
                        updates.append((mem.id, distance, ret_score))

                        if len(updates) >= batch_size:
                            backend.batch_update_galactic(updates)
                            report.memories_updated += len(updates)
                            updates.clear()

                    used_rust = True
                    logger.debug("Galactic sweep used Rust accelerator")
            except Exception as e:
                logger.debug(f"Rust galactic scoring unavailable, using Python: {e}")

        # Python fallback path
        if not used_rust:
            for mem in all_memories:
                verdict = retention.evaluate(mem)
                ret_score = verdict.score
                distance = self.compute_distance(mem, retention_score=ret_score)
                zone = classify_zone(distance)
                zone_counts[zone.value] += 1
                total_retention += ret_score
                total_distance += distance
                if mem.is_protected or mem.is_core_identity or mem.is_sacred or mem.is_pinned:
                    report.protected_count += 1
                if zone == GalacticZone.CORE:
                    report.core_count += 1
                elif zone == GalacticZone.FAR_EDGE:
                    report.edge_count += 1
                updates.append((mem.id, distance, ret_score))

                if len(updates) >= batch_size:
                    backend.batch_update_galactic(updates)
                    report.memories_updated += len(updates)
                    updates.clear()

        # Flush remaining
        if updates:
            backend.batch_update_galactic(updates)
            report.memories_updated += len(updates)

        # Finalize report
        n = max(report.total_memories, 1)
        report.zone_counts = zone_counts
        report.avg_retention = total_retention / n
        report.avg_distance = total_distance / n
        report.sweep_duration_ms = (time.perf_counter() - start) * 1000

        self._total_sweeps += 1
        logger.info(
            f"🌌 Galactic Map Sweep #{self._total_sweeps}: "
            f"{report.total_memories} memories mapped, "
            f"CORE={report.core_count}, EDGE={report.edge_count}, "
            f"avg_distance={report.avg_distance:.3f}, "
            f"in {report.sweep_duration_ms:.0f}ms",
        )

        return report

    # ------------------------------------------------------------------
    # Decay drift — unaccessed memories drift outward
    # ------------------------------------------------------------------

    def decay_drift(self, drift_rate: float = 0.005, inactivity_days: int = 14) -> dict[str, Any]:
        """Apply outward drift to memories not accessed in `inactivity_days`.

        Protected memories are immune. Memories already at FAR_EDGE don't
        drift further.  Drift rate is additive: each call adds `drift_rate`
        to galactic_distance for qualifying memories.

        Returns a summary dict.
        """

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            backend = um.backend
        except Exception as exc:
            logger.error(f"Decay drift: backend unavailable: {exc}")
            return {"status": "error", "message": str(exc)}

        # Try Rust SQLite accelerator first (v13.1)
        try:
            from whitemagic.optimization.rust_accelerators import (
                rust_v131_available,
            )
            from whitemagic.optimization.rust_accelerators import (
                sqlite_decay_drift as rust_decay_drift,
            )
            if rust_v131_available():
                db_path = str(backend.db_path)
                result = rust_decay_drift(db_path, drift_rate, 0.95)
                if result is not None:
                    drifted = result.get("drifted", 0)
                    logger.info(
                        f"🌀 Decay drift (Rust): {drifted} memories drifted outward by {drift_rate} "
                        f"(inactive > {inactivity_days}d)",
                    )
                    return {
                        "status": "success",
                        "memories_drifted": drifted,
                        "drift_rate": drift_rate,
                        "inactivity_days": inactivity_days,
                        "backend": "rust",
                    }
        except Exception as e:
            logger.debug(f"Rust decay drift unavailable, using Python: {e}")

        # Python fallback
        cutoff = datetime.now()
        from datetime import timedelta
        cutoff = cutoff - timedelta(days=inactivity_days)
        cutoff_iso = cutoff.isoformat()

        with backend.pool.connection() as conn:
            # Drift unaccessed, unprotected memories outward (cap at 0.95)
            cursor = conn.execute(
                """UPDATE memories SET
                     galactic_distance = MIN(0.95, galactic_distance + ?)
                   WHERE is_protected = 0
                     AND accessed_at < ?
                     AND galactic_distance < 0.95""",
                (drift_rate, cutoff_iso),
            )
            drifted = cursor.rowcount
            conn.commit()

        logger.info(
            f"🌀 Decay drift: {drifted} memories drifted outward by {drift_rate} "
            f"(inactive > {inactivity_days}d)",
        )
        return {
            "status": "success",
            "memories_drifted": drifted,
            "drift_rate": drift_rate,
            "inactivity_days": inactivity_days,
        }

    # ------------------------------------------------------------------
    # Lightweight zone counting (for Harmony Vector A4 synthesis)
    # ------------------------------------------------------------------

    def get_zone_counts(self) -> dict[str, int]:
        """Query the DB directly for zone distribution without running a
        full sweep.  Returns {zone_name: count} or empty dict on failure.

        Used by HarmonyVector._galactic_vitality() to compute energy.
        """
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            backend = get_unified_memory().backend

            # Try Rust SQLite accelerator (v13.1)
            try:
                from whitemagic.optimization.rust_accelerators import (
                    rust_v131_available,
                )
                from whitemagic.optimization.rust_accelerators import (
                    sqlite_zone_stats as rust_zone_stats,
                )
                if rust_v131_available():
                    result = rust_zone_stats(str(backend.db_path))
                    if result and "zones" in result:
                        return result["zones"]  # type: ignore[no-any-return]
            except Exception:
                pass

            # Python fallback
            counts: dict[str, int] = {z.value: 0 for z in GalacticZone}
            with backend.pool.connection() as conn:
                rows = conn.execute(
                    "SELECT galactic_distance FROM memories WHERE galactic_distance IS NOT NULL",
                ).fetchall()

            for (dist,) in rows:
                zone = classify_zone(dist)
                counts[zone.value] = counts.get(zone.value, 0) + 1

            return counts
        except Exception as e:
            logger.debug(f"get_zone_counts failed: {e}")
            return {}

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {
            "total_sweeps": self._total_sweeps,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_map_instance: GalacticMap | None = None
_map_lock = threading.Lock()


def get_galactic_map() -> GalacticMap:
    """Get or create the global GalacticMap singleton."""
    global _map_instance
    with _map_lock:
        if _map_instance is None:
            _map_instance = GalacticMap()
        return _map_instance
