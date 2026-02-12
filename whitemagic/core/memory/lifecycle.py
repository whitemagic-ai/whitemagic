"""Memory Lifecycle Manager — Automatic Retention, Decay & Consolidation.
======================================================================
Bridges the RetentionEngine (mindful forgetting) with the Temporal
Scheduler (slow-lane periodic flush) and the Harmony Vector (system
health feedback).

When the TemporalScheduler's SLOW lane flushes, this module:
  1. Runs a retention sweep over all memories.
  2. Applies decay / archive actions per the RetentionVerdict.
  3. Feeds sweep stats into the Harmony Vector's `energy` dimension.
  4. Emits a MEMORY_CONSOLIDATED event so other subsystems can react.

Additionally it exposes MCP tools so AI callers can trigger or inspect
the sweep manually.

Usage:
    from whitemagic.core.memory.lifecycle import get_lifecycle_manager
    mgr = get_lifecycle_manager()
    mgr.attach()  # hooks into temporal scheduler
    # ... or run manually:
    report = mgr.run_sweep()
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class LifecycleConfig:
    """Configuration for the memory lifecycle manager."""

    sweep_interval_sweeps: int = 3      # Run a sweep every N slow-lane flushes
    max_memories_per_sweep: int = 5000  # Cap on memories evaluated per sweep
    persist_scores: bool = True         # Persist retention scores to SQLite
    auto_attach: bool = True            # Auto-hook into temporal scheduler


@dataclass
class LifecycleStats:
    """Accumulated lifecycle statistics."""

    total_sweeps: int = 0
    total_evaluated: int = 0
    total_retained: int = 0
    total_decayed: int = 0
    total_archived: int = 0
    total_protected: int = 0
    last_sweep_duration_ms: float = 0.0
    last_sweep_at: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "total_sweeps": self.total_sweeps,
            "total_evaluated": self.total_evaluated,
            "total_retained": self.total_retained,
            "total_decayed": self.total_decayed,
            "total_archived": self.total_archived,
            "total_protected": self.total_protected,
            "last_sweep_duration_ms": round(self.last_sweep_duration_ms, 1),
            "last_sweep_at": self.last_sweep_at,
        }


class MemoryLifecycleManager:
    """Connects mindful forgetting to the temporal scheduler and harmony vector.

    The lifecycle manager hooks into the SLOW lane's post-flush callback.
    Every N slow-lane flushes, it runs a retention sweep. This ensures
    memory maintenance happens on the "hippocampal" timescale — minutes,
    not milliseconds.
    """

    def __init__(self, config: LifecycleConfig | None = None) -> None:
        self._config = config or LifecycleConfig()
        self._lock = threading.Lock()
        self._stats = LifecycleStats()
        self._flush_count = 0
        self._attached = False

    # ------------------------------------------------------------------
    # Temporal Scheduler integration
    # ------------------------------------------------------------------

    def attach(self) -> bool:
        """Hook into the temporal scheduler's SLOW lane post-flush."""
        if self._attached:
            return True
        try:
            from whitemagic.core.resonance.temporal_scheduler import (
                TemporalLane,
                get_temporal_scheduler,
            )
            scheduler = get_temporal_scheduler()
            scheduler.add_post_flush_hook(TemporalLane.SLOW, self._on_slow_flush)
            self._attached = True
            logger.info(
                f"Memory Lifecycle Manager attached to SLOW lane "
                f"(sweep every {self._config.sweep_interval_sweeps} flushes)",
            )
            return True
        except Exception as e:
            logger.debug(f"Could not attach lifecycle manager: {e}")
            return False

    def _on_slow_flush(self, events: list) -> None:
        """Called after every SLOW lane flush. Conditionally triggers a sweep."""
        self._flush_count += 1
        if self._flush_count % self._config.sweep_interval_sweeps == 0:
            # Run sweep in a background thread to avoid blocking the flush
            t = threading.Thread(
                target=self.run_sweep,
                daemon=True,
                name="memory-lifecycle-sweep",
            )
            t.start()

    # ------------------------------------------------------------------
    # Core sweep
    # ------------------------------------------------------------------

    def run_sweep(self, persist: bool | None = None) -> dict[str, Any]:
        """Run a full retention sweep + galactic rotation.

        Phase 1: Retention scoring (mindful forgetting engine)
        Phase 2: Galactic rotation (update distances based on retention scores)

        Returns a summary dict suitable for MCP tool output.
        """
        if persist is None:
            persist = self._config.persist_scores

        start = time.perf_counter()

        # Phase 1: Retention sweep
        try:
            from whitemagic.core.memory.mindful_forgetting import get_retention_engine
            engine = get_retention_engine()
            report = engine.sweep(persist=persist)
        except Exception as e:
            logger.error(f"Lifecycle sweep failed: {e}")
            return {"status": "error", "message": str(e)}

        # Phase 2: Galactic rotation (update distances from retention scores)
        galactic_report = None
        try:
            from whitemagic.core.memory.galactic_map import get_galactic_map
            gmap = get_galactic_map()
            galactic_report = gmap.full_sweep(
                batch_size=self._config.max_memories_per_sweep,
            )
        except Exception as e:
            logger.debug(f"Galactic rotation skipped: {e}")

        # Phase 3: Decay drift (inactive memories drift outward)
        drift_report = None
        try:
            from whitemagic.core.memory.galactic_map import get_galactic_map
            gmap = get_galactic_map()
            drift_report = gmap.decay_drift()
        except Exception as e:
            logger.debug(f"Decay drift skipped: {e}")

        # Phase 4: Association strength decay (v14.0 Living Graph)
        assoc_decay_report = None
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            assoc_decay_report = um.backend.decay_associations()
        except Exception as e:
            logger.debug(f"Association decay skipped: {e}")

        elapsed_ms = (time.perf_counter() - start) * 1000

        with self._lock:
            self._stats.total_sweeps += 1
            self._stats.total_evaluated += report.total_evaluated
            self._stats.total_retained += report.retained
            self._stats.total_decayed += report.decayed
            self._stats.total_archived += report.archived
            self._stats.total_protected += report.protected
            self._stats.last_sweep_duration_ms = elapsed_ms
            self._stats.last_sweep_at = datetime.now().isoformat()

        # Feed Harmony Vector energy dimension
        self._update_harmony(report)

        # Emit consolidation event
        self._emit_event(report)

        summary = {
            "status": "success",
            "sweep": {
                "evaluated": report.total_evaluated,
                "retained": report.retained,
                "decayed": report.decayed,
                "archived": report.archived,
                "protected": report.protected,
                "duration_ms": round(elapsed_ms, 1),
            },
            "lifetime": self._stats.to_dict(),
        }

        if galactic_report:
            summary["galactic"] = {
                "memories_rotated": galactic_report.memories_updated,
                "zone_counts": galactic_report.zone_counts,
                "avg_distance": round(galactic_report.avg_distance, 4),
            }

        if drift_report and drift_report.get("status") == "success":
            summary["decay_drift"] = {
                "memories_drifted": drift_report["memories_drifted"],
                "drift_rate": drift_report["drift_rate"],
            }

        if assoc_decay_report and assoc_decay_report.get("status") == "success":
            summary["association_decay"] = {
                "evaluated": assoc_decay_report["associations_evaluated"],
                "decayed": assoc_decay_report["associations_decayed"],
                "pruned": assoc_decay_report["associations_pruned"],
            }

        logger.info(
            f"🌌 Memory lifecycle sweep: {report.total_evaluated} evaluated, "
            f"{report.archived} rotated to edge, {elapsed_ms:.0f}ms",
        )
        return summary

    def _update_harmony(self, report: Any) -> None:
        """Feed sweep results into the Harmony Vector."""
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            # If a large fraction of memories are being archived, energy is low
            if report.total_evaluated > 0:
                report.archived / report.total_evaluated
                # Record a synthetic call representing the sweep's health impact
                hv.record_call(
                    tool_name="_lifecycle_sweep",
                    duration_s=self._stats.last_sweep_duration_ms / 1000.0,
                    success=True,
                    declared_safety="READ",
                    actual_writes=report.archived,  # archives are "writes"
                )
        except Exception:
            pass

    def _emit_event(self, report: Any) -> None:
        """Emit a MEMORY_CONSOLIDATED event."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            get_bus().emit(ResonanceEvent(
                event_type=EventType.MEMORY_CONSOLIDATED,
                source="memory_lifecycle",
                data={
                    "evaluated": report.total_evaluated,
                    "archived": report.archived,
                    "decayed": report.decayed,
                },
            ))
        except Exception:
            pass

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return self._stats.to_dict()

    @property
    def is_attached(self) -> bool:
        return self._attached


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_manager: MemoryLifecycleManager | None = None
_manager_lock = threading.Lock()


def get_lifecycle_manager(
    config: LifecycleConfig | None = None,
) -> MemoryLifecycleManager:
    """Get the global Memory Lifecycle Manager."""
    global _manager
    if _manager is None:
        with _manager_lock:
            if _manager is None:
                _manager = MemoryLifecycleManager(config=config)
    return _manager
