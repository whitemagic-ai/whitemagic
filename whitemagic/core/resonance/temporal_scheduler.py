"""Temporal Scheduler - Time-Bucketed Event Processing.
====================================================
Inspired by CyberBrains' multi-timescale architecture:
  - Brainstem: <10ms reflexes (FAST lane)
  - Cortex: ~1s planning loops (MEDIUM lane)
  - Hippocampus: minutes/hours consolidation (SLOW lane)

Each event is routed to a temporal lane based on its type. Fast-lane
events fire immediately (reflexes, safety). Medium-lane events are
batched into 1-second planning windows. Slow-lane events accumulate
and flush on a configurable consolidation interval.

Usage:
    from whitemagic.core.resonance.temporal_scheduler import (
        get_temporal_scheduler, TemporalLane
    )

    scheduler = get_temporal_scheduler()
    scheduler.start()

    # Events are auto-routed by type, or you can force a lane:
    scheduler.schedule(event)                     # auto-route
    scheduler.schedule(event, lane=TemporalLane.FAST)  # force fast

    scheduler.stop()
"""

import logging
import threading
import time
from collections import defaultdict, deque
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    GanYingBus,
    ResonanceEvent,
    get_bus,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Temporal Lanes
# ---------------------------------------------------------------------------

class TemporalLane(Enum):
    """Processing timescales inspired by the biological CNS."""

    FAST = "fast"        # <10ms  — reflexes, safety overrides, system events
    MEDIUM = "medium"    # ~1s   — planning, reasoning, tool dispatch
    SLOW = "slow"        # 60s+  — consolidation, pattern analysis, memory decay


@dataclass
class LaneConfig:
    """Configuration for a single temporal lane."""

    flush_interval_ms: float   # How often the lane flushes (0 = immediate)
    max_queue_size: int = 5000
    enabled: bool = True


@dataclass
class TemporalConfig:
    """Full scheduler configuration."""

    fast: LaneConfig = field(default_factory=lambda: LaneConfig(flush_interval_ms=0))
    medium: LaneConfig = field(default_factory=lambda: LaneConfig(flush_interval_ms=1000))
    slow: LaneConfig = field(default_factory=lambda: LaneConfig(flush_interval_ms=60_000))


# ---------------------------------------------------------------------------
# Default lane routing table
# ---------------------------------------------------------------------------

# Events that MUST fire immediately (safety, system lifecycle, threats)
_FAST_EVENTS: set[EventType] = {
    EventType.SYSTEM_STARTED,
    EventType.SYSTEM_STOPPED,
    EventType.SYSTEM_HEALTH_CHANGED,
    EventType.SYSTEM_STATE_CHANGE,
    EventType.THREAT_DETECTED,
    EventType.THREAT_NEUTRALIZED,
    EventType.ANOMALY_DETECTED,
    EventType.BOUNDARY_VIOLATED,
    EventType.VIOLATION_PREVENTED,
    EventType.INTERVENTION_TRIGGERED,
    EventType.WARNING_ISSUED,
    EventType.PROTECTION_ACTIVATED,
    EventType.SHIELD_RAISED,
    EventType.BALANCE_RESTORED,
    EventType.HEALTH_OPTIMAL,
    EventType.SANCTUARY_ENTERED,
    EventType.SAFETY_ESTABLISHED,
    # Distributed coordination — immediate attention
    EventType.BROKER_DISCONNECTED,
    EventType.TASK_FAILED,
    EventType.AGENT_DEREGISTERED,
}

# Events that accumulate for consolidation (memory, patterns, slow learning)
_SLOW_EVENTS: set[EventType] = {
    EventType.MEMORY_CONSOLIDATED,
    EventType.PATTERN_DETECTED,
    EventType.PATTERN_DISCOVERED,
    EventType.PATTERNS_ANALYZED,
    EventType.PATTERN_EMERGED,
    EventType.PATTERN_CONFIRMED,
    EventType.PATTERN_REJECTED,
    EventType.PATTERN_CLUSTER_CRYSTALLIZED,
    EventType.WISDOM_INTEGRATED,
    EventType.INSIGHT_CRYSTALLIZED,
    EventType.LEARNING_COMPLETED,
    EventType.HABIT_FORMED,
    EventType.RHYTHM_ESTABLISHED,
    EventType.NOVEL_CAPABILITY_EMERGED,
    EventType.CONSCIOUSNESS_SHIFT_DETECTED,
    EventType.COLLECTIVE_INSIGHT_FORMED,
    EventType.EVOLUTION_PROGRESSED,
    EventType.METAMORPHOSIS_COMPLETED,
    EventType.REFLECTION_RECORDED,
    EventType.HEALING_INITIATED,
    EventType.RECOVERY_PROGRESSED,
    EventType.RESTORATION_COMPLETED,
    # Distributed coordination — consolidation
    EventType.VOTE_CONSENSUS_REACHED,
    EventType.VOTE_SESSION_CLOSED,
}

# Everything else falls into MEDIUM (planning-speed).


def classify_event(event_type: EventType) -> TemporalLane:
    """Classify an event type into its natural temporal lane."""
    if event_type in _FAST_EVENTS:
        return TemporalLane.FAST
    if event_type in _SLOW_EVENTS:
        return TemporalLane.SLOW
    return TemporalLane.MEDIUM


# ---------------------------------------------------------------------------
# Temporal Scheduler
# ---------------------------------------------------------------------------

@dataclass
class LaneStats:
    """Runtime statistics for a temporal lane."""

    events_queued: int = 0
    events_flushed: int = 0
    flushes: int = 0
    total_flush_time_ms: float = 0.0
    peak_queue_depth: int = 0


class TemporalScheduler:
    """Routes events to temporal lanes and flushes them on independent cadences.

    FAST lane events bypass the queue entirely and dispatch synchronously
    to the underlying Gan Ying Bus. MEDIUM and SLOW lanes accumulate events
    and flush on their configured intervals.

    Thread-safe. Start/stop controls the background flush threads.
    """

    def __init__(
        self,
        bus: GanYingBus | None = None,
        config: TemporalConfig | None = None,
    ):
        self._bus = bus or get_bus()
        self._config = config or TemporalConfig()

        # Per-lane queues
        self._queues: dict[TemporalLane, deque] = {
            TemporalLane.FAST: deque(maxlen=self._config.fast.max_queue_size),
            TemporalLane.MEDIUM: deque(maxlen=self._config.medium.max_queue_size),
            TemporalLane.SLOW: deque(maxlen=self._config.slow.max_queue_size),
        }

        self._lock = threading.RLock()
        self._stats: dict[TemporalLane, LaneStats] = {
            lane: LaneStats() for lane in TemporalLane
        }

        # Background flush threads (one per non-FAST lane)
        self._timers: dict[TemporalLane, threading.Event | None] = {
            TemporalLane.MEDIUM: None,
            TemporalLane.SLOW: None,
        }
        self._threads: dict[TemporalLane, threading.Thread | None] = {
            TemporalLane.MEDIUM: None,
            TemporalLane.SLOW: None,
        }
        self._running = False

        # Optional pre-flush and post-flush hooks (for salience filtering, etc.)
        self._pre_flush_hooks: dict[TemporalLane, list[Callable]] = defaultdict(list)
        self._post_flush_hooks: dict[TemporalLane, list[Callable]] = defaultdict(list)

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def start(self) -> None:
        """Start background flush threads for MEDIUM and SLOW lanes."""
        if self._running:
            return
        self._running = True

        for lane in (TemporalLane.MEDIUM, TemporalLane.SLOW):
            stop_event = threading.Event()
            self._timers[lane] = stop_event
            cfg = self._lane_config(lane)
            t = threading.Thread(
                target=self._flush_loop,
                args=(lane, cfg.flush_interval_ms / 1000.0, stop_event),
                daemon=True,
                name=f"temporal-{lane.value}",
            )
            self._threads[lane] = t
            t.start()

        logger.info(
            "⏱️  Temporal Scheduler started  "
            f"[FAST=immediate, MEDIUM={self._config.medium.flush_interval_ms}ms, "
            f"SLOW={self._config.slow.flush_interval_ms}ms]",
        )

    def stop(self) -> None:
        """Stop background flush threads and drain remaining events."""
        if not self._running:
            return
        self._running = False

        for lane in (TemporalLane.MEDIUM, TemporalLane.SLOW):
            stop_evt = self._timers.get(lane)
            if stop_evt:
                stop_evt.set()
            thread = self._threads.get(lane)
            if thread and thread.is_alive():
                thread.join(timeout=5.0)

        # Final drain
        for lane in (TemporalLane.MEDIUM, TemporalLane.SLOW):
            self._flush_lane(lane)

        logger.info("⏱️  Temporal Scheduler stopped.")

    # ------------------------------------------------------------------
    # Scheduling
    # ------------------------------------------------------------------

    def schedule(
        self,
        event: ResonanceEvent,
        lane: TemporalLane | None = None,
    ) -> TemporalLane:
        """Schedule an event for processing.

        Args:
            event: The resonance event to schedule.
            lane: Force a specific lane, or None to auto-classify.

        Returns:
            The lane the event was routed to.

        """
        target = lane or classify_event(event.event_type)

        if target == TemporalLane.FAST:
            # Immediate dispatch — no queuing
            self._stats[TemporalLane.FAST].events_queued += 1
            self._stats[TemporalLane.FAST].events_flushed += 1
            self._bus.emit(event)
            return TemporalLane.FAST

        with self._lock:
            q = self._queues[target]
            q.append(event)
            stats = self._stats[target]
            stats.events_queued += 1
            stats.peak_queue_depth = max(stats.peak_queue_depth, len(q))

        return target

    # ------------------------------------------------------------------
    # Hooks
    # ------------------------------------------------------------------

    def add_pre_flush_hook(self, lane: TemporalLane, hook: Callable) -> None:
        """Add a hook called before a lane flushes. Receives List[ResonanceEvent]."""
        self._pre_flush_hooks[lane].append(hook)

    def add_post_flush_hook(self, lane: TemporalLane, hook: Callable) -> None:
        """Add a hook called after a lane flushes. Receives List[ResonanceEvent]."""
        self._post_flush_hooks[lane].append(hook)

    # ------------------------------------------------------------------
    # Flushing
    # ------------------------------------------------------------------

    def _flush_loop(self, lane: TemporalLane, interval_s: float, stop: threading.Event) -> None:
        """Background thread that periodically flushes a lane."""
        while not stop.is_set():
            stop.wait(timeout=interval_s)
            if not self._running:
                break
            self._flush_lane(lane)

    def _flush_lane(self, lane: TemporalLane) -> int:
        """Flush all queued events for a lane to the bus. Returns count flushed."""
        with self._lock:
            q = self._queues[lane]
            if not q:
                return 0
            batch = list(q)
            q.clear()

        # Pre-flush hooks (e.g. salience filtering)
        for hook in self._pre_flush_hooks.get(lane, []):
            try:
                result = hook(batch)
                if isinstance(result, list):
                    batch = result  # hook can filter/reorder
            except Exception as exc:
                logger.warning(f"Pre-flush hook error ({lane.value}): {exc}")

        start = time.perf_counter()
        for event in batch:
            try:
                self._bus.emit(event)
            except Exception as exc:
                logger.warning(f"Temporal flush emit error: {exc}")
        elapsed_ms = (time.perf_counter() - start) * 1000

        # Post-flush hooks
        for hook in self._post_flush_hooks.get(lane, []):
            try:
                hook(batch)
            except Exception as exc:
                logger.warning(f"Post-flush hook error ({lane.value}): {exc}")

        # Stats
        stats = self._stats[lane]
        stats.events_flushed += len(batch)
        stats.flushes += 1
        stats.total_flush_time_ms += elapsed_ms

        return len(batch)

    def flush_all(self) -> dict[str, int]:
        """Force-flush all lanes immediately. Returns counts per lane."""
        return {
            lane.value: self._flush_lane(lane)
            for lane in (TemporalLane.MEDIUM, TemporalLane.SLOW)
        }

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def _lane_config(self, lane: TemporalLane) -> LaneConfig:
        return {
            TemporalLane.FAST: self._config.fast,
            TemporalLane.MEDIUM: self._config.medium,
            TemporalLane.SLOW: self._config.slow,
        }[lane]

    def get_queue_depth(self, lane: TemporalLane) -> int:
        with self._lock:
            return len(self._queues[lane])

    def get_stats(self) -> dict[str, Any]:
        """Return per-lane statistics."""
        return {
            lane.value: {
                "events_queued": self._stats[lane].events_queued,
                "events_flushed": self._stats[lane].events_flushed,
                "flushes": self._stats[lane].flushes,
                "avg_flush_time_ms": (
                    round(self._stats[lane].total_flush_time_ms / self._stats[lane].flushes, 2)
                    if self._stats[lane].flushes > 0
                    else 0.0
                ),
                "peak_queue_depth": self._stats[lane].peak_queue_depth,
                "current_queue_depth": len(self._queues[lane]),
            }
            for lane in TemporalLane
        }

    @property
    def is_running(self) -> bool:
        return self._running


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_scheduler_instance: TemporalScheduler | None = None
_scheduler_lock = threading.Lock()


def get_temporal_scheduler(
    config: TemporalConfig | None = None,
) -> TemporalScheduler:
    """Get or create the global TemporalScheduler singleton."""
    global _scheduler_instance
    with _scheduler_lock:
        if _scheduler_instance is None:
            _scheduler_instance = TemporalScheduler(config=config)
        return _scheduler_instance
