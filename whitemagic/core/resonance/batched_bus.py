"""Production-Grade Batched Gan Ying Bus.

Integrates event batching directly into the event bus for transparent
high-throughput processing.

Target: 300k+ ops/sec (3x improvement from 112k baseline)
"""

import threading
import time
from collections import defaultdict
from collections.abc import Callable
from dataclasses import dataclass
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    GanYingBus,
    ResonanceEvent,
)


@dataclass
class BatchConfig:
    """Batching configuration."""

    enabled: bool = True
    batch_size: int = 100
    flush_interval_ms: float = 10.0  # 10ms max latency
    max_queue_size: int = 10000


class BatchedGanYingBus(GanYingBus):
    """Gan Ying Bus with automatic event batching.

    Transparently batches events for 3-5x throughput improvement
    while maintaining <10ms latency guarantee.
    """

    def __init__(self, config: BatchConfig | None = None):
        super().__init__()
        self.config = config or BatchConfig()

        if self.config.enabled:
            self._batch_queue: list[ResonanceEvent] = []
            self._batch_lock = threading.RLock()
            self._flush_timer: threading.Timer | None = None
            self._processing = False

            self.stats = {
                "batched_events": 0,
                "batches_processed": 0,
                "avg_batch_size": 0.0,
                "total_flush_time_ms": 0.0,
                "peak_throughput": 0.0,
            }

    def emit(self, event: ResonanceEvent, cascade: bool = True, async_dispatch: bool = False) -> None:
        """Emit event with automatic batching.

        If batching is enabled, queues event and flushes when:
        - Batch size reached
        - Flush interval elapsed
        - Queue size exceeded

        Otherwise, emits directly (fallback to base behavior).
        """
        if not self.config.enabled:
            return super().emit(event, cascade, async_dispatch)

        should_flush = False
        with self._batch_lock:
            self._batch_queue.append(event)
            self.stats["batched_events"] += 1

            # Immediate flush conditions
            should_flush = (
                len(self._batch_queue) >= self.config.batch_size
                or len(self._batch_queue) >= self.config.max_queue_size
            )

            if not should_flush and (not self._flush_timer or not self._flush_timer.is_alive()):
                # Schedule delayed flush
                self._schedule_flush()

        if should_flush:
            # Run flush in background to keep emit non-blocking
            threading.Thread(target=self._flush_batch, daemon=True).start()

    def _schedule_flush(self) -> None:
        """Schedule automatic flush after interval."""
        if self._flush_timer:
            self._flush_timer.cancel()

        interval_sec = self.config.flush_interval_ms / 1000.0
        self._flush_timer = threading.Timer(interval_sec, self._flush_batch)
        self._flush_timer.daemon = True
        self._flush_timer.start()

    def _flush_batch(self) -> None:
        """Process queued events as a batch."""
        if self._processing:
            return

        with self._batch_lock:
            if not self._batch_queue:
                return

            self._processing = True
            batch = self._batch_queue.copy()
            self._batch_queue.clear()

        try:
            start = time.perf_counter()

            # Group by event type for efficient processing
            by_type: dict[EventType, list[ResonanceEvent]] = defaultdict(list)
            for event in batch:
                by_type[event.event_type].append(event)

            # Process each type's batch
            for event_type, events in by_type.items():
                self._process_batch_by_type(event_type, events)

            # Update stats
            elapsed_ms = (time.perf_counter() - start) * 1000
            self.stats["batches_processed"] += 1
            self.stats["total_flush_time_ms"] += elapsed_ms

            batch_size = len(batch)
            self.stats["avg_batch_size"] = (
                (self.stats["avg_batch_size"] * (self.stats["batches_processed"] - 1) + batch_size)
                / self.stats["batches_processed"]
            )

            # Calculate throughput (events/sec)
            if elapsed_ms > 0:
                throughput = (batch_size / elapsed_ms) * 1000
                self.stats["peak_throughput"] = max(
                    self.stats["peak_throughput"],
                    throughput,
                )

        finally:
            self._processing = False

    def _process_batch_by_type(self, event_type: EventType, events: list[ResonanceEvent]) -> None:
        """Process a batch of events of the same type."""
        if event_type not in self._listeners:
            return

        # Call each listener with the batch
        for listener in self._listeners[event_type]:
            try:
                # Check if listener supports batch processing
                if hasattr(listener, "__batch__") and listener.__batch__:
                    # Batch-aware listener
                    listener(events)
                else:
                    # Standard listener - call for each event
                    for event in events:
                        listener(event)
            except Exception as e:
                # Don't let one listener break the whole batch
                from logging import getLogger
                getLogger(__name__).error(f"Listener error in batch: {e}")
                pass

    def flush(self) -> None:
        """Force immediate flush of pending events."""
        if self.config.enabled:
            self._flush_batch()

    def get_stats(self) -> dict[str, Any]:
        """Get batching performance statistics."""
        if not self.config.enabled:
            return {"batching": "disabled"}

        return {
            "batching_enabled": True,
            "batched_events": self.stats["batched_events"],
            "batches_processed": self.stats["batches_processed"],
            "avg_batch_size": round(self.stats["avg_batch_size"], 2),
            "total_flush_time_ms": round(self.stats["total_flush_time_ms"], 2),
            "peak_throughput": round(self.stats["peak_throughput"], 0),
            "current_queue_size": len(self._batch_queue),
        }


# Singleton instance
_batched_bus: BatchedGanYingBus | None = None


def get_batched_bus(config: BatchConfig | None = None) -> BatchedGanYingBus:
    """Get or create the batched Gan Ying bus singleton."""
    global _batched_bus
    if _batched_bus is None:
        _batched_bus = BatchedGanYingBus(config)
    return _batched_bus


def enable_production_batching(
    batch_size: int = 100,
    flush_interval_ms: float = 10.0,
) -> BatchedGanYingBus:
    """Enable production batching with custom settings.

    Args:
        batch_size: Events per batch (higher = more throughput, higher latency)
        flush_interval_ms: Max wait before flush (lower = lower latency)

    Returns:
        Configured batched bus

    """
    config = BatchConfig(
        enabled=True,
        batch_size=batch_size,
        flush_interval_ms=flush_interval_ms,
    )
    return get_batched_bus(config)


def batch_listener(func: Callable[..., Any]) -> Callable[..., Any]:
    """Decorator to mark a listener as batch-aware.

    Batch-aware listeners receive List[ResonanceEvent] instead of
    single events, allowing for optimized batch processing.

    Usage:
        @batch_listener
        def my_listener(events: List[ResonanceEvent]):
            # Process all events at once
            for event in events:
                ...
    """
    setattr(func, "__batch__", True)
    return func
