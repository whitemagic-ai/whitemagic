"""Async Gan Ying Bus - High-performance event system.
Maintains backward compatibility while enabling async operations.
"""

import asyncio
import logging
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from ..async_layer import async_compat

logger = logging.getLogger(__name__)

@dataclass
class AsyncEvent:
    """Async event structure."""

    type: str
    data: dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)
    source: str = "unknown"
    id: str = field(default_factory=lambda: f"evt_{datetime.now().timestamp()}")

@dataclass
class BatchedAsyncEvent:
    """A batch of async events to reduce bus overhead."""

    events: list[AsyncEvent]
    timestamp: datetime = field(default_factory=datetime.now)
    source: str = "batch_processor"
    id: str = field(default_factory=lambda: f"batch_{datetime.now().timestamp()}")

class AsyncGanYingBus:
    """Async event bus for high-throughput scenarios.

    Features:
    - 10,000+ events/second throughput
    - Backpressure handling
    - Event filtering
    - Metrics collection
    """

    def __init__(self, max_queue_size: int = 10000):
        self._handlers: dict[str, list[Callable]] = {}
        self._filters: list[Callable[[AsyncEvent], bool]] = []
        self._queue: asyncio.Queue = asyncio.Queue(maxsize=max_queue_size)
        self._running = False
        self._task: asyncio.Task | None = None
        self._metrics = {
            "events_processed": 0,
            "events_dropped": 0,
            "handlers_called": 0,
            "errors": 0,
        }

    async def start(self) -> None:
        """Start event processing loop."""
        if self._running:
            return

        self._running = True
        self._task = asyncio.create_task(self._process_events())
        logger.info("Async Gan Ying Bus started")

    async def stop(self) -> None:
        """Stop event processing."""
        if not self._running:
            return

        self._running = False
        if self._task:
            self._task.cancel()
            try:
                await self._task
            except asyncio.CancelledError:
                pass
        logger.info("Async Gan Ying Bus stopped")

    async def emit(self, event_type: str, data: dict[str, Any], source: str = "unknown") -> None:
        """Emit an event asynchronously.

        Args:
            event_type: Type of event
            data: Event data
            source: Event source identifier

        """
        event = AsyncEvent(
            type=event_type,
            data=data,
            source=source,
        )
        await self.queue_event(event)

    async def emit_batch(self, events: list[tuple[str, dict[str, Any]]], source: str = "batch_source") -> None:
        """Emit a batch of events to reduce overhead."""
        async_events = []
        for etype, data in events:
            async_events.append(AsyncEvent(type=etype, data=data, source=source))

        batch = BatchedAsyncEvent(events=async_events, source=source)
        try:
            self._queue.put_nowait(batch)
        except asyncio.QueueFull:
            self._metrics["events_dropped"] += len(async_events)
            logger.warning(f"Event queue full, dropping batch of {len(async_events)} events")

    async def queue_event(self, event: AsyncEvent) -> None:
        """Internal helper to queue a single event with filtering."""
        # Apply filters
        for filter_func in self._filters:
            try:
                if not filter_func(event):
                    return
            except Exception as e:
                logger.error(f"Filter error: {e}")

        try:
            self._queue.put_nowait(event)
        except asyncio.QueueFull:
            self._metrics["events_dropped"] += 1
            logger.warning("Event queue full, dropping event")

    async def _process_events(self) -> None:
        """Process events from queue with error handling."""
        while self._running:
            try:
                item = await asyncio.wait_for(
                    self._queue.get(),
                    timeout=1.0,
                )

                if isinstance(item, BatchedAsyncEvent):
                    for event in item.events:
                        await self._dispatch(event)
                        self._metrics["events_processed"] += 1
                else:
                    await self._dispatch(item)
                    self._metrics["events_processed"] += 1

            except asyncio.TimeoutError:
                continue
            except Exception as e:
                logger.error(f"Error processing event: {e}")
                self._metrics["errors"] += 1

    def on(self, event_type: str, handler: Callable[..., Any]) -> None:
        """Register event handler.

        Args:
            event_type: Event type to handle
            handler: Handler function (sync or async)

        """
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)
        logger.debug(f"Registered handler for {event_type}")

    def off(self, event_type: str, handler: Callable[..., Any]) -> None:
        """Remove event handler."""
        if event_type in self._handlers:
            try:
                self._handlers[event_type].remove(handler)
                logger.debug(f"Removed handler for {event_type}")
            except ValueError:
                pass

    def add_filter(self, filter_func: Callable[[AsyncEvent], bool]) -> None:
        """Add event filter."""
        self._filters.append(filter_func)

    async def _dispatch(self, event: AsyncEvent) -> None:
        """Dispatch event to handlers."""
        handlers = self._handlers.get(event.type, [])
        if not handlers:
            return

        tasks = []

        for handler in handlers:
            try:
                if asyncio.iscoroutinefunction(handler):
                    tasks.append(handler(event))
                else:
                    # Sync handler - run in executor
                    loop = asyncio.get_running_loop()
                    tasks.append(
                        loop.run_in_executor(None, handler, event),
                    )
            except Exception as e:
                logger.error(f"Error preparing handler: {e}")
                self._metrics["errors"] += 1

        if tasks:
            results = await asyncio.gather(*tasks, return_exceptions=True)
            for result in results:
                if isinstance(result, Exception):
                    logger.error(f"Handler error: {result}")
                    self._metrics["errors"] += 1
                else:
                    self._metrics["handlers_called"] += 1

    def get_metrics(self) -> dict[str, Any]:
        """Get bus metrics."""
        return {
            **self._metrics,
            "queue_size": self._queue.qsize(),
            "handler_count": sum(len(h) for h in self._handlers.values()),
            "running": self._running,
        }

    async def wait_for_empty(self, timeout: float = 5.0) -> None:
        """Wait for queue to be empty."""
        start = datetime.now()
        while not self._queue.empty():
            if (datetime.now() - start).total_seconds() > timeout:
                raise TimeoutError("Queue not empty after timeout")
            await asyncio.sleep(0.01)

# Global instance
_async_bus: AsyncGanYingBus | None = None

def get_async_bus() -> AsyncGanYingBus:
    """Get or create global async bus."""
    global _async_bus
    if _async_bus is None:
        _async_bus = AsyncGanYingBus()
    return _async_bus

# Backward compatibility with sync bus
@async_compat
async def emit_async(event_type: str, data: dict[str, Any], source: str = "unknown") -> None:
    """Emit event using async bus (works in sync context)."""
    bus = get_async_bus()
    await bus.emit(event_type, data, source)

@async_compat
async def start_async_bus() -> None:
    """Start async bus (works in sync context)."""
    bus = get_async_bus()
    await bus.start()

@async_compat
async def stop_async_bus() -> None:
    """Stop async bus (works in sync context)."""
    bus = get_async_bus()
    await bus.stop()

# Utility functions for common patterns
class EventSubscriber:
    """Helper for subscribing to multiple events."""

    def __init__(self, bus: AsyncGanYingBus | None = None) -> None:
        self.bus = bus or get_async_bus()
        self._subscriptions: list[tuple[str, Callable]] = []

    def subscribe(self, event_type: str, handler: Callable[..., Any]) -> None:
        """Subscribe to event type."""
        self.bus.on(event_type, handler)
        self._subscriptions.append((event_type, handler))

    def unsubscribe_all(self) -> None:
        """Unsubscribe from all events."""
        for event_type, handler in self._subscriptions:
            self.bus.off(event_type, handler)
        self._subscriptions.clear()

    async def wait_for(self, event_type: str, timeout: float = 5.0) -> AsyncEvent:
        """Wait for specific event."""
        future: asyncio.Future[AsyncEvent] = asyncio.Future()

        def handler(event: AsyncEvent) -> None:
            if not future.done():
                future.set_result(event)

        self.subscribe(event_type, handler)
        try:
            return await asyncio.wait_for(future, timeout=timeout)
        finally:
            self.bus.off(event_type, handler)

# Performance monitoring
class PerformanceMonitor:
    """Monitor bus performance."""

    def __init__(self, bus: AsyncGanYingBus | None = None) -> None:
        self.bus = bus or get_async_bus()
        self._start_metrics: dict[str, Any] | None = None

    async def start_monitoring(self) -> None:
        """Start performance monitoring."""
        self._start_metrics = self.bus.get_metrics()

    async def get_stats(self, duration: float = 1.0) -> dict[str, Any]:
        """Get performance stats over duration."""
        await asyncio.sleep(duration)
        current = self.bus.get_metrics()

        if self._start_metrics:
            events_per_sec = (
                current["events_processed"] - self._start_metrics["events_processed"]
            ) / duration
            return {
                "events_per_second": events_per_sec,
                "queue_size": current["queue_size"],
                "error_rate": current["errors"] / max(1, current["events_processed"]),
                "handler_efficiency": current["handlers_called"] / max(1, current["events_processed"]),
            }
        return current
