"""Gan Ying (感應) - Sympathetic Resonance.

v4.9.0: Now with AsyncGanYingBus support for 10,000+ events/sec throughput!
v5.0.0: Lazy loading for faster imports (~50% speedup)

The principle of Gan Ying ("sympathetic resonance") from Chinese philosophy:
"Things that accord in tone vibrate together"

This is the event-driven nervous system of WhiteMagic.
"""
import os
from datetime import datetime
from typing import TYPE_CHECKING, Any, Union

if TYPE_CHECKING:
    from .gan_ying_async import (
        AsyncEvent,
        AsyncGanYingBus,
        EventSubscriber,
        PerformanceMonitor,
    )

# Re-export everything from the enhanced module (sync bus) - always needed
from .gan_ying_enhanced import (
    CascadeTrigger,
    EventType,
    GanYingBus,
    ResonanceEvent,
    get_bus,
    listen_to,
)

# Compatibility alias
listen_for = listen_to

# ---------------------------------------------------------------------------
# Public emit wrapper
# ---------------------------------------------------------------------------

def _normalize_event_type(event_type: Union[EventType, str]) -> EventType:
    if isinstance(event_type, EventType):
        return event_type
    # Try value match first (EventType values are snake_case strings)
    for e in EventType:
        if e.value == event_type:
            return e
    # Try name match (allow callers to pass "MEMORY_CREATED", etc.)
    key = event_type.upper()
    if key in EventType.__members__:
        return EventType.__members__[key]
    # Final fallback: treat as internal state change.
    return EventType.INTERNAL_STATE_CHANGED


def emit_event(*args: Any, **kwargs: Any) -> None:
    """Emit an event to the global Gan Ying bus.

    Supported call forms:
    - `emit_event(event_type, data, source="...", confidence=...)`
    - `emit_event(source, event_type, data, confidence=...)` (legacy)
    """
    if (
        len(args) >= 3
        and isinstance(args[0], str)
        and isinstance(args[2], dict)
        and isinstance(args[1], (EventType, str))
    ):
        source = args[0]
        raw_event_type = args[1]
        data = args[2]
        confidence = float(args[3]) if len(args) >= 4 else float(kwargs.get("confidence", 1.0))
    else:
        if len(args) < 2:
            raise TypeError("emit_event(event_type, data, ...) requires at least 2 positional args")
        raw_event_type = args[0]
        data = args[1]
        source = str(kwargs.get("source", "unknown"))
        confidence = float(kwargs.get("confidence", 1.0))

    normalized = _normalize_event_type(raw_event_type)
    payload: dict[str, Any] = dict(data)
    if isinstance(raw_event_type, str) and normalized == EventType.INTERNAL_STATE_CHANGED:
        payload.setdefault("_raw_event_type", raw_event_type)

    event = ResonanceEvent(
        source=source,
        event_type=normalized,
        data=payload,
        timestamp=datetime.now(),
        confidence=confidence,
    )
    get_bus().emit(event)


# v5.0.0: Async bus support - LAZY LOADED for faster imports
# Only import async components when actually needed
_async_module = None
_use_async: bool = os.getenv("WHITEMAGIC_SYNC_MODE", "0") != "1"  # Default to async (0)

def _get_async_module() -> Any:
    """Lazy load async module for faster startup."""
    global _async_module
    if _async_module is None:
        from . import gan_ying_async
        _async_module = gan_ying_async
    return _async_module

# Lazy accessor functions for async components
def get_async_bus() -> Any:
    """Get the async event bus (lazy loaded)."""
    return _get_async_module().get_async_bus()

def emit_async(event_type: EventType, data: dict[str, Any], source: str = "unknown") -> Any:
    """Emit an async event (lazy loaded)."""
    return _get_async_module().emit_async(event_type, data, source)

def emit_batch_async(events: list[tuple[EventType, dict[str, Any]]], source: str = "batch_source") -> Any:
    """Emit a batch of async events (lazy loaded)."""
    return _get_async_module().emit_batch_async(events, source)

def start_async_bus() -> Any:
    """Start the async bus (lazy loaded)."""
    return _get_async_module().start_async_bus()

def stop_async_bus() -> Any:
    """Stop the async bus (lazy loaded)."""
    return _get_async_module().stop_async_bus()

def enable_async_mode() -> Any:
    """Enable async event processing (v4.9.0 feature)."""
    global _use_async
    _use_async = True
    return get_async_bus()

def disable_async_mode() -> GanYingBus:
    """Disable async mode (fallback to sync for compatibility)."""
    global _use_async
    _use_async = False
    return get_bus()

def is_async_mode() -> bool:
    """Check if async mode is enabled."""
    return _use_async

def get_active_bus() -> Any:
    """Get the currently active bus (sync or async based on mode)."""
    if _use_async:
        return get_async_bus()
    return get_bus()

# Module-level __getattr__ for lazy loading of async classes (PEP 562)
_LAZY_ASYNC_ATTRS = {
    "AsyncGanYingBus", "AsyncEvent", "EventSubscriber", "PerformanceMonitor",
}

def __getattr__(name: str) -> Any:
    """Lazy load async classes only when accessed."""
    if name in _LAZY_ASYNC_ATTRS:
        return getattr(_get_async_module(), name)
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

__all__ = [
    # Sync bus (default)
    "EventType",
    "ResonanceEvent",
    "CascadeTrigger",
    "GanYingBus",
    "get_bus",
    "emit_event",
    "listen_for",
    "listen_to",
    # v4.9.0 Async bus (lazy loaded)
    "AsyncGanYingBus",
    "AsyncEvent",
    "get_async_bus",
    "emit_async",
    "start_async_bus",
    "stop_async_bus",
    "EventSubscriber",
    "PerformanceMonitor",
    # Mode switching
    "enable_async_mode",
    "disable_async_mode",
    "is_async_mode",
    "get_active_bus",
]
