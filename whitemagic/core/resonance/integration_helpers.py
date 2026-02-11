"""Gan Ying Integration Helpers - Make ANY system resonance-capable!

Usage:
    from whitemagic.core.resonance.integration_helpers import GanYingMixin, auto_emit

    class MySystem(GanYingMixin):
        @auto_emit(EventType.PATTERN_DETECTED)
        def detect_pattern(self):
            # Your code here
            return {"pattern": "something cool"}
"""

import logging
from collections.abc import Callable
from datetime import datetime
from functools import wraps
from typing import Any

from .gan_ying_enhanced import EventType, GanYingBus, ResonanceEvent, get_bus

logger = logging.getLogger(__name__)


class GanYingMixin:
    """Mixin to add Gan Ying capabilities to ANY class!

    Just inherit from this and you get:
    - self.emit() method
    - self.listen() method
    - self.bus property
    - Automatic source naming
    """

    @property
    def bus(self) -> GanYingBus:
        """Access to global Gan Ying Bus."""
        return get_bus()

    @property
    def gan_ying_source(self) -> str:
        """Source name for events (class name by default)."""
        return self.__class__.__name__

    def emit(self, event_type: EventType, data: dict[str, Any], confidence: float = 1.0) -> None:
        """Emit event to Gan Ying Bus."""
        event = ResonanceEvent(
            source=self.gan_ying_source,
            event_type=event_type,
            data=data,
            timestamp=datetime.now(),
            confidence=confidence,
        )
        self.bus.emit(event)

    def listen(self, event_type: EventType, callback: Callable[..., Any]) -> None:
        """Listen to events on Gan Ying Bus."""
        self.bus.listen(event_type, callback)

    def listen_multiple(self, event_types: list[EventType], callback: Callable[..., Any]) -> None:
        """Listen to multiple event types."""
        for event_type in event_types:
            self.listen(event_type, callback)


def auto_emit(event_type: EventType, extract_data: Callable[[Any], dict[str, Any]] | None = None) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """Decorator to automatically emit events after method execution.

    Usage:
        @auto_emit(EventType.PATTERN_DETECTED)
        def my_method(self):
            return {"pattern": "cool"}
    """
    def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
        @wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            # Execute method
            result = func(*args, **kwargs)

            # Extract data
            if extract_data:
                data = extract_data(result)
            elif isinstance(result, dict):
                data = result
            else:
                data = {"result": result}

            # Emit event
            self_obj = args[0] if args else None
            if hasattr(self_obj, "emit"):
                emit_func = getattr(self_obj, "emit")
                emit_func(event_type, data)

            return result
        return wrapper
    return decorator


def listen_for(*event_types: EventType) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """Decorator to mark a method as event listener.

    Usage:
        @listen_for(EventType.PATTERN_DETECTED, EventType.INSIGHT_FLASH)
        def handle_insight(self, event):
            logger.info(f"Got insight: {event.data}")
    """
    def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
        setattr(func, "_listen_for", event_types)
        return func
    return decorator


def init_listeners(instance: Any) -> None:
    """Initialize all @listen_for decorated methods.
    Call this in __init__ after inheriting from GanYingMixin.

    Usage:
        def __init__(self):
            super().__init__()
            init_listeners(self)
    """
    for attr_name in dir(instance):
        attr = getattr(instance, attr_name)
        if hasattr(attr, "_listen_for"):
            for event_type in attr._listen_for:
                instance.listen(event_type, attr)


# Quick integration functions
def quick_emit(source: str, event_type: EventType, data: dict[str, Any]) -> None:
    """Quick emit without needing a class."""
    from .gan_ying_enhanced import emit_event
    emit_event(source, event_type, data)


def quick_listen(event_type: EventType, callback: Callable[..., Any]) -> None:
    """Quick listen without needing a class."""
    from .gan_ying_enhanced import listen_to
    listen_to(event_type, callback)


if __name__ == "__main__":
    # Test the mixin
    class TestSystem(GanYingMixin):
        def __init__(self) -> None:
            init_listeners(self)

        @auto_emit(EventType.PATTERN_DETECTED)
        def find_pattern(self) -> dict[str, Any]:
            return {"pattern": "test_pattern", "confidence": 0.9}

        @listen_for(EventType.PATTERN_DETECTED)
        def on_pattern(self, event: ResonanceEvent) -> None:
            logger.info(f"🎯 Heard pattern: {event.data}")

    # Test it
    system = TestSystem()
    system.find_pattern()

    logger.info("✅ Integration helpers working!")
