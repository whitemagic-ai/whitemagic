"""Extension point system."""

import logging
from collections.abc import Callable
from typing import Any

logger = logging.getLogger(__name__)


class ExtensionPoint:
    """Represents a hookable point in core code."""

    def __init__(self, name: str, description: str = ""):
        self.name = name
        self.description = description
        self.handlers: list[Callable] = []

    def register(self, handler: Callable) -> None:
        """Register a plugin handler."""
        if handler not in self.handlers:
            self.handlers.append(handler)
            logger.debug(f"Registered handler for extension point '{self.name}'")

    def unregister(self, handler: Callable) -> None:
        """Unregister a plugin handler."""
        if handler in self.handlers:
            self.handlers.remove(handler)
            logger.debug(f"Unregistered handler from extension point '{self.name}'")

    def invoke(self, *args: Any, **kwargs: Any) -> list[Any]:
        """Invoke all registered handlers."""
        results = []
        for handler in self.handlers:
            try:
                result = handler(*args, **kwargs)
                results.append(result)
            except Exception as e:
                logger.error(f"Extension handler {handler.__name__} failed: {e}")
        return results

    def invoke_first(self, *args: Any, **kwargs: Any) -> Any | None:
        """Invoke first handler that returns non-None."""
        for handler in self.handlers:
            try:
                result = handler(*args, **kwargs)
                if result is not None:
                    return result
            except Exception as e:
                logger.error(f"Extension handler {handler.__name__} failed: {e}")
        return None
