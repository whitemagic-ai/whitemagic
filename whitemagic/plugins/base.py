"""Base Plugin Class for WhiteMagic

All plugins must inherit from PluginBase and implement required methods.
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any, TypeVar

import click


@dataclass
class PluginInfo:
    """Plugin metadata."""

    name: str
    version: str
    description: str
    author: str
    homepage: str | None = None
    dependencies: list[str] = field(default_factory=list)
    min_whitemagic_version: str = "2.0.0"

    def __post_init__(self) -> None:
        if self.dependencies is None:
            self.dependencies = []


class PluginBase(ABC):
    """Base class for all WhiteMagic plugins.

    Plugins can extend WhiteMagic by:
    - Adding new CLI commands
    - Creating custom gardens
    - Implementing new synergies
    - Processing data
    """

    @property
    @abstractmethod
    def info(self) -> PluginInfo:
        """Return plugin metadata."""

    def on_load(self) -> None:
        """Called when plugin is loaded.

        Use this to:
        - Initialize plugin state
        - Register event handlers
        - Validate dependencies
        """

    def on_unload(self) -> None:
        """Called when plugin is unloaded.

        Use this to:
        - Clean up resources
        - Unregister handlers
        - Save state
        """

    def register_commands(self) -> list[click.Command]:
        """Register CLI commands.

        Returns:
            List of Click commands to add to CLI

        """
        return []

    def register_gardens(self) -> list[Any]:
        """Register custom gardens.

        Returns:
            List of garden classes to register

        """
        return []

    def register_synergies(self) -> list[Any]:
        """Register custom synergies.

        Returns:
            List of synergy classes to register

        """
        return []

    def process_memory(self, memory: dict[str, Any]) -> dict[str, Any] | None:
        """Process a memory before saving.

        Args:
            memory: Memory dictionary

        Returns:
            Modified memory or None to discard

        """
        return memory

    def process_query(self, query: str, context: dict[str, Any]) -> str | None:
        """Process a search query.

        Args:
            query: Search query string
            context: Query context

        Returns:
            Modified query or None to use original

        """
        return query

    def on_memory_created(self, memory: dict[str, Any]) -> None:
        """Called when a new memory is created.

        Args:
            memory: Created memory

        """

    def on_memory_updated(self, memory: dict[str, Any]) -> None:
        """Called when a memory is updated.

        Args:
            memory: Updated memory

        """

    def on_memory_deleted(self, memory_id: str) -> None:
        """Called when a memory is deleted.

        Args:
            memory_id: ID of deleted memory

        """

    def get_config_schema(self) -> dict[str, Any]:
        """Return configuration schema for plugin.

        Returns:
            JSON schema for plugin configuration

        """
        return {}

    def set_config(self, config: dict[str, Any]) -> None:
        """Set plugin configuration.

        Args:
            config: Plugin configuration

        """

    def get_config(self) -> dict[str, Any]:
        """Get current plugin configuration.

        Returns:
            Current configuration

        """
        return {}

    def reload(self) -> None:
        """Reload plugin configuration and state.
        """
        self.on_unload()
        self.on_load()

    def health_check(self) -> dict[str, Any]:
        """Check plugin health.

        Returns:
            Health status information

        """
        return {
            "status": "healthy",
            "message": "Plugin is running normally",
        }


class SimplePlugin(PluginBase):
    """Simplified base class for basic plugins.

    Automatically handles plugin info from class attributes.
    Can also accept name/version/description in __init__ for convenience.
    """

    # Override these in your plugin (class attributes)
    name: str = "unnamed_plugin"
    version: str = "1.0.0"
    description: str = "A simple plugin"
    author: str = "Anonymous"

    def __init__(self, name: str | None = None, version: str | None = None, description: str | None = None, author: str | None = None) -> None:
        """Initialize plugin with optional overrides for class attributes."""
        if name is not None:
            self._instance_name = name
        if version is not None:
            self._instance_version = version
        if description is not None:
            self._instance_description = description
        if author is not None:
            self._instance_author = author

    @property
    def info(self) -> PluginInfo:
        # Priority: instance attributes > class attributes > defaults
        name = getattr(self, "_instance_name", None) or getattr(self.__class__, "name", "unnamed_plugin")
        if name == "unnamed_plugin" and hasattr(self, "_decorated_name"):
            name = self._decorated_name

        return PluginInfo(
            name=str(name),
            version=str(getattr(self, "_instance_version", None) or getattr(self.__class__, "version", "1.0.0")),
            description=str(getattr(self, "_instance_description", None) or getattr(self.__class__, "description", "A simple plugin")),
            author=str(getattr(self, "_instance_author", None) or getattr(self.__class__, "author", "Anonymous")),
        )


# Decorator for easy plugin creation
_PluginT = TypeVar("_PluginT", bound=SimplePlugin)


def plugin(name: str, version: str = "1.0.0", description: str = "", author: str = "") -> Callable[[type[_PluginT]], type[_PluginT]]:
    """Decorator to easily create a plugin from a class.

    @plugin("my_plugin", version="1.0.0", description="My awesome plugin")
    class MyPlugin(SimplePlugin):
        pass
    """
    def decorator(cls: type[_PluginT]) -> type[_PluginT]:
        cls.name = name
        cls.version = version
        cls.description = description
        cls.author = author
        return cls
    return decorator
