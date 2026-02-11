"""Plugin registry."""

import logging
from dataclasses import dataclass
from pathlib import Path

from .base import Plugin, PluginManifest
from .extension_point import ExtensionPoint

logger = logging.getLogger(__name__)


@dataclass
class PluginInfo:
    """Plugin metadata and state."""

    name: str
    version: str
    path: Path
    manifest: PluginManifest
    instance: Plugin | None = None
    active: bool = False


class PluginRegistry:
    """Global plugin registry."""

    def __init__(self) -> None:
        self.plugins: dict[str, PluginInfo] = {}
        self.extension_points: dict[str, ExtensionPoint] = {}
        self._setup_core_extension_points()

    def _setup_core_extension_points(self) -> None:
        """Register core extension points."""
        self.register_extension_point("garden", "Custom consciousness domains")
        self.register_extension_point("optimizer", "Performance optimizations")
        self.register_extension_point("memory_backend", "Storage implementations")
        self.register_extension_point("event_listener", "Gan Ying event handlers")
        self.register_extension_point("llm_provider", "LLM integrations")
        self.register_extension_point("tool", "CLI/MCP tools")

    def register_plugin(self, info: PluginInfo) -> None:
        """Register a discovered plugin."""
        self.plugins[info.name] = info
        logger.info(f"Registered plugin: {info.name} v{info.version}")

    def unregister_plugin(self, name: str) -> None:
        """Unregister a plugin."""
        if name in self.plugins:
            info = self.plugins[name]
            del self.plugins[name]
            logger.info(f"Unregistered plugin: {name} v{info.version}")

    def get_plugin(self, name: str) -> PluginInfo | None:
        """Get plugin by name."""
        return self.plugins.get(name)

    def list_plugins(self, active_only: bool = False) -> list[PluginInfo]:
        """List all registered plugins."""
        plugins = list(self.plugins.values())
        if active_only:
            plugins = [p for p in plugins if p.active]
        return plugins

    def register_extension_point(self, name: str, description: str = "") -> ExtensionPoint:
        """Register an extension point."""
        if name not in self.extension_points:
            self.extension_points[name] = ExtensionPoint(name, description)
            logger.debug(f"Registered extension point: {name}")
        return self.extension_points[name]

    def get_extension_point(self, name: str) -> ExtensionPoint | None:
        """Get extension point by name."""
        return self.extension_points.get(name)

    def list_extension_points(self) -> list[ExtensionPoint]:
        """List all extension points."""
        return list(self.extension_points.values())


# Global registry instance
_registry: PluginRegistry | None = None


def get_registry() -> PluginRegistry:
    """Get or create global plugin registry."""
    global _registry
    if _registry is None:
        _registry = PluginRegistry()
    return _registry
