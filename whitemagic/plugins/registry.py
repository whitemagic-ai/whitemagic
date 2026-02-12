"""Plugin Registry for WhiteMagic

Manages loaded plugins and their lifecycle.
"""

import json
import logging
import os
from datetime import datetime
from pathlib import Path
from typing import Any

from .base import PluginBase

logger = logging.getLogger(__name__)


def _log_info(msg: str) -> None:
    # Avoid noisy plugin logs for AI callers / json mode.
    if not os.getenv("WM_SILENT_INIT"):
        logger.info(msg)


class PluginRegistry:
    """Registry for managing loaded plugins."""

    def __init__(self) -> None:
        self.plugins: dict[str, PluginBase] = {}
        self.load_order: list[str] = []
        self.loaded_at: dict[str, str] = {}

    def register(self, plugin: PluginBase) -> bool:
        """Register a plugin.

        Args:
            plugin: Plugin instance to register

        Returns:
            True if registered successfully

        """
        name = plugin.info.name

        if name in self.plugins:
            _log_info(f"Plugin {name} is already registered")
            return False

        try:
            # Call on_load
            plugin.on_load()

            # Register
            self.plugins[name] = plugin
            self.load_order.append(name)
            self.loaded_at[name] = datetime.now().isoformat()

            _log_info(f"Registered plugin: {name} v{plugin.info.version}")
            return True
        except Exception as e:
            _log_info(f"Failed to register plugin {name}: {e}")
            return False

    def unregister(self, name: str) -> bool:
        """Unregister a plugin.

        Args:
            name: Plugin name to unregister

        Returns:
            True if unregistered successfully

        """
        if name not in self.plugins:
            return False

        try:
            plugin = self.plugins[name]
            plugin.on_unload()

            del self.plugins[name]
            self.load_order.remove(name)
            del self.loaded_at[name]

            _log_info(f"Unregistered plugin: {name}")
            return True
        except Exception as e:
            _log_info(f"Failed to unregister plugin {name}: {e}")
            return False

    def get(self, name: str) -> PluginBase | None:
        """Get a plugin by name."""
        return self.plugins.get(name)

    def list_all(self) -> dict[str, dict[str, Any]]:
        """List all registered plugins with info."""
        result = {}

        for name, plugin in self.plugins.items():
            info = plugin.info
            result[name] = {
                "name": info.name,
                "version": info.version,
                "description": info.description,
                "author": info.author,
                "homepage": info.homepage,
                "loaded_at": self.loaded_at.get(name),
                "health": plugin.health_check(),
            }

        return result

    def get_by_type(self, plugin_type: str) -> list[PluginBase]:
        """Get plugins that provide certain features.

        Args:
            plugin_type: Type of plugin (e.g., 'garden', 'synergy', 'command')

        Returns:
            List of matching plugins

        """
        result = []

        for plugin in self.plugins.values():
            if plugin_type == "garden" and plugin.register_gardens() or plugin_type == "synergy" and plugin.register_synergies() or plugin_type == "command" and plugin.register_commands():
                result.append(plugin)

        return result

    def save_state(self, path: Path) -> None:
        """Save registry state to file."""
        state = {
            "plugins": list(self.plugins.keys()),
            "load_order": self.load_order,
            "loaded_at": self.loaded_at,
            "saved_at": datetime.now().isoformat(),
        }

        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(state, indent=2))

    def load_state(self, path: Path) -> bool:
        """Load registry state from file."""
        if not path.exists():
            return False

        try:
            state = json.loads(path.read_text())
            self.load_order = state.get("load_order", [])
            self.loaded_at = state.get("loaded_at", {})
            return True
        except Exception:
            return False

    def get_dependencies(self, name: str) -> list[str]:
        """Get dependencies for a plugin."""
        plugin = self.get(name)
        if plugin:
            return plugin.info.dependencies
        return []

    def resolve_load_order(self, plugins: list[str]) -> list[str]:
        """Resolve plugin load order based on dependencies.

        Args:
            plugins: List of plugin names to load

        Returns:
            Ordered list respecting dependencies

        """
        # Simple topological sort
        resolved = []
        visited = set()
        visiting = set()

        def visit(name: str) -> Any:
            if name in visiting:
                raise ValueError(f"Circular dependency detected: {name}")
            if name in visited:
                return

            visiting.add(name)

            # Visit dependencies first
            for dep in self.get_dependencies(name):
                if dep in plugins:
                    visit(dep)

            visiting.remove(name)
            visited.add(name)
            resolved.append(name)

        for name in plugins:
            if name not in visited:
                visit(name)

        return resolved

    def validate_dependencies(self) -> dict[str, list[str]]:
        """Validate all plugin dependencies.

        Returns:
            Dict mapping plugin to list of missing dependencies

        """
        missing = {}

        for name, plugin in self.plugins.items():
            missing_deps = []
            for dep in plugin.info.dependencies:
                if dep not in self.plugins:
                    missing_deps.append(dep)

            if missing_deps:
                missing[name] = missing_deps

        return missing
