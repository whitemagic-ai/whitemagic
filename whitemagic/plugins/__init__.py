"""WhiteMagic Plugin System

Allows community extensions to add:
- Custom gardens
- New synergies
- CLI commands
- Data processors

Usage:
    from whitemagic.plugins import load_plugins, get_plugin

    # Load all plugins
    plugins = load_plugins()

    # Get specific plugin
    my_plugin = get_plugin("my_plugin_name")
"""

from __future__ import annotations

import importlib.util
import inspect
import logging
import os
import sys
from pathlib import Path
from typing import Any, cast

from whitemagic.config.paths import WM_ROOT

from .base import PluginBase
from .registry import PluginRegistry

logger = logging.getLogger(__name__)

# Global registry
_registry = PluginRegistry()


def load_plugins() -> list[PluginBase]:
    """Load all available plugins.

    Searches:
    1. $WM_STATE_ROOT/plugins/ (defaults to ~/.whitemagic/plugins/)
    2. Installed packages with entry points

    Optional (disabled by default):
    - whitemagic/plugins/examples/ (set WHITEMAGIC_LOAD_EXAMPLE_PLUGINS=1)
    """
    loaded = []

    # 1. User plugins directory (under WM_STATE_ROOT)
    user_dir = WM_ROOT / "plugins"
    if user_dir.exists():
        for plugin_file in user_dir.glob("*.py"):
            if plugin_file.name.startswith("_"):
                continue
            plugin = _load_plugin_file(plugin_file)
            if plugin:
                loaded.append(plugin)
                _registry.register(plugin)

    # 2. Example plugins (opt-in; avoid surprising side effects/noise in OSS builds)
    if os.getenv("WHITEMAGIC_LOAD_EXAMPLE_PLUGINS", "").strip().lower() in {"1", "true", "yes", "on"}:
        examples_dir = Path(__file__).parent / "examples"
        if examples_dir.exists():
            for plugin_file in examples_dir.glob("*.py"):
                if plugin_file.name.startswith("_"):
                    continue
                plugin = _load_plugin_file(plugin_file)
                if plugin:
                    loaded.append(plugin)
                    _registry.register(plugin)

    # 3. Entry points (pip installable plugins)
    try:
        from importlib.metadata import entry_points

        # Get whitemagic.plugins entry points
        eps_any: Any = entry_points()
        if hasattr(eps_any, "select"):
            plugin_eps = eps_any.select(group="whitemagic.plugins")
        else:
            plugin_eps = eps_any.get("whitemagic.plugins", [])

        for entry_point in plugin_eps:
            try:
                plugin = entry_point.load()
                if isinstance(plugin, PluginBase):
                    loaded.append(plugin)
                    _registry.register(plugin)
            except Exception as e:
                logger.info(f"Failed to load plugin {entry_point.name}: {e}")
    except ImportError:
        pass

    return loaded


def _load_plugin_file(plugin_path: Path) -> PluginBase | None:
    """Load a plugin from a Python file."""
    try:
        spec = importlib.util.spec_from_file_location(
            plugin_path.stem, plugin_path,
        )
        if spec is None or spec.loader is None:
            logger.info(f"Failed to create import spec for plugin {plugin_path}")
            return None
        module = importlib.util.module_from_spec(spec)
        sys.modules[plugin_path.stem] = module
        spec.loader.exec_module(module)

        # Find PluginBase subclass (but not SimplePlugin itself)
        plugin_classes = []
        for name, obj in inspect.getmembers(module):
            if (inspect.isclass(obj) and
                issubclass(obj, PluginBase) and
                obj != PluginBase and
                obj.__name__ != "SimplePlugin"):
                plugin_classes.append(obj)

        # Return the first plugin class found
        if plugin_classes:
            return plugin_classes[0]()
    except Exception as e:
        logger.info(f"Failed to load plugin {plugin_path}: {e}")

    return None


def get_plugin(name: str) -> PluginBase | None:
    """Get a loaded plugin by name."""
    return _registry.get(name)


def list_plugins() -> dict[str, dict[str, Any]]:
    """List all loaded plugins."""
    return cast(dict[str, dict[str, Any]], _registry.list_all())


def reload_plugin(name: str) -> bool:
    """Reload a plugin."""
    plugin = _registry.get(name)
    if plugin and hasattr(plugin, "reload"):
        try:
            plugin.reload()
            return True
        except Exception as e:
            logger.info(f"Failed to reload plugin {name}: {e}")
    return False


def unload_plugin(name: str) -> bool:
    """Unload a plugin."""
    plugin = _registry.get(name)
    if plugin:
        try:
            plugin.on_unload()
            _registry.unregister(name)
            return True
        except Exception as e:
            logger.info(f"Failed to unload plugin {name}: {e}")
    return False


# Convenience functions
def register_commands(cli_group: Any) -> None:
    """Register all plugin commands with CLI."""
    import click
    for plugin in _registry.plugins.values():
        commands = plugin.register_commands()
        if commands:
            for cmd in commands:
                if isinstance(cmd, click.Command):
                    cli_group.add_command(cmd)
                elif isinstance(cmd, tuple) and len(cmd) >= 3:
                    # Handle tuple (name, help, callback)
                    name, help_text, callback = cmd[:3]
                    # Wrap in a click command
                    @click.command(name=name, help=help_text)
                    @click.argument("args", nargs=-1)
                    @click.pass_context
                    def wrapper(ctx: Any, args: tuple[str, ...]) -> None:
                        # Simple callback wrapper - might need more sophisticated arg parsing
                        # For now, pass a simple namespace-like object
                        class Args:
                            pass
                        parsed_args = Args()
                        # This is very basic and won't handle named args well without proper parsing
                        # But it prevents the crash
                        callback(plugin, parsed_args)

                    cli_group.add_command(wrapper)
                else:
                    logger.info(f"Warning: Skipping invalid command format in plugin {getattr(plugin, 'name', 'unknown')}: {cmd}")


def register_gardens(garden_manager: Any) -> None:
    """Register all plugin gardens."""
    for plugin in _registry.plugins.values():
        gardens = plugin.register_gardens()
        if gardens:
            for garden in gardens:
                garden_manager.register(garden)


def register_synergies(synergy_manager: Any) -> None:
    """Register all plugin synergies."""
    for plugin in _registry.plugins.values():
        synergies = plugin.register_synergies()
        if synergies:
            for synergy in synergies:
                synergy_manager.register(synergy)


__all__ = [
    "get_plugin",
    "list_plugins",
    "load_plugins",
    "register_commands",
    "register_gardens",
    "register_synergies",
    "reload_plugin",
    "unload_plugin",
]
