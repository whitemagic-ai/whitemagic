"""WhiteMagic v4.10.0 Plugin System.

Enables third-party extensions without modifying core.
"""

from .base import Plugin, PluginManifest
from .discovery import PluginDiscovery
from .extension_point import ExtensionPoint
from .loader import PluginLoader
from .registry import PluginInfo, PluginRegistry, get_registry

__all__ = [
    "Plugin",
    "PluginManifest",
    "ExtensionPoint",
    "PluginRegistry",
    "PluginInfo",
    "PluginLoader",
    "PluginDiscovery",
    "get_registry",
]
