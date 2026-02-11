"""Plugin loader."""

import importlib
import logging
import sys

from .base import Plugin
from .registry import PluginRegistry

logger = logging.getLogger(__name__)


class PluginLoader:
    """Load and initialize plugins."""

    def __init__(self, registry: PluginRegistry):
        self.registry = registry

    def load_plugin(self, name: str) -> bool:
        """Load and activate a plugin.

        Returns True if successful.
        """
        info = self.registry.get_plugin(name)
        if not info:
            logger.error(f"Plugin {name} not found in registry")
            return False

        if info.active:
            logger.warning(f"Plugin {name} already active")
            return True

        try:
            # Add plugin path to sys.path
            plugin_path = str(info.path.parent)
            if plugin_path not in sys.path:
                sys.path.insert(0, plugin_path)

            # Import plugin module
            module_name = info.path.name
            module = importlib.import_module(module_name)

            # Find plugin class
            plugin_class = None
            for attr_name in dir(module):
                attr = getattr(module, attr_name)
                if (isinstance(attr, type) and
                    issubclass(attr, Plugin) and
                    attr is not Plugin):
                    plugin_class = attr
                    break

            if not plugin_class:
                logger.error(f"No Plugin class found in {module_name}")
                return False

            # Instantiate plugin
            instance = plugin_class()
            info.instance = instance

            # Activate plugin
            instance.activate()
            info.active = True

            logger.info(f"✅ Plugin {name} activated successfully")
            return True

        except Exception as e:
            logger.error(f"Failed to load plugin {name}: {e}")
            import traceback
            traceback.print_exc()
            return False

    def unload_plugin(self, name: str) -> bool:
        """Deactivate and unload a plugin."""
        info = self.registry.get_plugin(name)
        if not info or not info.active:
            logger.warning(f"Plugin {name} not active")
            return False

        try:
            if info.instance:
                info.instance.deactivate()
            info.active = False
            info.instance = None

            logger.info(f"✅ Plugin {name} deactivated")
            return True

        except Exception as e:
            logger.error(f"Failed to unload plugin {name}: {e}")
            return False

    def reload_plugin(self, name: str) -> bool:
        """Reload a plugin (unload + load)."""
        if not self.unload_plugin(name):
            return False
        return self.load_plugin(name)

