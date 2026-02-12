"""Registry Domain Definitions — Auto-Discovery.
===============================================
Each module in this package exports a ``TOOLS`` list of ``ToolDefinition``
objects.  The ``collect()`` function discovers and aggregates them all.

To add a new domain file:
  1. Create ``registry_defs/my_domain.py``
  2. Import from ``whitemagic.tools.tool_types``
  3. Define ``TOOLS: list[ToolDefinition] = [...]``

That's it — ``registry.py`` will auto-collect on import.
"""

from __future__ import annotations

import importlib
import logging
import os
import pkgutil
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from whitemagic.tools.tool_types import ToolDefinition

logger = logging.getLogger(__name__)
# Suppress noisy per-module DEBUG lines on import unless WM_DEBUG is set
if not os.environ.get("WM_DEBUG"):
    logger.setLevel(logging.WARNING)


def collect() -> "list[ToolDefinition]":
    """Discover all domain modules and collect their TOOLS lists."""
    all_tools: list[ToolDefinition] = []
    package_path = __path__  # type: ignore[name-defined]
    package_name = __name__

    for finder, module_name, is_pkg in pkgutil.iter_modules(package_path):
        if module_name.startswith("_"):
            continue
        try:
            mod = importlib.import_module(f"{package_name}.{module_name}")
            tools = getattr(mod, "TOOLS", None)
            if tools and isinstance(tools, list):
                all_tools.extend(tools)
                logger.debug(f"registry_defs/{module_name}: {len(tools)} tools")
        except Exception as e:
            logger.warning(f"registry_defs/{module_name} failed to load: {e}")

    return all_tools
