"""Interfaces Package - API, CLI, and Dashboard."""
# Export submodules
from . import api, cli

# Dashboard is optional (requires flask)
try:
    from . import dashboard
    __all__ = ["api", "cli", "dashboard"]
except ImportError:
    __all__ = ["api", "cli"]
