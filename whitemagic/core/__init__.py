"""Systems module initialization.
"""

from . import automation, immune, monitoring
from .governor import Governor, check_drift, get_governor, is_safe, validate_command
from .semantic_fs import (
    SemanticFileWatcher,
    get_watcher,
    start_watching,
    stop_watching,
    watch_directory,
    watcher_status,
)

__all__ = [
    "immune", "automation", "monitoring",
    # Governor
    "Governor", "get_governor", "validate_command", "check_drift", "is_safe",
    # Semantic FS Watcher
    "SemanticFileWatcher", "get_watcher", "watch_directory",
    "start_watching", "stop_watching", "watcher_status",
]
