"""Filesystem watcher tool handlers."""
from typing import Any


def handle_watcher_add(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    result = get_watcher().add_watch(kwargs.get("path", "."), recursive=kwargs.get("recursive", True), patterns=kwargs.get("patterns"), ignore_patterns=kwargs.get("ignore_patterns"))
    return {"status": "success" if result else "error", "path": kwargs.get("path", ".")}


def handle_watcher_remove(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    result = get_watcher().remove_watch(kwargs.get("path", ""))
    return {"status": "success" if result else "error", "removed": result}


def handle_watcher_start(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    result = get_watcher().start()
    return {"status": "success" if result else "error", "started": result}


def handle_watcher_stop(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    result = get_watcher().stop()
    return {"status": "success" if result else "error", "stopped": result}


def handle_watcher_status(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    return {"status": "success", **get_watcher().status()}


def handle_watcher_recent_events(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    events = get_watcher().recent_events(kwargs.get("limit", 50))
    return {"status": "success", "events": events}


def handle_watcher_stats(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    return {"status": "success", **get_watcher().stats()}


def handle_watcher_list(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.semantic_fs import get_watcher
    return {"status": "success", "watches": get_watcher().list_watches()}
