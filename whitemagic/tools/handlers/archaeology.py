"""Archaeology tool handlers."""
from typing import Any

_ACTIONS = {
    "mark_read", "mark_written", "have_read", "find_unread",
    "find_changed", "recent_reads", "stats", "scan", "report",
    "search", "process_wisdom", "daily_digest",
}


def handle_archaeology(**kwargs: Any) -> dict[str, Any]:
    """Unified archaeology handler — routes by action parameter."""
    action = kwargs.get("action", "stats")
    dispatch = {
        "mark_read": handle_archaeology_mark_read,
        "mark_written": handle_archaeology_mark_written,
        "have_read": handle_archaeology_have_read,
        "find_unread": handle_archaeology_find_unread,
        "find_changed": handle_archaeology_find_changed,
        "recent_reads": handle_archaeology_recent_reads,
        "stats": handle_archaeology_stats,
        "scan": handle_archaeology_scan_directory,
        "report": handle_archaeology_report,
        "search": handle_archaeology_search,
        "process_wisdom": handle_archaeology_process_wisdom,
        "daily_digest": handle_archaeology_daily_digest,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def _arch() -> Any:
    from whitemagic.archaeology import get_archaeologist
    if get_archaeologist is None:
        raise ImportError("FileArchaeologist not available (module archived)")
    return get_archaeologist()


def handle_archaeology_scan_directory(**kwargs: Any) -> dict[str, Any]:
    directory = kwargs.get("directory")
    if not directory:
        raise ValueError("directory is required")
    depth = kwargs.get("depth", 3)
    patterns = kwargs.get("patterns", None)
    recursive = kwargs.get("recursive", True)
    arch = _arch()
    found_files = arch.find_unread(directory, patterns) if recursive else []
    stats = arch.stats(scan_disk=True)
    file_list = [f.path for f in found_files[:100]]
    return {
        "status": "success",
        "directory": directory,
        "depth": depth,
        "recursive": recursive,
        "new_files_found_count": len(found_files),
        "files_found": file_list,
        "total_files_tracked": stats.get("total_files", 0),
        "disk_usage_mb": stats.get("disk_usage_mb", 0),
        "message": f"Scan complete. Found {len(found_files)} new files (showing max 100).",
    }


def handle_archaeology_mark_read(**kwargs: Any) -> dict[str, Any]:
    entry = _arch().mark_read(
        path=kwargs.get("path"),
        context=kwargs.get("context"),
        note=kwargs.get("note"),
        insight=kwargs.get("insight"),
    )
    return {"status": "success", "entry": entry.to_dict()}


def handle_archaeology_mark_written(**kwargs: Any) -> dict[str, Any]:
    entry = _arch().mark_written(
        path=kwargs.get("path"),
        context=kwargs.get("context"),
        note=kwargs.get("note"),
    )
    return {"status": "success", "entry": entry.to_dict()}


def handle_archaeology_have_read(**kwargs: Any) -> dict[str, Any]:
    result = _arch().have_read(kwargs.get("path", ""))
    return {"status": "success", "have_read": result}


def handle_archaeology_find_unread(**kwargs: Any) -> dict[str, Any]:
    unread = _arch().find_unread(
        directory=kwargs.get("directory", "."),
        patterns=kwargs.get("patterns"),
    )
    return {"status": "success", "unread_files": unread, "count": len(unread)}


def handle_archaeology_find_changed(**kwargs: Any) -> dict[str, Any]:
    changed = _arch().find_changed(kwargs.get("directory"))
    return {"status": "success", "changed_files": [e.to_dict() for e in changed], "count": len(changed)}


def handle_archaeology_recent_reads(**kwargs: Any) -> dict[str, Any]:
    recent = _arch().get_recent_reads(kwargs.get("limit", 50))
    return {"status": "success", "recent": [e.to_dict() for e in recent]}


def handle_archaeology_stats(**kwargs: Any) -> dict[str, Any]:
    return {"status": "success", **_arch().stats()}


def handle_archaeology_report(**kwargs: Any) -> dict[str, Any]:
    return {"status": "success", "report": _arch().reading_report()}


def handle_archaeology_search(**kwargs: Any) -> dict[str, Any]:
    results = _arch().search(kwargs.get("query", ""))
    return {"status": "success", "results": [e.to_dict() for e in results]}


def handle_archaeology_process_wisdom(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import process_wisdom_archives
    result = process_wisdom_archives(
        limit_files=kwargs.get("limit_files", 1000),
        memory_type=kwargs.get("memory_type", "long_term"),
    )
    return {"status": "success", **result}


def handle_archaeology_daily_digest(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import create_daily_wisdom_digest
    digest_path = create_daily_wisdom_digest()
    return {"status": "success", "digest_path": digest_path}
