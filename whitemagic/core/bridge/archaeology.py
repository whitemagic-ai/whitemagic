
from typing import Any


def archaeology_process_wisdom(limit_files: int = 1000, memory_type: str = "long_term", **kwargs: Any) -> dict[str, Any]:
    """Extract insights from memory archives and store as wisdom memories."""
    from whitemagic.archaeology import process_wisdom_archives

    return process_wisdom_archives(limit_files=limit_files, memory_type=memory_type)


def archaeology_daily_digest(**kwargs: Any) -> dict[str, Any]:
    """Create a daily wisdom digest from recent insights."""
    from whitemagic.archaeology import create_daily_wisdom_digest

    return {"digest_path": create_daily_wisdom_digest()}


def archaeology_mark_read(file_path: str, context: str | None = None, notes: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Mark a file as read."""
    from whitemagic.archaeology import mark_read

    entry = mark_read(file_path, context, notes)
    return {
        "path": entry.path,
        "first_read": entry.first_read if hasattr(entry, "first_read") else str(entry.timestamp) if hasattr(entry, "timestamp") else "unknown",
        "read_count": getattr(entry, "read_count", 1),
    }


def archaeology_mark_written(file_path: str, context: str | None = None, notes: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Mark a file as written."""
    from whitemagic.archaeology import mark_written

    entry = mark_written(file_path, context, notes)
    return {
        "path": entry.path,
        "last_write": entry.last_write,
        "write_count": getattr(entry, "times_written", 1),
    }


def archaeology_find_unread(directory: str, patterns: list[str] | None = None, **kwargs: Any) -> dict[str, Any]:
    """Find unread files in a directory."""
    from whitemagic.archaeology import find_unread

    unread = find_unread(directory, patterns)
    return {"unread_files": unread, "count": len(unread)}


def archaeology_find_changed(directory: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Find files that have changed since they were last read."""
    from whitemagic.archaeology import get_archaeologist

    changed = get_archaeologist().find_changed(directory)
    return {
        "changed_files": [entry.to_dict() for entry in changed],
        "count": len(changed),
    }


def archaeology_recent_reads(limit: int = 50, **kwargs: Any) -> dict[str, Any]:
    """Get recently read files."""
    from whitemagic.archaeology import get_archaeologist

    recent = get_archaeologist().get_recent_reads(limit)
    return {"recent": [entry.to_dict() for entry in recent]}


def archaeology_stats(scan_disk: bool = False, **kwargs: Any) -> dict[str, Any]:
    """Get archaeology statistics."""
    from whitemagic.archaeology import get_archaeologist

    stats = get_archaeologist().stats(scan_disk=scan_disk)
    return stats if isinstance(stats, dict) else {}


def archaeology_report(**kwargs: Any) -> dict[str, Any]:
    """Generate a human-readable archaeology report."""
    from whitemagic.archaeology import get_archaeologist

    return {"report": get_archaeologist().reading_report()}


def archaeology_search(query: str, **kwargs: Any) -> dict[str, Any]:
    """Search archaeology entries by path, notes, or insights."""
    from whitemagic.archaeology import get_archaeologist

    results = get_archaeologist().search(query)
    return {"results": [entry.to_dict() for entry in results]}


def archaeology_extract_wisdom(**kwargs: Any) -> dict[str, Any]:
    """Extract wisdom from memory archives."""
    from whitemagic.archaeology import extract_wisdom

    wisdom = extract_wisdom()
    result = {
        "quotes": wisdom.quotes[:10] if wisdom.quotes else [],
        "principles": wisdom.principles[:10] if wisdom.principles else [],
    }

    # Add patterns if available
    if hasattr(wisdom, "patterns") and wisdom.patterns:
        result["patterns"] = wisdom.patterns[:10]

    return result


def archaeology_generate_report(**kwargs: Any) -> dict[str, Any]:
    """Generate archaeology report."""
    from whitemagic.archaeology import wisdom_report

    report = wisdom_report()
    return {"report": report}


def archaeology_scan_directory(
    directory: str,
    depth: int = 3,
    patterns: list[str] | None = None,
    recursive: bool = True,
    **kwargs: Any,
) -> dict[str, Any]:
    """Scan a directory and track files."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()

    # Perform the scan
    found_files = arch.find_unread(directory, patterns) if recursive else []
    # If recursive is False, we might want a shallow list, but find_unread is recursive by default
    # For now, we'll just return the stats and the unread count

    stats = arch.stats(scan_disk=True)

    return {
        "directory": directory,
        "depth": depth,
        "recursive": recursive,
        "new_files_found": len(found_files),
        "total_files_tracked": stats.get("total_files", 0),
        "disk_usage_mb": stats.get("disk_usage_mb", 0),
        "artifacts": stats.get("artifacts", {}),
        "message": f'Scan complete. Found {len(found_files)} new files. Disk usage: {stats.get("disk_usage_mb", 0)} MB',
    }
