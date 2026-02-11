"""WhiteMagic Archaeology Module
==============================

Tools for tracking, analyzing, and remembering what files we've read.
Integrates with the memory system to keep relevant knowledge at hand.

Features:
- File reading tracker with timestamps and contexts
- Content change detection via hashing
- Integration with memory system for relevance scoring
- Windsurf conversation protobuf reader
- MCP tool exposure for AI agents

Usage:
    from whitemagic.archaeology import FileArchaeologist

    arch = FileArchaeologist()
    arch.mark_read("/path/to/file.md", context="Phase 1 scan", note="Contains API docs")

    # Get reading history
    recent = arch.get_recent_reads(limit=50)

    # Find unread files in a directory
    unread = arch.find_unread("/path/to/docs/")

    # Get relevant memories for a file
    memories = arch.get_relevant_memories("/path/to/file.md")
"""

try:
    from .file_archaeologist import (  # type: ignore[import-not-found]
        FileArchaeologist,
        find_unread,
        get_archaeologist,
        mark_read,
        mark_read_async,
        mark_written,
        mark_written_async,
    )
except ImportError:
    # file_archaeologist was archived to _archived/archaeology/
    FileArchaeologist = None  # type: ignore[assignment,misc]
    get_archaeologist = None  # type: ignore[assignment]
    mark_read = None  # type: ignore[assignment]
    mark_written = None  # type: ignore[assignment]
    mark_read_async = None  # type: ignore[assignment]
    mark_written_async = None  # type: ignore[assignment]
    find_unread = None  # type: ignore[assignment]

try:
    from .windsurf_reader import WindsurfConversationReader  # type: ignore[import-not-found]
except ImportError:
    WindsurfConversationReader = None  # type: ignore[assignment,misc]

from .wisdom_extractor import (
    WisdomExtractor,
    create_daily_wisdom_digest,
    extract_wisdom,
    get_wisdom_extractor,
    process_wisdom_archives,
    wisdom_report,
)

__all__ = [
    "FileArchaeologist",
    "WindsurfConversationReader",
    "WisdomExtractor",
    "create_daily_wisdom_digest",
    "extract_wisdom",
    "find_unread",
    "get_archaeologist",
    "get_wisdom_extractor",
    "mark_read",
    "mark_read_async",
    "mark_written",
    "mark_written_async",
    "process_wisdom_archives",
    "wisdom_report",
]

__version__ = "1.0.0"
