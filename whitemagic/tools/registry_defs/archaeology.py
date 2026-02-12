"""Archaeology Tools — unified file tracking, dig, analysis, timeline.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="archaeology",
    description=(
        "Unified file archaeology — track reads/writes, find unread/changed files, "
        "search history, generate reports. Actions: mark_read, mark_written, have_read, "
        "find_unread, find_changed, recent_reads, stats, scan, report, search, "
        "process_wisdom, daily_digest."
    ),
    category=ToolCategory.ARCHAEOLOGY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": [
                    "mark_read", "mark_written", "have_read", "find_unread",
                    "find_changed", "recent_reads", "stats", "scan", "report",
                    "search", "process_wisdom", "daily_digest",
                ],
                "description": "Action to perform",
            },
            "path": {"type": "string", "description": "File path (for mark_read/written/have_read)"},
            "directory": {"type": "string", "description": "Directory to scan (for find_*/scan)"},
            "context": {"type": "string", "description": "Read/write context"},
            "note": {"type": "string", "description": "Optional note"},
            "insight": {"type": "string", "description": "Key insight (for mark_read)"},
            "query": {"type": "string", "description": "Search query (for search)"},
            "patterns": {
                "type": "array", "items": {"type": "string"},
                "description": "Glob patterns (for find_unread/scan)",
            },
            "limit": {"type": "integer", "default": 50, "description": "Result limit"},
        },
        "required": ["action"],
    },
),
]
