"""Synthesis Tools — kaizen, serendipity, quality gate, pattern analysis.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="kaizen_analyze",
    description="Analyze code/system for continuous improvement opportunities",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "target": {"type": "string", "description": "Target to analyze (path or system name)"},
            "depth": {"type": "string", "enum": ["shallow", "deep"], "default": "shallow"},
        },
        "required": ["target"],
    },
),
ToolDefinition(
    name="kaizen_apply_fixes",
    description="Apply recommended fixes from Kaizen analysis",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "fix_ids": {"type": "array", "items": {"type": "string"}, "description": "IDs of fixes to apply"},
            "dry_run": {"type": "boolean", "default": True},
        },
        "required": ["fix_ids"],
    },
),
ToolDefinition(
    name="serendipity_surface",
    description="Surface unexpected but potentially valuable memory connections",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "count": {"type": "integer", "default": 5, "description": "Number of connections to surface"},
            "context": {"type": "string", "description": "Optional context to bias selection"},
        },
    },
),
ToolDefinition(
    name="serendipity_mark_accessed",
    description="Mark a surfaced memory as accessed (updates access count)",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "memory_id": {"type": "string", "description": "ID of memory to mark"},
        },
        "required": ["memory_id"],
    },
),
ToolDefinition(
    name="pattern_search",
    description="Search patterns across all pattern engines (core, holographic, edge)",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "Search query"},
            "engines": {"type": "array", "items": {"type": "string"}, "description": "Engines to search"},
            "min_confidence": {"type": "number", "default": 0.5},
        },
    },
),
ToolDefinition(
    name="cluster_stats",
    description="Get statistics about memory clustering in 4D holographic space",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
]
