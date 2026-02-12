"""Memory Tools — create, search, read, update, delete, export/import, vector search, holographic, lifecycle.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="create_memory",
    description="Create a new memory entry (short-term or long-term)",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "title": {"type": "string", "description": "Memory title"},
            "content": {"type": "string", "description": "Memory content (markdown supported)"},
            "type": {
                "type": "string",
                "enum": ["short_term", "long_term"],
                "description": "Memory type",
                "default": "short_term",
            },
            "tags": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Tags for categorization",
                "default": [],
            },
        },
        "required": ["title", "content"],
    },
    gana="Neck", garden="presence", quadrant="eastern", element="wood",
),
ToolDefinition(
    name="search_memories",
    description="Search memories by query, type, and tags",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "Search query"},
            "type": {
                "type": "string",
                "enum": ["short_term", "long_term"],
                "description": "Filter by memory type",
            },
            "tags": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Filter by tags (AND logic)",
            },
            "include_archived": {
                "type": "boolean",
                "description": "Include archived memories",
                "default": False,
            },
            "limit": {
                "type": "integer",
                "description": "Maximum results to return",
                "default": 20,
            },
        },
    },
),
ToolDefinition(
    name="export_memories",
    description="Export memories in JSON, CSV, Markdown, or ZIP format",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "format": {
                "type": "string",
                "enum": ["json", "csv", "markdown", "zip"],
                "description": "Export format",
                "default": "json",
            },
            "tags": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Filter by tags",
            },
            "memory_type": {"type": "string", "description": "Filter by memory type"},
            "search": {"type": "string", "description": "Filter by content search"},
            "limit": {"type": "integer", "description": "Max memories to export", "default": 100},
            "include_metadata": {"type": "boolean", "default": True},
        },
    },
),
ToolDefinition(
    name="import_memories",
    description="Import memories from JSON, CSV, or Markdown data",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "format": {
                "type": "string",
                "enum": ["json", "csv", "markdown"],
                "description": "Import format",
            },
            "data": {"type": "string", "description": "The data to import"},
            "merge_strategy": {
                "type": "string",
                "enum": ["skip", "overwrite", "merge"],
                "default": "skip",
            },
            "validate_only": {"type": "boolean", "default": False},
        },
        "required": ["format", "data"],
    },
),
ToolDefinition(
    name="read_memory",
    description="Read full content of a specific memory",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "filename": {
                "type": "string",
                "description": "Memory filename (e.g., '20251115_setup_wizard.md')",
            },
            "include_metadata": {
                "type": "boolean",
                "description": "Include metadata (tags, dates, etc)",
                "default": True,
            },
        },
        "required": ["filename"],
    },
),
ToolDefinition(
    name="fast_read_memory",
    description="FAST: Read memory content with Rust optimizations (10-100x faster)",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "filename": {"type": "string", "description": "Memory filename"},
            "cache": {"type": "boolean", "default": True},
            "fast_mode": {"type": "boolean", "default": True},
        },
        "required": ["filename"],
    },
),
ToolDefinition(
    name="batch_read_memories",
    description="BATCH: Read multiple memories in one operation",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "filenames": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Array of memory filenames to read",
            },
            "cache": {"type": "boolean", "default": True},
            "fast_mode": {"type": "boolean", "default": True},
        },
        "required": ["filenames"],
    },
),
ToolDefinition(
    name="list_memories",
    description="List all memories with metadata",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "include_archived": {"type": "boolean", "default": False},
            "sort_by": {
                "type": "string",
                "enum": ["created", "updated", "accessed", "title"],
                "default": "created",
            },
            "type": {
                "type": "string",
                "enum": ["short_term", "long_term"],
            },
            "limit": {"type": "integer", "default": 50},
        },
    },
),
ToolDefinition(
    name="update_memory",
    description="Update an existing memory's content, title, or tags",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "filename": {"type": "string", "description": "Memory filename to update"},
            "title": {"type": "string", "description": "New title"},
            "content": {"type": "string", "description": "New content"},
            "tags": {"type": "array", "items": {"type": "string"}},
            "add_tags": {"type": "array", "items": {"type": "string"}},
            "remove_tags": {"type": "array", "items": {"type": "string"}},
        },
        "required": ["filename"],
    },
),
ToolDefinition(
    name="delete_memory",
    description="Delete or archive a memory",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.DELETE,
    input_schema={
        "type": "object",
        "properties": {
            "filename": {"type": "string", "description": "Memory filename to delete"},
            "permanent": {
                "type": "boolean",
                "description": "If true, permanently delete; otherwise archive",
                "default": False,
            },
        },
        "required": ["filename"],
    },
),

ToolDefinition(
    name="vector.search",
    description="Search memories by semantic similarity using embeddings (sentence-transformers or TF-IDF fallback)",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "Natural language search query"},
            "limit": {"type": "integer", "default": 10, "description": "Max results"},
        },
        "required": ["query"],
    },
),
ToolDefinition(
    name="vector.index",
    description="Index a memory for vector similarity search",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "memory_id": {"type": "string", "description": "Memory ID to index"},
            "content": {"type": "string", "description": "Content to embed"},
            "title": {"type": "string", "description": "Optional title"},
        },
        "required": ["memory_id", "content"],
    },
),
ToolDefinition(
    name="vector.status",
    description="Get vector search engine status — index size, model info, backend",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="view_hologram",
    description="Get a holographic snapshot of the memory space or query a specific sector",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "operation": {
                "type": "string",
                "enum": ["snapshot", "query", "status"],
                "description": "Operation to perform",
                "default": "snapshot",
            },
            "x": {"type": "number", "description": "X coordinate (Logic-Emotion)", "default": 0.0},
            "y": {"type": "number", "description": "Y coordinate (Micro-Macro)", "default": 0.0},
            "z": {"type": "number", "description": "Z coordinate (Time)", "default": 0.0},
            "w": {"type": "number", "description": "W coordinate (Importance)", "default": 0.5},
            "limit": {"type": "integer", "description": "Max results for query", "default": 10},
        },
    },
),

ToolDefinition(
    name="memory.lifecycle",
    description=(
        "Unified memory lifecycle management — mindful forgetting & hippocampal consolidation. "
        "Actions: sweep (retention evaluation + gentle decay), stats (lifecycle statistics), "
        "consolidate (cluster + synthesize strategy memories), consolidation_stats (consolidation engine stats)."
    ),
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["sweep", "stats", "consolidate", "consolidation_stats"],
                "description": "Action to perform",
                "default": "stats",
            },
        },
    },
),
]
