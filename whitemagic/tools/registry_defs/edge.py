"""Edge / Inference Tools — edge_infer, edge_stats, edge_status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="edge_infer",
    description="Rule-based edge inference (no API calls)",
    category=ToolCategory.EDGE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "Query to process locally"},
            "mode": {
                "type": "string",
                "enum": ["auto", "fast", "explore", "deep", "memory_augmented"],
                "default": "auto",
            },
            "ground_in_memory": {
                "type": "boolean",
                "description": "Use memory for RAG-style context",
                "default": False,
            },
        },
        "required": ["query"],
    },
),
ToolDefinition(
    name="edge_batch_infer",
    description="Batch edge inference for multiple queries",
    category=ToolCategory.EDGE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "queries": {
                "type": "array",
                "items": {"type": "string"},
                "description": "List of queries to process",
            },
        },
        "required": ["queries"],
    },
),
ToolDefinition(
    name="edge_stats",
    description="Get edge inference statistics (tokens saved, resolution rate)",
    category=ToolCategory.EDGE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),
]
