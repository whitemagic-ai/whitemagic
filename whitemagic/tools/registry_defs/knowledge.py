"""Knowledge Graph Tools — extract, query, top, status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="kg.extract",
    description="Extract entities and relations from text into the knowledge graph (spaCy NER + regex fallback)",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "source_id": {"type": "string", "description": "Source memory/document ID"},
            "text": {"type": "string", "description": "Text to extract entities from"},
        },
        "required": ["text"],
    },
),
ToolDefinition(
    name="kg.query",
    description="Query an entity and its connections in the knowledge graph",
    category=ToolCategory.MEMORY,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {"name": {"type": "string", "description": "Entity name to query"}},
        "required": ["name"],
    },
),
ToolDefinition(
    name="kg.top",
    description="Get top entities by mention count from the knowledge graph",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {"limit": {"type": "integer", "default": 20}},
    },
),
ToolDefinition(
    name="kg.status",
    description="Get knowledge graph status — entity/relation counts, spaCy availability",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
]
