"""Intelligence Tools — context optimizer, prompt templates.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="context.pack",
    description="Pack memories into an optimized context window for LLM calls — salience scoring + primacy/recency reorder",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "Search query to find and score relevant memories"},
            "token_budget": {"type": "integer", "default": 8000, "description": "Maximum tokens for the context window"},
            "limit": {"type": "integer", "default": 50, "description": "Max memories to consider"},
        },
        "required": ["query"],
    },
),
ToolDefinition(
    name="context.status",
    description="Get Context Window Optimizer configuration and status",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="prompt.render",
    description="Render a named prompt template with variable substitution and optional Wu Xing tone",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "name": {"type": "string", "description": "Template name (e.g. session_greeting, memory_synthesis)"},
            "wu_xing": {"type": "string", "enum": ["wood", "fire", "earth", "metal", "water"], "description": "Wu Xing element for tone selection"},
            "variables": {"type": "object", "description": "Key-value pairs for template variable substitution"},
        },
        "required": ["name"],
    },
),
ToolDefinition(
    name="prompt.list",
    description="List all available prompt templates, optionally filtered by tag",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "tag": {"type": "string", "description": "Filter templates by tag"},
        },
    },
),
ToolDefinition(
    name="prompt.reload",
    description="Reload prompt templates from $WM_STATE_ROOT/prompts/ YAML files",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {}},
),
]
