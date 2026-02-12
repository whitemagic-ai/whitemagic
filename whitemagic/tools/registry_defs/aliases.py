"""Simplified Tool Aliases — Human-friendly shorthand for common operations.
==========================================================================
Maps intuitive verbs to common WhiteMagic operations.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
    ToolDefinition(
        name="remember",
        description="Shorthand: create a memory. Equivalent to gana_neck → create_memory.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "content": {"type": "string", "description": "Memory content"},
                "title": {"type": "string", "description": "Memory title"},
                "tags": {"type": "array", "items": {"type": "string"}},
                "importance": {"type": "number", "default": 0.5},
            },
            "required": ["content"],
        },
        gana="Neck", garden="practice", quadrant="eastern", element="wood",
    ),
    ToolDefinition(
        name="recall",
        description="Shorthand: search memories. Equivalent to gana_winnowing_basket → hybrid_recall.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query"},
                "limit": {"type": "integer", "default": 5},
            },
            "required": ["query"],
        },
        gana="Winnowing Basket", garden="wisdom", quadrant="eastern", element="wood",
    ),
    ToolDefinition(
        name="think",
        description="Shorthand: bicameral reasoning. Equivalent to gana_three_stars → reasoning.bicameral.",
        category=ToolCategory.SYNTHESIS,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "topic": {"type": "string", "description": "Topic to reason about"},
                "perspectives": {"type": "array", "items": {"type": "string"}, "default": ["analytical", "creative", "critical"]},
            },
            "required": ["topic"],
        },
        gana="Three Stars", garden="dharma", quadrant="western", element="metal",
    ),
    ToolDefinition(
        name="check",
        description="Shorthand: system health check. Equivalent to gana_root → health_report.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
        gana="Root", garden="truth", quadrant="eastern", element="wood",
    ),
]
