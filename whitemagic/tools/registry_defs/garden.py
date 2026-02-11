"""Garden Tools — activate, status, synergy, health, galactic dashboard.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="galactic.dashboard",
    description="Rich Galactic Map dashboard — zone counts, crown jewels, type distribution, retention stats, dream cycle status",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="garden_activate",
    description="Activate a consciousness garden",
    category=ToolCategory.GARDEN,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "garden": {"type": "string", "description": "Garden name"},
        },
        "required": ["garden"],
    },
),
ToolDefinition(
    name="garden_status",
    description="Get garden activation status",
    category=ToolCategory.GARDEN,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),
ToolDefinition(
    name="garden_health",
    description="Check garden health metrics",
    category=ToolCategory.GARDEN,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),
]
