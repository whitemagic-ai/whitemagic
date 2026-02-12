"""Grimoire Tools — spell suggest/cast/recommend, walkthrough.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="grimoire_suggest",
    description="Suggest Grimoire spells for a given task context",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "task": {"type": "string", "description": "Task description to match spells against"},
            "emotional_state": {"type": "string", "default": "neutral"},
            "wu_xing": {"type": "string", "enum": ["wood", "fire", "earth", "metal", "water"], "default": "earth"},
            "urgency": {"type": "number", "minimum": 0, "maximum": 1, "default": 0.5},
        },
        "required": ["task"],
    },
    gana="Willow", garden="play", quadrant="southern", element="fire",
),
ToolDefinition(
    name="grimoire_cast",
    description="Cast a specific Grimoire spell by name",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "spell_name": {"type": "string", "description": "Name of the spell to cast"},
            "task": {"type": "string", "default": "manual cast"},
            "emotional_state": {"type": "string", "default": "neutral"},
        },
        "required": ["spell_name"],
    },
),
ToolDefinition(
    name="grimoire_recommend",
    description="Recommend MCP tools for a task using Grimoire chapter-tool mapping",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "task": {"type": "string", "description": "Task description to find tools for"},
            "limit": {"type": "integer", "description": "Max recommendations", "default": 5},
        },
        "required": ["task"],
    },
),
ToolDefinition(
    name="grimoire_auto_status",
    description="Get Grimoire auto-caster status and spell availability",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="grimoire_walkthrough",
    description="Interactive 28-chapter Grimoire walkthrough — get chapter details, exercises, and tool mappings",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "chapter": {"type": "integer", "minimum": 1, "maximum": 28, "description": "Chapter number (1-28)"},
            "quadrant": {"type": "string", "enum": ["eastern", "southern", "western", "northern"], "description": "Filter by quadrant"},
        },
    },
),
]
