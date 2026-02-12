"""Governor / Safety Tools — validate, goal setting, resource budget.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="governor_validate",
    description="Validate a command against safety rules",
    category=ToolCategory.GOVERNOR,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "command": {"type": "string", "description": "Command to validate"},
        },
        "required": ["command"],
    },
),
ToolDefinition(
    name="governor_set_goal",
    description="Set the current goal for drift detection",
    category=ToolCategory.GOVERNOR,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "goal": {"type": "string", "description": "Current goal"},
        },
        "required": ["goal"],
    },
),
ToolDefinition(
    name="governor_check_drift",
    description="Check if an action drifts from the goal",
    category=ToolCategory.GOVERNOR,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {"type": "string", "description": "Action to check"},
            "goal": {"type": "string", "description": "Goal to check against"},
        },
        "required": ["action"],
    },
),
]
