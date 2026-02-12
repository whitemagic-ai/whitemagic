"""Cross-Session Learning — usage patterns, recommendations, status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="learning.patterns",
    description="Get aggregated cross-session tool usage patterns — co-occurrence, sequences, error correlations",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="learning.suggest",
    description="Suggest next tools based on learned cross-session sequences",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "current_tool": {"type": "string", "description": "Tool you just used — suggestions based on what typically follows"},
        },
        "required": ["current_tool"],
    },
),
ToolDefinition(
    name="learning.status",
    description="Get cross-session learner status — sessions analyzed, unique tools, data file",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
]
