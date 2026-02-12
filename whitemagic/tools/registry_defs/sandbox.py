"""Tool Sandboxing — status, violations, set_limits.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="sandbox.status",
    description="Get sandbox status — per-tool execution stats, enabled state, resource module availability",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="sandbox.violations",
    description="Get recent sandbox limit violations (timeout, memory, CPU)",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {"limit": {"type": "integer", "default": 20}},
    },
),
ToolDefinition(
    name="sandbox.set_limits",
    description="Set custom resource limits for a specific tool (timeout, memory, CPU)",
    category=ToolCategory.GOVERNOR,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "target_tool": {"type": "string", "description": "Name of the tool to set limits for"},
            "timeout_s": {"type": "number", "default": 30},
            "max_memory_mb": {"type": "integer", "default": 512},
            "max_cpu_s": {"type": "number", "default": 10},
        },
        "required": ["target_tool"],
    },
),
]
