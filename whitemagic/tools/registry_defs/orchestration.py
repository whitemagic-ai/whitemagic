"""Cascade / Orchestration Tools — cascade protocol and status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="execute_cascade",
    description="Execute an intelligent tool chain pattern with Yin-Yang balance pacing",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "pattern_name": {
                "type": "string",
                "description": "Cascade pattern name (e.g., 'analyze_codebase')",
            },
            "context": {
                "type": "object",
                "description": "Context dictionary with inputs for the pattern",
            },
            "options": {
                "type": "object",
                "description": "Execution options (enable_yin_yang, max_parallel_calls, dry_run)",
            },
        },
        "required": ["pattern_name", "context"],
    },
),
ToolDefinition(
    name="list_cascade_patterns",
    description="List all available cascade patterns",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "tag": {"type": "string", "description": "Filter by tag"},
        },
    },
),
]
