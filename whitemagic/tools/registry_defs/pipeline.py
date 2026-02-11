"""Pipeline Tools — unified create/status/list.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="pipeline",
    description=(
        "Unified multi-step tool pipeline management. "
        "Actions: create (build & optionally execute a pipeline with $prev/$step[N] refs), "
        "status (check pipeline execution), list (browse pipelines)."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["create", "status", "list"],
                "description": "Action to perform",
            },
            "name": {"type": "string", "description": "Pipeline name (for create)"},
            "steps": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "tool": {"type": "string"},
                        "args": {"type": "object"},
                        "continue_on_error": {"type": "boolean", "default": False},
                    },
                    "required": ["tool"],
                },
                "description": "Ordered tool calls (for create)",
            },
            "execute": {"type": "boolean", "default": True, "description": "Execute immediately (for create)"},
            "pipeline_id": {"type": "string", "description": "Pipeline ID (for status)"},
            "filter_status": {
                "type": "string",
                "enum": ["created", "pending", "running", "completed", "failed"],
                "description": "Filter by status (for list)",
            },
            "limit": {"type": "integer", "default": 20},
        },
        "required": ["action"],
    },
),
]
