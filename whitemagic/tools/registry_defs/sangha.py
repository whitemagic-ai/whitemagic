"""Sangha / Multi-Agent Tools — council, coordinate, consensus, role, status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="sangha_chat_send",
    description="Send message to Sangha chat channel for multi-agent coordination",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "message": {"type": "string", "description": "Message content"},
            "channel": {"type": "string", "default": "general"},
            "sender": {"type": "string", "description": "Sender ID"},
            "tags": {"type": "array", "items": {"type": "string"}},
            "priority": {
                "type": "string",
                "enum": ["low", "normal", "high", "urgent"],
                "default": "normal",
            },
        },
        "required": ["message", "sender"],
    },
),
ToolDefinition(
    name="sangha_chat_read",
    description="Read messages from Sangha chat channel",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "channel": {"type": "string", "default": "general"},
            "limit": {"type": "integer", "default": 20},
            "priority": {"type": "string", "enum": ["low", "normal", "high", "urgent"]},
        },
    },
),
ToolDefinition(
    name="sangha_lock",
    description=(
        "Unified resource lock management for multi-agent coordination. "
        "Actions: acquire (lock resource), release (unlock), list (show active locks)."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["acquire", "release", "list"],
                "description": "Action to perform",
            },
            "resource": {"type": "string", "description": "Resource to lock/unlock (for acquire/release)"},
            "reason": {"type": "string", "description": "Lock reason (for acquire)"},
            "timeout": {"type": "integer", "default": 3600, "description": "Lock timeout seconds (for acquire)"},
        },
        "required": ["action"],
    },
),
]
