"""Session Tools — create, checkpoint, resume, bootstrap, scratchpad.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="create_session",
    description="Create new work session with automatic state management",
    category=ToolCategory.SESSION,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "name": {"type": "string", "description": "Session name"},
            "goals": {"type": "array", "items": {"type": "string"}},
            "tags": {"type": "array", "items": {"type": "string"}},
            "auto_checkpoint": {"type": "boolean", "default": True},
            "context_tier": {"type": "integer", "enum": [0, 1, 2], "default": 1},
        },
        "required": ["name"],
    },
),
ToolDefinition(
    name="checkpoint_session",
    description="Create a checkpoint in the current session",
    category=ToolCategory.SESSION,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "session_id": {"type": "string", "description": "Session ID"},
            "checkpoint_name": {"type": "string", "description": "Checkpoint name"},
        },
        "required": ["session_id"],
    },
),
ToolDefinition(
    name="resume_session",
    description="Resume a previous session with context restoration",
    category=ToolCategory.SESSION,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "session_id": {"type": "string", "description": "Session ID to resume"},
            "load_tier": {"type": "integer", "enum": [0, 1, 2], "default": 1},
        },
        "required": ["session_id"],
    },
),
ToolDefinition(
    name="session_bootstrap",
    description="Initialize session context for a new AI session",
    category=ToolCategory.SESSION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
    gana="Horn", garden="courage", quadrant="eastern", element="wood",
),
ToolDefinition(
    name="scratchpad",
    description=(
        "Unified scratchpad management for active work. "
        "Actions: create (new scratchpad), update (modify section), finalize (convert to permanent memory)."
    ),
    category=ToolCategory.SESSION,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["create", "update", "finalize"],
                "description": "Action to perform",
            },
            "name": {"type": "string", "description": "Scratchpad name (for create)"},
            "session_id": {"type": "string", "description": "Associated session ID (for create)"},
            "scratchpad_id": {"type": "string", "description": "Scratchpad ID (for update/finalize)"},
            "section": {
                "type": "string",
                "enum": ["current_focus", "decisions", "questions", "next_steps", "ideas"],
                "description": "Section to update (for update)",
            },
            "content": {"type": "string", "description": "Content (for update)"},
            "memory_type": {
                "type": "string",
                "enum": ["short_term", "long_term"],
                "default": "long_term",
                "description": "Target memory type (for finalize)",
            },
            "auto_analyze": {"type": "boolean", "default": True, "description": "Multi-spectral analysis (for finalize)"},
        },
        "required": ["action"],
    },
),
]
