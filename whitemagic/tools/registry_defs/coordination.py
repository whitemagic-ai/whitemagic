"""Coordination Tools — broker pub/sub, task distribution, voting, Ollama bridge.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="broker.publish",
    description="Publish a message to a Redis channel for inter-agent communication",
    category=ToolCategory.BROKER,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "channel": {"type": "string", "description": "Channel name to publish to"},
            "message": {
                "description": "Message content (string or object)",
            },
            "sender": {"type": "string", "description": "Sender identifier"},
            "priority": {
                "type": "string",
                "enum": ["low", "normal", "high", "urgent"],
                "default": "normal",
            },
        },
        "required": ["channel", "message"],
    },
),
ToolDefinition(
    name="broker.history",
    description="Retrieve recent message history from a Redis channel",
    category=ToolCategory.BROKER,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "channel": {"type": "string", "description": "Channel name to read history from"},
            "limit": {"type": "integer", "default": 20, "description": "Max messages to return"},
        },
        "required": ["channel"],
    },
),
ToolDefinition(
    name="broker.status",
    description="Check Redis broker connectivity and status",
    category=ToolCategory.BROKER,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),

ToolDefinition(
    name="task.distribute",
    description="Create and enqueue a task for distribution with intelligent routing",
    category=ToolCategory.TASK,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "task_type": {
                "type": "string",
                "enum": ["compilation", "testing", "build", "file_processing",
                         "ai_inference", "code_review", "backup", "sync", "general"],
                "default": "general",
                "description": "Task profile for routing and timeout defaults",
            },
            "command": {"type": "string", "description": "Command to execute"},
            "description": {"type": "string", "description": "Human-readable task description"},
            "target": {
                "type": "string",
                "description": "Execution target (auto, local, or hostname)",
            },
            "priority": {
                "type": "string",
                "enum": ["low", "medium", "high"],
            },
            "timeout": {"type": "integer", "description": "Timeout in seconds"},
            "cwd": {"type": "string", "description": "Working directory for execution"},
        },
    },
),
ToolDefinition(
    name="task.status",
    description="Get status of a specific task or summary of all tasks",
    category=ToolCategory.TASK,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "task_id": {"type": "string", "description": "Specific task ID (omit for summary)"},
        },
    },
),
ToolDefinition(
    name="task.list",
    description="List tasks with optional status and type filters",
    category=ToolCategory.TASK,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "filter_status": {
                "type": "string",
                "enum": ["pending", "running", "completed", "failed"],
                "description": "Filter by task status",
            },
            "filter_type": {"type": "string", "description": "Filter by task type"},
            "limit": {"type": "integer", "default": 50},
        },
    },
),
ToolDefinition(
    name="task.complete",
    description="Mark a task as completed or failed with results (used by workers)",
    category=ToolCategory.TASK,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "task_id": {"type": "string", "description": "Task ID to complete"},
            "success": {"type": "boolean", "default": True},
            "output": {"type": "string", "description": "Task output/result"},
            "error_message": {"type": "string", "description": "Error message if failed"},
        },
        "required": ["task_id"],
    },
),

ToolDefinition(
    name="vote.create",
    description="Create a voting session for multi-AI ensemble decision making",
    category=ToolCategory.VOTING,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "problem": {"type": "string", "description": "Problem or question to vote on"},
            "task_type": {
                "type": "string",
                "description": "Type of decision (architecture, security, refactoring, etc.)",
                "default": "general",
            },
            "options": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Pre-defined options to vote on (optional, free-form if omitted)",
            },
        },
        "required": ["problem"],
    },
),
ToolDefinition(
    name="vote.cast",
    description="Cast a vote in a voting session with solution, confidence, and reasoning",
    category=ToolCategory.VOTING,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "session_id": {"type": "string", "description": "Voting session ID"},
            "voter": {"type": "string", "description": "Voter identifier (agent name)"},
            "solution": {"type": "string", "description": "Proposed solution"},
            "confidence": {
                "type": "integer",
                "description": "Confidence 0-100% (auto-extracted from solution text if omitted)",
            },
            "reasoning": {"type": "string", "description": "Key reasoning behind the vote"},
            "risks": {"type": "string", "description": "Potential risks or concerns"},
        },
        "required": ["session_id", "voter"],
    },
),
ToolDefinition(
    name="vote.analyze",
    description="Analyze votes and determine consensus strength for a voting session",
    category=ToolCategory.VOTING,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "session_id": {"type": "string", "description": "Voting session ID to analyze"},
            "close": {
                "type": "boolean",
                "default": False,
                "description": "Close the session after analysis",
            },
        },
        "required": ["session_id"],
    },
),
ToolDefinition(
    name="vote.list",
    description="List voting sessions with optional status filter",
    category=ToolCategory.VOTING,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "filter_status": {
                "type": "string",
                "enum": ["open", "closed"],
                "description": "Filter by session status",
            },
            "limit": {"type": "integer", "default": 20},
        },
    },
),
ToolDefinition(
    name="vote.record_outcome",
    description="Record whether the winning solution actually worked — feeds per-voter accuracy tracking",
    category=ToolCategory.VOTING,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "session_id": {"type": "string", "description": "Voting session ID"},
            "success": {"type": "boolean", "description": "Did the winning solution work?"},
            "notes": {"type": "string", "description": "Optional outcome notes"},
        },
        "required": ["session_id"],
    },
),

ToolDefinition(
    name="ollama.models",
    description="List available models on the local Ollama server",
    category=ToolCategory.INFERENCE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),
ToolDefinition(
    name="ollama.generate",
    description="Generate text using a local Ollama model (single-turn prompt)",
    category=ToolCategory.INFERENCE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "model": {"type": "string", "description": "Model name (e.g. phi3, tinyllama, llama3.2)"},
            "prompt": {"type": "string", "description": "Prompt text"},
        },
        "required": ["model", "prompt"],
    },
),
ToolDefinition(
    name="ollama.chat",
    description="Chat with a local Ollama model (multi-turn conversation)",
    category=ToolCategory.INFERENCE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "model": {"type": "string", "description": "Model name"},
            "messages": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "role": {"type": "string", "enum": ["system", "user", "assistant"]},
                        "content": {"type": "string"},
                    },
                    "required": ["role", "content"],
                },
                "description": "Conversation messages",
            },
        },
        "required": ["model", "messages"],
    },
),
]
