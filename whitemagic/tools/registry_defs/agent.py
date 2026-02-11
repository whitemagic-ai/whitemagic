"""Agent Tools — swarm decompose/route/complete, registry CRUD.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="swarm.decompose",
    description="Decompose a goal into subtasks with capability requirements for multi-agent coordination",
    category=ToolCategory.AGENT,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "goal": {"type": "string", "description": "The high-level goal to decompose"},
            "hints": {"type": "array", "items": {"type": "string"}, "description": "Optional subtask hints"},
        },
        "required": ["goal"],
    },
),
ToolDefinition(
    name="swarm.route",
    description="Route subtasks to available agents by capability matching",
    category=ToolCategory.AGENT,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {"plan_id": {"type": "string"}},
        "required": ["plan_id"],
    },
),
ToolDefinition(
    name="swarm.complete",
    description="Mark a subtask as completed or failed",
    category=ToolCategory.AGENT,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "plan_id": {"type": "string"},
            "task_id": {"type": "string"},
            "result": {"type": "string"},
            "success": {"type": "boolean", "default": True},
        },
        "required": ["plan_id", "task_id"],
    },
),
ToolDefinition(
    name="swarm.vote",
    description="Record a vote from an agent on a consensus topic",
    category=ToolCategory.AGENT,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "topic_id": {"type": "string"},
            "agent_id": {"type": "string"},
            "value": {"type": "string"},
            "confidence": {"type": "number", "default": 1.0},
        },
        "required": ["topic_id", "agent_id", "value"],
    },
),
ToolDefinition(
    name="swarm.resolve",
    description="Resolve a consensus vote using majority, unanimous, first_wins, or weighted strategy",
    category=ToolCategory.AGENT,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "topic_id": {"type": "string"},
            "strategy": {"type": "string", "enum": ["majority", "unanimous", "first_wins", "weighted"], "default": "majority"},
        },
        "required": ["topic_id"],
    },
),
ToolDefinition(
    name="swarm.plan",
    description="Get a specific swarm plan by ID",
    category=ToolCategory.AGENT,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {"plan_id": {"type": "string"}},
        "required": ["plan_id"],
    },
),
ToolDefinition(
    name="swarm.status",
    description="Get swarm coordination status — active plans, votes, topics",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="agent.register",
    description="Register a new agent or update an existing one with name, capabilities, and metadata",
    category=ToolCategory.AGENT,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "name": {"type": "string", "description": "Human-readable agent name"},
            "agent_id": {"type": "string", "description": "Explicit ID (auto-generated if omitted)"},
            "capabilities": {
                "type": "array",
                "items": {"type": "string"},
                "description": "List of capabilities (e.g. code_review, testing, inference)",
            },
            "metadata": {"type": "object", "description": "Arbitrary metadata (model, version, etc.)"},
        },
        "required": ["name"],
    },
),
ToolDefinition(
    name="agent.heartbeat",
    description="Send a heartbeat to keep agent registration active, with optional workload update",
    category=ToolCategory.AGENT,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "agent_id": {"type": "string", "description": "Agent ID"},
            "workload": {"type": "number", "description": "Current workload 0.0-1.0"},
            "current_task": {"type": "string", "description": "Currently executing task ID"},
        },
        "required": ["agent_id"],
    },
),
ToolDefinition(
    name="agent.list",
    description="List registered agents with optional active-only and capability filters",
    category=ToolCategory.AGENT,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "only_active": {"type": "boolean", "default": False, "description": "Only show active agents"},
            "capability": {"type": "string", "description": "Filter by capability"},
        },
    },
),
ToolDefinition(
    name="agent.capabilities",
    description="Query what a specific agent can do (capabilities, metadata, status)",
    category=ToolCategory.AGENT,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "agent_id": {"type": "string", "description": "Agent ID to query"},
        },
        "required": ["agent_id"],
    },
),
ToolDefinition(
    name="agent.deregister",
    description="Remove an agent from the registry",
    category=ToolCategory.AGENT,
    safety=ToolSafety.DELETE,
    input_schema={
        "type": "object",
        "properties": {
            "agent_id": {"type": "string", "description": "Agent ID to remove"},
        },
        "required": ["agent_id"],
    },
),
]
