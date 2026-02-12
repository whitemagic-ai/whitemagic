"""Trust & Mesh — explain_this, agent trust, mesh status/broadcast.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="explain_this",
    description=(
        "Pre-execution impact preview. Before running a tool, call explain_this "
        "to see: Dharma evaluation, resource estimate, dependency chain, risk "
        "assessment, karma forecast, maturity gate, and circuit breaker state. "
        "Returns a verdict: SAFE_TO_PROCEED, PROCEED_WITH_CAUTION, or BLOCKED."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "target_tool": {"type": "string", "description": "Name of the tool to preview"},
            "tool_args": {"type": "object", "description": "Arguments that would be passed to the tool", "default": {}},
        },
        "required": ["target_tool"],
    },
),
ToolDefinition(
    name="agent.trust",
    description=(
        "Get agent reputation and trust scores derived from the Karma Ledger. "
        "Shows per-agent reliability, mismatch rate, debt contribution, and "
        "composite trust score. Optionally filter to a specific agent."
    ),
    category=ToolCategory.AGENT,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "agent_id": {"type": "string", "description": "Filter to a specific agent (optional)"},
        },
    },
),

ToolDefinition(
    name="mesh.status",
    description=(
        "Get cross-node mesh awareness status. Shows known peers, "
        "connectivity state, gRPC/Redis bridge health, and recent mesh events."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="mesh.broadcast",
    description=(
        "Broadcast a signal to all mesh peers via gRPC (if connected) "
        "or Redis pub/sub fallback. Used for cross-node coordination."
    ),
    category=ToolCategory.BROKER,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "signal_type": {"type": "string", "description": "Type of signal to broadcast"},
            "payload": {"type": "string", "description": "Signal payload (JSON string or text)"},
        },
        "required": ["signal_type"],
    },
),
]
