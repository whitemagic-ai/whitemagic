"""Dream Cycle & BitNet — unified dream control, bitnet infer/status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="dream",
    description=(
        "Unified Dream Cycle control — background processing during idle time "
        "(consolidation, serendipity, kaizen, oracle, decay). "
        "Actions: start, stop, status, now."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["start", "stop", "status", "now"],
                "description": "Action to perform",
            },
            "idle_threshold": {"type": "number", "description": "Seconds of idle before dreaming starts (for start)", "default": 120},
            "cycle_interval": {"type": "number", "description": "Seconds between dream phases (for start)", "default": 60},
        },
        "required": ["action"],
    },
    gana="Void", garden="stillness", quadrant="northern", element="water",
),

ToolDefinition(
    name="bitnet_infer",
    description="Run local inference via BitNet 1-bit LLM (requires WHITEMAGIC_ENABLE_BITNET=1)",
    category=ToolCategory.INFERENCE,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "prompt": {"type": "string", "description": "Input prompt for inference"},
            "n_predict": {"type": "integer", "description": "Max tokens to generate", "default": 128},
            "temp": {"type": "number", "description": "Sampling temperature", "default": 0.8},
            "mode": {
                "type": "string",
                "enum": ["auto", "redis", "direct"],
                "description": "Inference mode: redis (Gan Ying bus), direct (subprocess), or auto",
                "default": "auto",
            },
        },
        "required": ["prompt"],
    },
),
ToolDefinition(
    name="bitnet_status",
    description="Check BitNet availability and configuration",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
]
