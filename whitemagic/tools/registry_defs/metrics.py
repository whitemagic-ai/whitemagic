"""Metrics Tools — performance, health, token report, coherence, anti-loop.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="track_metric",
    description="Record a quantitative metric",
    category=ToolCategory.METRICS,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "category": {"type": "string", "description": "Metric category"},
            "metric": {"type": "string", "description": "Metric name"},
            "value": {"type": "number", "description": "Metric value"},
            "context": {"type": "string", "description": "Optional context"},
        },
        "required": ["category", "metric", "value"],
    },
),
ToolDefinition(
    name="get_metrics_summary",
    description="Retrieve metrics dashboard summary",
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "categories": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Categories to include",
            },
        },
    },
),
ToolDefinition(
    name="record_yin_yang_activity",
    description="Record Yin-Yang activity for balance tracking",
    category=ToolCategory.METRICS,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "activity": {
                "type": "string",
                "enum": ["READ", "WRITE", "THINK", "REST", "CREATE", "DELETE"],
                "description": "Activity type",
            },
        },
        "required": ["activity"],
    },
),
ToolDefinition(
    name="get_yin_yang_balance",
    description="Get current Yin-Yang balance report",
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),
ToolDefinition(
    name="harmony_vector",
    description=(
        "Get the multi-dimensional Harmony Vector — Whitemagic's real-time "
        "health pulse. Returns seven normalized [0-1] dimensions: balance "
        "(yin/yang ratio), throughput, latency, error_rate, dharma (ethical "
        "score), karma_debt (declared-vs-actual side-effect mismatches), and "
        "energy (resource pressure). Also reports guna distribution "
        "(sattvic/rajasic/tamasic), p50/p95 latency, and a composite "
        "harmony_score. Use this to self-regulate agent behavior."
    ),
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
    gana="Stomach", garden="vitality", quadrant="western", element="metal",
),
]
