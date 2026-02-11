"""Audit & Observability — Merkle chain, anomaly detection, OpenTelemetry.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="karma.verify_chain",
    description="Verify the Merkle hash chain integrity of the Karma Ledger — detects tampering",
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="anomaly",
    description=(
        "Unified anomaly detection on Harmony Vector dimensions. "
        "Actions: check (active anomalies), history (recent alerts), status (detector stats)."
    ),
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["check", "history", "status"],
                "description": "Action to perform",
                "default": "check",
            },
            "limit": {"type": "integer", "default": 20, "description": "Max alerts to return (for history)"},
        },
    },
),

ToolDefinition(
    name="otel",
    description=(
        "Unified OpenTelemetry observability. "
        "Actions: spans (recent trace records), metrics (aggregated per-tool stats), status (exporter state)."
    ),
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["spans", "metrics", "status"],
                "description": "Action to perform",
                "default": "metrics",
            },
            "limit": {"type": "integer", "default": 20, "description": "Max spans to return (for spans)"},
        },
    },
),
]
