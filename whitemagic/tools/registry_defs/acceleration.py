"""Acceleration Tools — SIMD cosine, Rust bridge, native status.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="simd.cosine",
    description="Compute cosine similarity between two vectors using Zig SIMD acceleration (Python fallback available)",
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "a": {"type": "array", "items": {"type": "number"}, "description": "First vector"},
            "b": {"type": "array", "items": {"type": "number"}, "description": "Second vector"},
        },
        "required": ["a", "b"],
    },
),
ToolDefinition(
    name="simd.batch",
    description="Batch cosine similarity — compare query against multiple vectors using SIMD",
    category=ToolCategory.METRICS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "array", "items": {"type": "number"}},
            "vectors": {"type": "array", "items": {"type": "array", "items": {"type": "number"}}},
        },
        "required": ["query", "vectors"],
    },
),
ToolDefinition(
    name="simd.status",
    description="Get SIMD acceleration status — Zig library availability, lane width, backend",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="rust_status",
    description="Check Rust acceleration availability and functions",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {},
    },
),
ToolDefinition(
    name="rust_similarity",
    description="Calculate text similarity using Rust SIMD (50x faster)",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "text1": {"type": "string"},
            "text2": {"type": "string"},
        },
        "required": ["text1", "text2"],
    },
),
]
