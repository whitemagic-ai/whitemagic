"""WhiteMagic Tool Registry - Python Version
Single source of truth for tool definitions, mirroring tools-registry.ts.

Used by:
- REST API gateway (/api/tools/*)
- OpenAI-compatible endpoint (/v1/chat/completions)
- Direct Python SDK access

Base classes live in ``tool_types.py`` (no circular-import issues).
Domain definition files live in ``registry_defs/`` and are auto-collected.
"""

from typing import Any

# Re-export base classes for backward compatibility
from whitemagic.tools.tool_types import (  # noqa: F401
    ToolCategory,
    ToolDefinition,
    ToolSafety,
)

# =============================================================================
# TOOL REGISTRY
# =============================================================================
# All tool definitions live in ``registry_defs/*.py`` domain files and are
# auto-collected below.  To add tools, create or edit a domain file — do NOT
# add inline definitions here.
# =============================================================================

TOOL_REGISTRY: list[ToolDefinition] = []

# ---------------------------------------------------------------------------
# Auto-collect tools from registry_defs/ domain files
# ---------------------------------------------------------------------------
try:
    from whitemagic.tools.registry_defs import collect as _collect_domain_tools
    TOOL_REGISTRY.extend(_collect_domain_tools())
except Exception:
    pass  # registry_defs not available — inline tools only


# =============================================================================
# COMMON PARAMS (REQUEST/IDEMPOTENCY/DETERMINISM)
# =============================================================================

_COMMON_PROPS: dict[str, Any] = {
    "request_id": {
        "type": "string",
        "description": "Optional caller-provided request id for tracing. If omitted, a UUID is generated.",
    },
    "idempotency_key": {
        "type": "string",
        "description": "Optional idempotency key. For write tools, retries with the same key will replay prior results.",
    },
    "dry_run": {
        "type": "boolean",
        "description": "If true, do not perform writes; return an execution preview when possible.",
        "default": False,
    },
    "now": {
        "type": "string",
        "description": "Optional ISO timestamp override for deterministic evaluation/replay (best-effort).",
    },
}

for _tool_def in TOOL_REGISTRY:
    schema = _tool_def.input_schema or {}
    props = schema.setdefault("properties", {})
    if isinstance(props, dict):
        for k, v in _COMMON_PROPS.items():
            props.setdefault(k, v)
    _tool_def.input_schema = schema


# =============================================================================
# REGISTRY FUNCTIONS
# =============================================================================

def get_all_tools() -> list[ToolDefinition]:
    """Get all registered tools."""
    return TOOL_REGISTRY.copy()


def get_tool(name: str) -> ToolDefinition | None:
    """Get a tool by name."""
    for tool in TOOL_REGISTRY:
        if tool.name == name:
            return tool
    return None


def get_tools_by_category(category: ToolCategory) -> list[ToolDefinition]:
    """Get tools filtered by category."""
    return [t for t in TOOL_REGISTRY if t.category == category]


def get_tools_by_safety(safety: ToolSafety) -> list[ToolDefinition]:
    """Get tools filtered by safety level."""
    return [t for t in TOOL_REGISTRY if t.safety == safety]


def get_safe_tools() -> list[ToolDefinition]:
    """Get only read-safe tools (for restricted environments)."""
    return get_tools_by_safety(ToolSafety.READ)


def to_openai_tools() -> list[dict[str, Any]]:
    """Convert all tools to OpenAI function calling format."""
    return [t.to_openai_function() for t in TOOL_REGISTRY]


def to_mcp_tools() -> list[dict[str, Any]]:
    """Convert all tools to MCP format."""
    return [t.to_mcp_tool() for t in TOOL_REGISTRY]


def get_tool_names() -> list[str]:
    """Get list of all tool names."""
    return [t.name for t in TOOL_REGISTRY]
