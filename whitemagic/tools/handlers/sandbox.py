"""MCP handlers for Tool Sandboxing."""

from typing import Any


def handle_sandbox_status(**kwargs: Any) -> dict[str, Any]:
    """Get sandbox status and per-tool execution stats."""
    from whitemagic.tools.sandbox import get_sandbox
    return {"status": "success", **get_sandbox().status()}


def handle_sandbox_violations(**kwargs: Any) -> dict[str, Any]:
    """Get recent sandbox limit violations."""
    from whitemagic.tools.sandbox import get_sandbox
    limit = int(kwargs.get("limit", 20))
    sandbox = get_sandbox()
    return {"status": "success", "violations": sandbox.get_violations(limit=limit)}


def handle_sandbox_set_limits(**kwargs: Any) -> dict[str, Any]:
    """Set custom resource limits for a specific tool."""
    from whitemagic.tools.sandbox import ResourceLimits, get_sandbox
    tool_name = kwargs.get("target_tool", kwargs.get("tool_name", ""))
    if not tool_name:
        return {"status": "error", "error": "tool_name is required"}
    limits = ResourceLimits(
        timeout_s=float(kwargs.get("timeout_s", 30)),
        max_memory_mb=int(kwargs.get("max_memory_mb", 512)),
        max_cpu_s=float(kwargs.get("max_cpu_s", 10)),
    )
    sandbox = get_sandbox()
    sandbox.set_limits(tool_name, limits)
    return {"status": "success", "tool": tool_name, "limits": limits.to_dict()}
