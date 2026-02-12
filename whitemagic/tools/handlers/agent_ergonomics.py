"""Agent ergonomics tool handlers — starter packs, rate limiter, audit, explain, trust, mesh."""
from typing import Any, cast


def handle_starter_packs(**kwargs: Any) -> dict[str, Any]:
    """Unified starter packs handler — routes by action parameter."""
    action = kwargs.get("action", "list")
    dispatch = {
        "list": handle_starter_packs_list,
        "get": handle_starter_packs_get,
        "suggest": handle_starter_packs_suggest,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_starter_packs_list(**kwargs: Any) -> dict[str, Any]:
    """List available tool starter packs for common agent workflows."""
    from whitemagic.tools.starter_packs import list_packs
    return {"status": "success", "packs": list_packs()}


def handle_starter_packs_get(**kwargs: Any) -> dict[str, Any]:
    """Get a specific starter pack by name."""
    from whitemagic.tools.starter_packs import get_pack
    name = kwargs.get("name")
    if not name:
        return {"status": "error", "error": "name is required"}
    pack = get_pack(name)
    if pack is None:
        from whitemagic.tools.starter_packs import list_packs
        return {"status": "error", "error": f"Unknown pack: {name}", "available": [p["name"] for p in list_packs()]}
    return {"status": "success", **pack}


def handle_starter_packs_suggest(**kwargs: Any) -> dict[str, Any]:
    """Suggest the best starter pack based on a description of what you need."""
    from whitemagic.tools.starter_packs import suggest_pack
    context = kwargs.get("context", kwargs.get("description", ""))
    if not context:
        return {"status": "error", "error": "context or description is required"}
    return {"status": "success", **suggest_pack(context)}


def handle_rate_limiter_stats(**kwargs: Any) -> dict[str, Any]:
    """Get rate limiter statistics and per-agent usage."""
    from whitemagic.tools.rate_limiter import get_rate_limiter
    limiter = get_rate_limiter()
    agent_id = kwargs.get("agent_id")
    result = {"status": "success", "stats": limiter.get_stats()}
    if agent_id:
        result["agent_usage"] = limiter.get_agent_usage(agent_id)
    return result


def handle_audit_export(**kwargs: Any) -> dict[str, Any]:
    """Export audit log in MCP-compatible format."""
    from whitemagic.tools.audit_export import export_audit_log
    return cast(
        "dict[str, Any]",
        export_audit_log(
        limit=kwargs.get("limit", 100),
        since=kwargs.get("since"),
        tool_filter=kwargs.get("tool"),
        agent_filter=kwargs.get("agent_id"),
        format=kwargs.get("format", "mcp"),
        ),
    )


def handle_explain_this(**kwargs: Any) -> dict[str, Any]:
    """Pre-execution impact preview — explain what a tool will do before running it."""
    from whitemagic.tools.explain_this import explain_tool
    tool_name = kwargs.get("target_tool", kwargs.get("tool_name", kwargs.get("tool", "")))
    tool_args = kwargs.get("tool_args", {})
    if not tool_name:
        return {"status": "error", "error": "tool_name is required"}
    return cast("dict[str, Any]", explain_tool(tool_name, **tool_args))


def handle_agent_trust(**kwargs: Any) -> dict[str, Any]:
    """Get agent reputation / trust scores derived from Karma Ledger."""
    from whitemagic.tools.agent_trust import get_agent_trust_scores
    agent_id = kwargs.get("agent_id")
    return cast("dict[str, Any]", get_agent_trust_scores(agent_id=agent_id))


def handle_mesh_status(**kwargs: Any) -> dict[str, Any]:
    """Get cross-node mesh awareness status — peers, events, connectivity."""
    from whitemagic.mesh.awareness import get_mesh_awareness
    return cast("dict[str, Any]", get_mesh_awareness().status())


def handle_mesh_broadcast(**kwargs: Any) -> dict[str, Any]:
    """Broadcast a signal to all mesh peers via gRPC or Redis."""
    from whitemagic.mesh.client import get_mesh_client
    signal_type = kwargs.get("signal_type", "GENERIC")
    payload = kwargs.get("payload", "")
    client = get_mesh_client()
    result = client.broadcast_signal(signal_type, payload=str(payload))
    return {"status": "success" if result.success else "error", "message": result.message}
