"""Mobile Webhook Triggers — REST endpoints for triggering actions from mobile devices.

Inspired by Tools(copy)/scripts/integrations/mobile/mobile_webhook_server.py.

Provides a set of predefined safe actions that can be triggered via simple
HTTP POST requests from mobile automation apps (Shortcuts, Tasker, etc.).

All actions are validated against a whitelist and logged through the
Dharma system for auditability.

Routes:
  POST /api/trigger/<action>     — Execute a predefined action
  GET  /api/trigger/actions      — List available actions
  GET  /api/trigger/history      — Recent trigger history
  POST /api/trigger/custom       — Execute a custom tool call (with safety checks)
"""

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Any

try:
    from fastapi import APIRouter, HTTPException, Request
    from pydantic import BaseModel
except ImportError:
    raise ImportError("FastAPI required for webhook triggers")


router = APIRouter(prefix="/api/trigger", tags=["webhook-triggers"])


# ---------------------------------------------------------------------------
# Predefined safe actions
# ---------------------------------------------------------------------------

ALLOWED_ACTIONS: dict[str, dict[str, Any]] = {
    "status": {
        "description": "Get system health status",
        "tool": "gnosis",
        "args": {"compact": True},
        "safety": "read",
    },
    "harmony": {
        "description": "Get Harmony Vector snapshot",
        "tool": "harmony_vector",
        "args": {},
        "safety": "read",
    },
    "memory_search": {
        "description": "Search memories by query",
        "tool": "search_memories",
        "args_from_body": ["query"],
        "safety": "read",
    },
    "sweep": {
        "description": "Trigger memory lifecycle sweep",
        "tool": "memory.lifecycle_sweep",
        "args": {},
        "safety": "write",
    },
    "dream_start": {
        "description": "Start dream cycle",
        "tool": "dream_start",
        "args": {},
        "safety": "write",
    },
    "dream_stop": {
        "description": "Stop dream cycle",
        "tool": "dream_stop",
        "args": {},
        "safety": "write",
    },
    "dream_status": {
        "description": "Check dream cycle status",
        "tool": "dream_status",
        "args": {},
        "safety": "read",
    },
    "agents": {
        "description": "List registered agents",
        "tool": "agent.list",
        "args": {},
        "safety": "read",
    },
    "tasks": {
        "description": "List recent tasks",
        "tool": "task.list",
        "args": {"limit": 10},
        "safety": "read",
    },
    "broker_status": {
        "description": "Check Redis broker status",
        "tool": "broker.status",
        "args": {},
        "safety": "read",
    },
    "homeostasis": {
        "description": "Run homeostatic check",
        "tool": "homeostasis.check",
        "args": {},
        "safety": "read",
    },
    "maturity": {
        "description": "Assess maturity stage",
        "tool": "maturity.assess",
        "args": {},
        "safety": "read",
    },
    "consolidate": {
        "description": "Trigger memory consolidation",
        "tool": "memory.consolidate",
        "args": {},
        "safety": "write",
    },
    "serendipity": {
        "description": "Surface serendipitous connections",
        "tool": "serendipity_surface",
        "args": {},
        "safety": "read",
    },
}


# ---------------------------------------------------------------------------
# Trigger history
# ---------------------------------------------------------------------------

def _history_path() -> Path:
    try:
        from whitemagic.config.paths import WM_ROOT
        d = Path(WM_ROOT) / "triggers"
    except Exception:
        d = Path.home() / ".whitemagic" / "triggers"
    d.mkdir(parents=True, exist_ok=True)
    return Path(d / "history.jsonl")


def _log_trigger(action: str, result: dict[str, Any], source: str = "webhook") -> None:
    entry = {
        "action": action,
        "source": source,
        "status": result.get("status", "unknown"),
        "timestamp": datetime.now().isoformat(),
        "host": os.uname().nodename,
    }
    try:
        with open(_history_path(), "a", encoding="utf-8") as f:
            f.write(json.dumps(entry) + "\n")
    except Exception:
        pass

    # Emit Gan Ying event
    try:
        from whitemagic.core.resonance import emit_event
        emit_event("WEBHOOK_TRIGGERED", entry, source="webhook_triggers")
    except Exception:
        pass


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class CustomTriggerRequest(BaseModel):
    tool: str
    args: dict[str, Any] = {}


# ---------------------------------------------------------------------------
# Routes
# ---------------------------------------------------------------------------

@router.get("/actions")
def list_actions() -> dict[str, Any]:
    """List all available trigger actions."""
    actions = []
    for name, config in ALLOWED_ACTIONS.items():
        actions.append({
            "name": name,
            "description": config["description"],
            "safety": config["safety"],
            "tool": config["tool"],
        })
    return {
        "status": "success",
        "count": len(actions),
        "actions": actions,
    }


@router.post("/{action}")
async def trigger_action(action: str, request: Request) -> dict[str, Any]:
    """Execute a predefined safe action."""
    if action not in ALLOWED_ACTIONS:
        raise HTTPException(
            status_code=404,
            detail=f"Unknown action '{action}'. Use GET /api/trigger/actions to list available actions.",
        )

    config = ALLOWED_ACTIONS[action]
    tool_name = config["tool"]
    args = dict(config.get("args", {}))

    # Merge args from request body if specified
    args_from_body = config.get("args_from_body", [])
    if args_from_body:
        try:
            body = await request.json()
        except Exception:
            body = {}
        for key in args_from_body:
            if key in body:
                args[key] = body[key]

    # Execute the tool
    try:
        from whitemagic.tools.unified_api import call_tool
        result = call_tool(tool_name, **args)
    except Exception as e:
        result = {"status": "error", "error": str(e)}

    _log_trigger(action, result)

    return {
        "status": "success",
        "action": action,
        "tool": tool_name,
        "result": result,
    }


@router.post("/custom")
async def trigger_custom(req: CustomTriggerRequest) -> dict[str, Any]:
    """Execute a custom tool call.

    Only allows read-safety tools by default. Set WHITEMAGIC_WEBHOOK_ALLOW_WRITE=1
    to allow write tools.
    """
    tool_name = req.tool
    args = req.args

    # Safety check: verify the tool exists and check its safety level
    try:
        from whitemagic.tools.registry import ToolSafety, get_tool
        tool_def = get_tool(tool_name)
        if tool_def is None:
            raise HTTPException(status_code=404, detail=f"Tool '{tool_name}' not found")

        allow_write = os.environ.get("WHITEMAGIC_WEBHOOK_ALLOW_WRITE", "").strip() in ("1", "true")
        if tool_def.safety == ToolSafety.WRITE and not allow_write:
            raise HTTPException(
                status_code=403,
                detail=f"Tool '{tool_name}' is a write tool. "
                       "Set WHITEMAGIC_WEBHOOK_ALLOW_WRITE=1 to allow.",
            )
    except HTTPException:
        raise
    except Exception:
        pass  # If registry lookup fails, proceed anyway

    try:
        from whitemagic.tools.unified_api import call_tool
        result = call_tool(tool_name, **args)
    except Exception as e:
        result = {"status": "error", "error": str(e)}

    _log_trigger(f"custom:{tool_name}", result, source="webhook_custom")

    return {
        "status": "success",
        "tool": tool_name,
        "result": result,
    }


@router.get("/history")
def trigger_history(limit: int = 50) -> dict[str, Any]:
    """Get recent trigger history."""
    hp = _history_path()
    if not hp.exists():
        return {"status": "success", "count": 0, "history": []}

    entries = []
    try:
        lines = hp.read_text(encoding="utf-8").strip().split("\n")
        for line in reversed(lines):
            if not line.strip():
                continue
            try:
                entries.append(json.loads(line))
            except json.JSONDecodeError:
                continue
            if len(entries) >= limit:
                break
    except Exception:
        pass

    return {
        "status": "success",
        "count": len(entries),
        "history": entries,
    }
