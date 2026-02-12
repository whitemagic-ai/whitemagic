"""Agent Registry tool handlers — multi-agent identity and discovery.

Provides agent registration, heartbeat, capability declaration, and
discovery for coordinated multi-agent workflows.  All state is stored
under WM_STATE_ROOT/agents/.
"""
import json
import os
from datetime import datetime
from pathlib import Path
from typing import Any, cast
from uuid import uuid4


def _emit(event_type_name: str, data: dict) -> None:
    """Best-effort Gan Ying event emission."""
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type_name, data, source="agent_registry")
    except Exception:
        pass


def _agents_dir() -> Path:
    """Return the agents directory under WM_STATE_ROOT."""
    from whitemagic.config.paths import WM_ROOT
    d = cast("Path", WM_ROOT) / "agents"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _agent_path(agent_id: str) -> Path:
    return _agents_dir() / f"{agent_id}.json"


def _load_agent(agent_id: str) -> dict[str, Any] | None:
    p = _agent_path(agent_id)
    if not p.exists():
        return None
    try:
        return cast("dict[str, Any]", json.loads(p.read_text(encoding="utf-8")))
    except (json.JSONDecodeError, OSError):
        return None


def _save_agent(agent: dict[str, Any]) -> None:
    p = _agent_path(agent["id"])
    p.write_text(json.dumps(agent, indent=2), encoding="utf-8")


def _delete_agent(agent_id: str) -> bool:
    p = _agent_path(agent_id)
    if p.exists():
        p.unlink()
        return True
    return False


def _all_agents() -> list[dict[str, Any]]:
    agents = []
    adir = _agents_dir()
    for f in sorted(adir.glob("*.json")):
        try:
            agents.append(json.loads(f.read_text(encoding="utf-8")))
        except (json.JSONDecodeError, OSError):
            continue
    return agents


# Heartbeat staleness threshold (seconds)
_HEARTBEAT_STALE_S = 300  # 5 minutes


def _is_active(agent: dict[str, Any]) -> bool:
    """An agent is active if it heartbeated within the staleness window."""
    last_hb = agent.get("last_heartbeat")
    if not last_hb:
        # Fall back to registered_at
        last_hb = agent.get("registered_at", "")
    try:
        from datetime import datetime as dt
        ts = dt.fromisoformat(last_hb)
        return (dt.now() - ts).total_seconds() < _HEARTBEAT_STALE_S
    except (ValueError, TypeError):
        return False


# ---------------------------------------------------------------------------
# Handler: agent.register
# ---------------------------------------------------------------------------

def handle_agent_register(**kwargs: Any) -> dict[str, Any]:
    """Register a new agent or update an existing one."""
    name = kwargs.get("name")
    if not name:
        return {"status": "error", "error": "name is required"}

    agent_id = kwargs.get("agent_id") or f"agent-{uuid4().hex[:8]}"
    capabilities = kwargs.get("capabilities", [])
    metadata = kwargs.get("metadata", {})

    # Check for existing agent with same id
    existing = _load_agent(agent_id)
    now = datetime.now().isoformat()

    if existing:
        # Update existing registration
        existing["name"] = name
        existing["capabilities"] = capabilities
        existing["metadata"] = metadata
        existing["updated_at"] = now
        existing["last_heartbeat"] = now
        _save_agent(existing)
        return {
            "status": "success",
            "message": f"Agent {agent_id} updated",
            "agent": existing,
            "new": False,
        }

    agent = {
        "id": agent_id,
        "name": name,
        "capabilities": capabilities,
        "metadata": metadata,
        "host": os.uname().nodename,
        "registered_at": now,
        "last_heartbeat": now,
        "heartbeat_count": 0,
        "status": "active",
    }
    _save_agent(agent)

    _emit("AGENT_REGISTERED", {"agent_id": agent_id, "name": name, "capabilities": capabilities})

    return {
        "status": "success",
        "message": f"Agent {agent_id} registered",
        "agent": agent,
        "new": True,
    }


# ---------------------------------------------------------------------------
# Handler: agent.heartbeat
# ---------------------------------------------------------------------------

def handle_agent_heartbeat(**kwargs: Any) -> dict[str, Any]:
    """Update agent heartbeat and optional workload info."""
    agent_id = kwargs.get("agent_id")
    if not agent_id:
        return {"status": "error", "error": "agent_id is required"}

    agent = _load_agent(agent_id)
    if not agent:
        return {"status": "error", "error": f"Agent {agent_id} not found", "error_code": "not_found"}

    now = datetime.now().isoformat()
    agent["last_heartbeat"] = now
    agent["heartbeat_count"] = agent.get("heartbeat_count", 0) + 1
    agent["status"] = "active"

    # Optional workload update
    workload = kwargs.get("workload")
    if workload is not None:
        agent["workload"] = workload

    current_task = kwargs.get("current_task")
    if current_task is not None:
        agent["current_task"] = current_task

    _save_agent(agent)

    _emit("AGENT_HEARTBEAT", {"agent_id": agent_id, "heartbeat_count": agent["heartbeat_count"]})

    return {
        "status": "success",
        "message": f"Heartbeat recorded for {agent_id}",
        "heartbeat_count": agent["heartbeat_count"],
    }


# ---------------------------------------------------------------------------
# Handler: agent.list
# ---------------------------------------------------------------------------

def handle_agent_list(**kwargs: Any) -> dict[str, Any]:
    """List all registered agents with optional filters."""
    only_active = kwargs.get("only_active", False)
    capability_filter = kwargs.get("capability")

    agents = _all_agents()

    if only_active:
        agents = [a for a in agents if _is_active(a)]

    if capability_filter:
        agents = [a for a in agents if capability_filter in a.get("capabilities", [])]

    active_count = sum(1 for a in _all_agents() if _is_active(a))

    agent_summaries = [
        {
            "id": a["id"],
            "name": a.get("name"),
            "capabilities": a.get("capabilities", []),
            "host": a.get("host"),
            "active": _is_active(a),
            "last_heartbeat": a.get("last_heartbeat"),
            "workload": a.get("workload"),
            "current_task": a.get("current_task"),
        }
        for a in agents
    ]

    return {
        "status": "success",
        "count": len(agent_summaries),
        "active_count": active_count,
        "agents": agent_summaries,
    }


# ---------------------------------------------------------------------------
# Handler: agent.capabilities
# ---------------------------------------------------------------------------

def handle_agent_capabilities(**kwargs: Any) -> dict[str, Any]:
    """Query agent capabilities — what can a specific agent do?"""
    agent_id = kwargs.get("agent_id")
    if not agent_id:
        return {"status": "error", "error": "agent_id is required"}

    agent = _load_agent(agent_id)
    if not agent:
        return {"status": "error", "error": f"Agent {agent_id} not found", "error_code": "not_found"}

    return {
        "status": "success",
        "agent_id": agent_id,
        "name": agent.get("name"),
        "capabilities": agent.get("capabilities", []),
        "metadata": agent.get("metadata", {}),
        "active": _is_active(agent),
        "host": agent.get("host"),
    }


# ---------------------------------------------------------------------------
# Handler: agent.deregister
# ---------------------------------------------------------------------------

def handle_agent_deregister(**kwargs: Any) -> dict[str, Any]:
    """Remove an agent from the registry."""
    agent_id = kwargs.get("agent_id")
    if not agent_id:
        return {"status": "error", "error": "agent_id is required"}

    if _delete_agent(agent_id):
        _emit("AGENT_DEREGISTERED", {"agent_id": agent_id})
        return {
            "status": "success",
            "message": f"Agent {agent_id} deregistered",
        }
    return {"status": "error", "error": f"Agent {agent_id} not found", "error_code": "not_found"}
