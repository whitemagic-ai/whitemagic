
import os
from typing import Any

from whitemagic.core.bridge.utils import _emit_resonance_event


def manage_agent_collaboration(
    operation: str = "list_agents",
    name: str | None = None,
    model: str | None = None,
    specialty: str | None = None,
    description: str | None = None,
    channels: list[str] | None = None,
    content: str | None = None,
    channel: str = "sangha",
    task: str | None = None,
    preferred_specialty: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Manage multi-agent collaboration."""
    import asyncio
    import json

    from whitemagic.config.paths import WM_ROOT

    base_dir = WM_ROOT / "agent_collaboration"
    base_dir.mkdir(parents=True, exist_ok=True)
    registry_path = base_dir / "agents.json"

    def load_registry() -> list[dict[str, Any]]:
        if not registry_path.exists():
            return []
        try:
            raw = json.loads(registry_path.read_text())
            if not isinstance(raw, list):
                return []
            return [item for item in raw if isinstance(item, dict)]
        except json.JSONDecodeError:
            return []

    def save_registry(agents: list[dict[str, Any]]) -> None:
        registry_path.write_text(json.dumps(agents, indent=2))

    op = (operation or "list_agents").lower()
    if op == "list":
        op = "list_agents"
    if op == "register":
        op = "register_agent"
    if op == "delegate":
        op = "delegate_task"

    if op == "register_agent":
        if not name or not model or not specialty:
            return {"error": "name, model, and specialty are required"}
        agents = load_registry()
        agents = [a for a in agents if a.get("name") != name]
        agents.append({
            "name": name,
            "model": model,
            "specialty": specialty,
            "description": description or "",
            "channels": channels or [],
        })
        save_registry(agents)
        _emit_resonance_event("COLLABORATION_STARTED", {"agent": name, "specialty": specialty})
        return {"status": "registered", "agent": {"name": name, "model": model, "specialty": specialty, "channels": len(channels or [])}}

    if op == "list_agents":
        agents = load_registry()
        return {"agents": agents, "count": len(agents)}

    if op == "broadcast":
        if not content:
            return {"error": "content is required for broadcast"}
        channel_dir = base_dir / "channels"
        channel_dir.mkdir(parents=True, exist_ok=True)
        channel_path = channel_dir / f"{channel}.jsonl"
        message = {
            "sender": name or "system",
            "content": content,
            "channel": channel,
        }
        with channel_path.open("a") as handle:
            handle.write(json.dumps(message) + "\n")
        _emit_resonance_event("COLLABORATION_BROADCAST", {"channel": channel, "sender": message["sender"]})
        return {"status": "broadcasted", "channel": channel, "message": message}

    if op == "delegate_task":
        if not task:
            return {"error": "task is required for delegate_task"}

        # Local-model delegation is archived/disabled by default.
        if os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() not in {"1", "true", "yes", "on"}:
            return {
                "error": "delegate_task requires archived local-model support (disabled by default).",
                "archived": True,
                "hint": "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model delegation.",
            }

        agents = load_registry()
        selected: dict[str, Any] | None = None
        if preferred_specialty:
            selected = next((a for a in agents if a.get("specialty") == preferred_specialty), None)
        if not selected and agents:
            selected = agents[0]
        if not selected:
            return {"error": "No agents registered"}

        from whitemagic.cascade.multi_agent import LocalModelAgent

        agent: Any = LocalModelAgent(
            name=selected["name"],
            model=selected["model"],
            specialty=selected["specialty"],
            description=selected.get("description", ""),
        )
        result_text = asyncio.run(agent.generate_response(task))
        result = {
            "agent": str(getattr(agent, "name", selected["name"])),
            "result": str(result_text),
            "method": "direct",
        }
        _emit_resonance_event("COLLABORATION_DELEGATED", {"task": task, "agent": result.get("agent")})
        return result

    return {"error": f"Unknown collaboration operation: {operation}"}
