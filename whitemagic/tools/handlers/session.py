"""Session tool handlers — including cross-device handoff."""
import json
import os
from datetime import datetime
from pathlib import Path
from typing import Any, cast
from uuid import uuid4


def _resolve_base_path(kwargs: dict[str, Any]) -> Path:
    """Import and delegate to unified_api's path resolver."""
    from whitemagic.tools.unified_api import _resolve_base_path as _rbp
    return cast("Path", _rbp(kwargs))


def _session_dir(base_path: Path) -> Path:
    session_dir = base_path / "sessions"
    session_dir.mkdir(parents=True, exist_ok=True)
    return session_dir


def _session_path(base_path: Path, session_id: str) -> Path:
    return _session_dir(base_path) / f"{session_id}.json"


def _load_session(base_path: Path, session_id: str) -> dict[str, Any]:
    path = _session_path(base_path, session_id)
    if not path.exists():
        raise FileNotFoundError(f"Session not found: {session_id}")
    return cast("dict[str, Any]", json.loads(path.read_text(encoding="utf-8")))


def _save_session(base_path: Path, session: dict[str, Any]) -> None:
    path = _session_path(base_path, session["id"])
    path.write_text(json.dumps(session, indent=2), encoding="utf-8")


def _emit(event_type: str, data: dict[str, Any]) -> None:
    from whitemagic.tools.unified_api import _emit_gan_ying
    _emit_gan_ying(event_type, data)


def handle_session_bootstrap(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.orchestration.session_startup import start_session
    result = start_session(verbose=False)
    _emit("SYSTEM_STARTED", {"mode": "bootstrap"})

    # Auto-context injection: load quickstart guides + recent memories
    context = {}
    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        # Load quickstart guides
        guides = um.search("quickstart guide", limit=5)
        if guides:
            context["quickstart_guides"] = [
                {"title": getattr(m, "title", ""), "id": getattr(m, "id", "")}
                for m in guides
                if "quickstart" in str(getattr(m, "tags", []))
            ]
        # Load recent memories for continuity
        recent = um.search("session handoff OR recent work", limit=3)
        if recent:
            context["recent_memories"] = [
                {"title": getattr(m, "title", ""), "id": getattr(m, "id", "")}
                for m in recent[:3]
            ]
        # Galaxy status
        try:
            from whitemagic.core.memory.galaxy_manager import get_galaxy_manager
            gm = get_galaxy_manager()
            context["active_galaxy"] = gm.get_active().name  # type: ignore[assignment]
            context["galaxy_count"] = len(gm._galaxies)  # type: ignore[assignment]
        except Exception:
            pass
    except Exception:
        pass

    return {"status": "success", "result": result, "context": context}



def handle_session_status(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.unified import get_unified_memory
    stats = get_unified_memory().get_stats()
    return {"status": "success", "memory_stats": stats}


def handle_session_handoff_summary(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.unified import get_unified_memory
    from whitemagic.utils.time import now_iso
    handoff = {
        "timestamp": now_iso(),
        "memory_count": get_unified_memory().get_stats().get("total", 0),
        "message": "Session handoff created",
    }
    return {"status": "success", "handoff": handoff}


def handle_create_session(**kwargs: Any) -> dict[str, Any]:
    base_path = _resolve_base_path(kwargs)
    name = kwargs.get("name")
    if not name:
        raise ValueError("name is required")
    session_id = f"session-{uuid4().hex[:8]}"
    now = datetime.now().isoformat()
    session = {
        "id": session_id,
        "name": name,
        "goals": kwargs.get("goals", []),
        "tags": kwargs.get("tags", []),
        "context_tier": kwargs.get("context_tier", 1),
        "auto_checkpoint": kwargs.get("auto_checkpoint", True),
        "status": "active",
        "created_at": now,
        "updated_at": now,
        "checkpoints": [],
    }
    _save_session(base_path, session)
    _emit("SYSTEM_STARTED", {"session_id": session_id, "name": name, "status": "active"})
    return {"status": "success", "session": session}


def handle_checkpoint_session(**kwargs: Any) -> dict[str, Any]:
    base_path = _resolve_base_path(kwargs)
    session_id = kwargs.get("session_id")
    if not session_id:
        raise ValueError("session_id is required")
    session = _load_session(base_path, session_id)
    checkpoint = {
        "id": f"checkpoint-{uuid4().hex[:8]}",
        "name": kwargs.get("checkpoint_name", "checkpoint"),
        "created_at": datetime.now().isoformat(),
    }
    session.setdefault("checkpoints", []).append(checkpoint)
    session["last_checkpoint"] = checkpoint["created_at"]
    session["updated_at"] = checkpoint["created_at"]
    session["status"] = "checkpointed"
    _save_session(base_path, session)
    return {"status": "success", "session": session, "checkpoint": checkpoint}


def handle_resume_session(**kwargs: Any) -> dict[str, Any]:
    base_path = _resolve_base_path(kwargs)
    session_id = kwargs.get("session_id")
    if not session_id:
        raise ValueError("session_id is required")
    session = _load_session(base_path, session_id)
    session["status"] = "resumed"
    session["last_resumed"] = datetime.now().isoformat()
    session["updated_at"] = session["last_resumed"]
    _save_session(base_path, session)
    try:
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager(base_dir=str(base_path))
        context_tier = kwargs.get("load_tier", session.get("context_tier", 1))
        context = manager.generate_context_summary(context_tier)
    except Exception as exc:
        context = ""
        session["resume_warning"] = str(exc)
    _emit(
        "CONTEXT_OPTIMIZED",
        {
            "session_id": session_id,
            "status": session.get("status"),
            "context_tier": kwargs.get("load_tier", session.get("context_tier", 1)),
        },
    )
    return {"status": "success", "session": session, "context": context}


# ---------------------------------------------------------------------------
# Cross-Device Session Handoff (inspired by Tools(copy)/core/session.py)
# ---------------------------------------------------------------------------

def _handoff_dir(base_path: Path) -> Path:
    d = base_path / "sessions" / "handoffs"
    d.mkdir(parents=True, exist_ok=True)
    return d


def handle_session_handoff(**kwargs: Any) -> dict[str, Any]:
    """Unified session handoff handler — routes by action parameter."""
    action = kwargs.get("action", "list")
    dispatch = {
        "summary": handle_session_handoff_summary,
        "transfer": handle_session_handoff_transfer,
        "accept": handle_session_accept_handoff,
        "list": handle_session_list_handoffs,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_session_handoff_transfer(**kwargs: Any) -> dict[str, Any]:
    """Transfer a session to another device with full context packaging.

    Creates a handoff package containing session state, recent memories,
    modified files, and context summary. Optionally notifies the target
    device via Redis.
    """
    base_path = _resolve_base_path(kwargs)
    session_id = kwargs.get("session_id")
    if not session_id:
        return {"status": "error", "error": "session_id is required"}

    target_device = kwargs.get("target_device", "any")
    message = kwargs.get("message", "")

    try:
        session = _load_session(base_path, session_id)
    except FileNotFoundError:
        return {"status": "error", "error": f"Session {session_id} not found"}

    now = datetime.now().isoformat()
    handoff_id = f"handoff-{uuid4().hex[:8]}"

    # Build handoff package
    handoff = {
        "id": handoff_id,
        "session_id": session_id,
        "session": session,
        "source_device": os.uname().nodename,
        "target_device": target_device,
        "message": message,
        "created_at": now,
        "status": "pending",
        "context_summary": "",
        "modified_files": session.get("modified_files", []),
    }

    # Generate context summary
    try:
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager(base_dir=str(base_path))
        handoff["context_summary"] = manager.generate_context_summary(
            session.get("context_tier", 1),
        )
    except Exception:
        handoff["context_summary"] = f"Session '{session.get('name', session_id)}' with {len(session.get('checkpoints', []))} checkpoints"

    # Update session status
    session["status"] = "handed_off"
    session["handed_off_to"] = target_device
    session["handed_off_at"] = now
    session["updated_at"] = now
    _save_session(base_path, session)

    # Save handoff package
    hdir = _handoff_dir(base_path)
    (hdir / f"{handoff_id}.json").write_text(
        json.dumps(handoff, indent=2), encoding="utf-8",
    )

    # Notify via Redis if available
    _notify_handoff(handoff)

    _emit("SESSION_HANDOFF", {
        "handoff_id": handoff_id,
        "session_id": session_id,
        "source": handoff["source_device"],
        "target": target_device,
    })

    return {
        "status": "success",
        "message": f"Session {session_id} handed off to {target_device}",
        "handoff_id": handoff_id,
        "handoff": handoff,
    }


def handle_session_accept_handoff(**kwargs: Any) -> dict[str, Any]:
    """Accept a session handoff from another device.

    Loads the handoff package, creates a local session copy, and marks
    the handoff as accepted.
    """
    base_path = _resolve_base_path(kwargs)
    handoff_id = kwargs.get("handoff_id")
    if not handoff_id:
        return {"status": "error", "error": "handoff_id is required"}

    hdir = _handoff_dir(base_path)
    hpath = hdir / f"{handoff_id}.json"
    if not hpath.exists():
        return {"status": "error", "error": f"Handoff {handoff_id} not found"}

    try:
        handoff = json.loads(hpath.read_text(encoding="utf-8"))
    except (json.JSONDecodeError, OSError) as e:
        return {"status": "error", "error": str(e)}

    now = datetime.now().isoformat()
    session = handoff.get("session", {})
    session["status"] = "resumed"
    session["resumed_from_handoff"] = handoff_id
    session["resumed_on"] = os.uname().nodename
    session["updated_at"] = now
    _save_session(base_path, session)

    handoff["status"] = "accepted"
    handoff["accepted_at"] = now
    handoff["accepted_by"] = os.uname().nodename
    hpath.write_text(json.dumps(handoff, indent=2), encoding="utf-8")

    _emit("SESSION_HANDOFF_ACCEPTED", {
        "handoff_id": handoff_id,
        "session_id": session.get("id"),
        "accepted_by": os.uname().nodename,
    })

    return {
        "status": "success",
        "message": f"Handoff {handoff_id} accepted",
        "session": session,
        "context_summary": handoff.get("context_summary", ""),
    }


def handle_session_list_handoffs(**kwargs: Any) -> dict[str, Any]:
    """List pending and recent session handoffs."""
    base_path = _resolve_base_path(kwargs)
    hdir = _handoff_dir(base_path)
    limit = kwargs.get("limit", 20)
    status_filter = kwargs.get("filter_status")

    handoffs: list[dict[str, Any]] = []
    for f in sorted(hdir.glob("handoff-*.json"), reverse=True):
        try:
            h = json.loads(f.read_text(encoding="utf-8"))
            if status_filter and h.get("status") != status_filter:
                continue
            handoffs.append({
                "id": h["id"],
                "session_id": h.get("session_id"),
                "source_device": h.get("source_device"),
                "target_device": h.get("target_device"),
                "status": h.get("status"),
                "message": h.get("message", ""),
                "created_at": h.get("created_at"),
            })
        except (json.JSONDecodeError, OSError, KeyError):
            continue
        if len(handoffs) >= limit:
            break

    return {
        "status": "success",
        "count": len(handoffs),
        "handoffs": handoffs,
    }


def _notify_handoff(handoff: dict[str, Any]) -> None:
    """Best-effort Redis notification for cross-device handoff."""
    try:
        import redis
        url = os.environ.get("REDIS_URL", "redis://localhost:6379")
        r = redis.Redis.from_url(url, decode_responses=True, socket_timeout=2)
        r.publish("ganying", json.dumps({
            "event_type": "SESSION_HANDOFF",
            "source": "session_handler",
            "data": {
                "handoff_id": handoff["id"],
                "session_id": handoff["session_id"],
                "source_device": handoff["source_device"],
                "target_device": handoff["target_device"],
                "message": handoff.get("message", ""),
            },
            "timestamp": datetime.now().isoformat(),
        }))
    except Exception:
        pass
