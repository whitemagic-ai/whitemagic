
from typing import Any


def session_init(
    name: str | None = None,
    session_name: str | None = None,
    goals: list | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Initialize a new work session."""
    from whitemagic.sessions.manager import SessionManager

    # Accept both 'name' and 'session_name' parameters
    actual_name = session_name or name or "default_session"

    manager = SessionManager()
    session = manager.create_session(name=actual_name, goals=goals)

    return {
        "session_id": session.id,
        "name": session.name,
        "status": session.status.value,
        "goals": session.goals,
        "created_at": session.created_at,
        "message": f"Session '{actual_name}' initialized",
    }

def session_get_context(
    operation: str | None = None,
    include_active_gardens: bool = True,
    include_recent_memories: bool = True,
    **kwargs: Any,
) -> dict[str, Any]:
    """Get context of current/active session."""
    from whitemagic.sessions.manager import SessionManager

    # Ignore 'operation' parameter - it's for routing only
    manager = SessionManager()
    session = manager.get_active_session()

    if not session:
        return {"status": "no_active_session", "message": "No active session found"}

    return {
        "session_id": session.id,
        "name": session.name,
        "status": session.status.value,
        "goals": session.goals,
        "context": session.context,
    }

def session_checkpoint(session_name: str | None = None, include_state: bool = True, **kwargs: Any) -> dict[str, Any]:
    """Checkpoint the current session."""
    from whitemagic.sessions.manager import SessionManager

    manager = SessionManager()
    session = manager.get_active_session()

    if not session:
        return {"status": "error", "message": "No active session to checkpoint"}

    # Just update the timestamp to 'checkpoint' it for now
    manager.update_session(session.id)

    return {
        "session_id": session.id,
        "name": session.name,
        "checkpoint_time": session.updated_at,
        "status": "checkpointed",
    }

def session_list(include_archived: bool = False, **kwargs: Any) -> dict[str, Any]:
    """List recent sessions."""
    from whitemagic.sessions.manager import SessionManager

    manager = SessionManager()
    sessions = manager.list_sessions(limit=5)

    return {
        "sessions": [s.to_dict() for s in sessions],
        "count": len(sessions),
    }

def session_create_handoff(
    target_role: str,
    context: str,
    priority: str = "normal",
    **kwargs: Any,
) -> dict[str, Any]:
    """Create a handoff package for another agent/role."""
    from whitemagic.sessions.manager import SessionManager

    manager = SessionManager()
    session = manager.get_active_session()

    # In a real impl, this would serialize full state
    handoff_data = {
        "source_session": session.id if session else "unknown",
        "target_role": target_role,
        "context": context,
        "priority": priority,
        "timestamp": get_system_time(),
    }

    return {
        "status": "created",
        "handoff_id": f"handoff_{get_system_time()}",
        "data": handoff_data,
    }

def session_handoff(**kwargs: Any) -> dict[str, Any]:
    """Alias/Wrapper for create_handoff."""
    return session_create_handoff(**kwargs)

# Helper for time (avoid circular import if possible, but bridge utils is safe)
def get_system_time() -> str:
    from datetime import datetime
    return datetime.now().isoformat()
