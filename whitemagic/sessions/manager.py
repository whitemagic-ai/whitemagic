"""Session Manager
Manages work sessions, state persistence, and context tracking.
"""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path
from typing import Any
from uuid import uuid4

from whitemagic.config.paths import SESSIONS_DIR


class SessionStatus(Enum):
    """Session lifecycle status."""

    ACTIVE = "active"
    PAUSED = "paused"
    COMPLETED = "completed"
    CANCELLED = "cancelled"

@dataclass
class Session:
    """Represents a work session."""

    id: str = field(default_factory=lambda: str(uuid4()))
    name: str = ""
    status: SessionStatus = SessionStatus.ACTIVE

    # Goals and context
    goals: list[str] = field(default_factory=list)
    tags: list[str] = field(default_factory=list)

    # Timestamps
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    updated_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    completed_at: str | None = None

    # Data
    context: dict[str, Any] = field(default_factory=dict)
    metrics: dict[str, Any] = field(default_factory=dict)

    # Enhanced Context (v4.3.0)
    accumulated_context: list[str] = field(default_factory=list)  # Summary of key insights/events
    resonance_scores: dict[str, float] = field(default_factory=dict)  # Resonance by category

    def to_dict(self) -> dict[str, Any]:
        return {**asdict(self), "status": self.status.value}

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Session":
        data = data.copy()
        if "status" in data:
            data["status"] = SessionStatus(data["status"])

        # Ensure all fields for cls(**data) exist (backward compatibility for v4.3.0)
        import inspect
        sig = inspect.signature(cls)
        for param in sig.parameters.values():
            if param.name not in data and param.default is inspect.Parameter.empty:
                # This shouldn't happen with field(default_factory=...) but good to be safe
                pass
            elif param.name not in data:
                # Data classes with field(default_factory=...) still need the key if passed to **kwargs
                # or they will use the default. cls(**data) will work fine if key is missing
                # only IF the field has a default.
                pass

        # Specifically handle the new fields added in v4.3.0 to avoid any issues
        if "accumulated_context" not in data:
            data["accumulated_context"] = []
        if "resonance_scores" not in data:
            data["resonance_scores"] = {}

        return cls(**data)

class SessionManager:
    """Manages session lifecycle and persistence."""

    def __init__(self, base_dir: Path | None = None) -> None:
        # Use canonical SESSIONS_DIR if not specified
        if base_dir is None:
            base_dir = SESSIONS_DIR
        self.base_dir = Path(base_dir)
        self.base_dir.mkdir(parents=True, exist_ok=True)
        self._active_sessions: dict[str, Session] = {}

    def _session_path(self, session_id: str) -> Path:
        return self.base_dir / f"{session_id}.json"

    def create_session(self, name: str, goals: list[str] | None = None, tags: list[str] | None = None) -> Session:
        """Create a new session."""
        session = Session(
            name=name,
            goals=goals or [],
            tags=tags or [],
        )
        self._save_session(session)
        self._active_sessions[session.id] = session
        return session

    def get_session(self, session_id: str) -> Session | None:
        """Get a session by ID."""
        if session_id in self._active_sessions:
            return self._active_sessions[session_id]

        path = self._session_path(session_id)
        if not path.exists():
            return None

        try:
            data = json.loads(path.read_text())
            session = Session.from_dict(data)
            self._active_sessions[session_id] = session
            return session
        except Exception:
            return None

    def list_sessions(self, status: SessionStatus | None = None, limit: int = 10) -> list[Session]:
        """List sessions."""
        sessions = []
        for path in self.base_dir.glob("*.json"):
            try:
                data = json.loads(path.read_text())
                session = Session.from_dict(data)
                if status and session.status != status:
                    continue
                sessions.append(session)
            except Exception:
                continue

        # Sort by updated_at desc
        sessions.sort(key=lambda s: s.updated_at, reverse=True)
        return sessions[:limit]

    def update_session(self, session_id: str, **updates: Any) -> Session | None:
        """Update a session."""
        session = self.get_session(session_id)
        if not session:
            return None

        for key, value in updates.items():
            if hasattr(session, key):
                setattr(session, key, value)

        session.updated_at = datetime.now(timezone.utc).isoformat()
        self._save_session(session)
        return session

    def end_session(self, session_id: str) -> Session | None:
        """End a session."""
        return self.update_session(
            session_id,
            status=SessionStatus.COMPLETED,
            completed_at=datetime.now(timezone.utc).isoformat(),
        )

    def _save_session(self, session: Session) -> None:
        """Save session to disk."""
        path = self._session_path(session.id)
        path.write_text(json.dumps(session.to_dict(), indent=2))

    def get_active_session(self) -> Session | None:
        """Get the most recently updated active session."""
        sessions = self.list_sessions(status=SessionStatus.ACTIVE, limit=1)
        return sessions[0] if sessions else None
