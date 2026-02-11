"""Courage Garden — Session Initialization & Sharp Beginnings.

Mansion: #1 Horn (角 Jiao)
Quadrant: Eastern (Azure Dragon)
PRAT Gana: gana_horn — 4 tools (session_bootstrap, create_session,
    resume_session, checkpoint_session)

The Horn Gana initiates. The Courage Garden provides the substrate for
session lifecycle: tracking active sessions, maintaining session goals,
and giving agents the courage to begin new work.

Holographic Integration:
- Action-oriented emotion (X-axis +0.2) — courage to begin
- Universal virtue (Y-axis 0.0) — every agent needs to start
- Future-oriented (Z-axis +0.4) — stepping into the unknown
- Important for growth (W-axis +0.3) — beginnings matter
"""

from __future__ import annotations

import logging
import threading
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin, init_listeners
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

logger = logging.getLogger(__name__)


class CourageGarden(BaseGarden, GanYingMixin):
    """Garden of Courage — Session lifecycle engine for the Horn Gana.

    Serves the Horn Gana's session tools by maintaining:
    - Active session registry with goals and checkpoints
    - Session history for resume operations
    - Initialization context for new sessions
    - Brave acts log (original emotional substrate)
    """

    name = "courage"
    category = "initialization"
    resonance_partners = ["truth", "practice", "adventure"]
    mansion_number = 1
    gana_name = "gana_horn"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.sessions: dict[str, dict[str, Any]] = {}
        self.session_history: list[dict[str, Any]] = []
        self.brave_acts: list[dict[str, Any]] = []
        self.courage_level: float = 0.6
        self._total_sessions: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Courage", "mansion": 1})

    def get_name(self) -> str:
        return "courage"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.2, y=0.0, z=0.4, w=0.3)

    # ------------------------------------------------------------------
    # Session lifecycle — serving Horn Gana tools
    # ------------------------------------------------------------------

    def create_session(self, session_id: str, name: str = "Untitled",
                       goals: list[str] | None = None,
                       agent_id: str = "default") -> dict[str, Any]:
        """Create a new session — called by session_bootstrap / create_session tools."""
        now = datetime.now().isoformat()
        session = {
            "id": session_id,
            "name": name,
            "goals": goals or [],
            "agent_id": agent_id,
            "created_at": now,
            "updated_at": now,
            "checkpoints": [],
            "status": "active",
        }
        with self._lock:
            self.sessions[session_id] = session
            self._total_sessions += 1
        self.courage_level = min(1.0, self.courage_level + 0.05)
        self.emit(EventType.COURAGE_SHOWN, {"action": "session_created", "session": name})
        return session

    def resume_session(self, session_id: str) -> dict[str, Any] | None:
        """Resume a previously checkpointed session."""
        with self._lock:
            session = self.sessions.get(session_id)
            if session:
                session["status"] = "active"
                session["updated_at"] = datetime.now().isoformat()
                return dict(session)
        return None

    def checkpoint_session(self, session_id: str, label: str = "",
                           state: dict[str, Any] | None = None) -> dict[str, Any]:
        """Save a checkpoint for the current session."""
        now = datetime.now().isoformat()
        checkpoint = {"label": label, "timestamp": now, "state": state or {}}
        with self._lock:
            session = self.sessions.get(session_id)
            if session:
                session["checkpoints"].append(checkpoint)
                session["updated_at"] = now
                return {"session_id": session_id, "checkpoint": checkpoint, "total": len(session["checkpoints"])}
        return {"error": f"Session {session_id} not found"}

    def end_session(self, session_id: str) -> dict[str, Any]:
        """End a session and move it to history."""
        with self._lock:
            session = self.sessions.pop(session_id, None)
            if session:
                session["status"] = "ended"
                session["ended_at"] = datetime.now().isoformat()
                self.session_history.append(session)
                return session
        return {"error": f"Session {session_id} not found"}

    def get_active_sessions(self) -> list[dict[str, Any]]:
        """Get all active sessions."""
        with self._lock:
            return [dict(s) for s in self.sessions.values() if s["status"] == "active"]

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def take_brave_action(self, what: str, fear: str) -> dict[str, Any]:
        act = {"what": what, "fear": fear, "timestamp": datetime.now().isoformat()}
        self.brave_acts.append(act)
        self.courage_level = min(1.0, self.courage_level + 0.1)
        self.emit(EventType.COURAGE_SHOWN, act)
        return act

    def encourage(self, task: str) -> str:
        return f"You have the courage to {task}. Trust yourself."

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        with self._lock:
            active = sum(1 for s in self.sessions.values() if s["status"] == "active")
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "active_sessions": active,
            "total_sessions": self._total_sessions,
            "courage_level": round(self.courage_level, 3),
            "brave_acts": len(self.brave_acts),
        })
        return base


_instance = None
def get_courage_garden() -> CourageGarden:
    global _instance
    if _instance is None:
        _instance = CourageGarden()
    return _instance
