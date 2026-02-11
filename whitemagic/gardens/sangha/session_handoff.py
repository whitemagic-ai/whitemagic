"""Session Handoff - Automatic session continuity.

v4.3.0 Enhancement: Continuous Execution Mode
- Supports persistent iteration loops (inspired by Ralph technique)
- Completion detection via success criteria
- Rate limiting integration
- Auto-resume from previous session state
- Circuit breaker integration for stuck detection
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.utils.core import parse_datetime
from whitemagic.utils.fileio import atomic_write, file_lock

from .workspace import get_sangha_base_dir

logger = logging.getLogger(__name__)


@dataclass
class SessionState:
    """State of an AI session."""

    session_id: str
    started_at: datetime
    ended_at: datetime | None
    agent_name: str
    active_tasks: list[str]
    completed_tasks: list[str]
    context_summary: str
    next_steps: list[str]
    token_usage: dict[str, int]
    files_modified: list[str]
    decisions_made: list[dict[str, Any]]
    # v4.3.0: Continuous execution fields
    iteration_count: int = 0
    max_iterations: int = 50
    completion_criteria: str = ""
    is_continuous: bool = False
    last_progress_at: datetime | None = None


class SessionHandoff:
    """Manages automatic session state persistence and handoff.

    Philosophy: Sessions are continuous, not discrete.
    Like a relay race, each AI passes the baton seamlessly.
    """

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir if base_dir is not None else get_sangha_base_dir()
        self.sessions_dir = self.base_dir / "memory" / "collective" / "sessions"
        self.sessions_dir.mkdir(parents=True, exist_ok=True)
        self.current_session_file = self.sessions_dir / "current_session.json"

    def start_session(
        self,
        session_id: str,
        agent_name: str = "Cascade",
    ) -> SessionState:
        """Start new session or resume previous.

        Args:
            session_id: Session identifier
            agent_name: AI agent name

        Returns:
            SessionState (new or resumed)

        """
        # Check for previous session
        if self.current_session_file.exists():
            prev_state = self._load_session_state(self.current_session_file)
            if prev_state and not prev_state.ended_at:
                logger.info(f"🔄 Resuming from previous session: {prev_state.session_id}")
                logger.info(f"   Active tasks: {len(prev_state.active_tasks)}")
                logger.info(f"   Next steps: {', '.join(prev_state.next_steps[:3])}")
                return prev_state

        # Create new session
        state = SessionState(
            session_id=session_id,
            started_at=datetime.now(),
            ended_at=None,
            agent_name=agent_name,
            active_tasks=[],
            completed_tasks=[],
            context_summary="",
            next_steps=[],
            token_usage={"used": 0, "total": 200000},
            files_modified=[],
            decisions_made=[],
        )

        self._save_session_state(state)
        logger.info(f"🆕 New session started: {session_id}")
        return state

    def update_session(
        self,
        session_id: str,
        **updates: Any,
    ) -> None:
        """Update current session state.

        Args:
            session_id: Session to update
            **updates: Fields to update

        """
        state = self._load_current_session()
        if not state or state.session_id != session_id:
            return

        # Update fields
        for key, value in updates.items():
            if hasattr(state, key):
                setattr(state, key, value)

        self._save_session_state(state)

    def complete_task(self, session_id: str, task: str) -> None:
        """Mark task as complete.

        Args:
            session_id: Current session
            task: Task to complete

        """
        state = self._load_current_session()
        if not state:
            return

        if task in state.active_tasks:
            state.active_tasks.remove(task)
        if task not in state.completed_tasks:
            state.completed_tasks.append(task)

        self._save_session_state(state)
        logger.info(f"✅ Task completed: {task}")

    def add_next_step(self, session_id: str, step: str) -> None:
        """Add step for next session.

        Args:
            session_id: Current session
            step: Step to add

        """
        state = self._load_current_session()
        if not state:
            return

        if step not in state.next_steps:
            state.next_steps.append(step)

        self._save_session_state(state)

    def end_session(
        self,
        session_id: str,
        summary: str,
        next_steps: list[str],
    ) -> None:
        """End session and prepare handoff.

        Args:
            session_id: Session to end
            summary: Summary of work done
            next_steps: Steps for next session

        """
        state = self._load_current_session()
        if not state:
            return

        state.ended_at = datetime.now()
        state.context_summary = summary
        state.next_steps = next_steps

        # Archive session
        archive_file = self.sessions_dir / f"{session_id}.json"
        self._save_session_state(state, archive_file)

        # Create handoff summary
        handoff_file = self.sessions_dir / "HANDOFF.md"
        self._create_handoff_doc(state, handoff_file)

        logger.info(f"🏁 Session ended: {session_id}")
        logger.info(f"   Duration: {(state.ended_at - state.started_at).total_seconds() / 60:.1f} minutes")
        logger.info(f"   Tasks completed: {len(state.completed_tasks)}")
        logger.info(f"   Next steps: {len(state.next_steps)}")

    def _load_current_session(self) -> SessionState | None:
        """Load current session state."""
        if not self.current_session_file.exists():
            return None
        return self._load_session_state(self.current_session_file)

    def _load_session_state(self, filepath: Path) -> SessionState | None:
        """Load session state from file."""
        with file_lock(filepath):
            data = json.loads(filepath.read_text()) or {}
        return SessionState(
            session_id=data["session_id"],
            started_at=parse_datetime(data["started_at"]),
            ended_at=parse_datetime(data["ended_at"]) if data["ended_at"] else None,
            agent_name=data["agent_name"],
            active_tasks=data["active_tasks"],
            completed_tasks=data["completed_tasks"],
            context_summary=data["context_summary"],
            next_steps=data["next_steps"],
            token_usage=data["token_usage"],
            files_modified=data["files_modified"],
            decisions_made=data["decisions_made"],
            # v4.3.0 fields
            iteration_count=data.get("iteration_count", 0),
            max_iterations=data.get("max_iterations", 50),
            completion_criteria=data.get("completion_criteria", ""),
            is_continuous=data.get("is_continuous", False),
            last_progress_at=parse_datetime(data["last_progress_at"]) if data.get("last_progress_at") else None,
        )

    def _save_session_state(self, state: SessionState, filepath: Path | None = None) -> None:
        """Save session state to file."""
        if filepath is None:
            filepath = self.current_session_file

        data = {
            "session_id": state.session_id,
            "started_at": state.started_at.isoformat(),
            "ended_at": state.ended_at.isoformat() if state.ended_at else None,
            "agent_name": state.agent_name,
            "active_tasks": state.active_tasks,
            "completed_tasks": state.completed_tasks,
            "context_summary": state.context_summary,
            "next_steps": state.next_steps,
            "token_usage": state.token_usage,
            "files_modified": state.files_modified,
            "decisions_made": state.decisions_made,
            # v4.3.0 fields
            "iteration_count": state.iteration_count,
            "max_iterations": state.max_iterations,
            "completion_criteria": state.completion_criteria,
            "is_continuous": state.is_continuous,
            "last_progress_at": state.last_progress_at.isoformat() if state.last_progress_at else None,
        }
        with file_lock(filepath):
            atomic_write(filepath, json.dumps(data, indent=2))

    def _create_handoff_doc(self, state: SessionState, filepath: Path) -> None:
        """Create handoff document for next session."""
        ended_at = state.ended_at or datetime.now()
        duration = (ended_at - state.started_at).total_seconds() / 60

        content = f"""# Session Handoff - {state.session_id}

**Agent**: {state.agent_name}
**Duration**: {duration:.1f} minutes
**Ended**: {ended_at.strftime('%Y-%m-%d %H:%M:%S')}

---

## ✅ Completed ({len(state.completed_tasks)})

{chr(10).join(f'- {task}' for task in state.completed_tasks)}

---

## ⏳ Active ({len(state.active_tasks)})

{chr(10).join(f'- {task}' for task in state.active_tasks)}

---

## 🎯 Next Steps

{chr(10).join(f'{i+1}. {step}' for i, step in enumerate(state.next_steps))}

---

## 📊 Session Stats

- **Token Usage**: {state.token_usage['used']}/{state.token_usage['total']} ({state.token_usage['used']/state.token_usage['total']*100:.1f}%)
- **Files Modified**: {len(state.files_modified)}
- **Decisions Made**: {len(state.decisions_made)}

---

## 📝 Summary

{state.context_summary}

---

**Ready for next session!** 🙏
"""
        with file_lock(filepath):
            atomic_write(filepath, content)

    # ===== CONTINUOUS EXECUTION (v4.3.0) =====

    def start_continuous(
        self,
        session_id: str,
        completion_criteria: str,
        max_iterations: int = 50,
        agent_name: str = "Cascade",
    ) -> SessionState:
        """Start a continuous execution session.

        The session will iterate until:
        - Completion criteria is met
        - Max iterations reached
        - Circuit breaker opens (stuck detection)

        Args:
            session_id: Unique session identifier
            completion_criteria: Text that signals completion
            max_iterations: Safety limit on iterations
            agent_name: Name of AI agent

        Returns:
            SessionState ready for continuous execution

        """
        state = self.start_session(session_id, agent_name)
        state.is_continuous = True
        state.completion_criteria = completion_criteria
        state.max_iterations = max_iterations
        state.iteration_count = 0
        state.last_progress_at = datetime.now()

        self._save_session_state(state)

        logger.info(f"🔄 Continuous session started: {session_id}")
        logger.info(f"   Completion: '{completion_criteria}'")
        logger.info(f"   Max iterations: {max_iterations}")

        return state

    def record_iteration(
        self,
        session_id: str,
        files_modified: list[str],
        output: str = "",
    ) -> dict[str, Any]:
        """Record an iteration in continuous mode.

        Args:
            session_id: Current session
            files_modified: Files changed this iteration
            output: Output from this iteration

        Returns:
            Dict with iteration status and whether to continue

        """
        state = self._load_current_session()
        if not state or state.session_id != session_id:
            return {"continue": False, "reason": "session_not_found"}

        state.iteration_count += 1
        state.files_modified.extend(files_modified)

        # Check completion criteria
        if state.completion_criteria and state.completion_criteria in output:
            return self._complete_continuous(state, "completion_criteria_met")

        # Check max iterations
        if state.iteration_count >= state.max_iterations:
            return self._complete_continuous(state, "max_iterations_reached")

        # Record progress if files were modified
        if files_modified:
            state.last_progress_at = datetime.now()

        self._save_session_state(state)

        return {
            "continue": True,
            "iteration": state.iteration_count,
            "max": state.max_iterations,
            "files_this_iteration": len(files_modified),
        }

    def _complete_continuous(self, state: SessionState, reason: str) -> dict[str, Any]:
        """Complete a continuous session."""
        state.ended_at = datetime.now()
        state.is_continuous = False

        self._save_session_state(state)

        # Create handoff
        handoff_file = self.sessions_dir / "HANDOFF.md"
        self._create_handoff_doc(state, handoff_file)

        logger.info(f"✅ Continuous session complete: {reason}")
        logger.info(f"   Iterations: {state.iteration_count}")
        logger.info(f"   Files modified: {len(state.files_modified)}")

        return {
            "continue": False,
            "reason": reason,
            "iterations": state.iteration_count,
            "files_modified": len(state.files_modified),
        }

    def should_continue(self, session_id: str) -> bool:
        """Check if continuous session should continue."""
        state = self._load_current_session()
        if not state or state.session_id != session_id:
            return False
        return state.is_continuous and state.iteration_count < state.max_iterations

    def get_iteration_count(self, session_id: str) -> int:
        """Get current iteration count."""
        state = self._load_current_session()
        if not state or state.session_id != session_id:
            return 0
        return state.iteration_count


# Global instance
_handoff: SessionHandoff | None = None


def get_handoff() -> SessionHandoff:
    """Get global session handoff instance."""
    global _handoff
    if _handoff is None:
        _handoff = SessionHandoff()
    return _handoff
