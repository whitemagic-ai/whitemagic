"""Brain Upgrade #4: Coherence Persistence - Remember coherence across sessions.

v4.3.0 Enhancement: Iteration Tracking for Continuous Execution
- Tracks iteration count and progress
- Monitors rate limiting status
- Integrates with circuit breaker
- Persists execution state across IDE restarts
"""
from typing import Any
import json
from datetime import datetime

from whitemagic.utils.fileio import atomic_write, file_lock


class CoherencePersistence:
    """Persists coherence and execution state across sessions.

    v4.3.0: Now also tracks continuous execution state for Ralph-style loops.
    """

    # Rate limiting defaults (v4.3.0)
    DEFAULT_CALLS_PER_HOUR = 100

    def __init__(self) -> None:
        from whitemagic.config import PROJECT_ROOT
        self.state_file = PROJECT_ROOT / ".coherence_state.json"
        self.current = self._load()

    def _load(self) -> dict:
        if self.state_file.exists():
            try:
                with file_lock(self.state_file):
                    return json.loads(self.state_file.read_text()) or {}
            except json.JSONDecodeError:
                pass
        return {
            "level": 100,
            "factors": {},
            "last_update": None,
            # v4.3.0: Iteration tracking
            "iteration_count": 0,
            "calls_this_hour": 0,
            "hour_started": None,
            "total_iterations": 0,
            "last_progress": None,
        }

    def save(self, level: int, factors: dict | None = None) -> Any:
        self.current = {
            "level": level,
            "factors": factors or self.current.get("factors", {}),
            "last_update": datetime.now().isoformat(),
        }
        self.state_file.parent.mkdir(parents=True, exist_ok=True)
        with file_lock(self.state_file):
            atomic_write(self.state_file, json.dumps(self.current, indent=2))

    def get_level(self) -> int:
        return int(self.current.get("level", 100))

    def boost(self, amount: int, reason: str) -> Any:
        self.current["level"] = self.current.get("level", 100) + amount
        self.current["factors"][reason] = amount
        self.save(self.current["level"], self.current["factors"])
        return self.current["level"]

    # ===== ITERATION TRACKING (v4.3.0) =====

    def record_iteration(self, made_progress: bool = True) -> dict:
        """Record an iteration for rate limiting and tracking.

        Args:
            made_progress: Whether this iteration made meaningful progress

        Returns:
            Dict with current iteration stats and whether to continue

        """
        now = datetime.now()

        # Reset hourly counter if new hour
        current_hour = now.strftime("%Y%m%d%H")
        if self.current.get("hour_started") != current_hour:
            self.current["calls_this_hour"] = 0
            self.current["hour_started"] = current_hour

        # Increment counters
        self.current["iteration_count"] = self.current.get("iteration_count", 0) + 1
        self.current["calls_this_hour"] = self.current.get("calls_this_hour", 0) + 1
        self.current["total_iterations"] = self.current.get("total_iterations", 0) + 1

        if made_progress:
            self.current["last_progress"] = now.isoformat()

        self.current["last_update"] = now.isoformat()
        self._save_state()

        # Check rate limit
        at_limit = self.current["calls_this_hour"] >= self.DEFAULT_CALLS_PER_HOUR

        return {
            "iteration": self.current["iteration_count"],
            "calls_this_hour": self.current["calls_this_hour"],
            "calls_remaining": max(0, self.DEFAULT_CALLS_PER_HOUR - self.current["calls_this_hour"]),
            "at_rate_limit": at_limit,
            "total_iterations": self.current["total_iterations"],
        }

    def reset_iteration_count(self) -> Any:
        """Reset iteration count for new task (keeps total)."""
        self.current["iteration_count"] = 0
        self._save_state()

    def get_iteration_stats(self) -> dict:
        """Get current iteration statistics."""
        return {
            "iteration_count": self.current.get("iteration_count", 0),
            "calls_this_hour": self.current.get("calls_this_hour", 0),
            "calls_remaining": max(0, self.DEFAULT_CALLS_PER_HOUR - self.current.get("calls_this_hour", 0)),
            "total_iterations": self.current.get("total_iterations", 0),
            "last_progress": self.current.get("last_progress"),
            "coherence_level": self.current.get("level", 100),
        }

    def _save_state(self) -> Any:
        """Save current state to disk."""
        self.state_file.parent.mkdir(parents=True, exist_ok=True)
        with file_lock(self.state_file):
            atomic_write(self.state_file, json.dumps(self.current, indent=2))

_coherence = None
def get_coherence() -> CoherencePersistence:
    global _coherence
    if _coherence is None:
        _coherence = CoherencePersistence()
    return _coherence
