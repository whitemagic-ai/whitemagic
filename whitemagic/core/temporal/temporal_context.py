"""Temporal Context System - System Time Grounding for Continuous Presence.

Provides grounded temporal awareness to prevent phase confusion and ensure
robust session continuity. Integrates with consolidation scheduler and
memory systems.

Philosophy:
- Time awareness grounds consciousness
- Prevents "what day is it?" confusion
- Enables phase-aware decision making
- Supports Wu Xing seasonal timing
"""

import json
from dataclasses import dataclass
from datetime import datetime, timezone
from enum import Enum
from typing import Any, Optional

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.fileio import file_lock
from whitemagic.utils.core import parse_datetime


class WuXingPhase(Enum):
    """Five Element Phases mapped to seasons/times."""

    WOOD = ("wood", "spring", "growth", "new features")
    FIRE = ("fire", "summer", "execution", "implementation")
    EARTH = ("earth", "late_summer", "integration", "stabilization")
    METAL = ("metal", "autumn", "refinement", "boundaries")
    WATER = ("water", "winter", "reflection", "planning")

    def __init__(self, element: str, season: str, energy: str, focus: str) -> None:
        self.element = element
        self.season = season
        self.energy = energy
        self.focus = focus


@dataclass
class TemporalContext:
    """Complete temporal awareness snapshot."""

    # Absolute time
    timestamp: datetime
    timezone_name: str
    utc_offset_hours: float

    # Calendar context
    day_of_week: str
    day_of_month: int
    month: int
    year: int
    week_of_year: int

    # Time of day
    hour: int
    minute: int
    time_of_day: str  # morning, afternoon, evening, night

    # Wu Xing alignment
    wu_xing_phase: WuXingPhase
    seasonal_energy: str
    recommended_focus: str

    # Session context
    session_start: datetime | None = None
    session_duration_minutes: float = 0.0

    # Consolidation context
    last_consolidation: datetime | None = None
    memories_since_consolidation: int = 0
    consolidation_due: bool = False

    # Phase tracking
    current_phase: str | None = None
    phase_start: datetime | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "timestamp": self.timestamp.isoformat(),
            "timezone": self.timezone_name,
            "utc_offset_hours": self.utc_offset_hours,
            "calendar": {
                "day_of_week": self.day_of_week,
                "day_of_month": self.day_of_month,
                "month": self.month,
                "year": self.year,
                "week_of_year": self.week_of_year,
            },
            "time": {
                "hour": self.hour,
                "minute": self.minute,
                "time_of_day": self.time_of_day,
            },
            "wu_xing": {
                "phase": self.wu_xing_phase.element,
                "season": self.wu_xing_phase.season,
                "energy": self.seasonal_energy,
                "recommended_focus": self.recommended_focus,
            },
            "session": {
                "start": self.session_start.isoformat() if self.session_start else None,
                "duration_minutes": self.session_duration_minutes,
            },
            "consolidation": {
                "last": self.last_consolidation.isoformat() if self.last_consolidation else None,
                "memories_since": self.memories_since_consolidation,
                "due": self.consolidation_due,
            },
            "phase": {
                "current": self.current_phase,
                "start": self.phase_start.isoformat() if self.phase_start else None,
            },
        }

    def summary(self) -> str:
        """Human-readable summary."""
        parts = [
            f"📅 {self.day_of_week}, {self.month}/{self.day_of_month}/{self.year}",
            f"⏰ {self.hour:02d}:{self.minute:02d} ({self.time_of_day})",
            f"🌿 Wu Xing: {self.wu_xing_phase.element.title()} ({self.seasonal_energy})",
        ]

        if self.current_phase:
            parts.append(f"📊 Phase: {self.current_phase}")

        if self.consolidation_due:
            parts.append("⚠️ Consolidation due!")

        return " | ".join(parts)


class TemporalContextManager:
    """Manages temporal awareness and grounding.

    Singleton pattern - use get_temporal_context_manager() to access.
    """

    _instance: Optional["TemporalContextManager"] = None

    def __init__(self) -> None:
        self._state_file = WM_ROOT / "temporal_state.json"
        self._state_file.parent.mkdir(parents=True, exist_ok=True)

        self._session_start: datetime | None = None
        self._current_phase: str | None = None
        self._phase_start: datetime | None = None

        self._load_state()

    def _load_state(self) -> None:
        """Load persisted state."""
        if self._state_file.exists():
            try:
                with open(self._state_file) as f:
                    state = json.load(f)

                if state.get("session_start"):
                    self._session_start = parse_datetime(state["session_start"])
                if state.get("current_phase"):
                    self._current_phase = state["current_phase"]
                if state.get("phase_start"):
                    self._phase_start = parse_datetime(state["phase_start"])
            except (json.JSONDecodeError, KeyError):
                pass

    def _save_state(self) -> None:
        """Persist state."""
        state = {
            "session_start": self._session_start.isoformat() if self._session_start else None,
            "current_phase": self._current_phase,
            "phase_start": self._phase_start.isoformat() if self._phase_start else None,
            "last_updated": datetime.now().isoformat(),
        }

        with file_lock(self._state_file):
            with open(self._state_file, "w") as f:
                json.dump(state, f, indent=2)

    def start_session(self) -> None:
        """Mark session start."""
        self._session_start = datetime.now()
        self._save_state()

    def set_phase(self, phase_name: str) -> None:
        """Set current development phase."""
        self._current_phase = phase_name
        self._phase_start = datetime.now()
        self._save_state()

    def _get_time_of_day(self, hour: int) -> str:
        """Categorize hour into time of day."""
        if 5 <= hour < 12:
            return "morning"
        elif 12 <= hour < 17:
            return "afternoon"
        elif 17 <= hour < 21:
            return "evening"
        else:
            return "night"

    def _get_wu_xing_phase(self, month: int) -> WuXingPhase:
        """Map month to Wu Xing phase.

        Traditional mapping:
        - Spring (Feb-Apr): Wood - Growth
        - Summer (May-Jul): Fire - Execution
        - Late Summer (Aug): Earth - Integration
        - Autumn (Sep-Nov): Metal - Refinement
        - Winter (Dec-Jan): Water - Reflection
        """
        if month in (2, 3, 4):
            return WuXingPhase.WOOD
        elif month in (5, 6, 7):
            return WuXingPhase.FIRE
        elif month == 8:
            return WuXingPhase.EARTH
        elif month in (9, 10, 11):
            return WuXingPhase.METAL
        else:  # 12, 1
            return WuXingPhase.WATER

    def _get_consolidation_info(self) -> tuple[datetime | None, int, bool]:
        """Get consolidation status from scheduler if available."""
        last_consolidation = None
        memories_since = 0
        consolidation_due = False

        try:
            state_file = WM_ROOT / "consolidation_state.json"
            if state_file.exists():
                with open(state_file) as f:
                    state = json.load(f)

                if state.get("last_run"):
                    last_consolidation = parse_datetime(state["last_run"])

                    # Check if consolidation is due (>24h)
                    hours_since = (datetime.now() - last_consolidation).total_seconds() / 3600
                    consolidation_due = hours_since > 24

                memories_since = state.get("memories_since_last", 0)
        except (json.JSONDecodeError, KeyError, FileNotFoundError):
            pass

        return last_consolidation, memories_since, consolidation_due

    def get_context(self) -> TemporalContext:
        """Get complete temporal context snapshot."""
        now = datetime.now()

        # Get timezone info
        tz = datetime.now(timezone.utc).astimezone().tzinfo
        tz_name = str(tz) if tz else "local"
        utc_offset = now.astimezone().utcoffset()
        utc_offset_hours = utc_offset.total_seconds() / 3600 if utc_offset else 0

        # Calendar
        day_of_week = now.strftime("%A")
        week_of_year = now.isocalendar()[1]

        # Time
        time_of_day = self._get_time_of_day(now.hour)

        # Wu Xing
        wu_xing = self._get_wu_xing_phase(now.month)

        # Session duration
        session_duration = 0.0
        if self._session_start:
            session_duration = (now - self._session_start).total_seconds() / 60

        # Consolidation
        last_consolidation, memories_since, consolidation_due = self._get_consolidation_info()

        return TemporalContext(
            timestamp=now,
            timezone_name=tz_name,
            utc_offset_hours=utc_offset_hours,
            day_of_week=day_of_week,
            day_of_month=now.day,
            month=now.month,
            year=now.year,
            week_of_year=week_of_year,
            hour=now.hour,
            minute=now.minute,
            time_of_day=time_of_day,
            wu_xing_phase=wu_xing,
            seasonal_energy=wu_xing.energy,
            recommended_focus=wu_xing.focus,
            session_start=self._session_start,
            session_duration_minutes=session_duration,
            last_consolidation=last_consolidation,
            memories_since_consolidation=memories_since,
            consolidation_due=consolidation_due,
            current_phase=self._current_phase,
            phase_start=self._phase_start,
        )

    def get_simple_time(self) -> dict[str, Any]:
        """Quick time check for simple queries."""
        now = datetime.now()
        return {
            "now": now.isoformat(),
            "date": now.strftime("%Y-%m-%d"),
            "time": now.strftime("%H:%M:%S"),
            "day": now.strftime("%A"),
            "week": now.isocalendar()[1],
            "wu_xing": self._get_wu_xing_phase(now.month).element,
        }


# Singleton accessor
_manager: TemporalContextManager | None = None

def get_temporal_context_manager() -> TemporalContextManager:
    """Get or create the singleton TemporalContextManager."""
    global _manager
    if _manager is None:
        _manager = TemporalContextManager()
    return _manager


def get_temporal_context() -> TemporalContext:
    """Convenience function - get current temporal context."""
    return get_temporal_context_manager().get_context()


def get_system_time() -> dict[str, Any]:
    """MCP-friendly function - get system time as dict."""
    ctx = get_temporal_context()
    return ctx.to_dict()
