"""Chronological Timeline - Time-Based Memory Navigation.

Organizes all memories, events, and interactions along a timeline.
Enables asking "What happened on November 20th?" or "Show me last week's work".

Usage:
    from whitemagic.core.memory.memory_matrix import get_timeline

    timeline = get_timeline()
    timeline.add_event("session_start", {"topic": "Phase 1 scan"})

    events = timeline.get_range("2025-11-20", "2025-11-29")
"""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field
from datetime import datetime, timedelta, timezone
from pathlib import Path
from threading import Lock
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.core import parse_datetime
from whitemagic.utils.fileio import atomic_write, file_lock

_timeline_instance: "ChronologicalTimeline" | None = None
_timeline_lock = Lock()


@dataclass
class TimelineEvent:
    """A single event on the timeline."""

    id: str
    timestamp: str
    event_type: str
    data: dict[str, Any] = field(default_factory=dict)
    tags: list[str] = field(default_factory=list)

    def to_dict(self) -> dict:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict) -> 'TimelineEvent':
        return cls(**data)


class ChronologicalTimeline:
    """Time-based memory organization.

    Features:
    - Add events with timestamps
    - Query by date range
    - Group by day/week/month
    - Track session continuity
    """

    def __init__(self, storage_path: Path | None = None):
        self.storage_path = storage_path or (WM_ROOT / "matrix" / "timeline.json")
        self.storage_path.parent.mkdir(parents=True, exist_ok=True)
        self._events: list[TimelineEvent] = []
        self._event_counter = 0
        self._load()

    def _load(self) -> None:
        """Load timeline from disk."""
        if self.storage_path.exists():
            try:
                with file_lock(self.storage_path):
                    data = json.loads(self.storage_path.read_text())
                self._events = [
                    TimelineEvent.from_dict(e)
                    for e in data.get("events", [])
                ]
                self._event_counter = data.get("counter", len(self._events))
            except Exception:
                self._events = []

    def _save(self) -> None:
        """Save timeline to disk."""
        data = {
            "version": "1.0",
            "updated": datetime.now(timezone.utc).isoformat(),
            "counter": self._event_counter,
            "total_events": len(self._events),
            "events": [e.to_dict() for e in self._events],
        }
        with file_lock(self.storage_path):
            atomic_write(self.storage_path, json.dumps(data, indent=2))

    def add_event(
        self,
        event_type: str,
        data: dict[str, Any] | None = None,
        tags: list[str] | None = None,
        timestamp: str | None = None,
    ) -> TimelineEvent:
        """Add an event to the timeline.

        Args:
            event_type: Type of event (session_start, file_read, memory_created, etc.)
            data: Event-specific data
            tags: Tags for categorization
            timestamp: Optional custom timestamp (defaults to now)

        Returns:
            The created TimelineEvent

        """
        self._event_counter += 1
        event = TimelineEvent(
            id=f"evt_{self._event_counter}",
            timestamp=timestamp or datetime.now(timezone.utc).isoformat(),
            event_type=event_type,
            data=data or {},
            tags=tags or [],
        )
        self._events.append(event)
        self._save()
        return event

    def get_range(
        self,
        start_date: str,
        end_date: str | None = None,
    ) -> list[TimelineEvent]:
        """Get events within a date range.

        Args:
            start_date: Start date (YYYY-MM-DD or ISO format)
            end_date: End date (defaults to now)

        Returns:
            List of events in range

        """
        start = parse_datetime(start_date.replace("Z", "+00:00"))
        end = parse_datetime(end_date.replace("Z", "+00:00")) if end_date else datetime.now(timezone.utc)

        results = []
        for event in self._events:
            try:
                event_time = parse_datetime(event.timestamp.replace("Z", "+00:00"))
                if start <= event_time <= end:
                    results.append(event)
            except Exception:
                continue

        return sorted(results, key=lambda e: e.timestamp)

    def get_by_type(self, event_type: str) -> list[TimelineEvent]:
        """Get all events of a specific type."""
        return [e for e in self._events if e.event_type == event_type]

    def get_by_tag(self, tag: str) -> list[TimelineEvent]:
        """Get all events with a specific tag."""
        return [e for e in self._events if tag in e.tags]

    def get_today(self) -> list[TimelineEvent]:
        """Get today's events."""
        today = datetime.now(timezone.utc).strftime("%Y-%m-%d")
        return self.get_range(today)

    def get_recent(self, hours: int = 24) -> list[TimelineEvent]:
        """Get events from the last N hours."""
        start = (datetime.now(timezone.utc) - timedelta(hours=hours)).isoformat()
        return self.get_range(start)

    def group_by_day(
        self,
        events: list[TimelineEvent] | None = None,
    ) -> dict[str, list[TimelineEvent]]:
        """Group events by day."""
        events = events or self._events
        by_day: dict[str, list[TimelineEvent]] = {}

        for event in events:
            day = event.timestamp[:10]  # YYYY-MM-DD
            if day not in by_day:
                by_day[day] = []
            by_day[day].append(event)

        return dict(sorted(by_day.items()))

    def get_session_summary(self, session_id: str | None = None) -> dict[str, Any]:
        """Get summary of a session's events."""
        if session_id:
            events = [e for e in self._events if e.data.get("session_id") == session_id]
        else:
            # Get most recent session
            sessions = self.get_by_type("session_start")
            if not sessions:
                return {"error": "No sessions found"}
            latest = sessions[-1]
            session_id = latest.data.get("session_id", latest.id)
            events = [e for e in self._events
                     if e.timestamp >= latest.timestamp]

        by_type: dict[str, int] = {}
        for event in events:
            by_type[event.event_type] = by_type.get(event.event_type, 0) + 1

        return {
            "session_id": session_id,
            "total_events": len(events),
            "event_types": by_type,
            "start": events[0].timestamp if events else None,
            "end": events[-1].timestamp if events else None,
        }

    def search(self, query: str) -> list[TimelineEvent]:
        """Search events by content."""
        query_lower = query.lower()
        results = []
        for event in self._events:
            # Search in event type
            if query_lower in event.event_type.lower():
                results.append(event)
                continue
            # Search in tags
            if any(query_lower in tag.lower() for tag in event.tags):
                results.append(event)
                continue
            # Search in data
            data_str = json.dumps(event.data).lower()
            if query_lower in data_str:
                results.append(event)
        return results

    def stats(self) -> dict[str, Any]:
        """Get timeline statistics."""
        by_type: dict[str, int] = {}
        by_day: dict[str, int] = {}

        for event in self._events:
            by_type[event.event_type] = by_type.get(event.event_type, 0) + 1
            day = event.timestamp[:10]
            by_day[day] = by_day.get(day, 0) + 1

        return {
            "total_events": len(self._events),
            "by_type": by_type,
            "events_per_day": by_day,
            "first_event": self._events[0].timestamp if self._events else None,
            "last_event": self._events[-1].timestamp if self._events else None,
        }

    def clear(self) -> None:
        """Clear all events."""
        self._events = []
        self._event_counter = 0
        self._save()


def get_timeline() -> ChronologicalTimeline:
    """Get singleton ChronologicalTimeline instance."""
    global _timeline_instance
    with _timeline_lock:
        if _timeline_instance is None:
            _timeline_instance = ChronologicalTimeline()
        return _timeline_instance
