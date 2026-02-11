"""Memory Matrix - 2D Grid Visualization of Memory Connections.

The unified interface for all memory operations.
Provides the "never forget" infrastructure.

Usage:
    from whitemagic.core.memory.memory_matrix import get_matrix

    matrix = get_matrix()
    matrix.record_interaction("read_file", "/path/to/file.md")

    report = matrix.generate_session_report()
"""

from __future__ import annotations

import json
from dataclasses import dataclass, fields
from datetime import datetime, timezone
from pathlib import Path
from threading import Lock
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.fileio import atomic_write, file_lock

from .embedding_index import get_embedding_index
from .seen_registry import get_seen_registry
from .timeline import get_timeline

_matrix_instance: "MemoryMatrix" | None = None
_matrix_lock = Lock()


@dataclass
class SessionContext:
    """Current session context for the Memory Matrix."""

    session_id: str
    started: str
    last_activity: str
    interactions: int = 0
    files_seen: int = 0
    memories_accessed: int = 0
    searches_performed: int = 0


class MemoryMatrix:
    """Unified memory management system.

    Combines:
    - SeenRegistry (what we've viewed)
    - EmbeddingIndex (semantic search)
    - ChronologicalTimeline (time-based navigation)

    Provides:
    - Automatic tracking of all interactions
    - Session continuity across restarts
    - Rich querying capabilities
    - Dashboard-ready data exports
    """

    def __init__(self, storage_dir: Path | None = None):
        self.storage_dir = storage_dir or (WM_ROOT / "matrix")
        self.storage_dir.mkdir(parents=True, exist_ok=True)

        # Core components
        self.seen = get_seen_registry()
        self.embeddings = get_embedding_index()
        self.timeline = get_timeline()

        # Session tracking
        self._session: SessionContext | None = None
        self._load_session()

    def _load_session(self) -> None:
        """Load or create session."""
        session_file = self.storage_dir / "current_session.json"
        if session_file.exists():
            try:
                with file_lock(session_file):
                    data = json.loads(session_file.read_text()) or {}
                allowed = {f.name for f in fields(SessionContext)}
                filtered = {k: v for k, v in data.items() if k in allowed}
                if {"session_id", "started", "last_activity"}.issubset(filtered.keys()):
                    self._session = SessionContext(**filtered)
                else:
                    self._new_session()
            except Exception:
                self._new_session()
        else:
            self._new_session()

    def _new_session(self) -> None:
        """Create new session."""
        now = datetime.now(timezone.utc).isoformat()
        self._session = SessionContext(
            session_id=f"session_{datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')}",
            started=now,
            last_activity=now,
        )
        self._save_session()

        # Record session start in timeline
        self.timeline.add_event(
            "session_start",
            {"session_id": self._session.session_id},
            tags=["session", "start"],
        )

    def _save_session(self) -> None:
        """Save session state."""
        if self._session:
            session_file = self.storage_dir / "current_session.json"
            existing: dict[str, Any] = {}
            existing.update({
                "session_id": self._session.session_id,
                "started": self._session.started,
                "last_activity": self._session.last_activity,
                "interactions": self._session.interactions,
                "files_seen": self._session.files_seen,
                "memories_accessed": self._session.memories_accessed,
                "searches_performed": self._session.searches_performed,
            })
            with file_lock(session_file):
                try:
                    merged = json.loads(session_file.read_text()) or {}
                except json.JSONDecodeError:
                    merged = {}
                merged.update(existing)
                atomic_write(session_file, json.dumps(merged, indent=2))

    def record_interaction(
        self,
        interaction_type: str,
        target: str,
        data: dict[str, Any] | None = None,
        context: str | None = None,
    ) -> None:
        """Record any interaction for perfect recall.

        Args:
            interaction_type: Type of interaction (read_file, create_memory, search, etc.)
            target: What was interacted with (file path, memory ID, etc.)
            data: Additional interaction data
            context: Why this interaction happened

        """
        now = datetime.now(timezone.utc).isoformat()

        # Update session
        if self._session:
            self._session.last_activity = now
            self._session.interactions += 1

        # Record in timeline
        self.timeline.add_event(
            interaction_type,
            {
                "target": target,
                "context": context,
                "session_id": self._session.session_id if self._session else None,
                **(data or {}),
            },
            tags=[interaction_type, "interaction"],
        )

        # Update specific trackers based on type
        if interaction_type in ("read_file", "view_image", "view_document"):
            self.seen.mark_seen(target, context=context)
            if self._session:
                self._session.files_seen += 1

        elif interaction_type == "search":
            if self._session:
                self._session.searches_performed += 1

        elif interaction_type in ("access_memory", "create_memory"):
            if self._session:
                self._session.memories_accessed += 1

        self._save_session()

    def have_seen(self, path: str) -> bool:
        """Check if we've seen a file before."""
        return self.seen.have_seen(path)

    def when_seen(self, path: str) -> str | None:
        """Get when we last saw a file."""
        return self.seen.when_seen(path)

    def has_changed(self, path: str) -> bool:
        """Check if file has changed since we last saw it."""
        return self.seen.has_changed(path)

    def search_semantic(
        self,
        query: str,
        limit: int = 10,
    ) -> list[tuple[str, float, str]]:
        """Semantic search across embedded content."""
        self.record_interaction("search", query, {"type": "semantic", "limit": limit})
        return self.embeddings.search(query, limit)

    def search_timeline(self, query: str) -> list[Any]:
        """Search timeline events."""
        self.record_interaction("search", query, {"type": "timeline"})
        return self.timeline.search(query)

    def get_today_summary(self) -> dict[str, Any]:
        """Get summary of today's activity."""
        events = self.timeline.get_today()
        by_type: dict[str, int] = {}
        for event in events:
            by_type[event.event_type] = by_type.get(event.event_type, 0) + 1

        return {
            "date": datetime.now(timezone.utc).strftime("%Y-%m-%d"),
            "total_events": len(events),
            "by_type": by_type,
            "files_seen": len(self.seen.get_recent(24)),
            "session": {
                "id": self._session.session_id if self._session else None,
                "started": self._session.started if self._session else None,
                "interactions": self._session.interactions if self._session else 0,
            },
        }

    def generate_session_report(self) -> dict[str, Any]:
        """Generate comprehensive session report."""
        if not self._session:
            return {"error": "No active session"}

        # Get events for this session
        events = self.timeline.get_by_tag("interaction")
        session_events = [
            e for e in events
            if e.data.get("session_id") == self._session.session_id
        ]

        # Categorize by type
        by_type: dict[str, list[str]] = {}
        for event in session_events:
            t = event.event_type
            if t not in by_type:
                by_type[t] = []
            target = event.data.get("target", "unknown")
            by_type[t].append(target)

        return {
            "session_id": self._session.session_id,
            "started": self._session.started,
            "last_activity": self._session.last_activity,
            "duration_estimate": "calculated from timestamps",
            "totals": {
                "interactions": self._session.interactions,
                "files_seen": self._session.files_seen,
                "memories_accessed": self._session.memories_accessed,
                "searches": self._session.searches_performed,
            },
            "by_type": {k: len(v) for k, v in by_type.items()},
            "recent_files": [e.path for e in self.seen.get_recent(10)],
        }

    def get_dashboard_data(self) -> dict[str, Any]:
        """Get data formatted for dashboard display."""
        return {
            "session": self._session.__dict__ if self._session else None,
            "seen_stats": self.seen.stats(),
            "embedding_stats": self.embeddings.stats(),
            "timeline_stats": self.timeline.stats(),
            "today_summary": self.get_today_summary(),
        }

    def export_for_visualization(self) -> dict[str, Any]:
        """Export data for 2D grid visualization.

        Returns structure suitable for rendering a memory matrix:
        - Nodes (memories, files, events)
        - Connections (relationships, references)
        - Timeline positions
        """
        # Get recent items
        recent_seen = self.seen.get_recent(100)
        recent_events = self.timeline.get_recent(168)  # Last week

        nodes = []
        connections = []

        # Create nodes from seen files
        for entry in recent_seen:
            nodes.append({
                "id": entry.path,
                "type": "file",
                "file_type": entry.file_type,
                "times_seen": entry.times_seen,
                "last_seen": entry.last_seen,
                "x": 0,  # To be calculated by visualizer
                "y": 0,
            })

        # Create nodes from events (grouped by type)
        event_types = set(e.event_type for e in recent_events)
        for etype in event_types:
            type_events = [e for e in recent_events if e.event_type == etype]
            nodes.append({
                "id": f"event_{etype}",
                "type": "event_group",
                "event_type": etype,
                "count": len(type_events),
                "x": 0,
                "y": 0,
            })

        # Create connections based on temporal proximity
        for i, entry in enumerate(recent_seen[:-1]):
            next_entry = recent_seen[i + 1]
            connections.append({
                "source": entry.path,
                "target": next_entry.path,
                "type": "temporal",
                "weight": 1.0,
            })

        return {
            "nodes": nodes,
            "connections": connections,
            "meta": {
                "total_nodes": len(nodes),
                "total_connections": len(connections),
                "generated": datetime.now(timezone.utc).isoformat(),
            },
        }

    def stats(self) -> dict[str, Any]:
        """Get overall matrix statistics."""
        return {
            "seen_registry": self.seen.stats(),
            "embedding_index": self.embeddings.stats(),
            "timeline": self.timeline.stats(),
            "current_session": self._session.__dict__ if self._session else None,
        }


def get_matrix() -> MemoryMatrix:
    """Get singleton MemoryMatrix instance."""
    global _matrix_instance
    with _matrix_lock:
        if _matrix_instance is None:
            _matrix_instance = MemoryMatrix()
        return _matrix_instance
