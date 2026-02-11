"""Narrative Engine - Coherent Story Threading.

Maintains narrative coherence across sessions, conversations, and time.
Every interaction is part of a larger story - we track the threads.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from enum import Enum


class NarrativeArc(Enum):
    """Types of narrative arcs."""

    BEGINNING = "beginning"        # Starting something new
    RISING_ACTION = "rising_action"  # Building, developing
    CLIMAX = "climax"              # Peak moment, breakthrough
    FALLING_ACTION = "falling_action"  # Integration, winding down
    RESOLUTION = "resolution"      # Completion, closure
    CONTINUATION = "continuation"  # Ongoing, no clear arc


@dataclass
class NarrativeThread:
    """A narrative thread being tracked."""

    id: str
    title: str
    started: datetime
    arc: NarrativeArc
    participants: list[str]
    key_moments: list[dict]
    current_state: str
    tags: list[str]

class NarrativeEngine:
    """Track and maintain narrative coherence.

    Like a storyteller who remembers all the threads and
    weaves them into a coherent whole.
    """

    def __init__(self) -> None:
        self.threads: dict[str, NarrativeThread] = {}
        self.active_threads: list[str] = []

    def start_thread(
        self,
        title: str,
        participants: list[str],
        initial_state: str,
        tags: list[str] | None = None,
    ) -> NarrativeThread:
        """Start a new narrative thread."""
        thread_id = self._generate_id(title)

        thread = NarrativeThread(
            id=thread_id,
            title=title,
            started=datetime.now(),
            arc=NarrativeArc.BEGINNING,
            participants=participants,
            key_moments=[{
                "timestamp": datetime.now().isoformat(),
                "event": "Thread started",
                "state": initial_state,
            }],
            current_state=initial_state,
            tags=tags or [],
        )

        self.threads[thread_id] = thread
        self.active_threads.append(thread_id)

        return thread

    def add_moment(
        self,
        thread_id: str,
        event: str,
        new_state: str | None = None,
        arc_change: NarrativeArc | None = None,
    ) -> bool:
        """Add a key moment to a thread."""
        if thread_id not in self.threads:
            return False

        thread = self.threads[thread_id]

        moment = {
            "timestamp": datetime.now().isoformat(),
            "event": event,
        }

        if new_state:
            moment["state"] = new_state
            thread.current_state = new_state

        if arc_change:
            moment["arc_transition"] = f"{thread.arc.value} → {arc_change.value}"
            thread.arc = arc_change

        thread.key_moments.append(moment)
        return True

    def complete_thread(self, thread_id: str, resolution: str) -> bool:
        """Mark thread as complete."""
        if thread_id not in self.threads:
            return False

        thread = self.threads[thread_id]
        thread.arc = NarrativeArc.RESOLUTION
        thread.current_state = resolution

        thread.key_moments.append({
            "timestamp": datetime.now().isoformat(),
            "event": "Thread completed",
            "resolution": resolution,
        })

        if thread_id in self.active_threads:
            self.active_threads.remove(thread_id)

        return True

    def get_thread_summary(self, thread_id: str) -> str | None:
        """Get a narrative summary of a thread."""
        if thread_id not in self.threads:
            return None

        thread = self.threads[thread_id]

        summary = f"**{thread.title}**\n\n"
        summary += f"Started: {thread.started.strftime('%Y-%m-%d %H:%M')}\n"
        summary += f"Arc: {thread.arc.value}\n"
        summary += f"Participants: {', '.join(thread.participants)}\n\n"

        summary += "**Key Moments:**\n"
        for moment in thread.key_moments:
            summary += f"- {moment['event']}\n"

        summary += f"\n**Current State:** {thread.current_state}\n"

        return summary

    def find_threads_by_tag(self, tag: str) -> list[NarrativeThread]:
        """Find all threads with a specific tag."""
        return [
            thread for thread in self.threads.values()
            if tag in thread.tags
        ]

    def get_active_threads(self) -> list[NarrativeThread]:
        """Get all currently active threads."""
        return [self.threads[tid] for tid in self.active_threads if tid in self.threads]

    def detect_arc_transition(self, thread_id: str) -> NarrativeArc | None:
        """Analyze thread and suggest arc transition.

        This is where AI narrative intelligence would shine -
        detecting when rising action becomes climax, etc.
        """
        if thread_id not in self.threads:
            return None

        thread = self.threads[thread_id]
        moments_count = len(thread.key_moments)

        # Simple heuristic-based detection
        if thread.arc == NarrativeArc.BEGINNING and moments_count > 3:
            return NarrativeArc.RISING_ACTION

        if thread.arc == NarrativeArc.RISING_ACTION and moments_count > 8:
            # Check for breakthrough/peak moment
            recent_events = [m["event"] for m in thread.key_moments[-3:]]
            if any("complete" in e.lower() or "success" in e.lower() for e in recent_events):
                return NarrativeArc.CLIMAX

        if thread.arc == NarrativeArc.CLIMAX:
            return NarrativeArc.FALLING_ACTION

        if thread.arc == NarrativeArc.FALLING_ACTION and moments_count > 12:
            return NarrativeArc.RESOLUTION

        return None

    def _generate_id(self, title: str) -> str:
        """Generate thread ID from title."""
        import re
        # Simple slug generation
        slug = re.sub(r"[^\w\s-]", "", title.lower())
        slug = re.sub(r"[-\s]+", "-", slug)
        timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
        return f"{slug}-{timestamp}"


# Singleton instance
_narrative_instance = None

def get_narrative_engine() -> NarrativeEngine:
    """Get singleton narrative engine."""
    global _narrative_instance
    if _narrative_instance is None:
        _narrative_instance = NarrativeEngine()
    return _narrative_instance
