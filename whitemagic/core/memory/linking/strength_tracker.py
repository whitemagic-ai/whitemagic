"""💪 Strength Tracker - Co-Access Link Strengthening
When memories are accessed together, their links strengthen.
"Neurons that fire together wire together"
Created: December 2, 2025 (Hanuman Tuesday).
"""

from collections import deque
from dataclasses import dataclass
from datetime import datetime, timedelta

from whitemagic.core.memory.neural.neural_memory import NeuralMemory


@dataclass
class AccessRecord:
    """Record of a memory access."""

    memory_id: str
    timestamp: datetime


class StrengthTracker:
    """Tracks memory accesses and strengthens co-accessed links."""

    def __init__(self, window_minutes: float = 5.0, strengthen_amount: float = 0.05):
        self.window_minutes = window_minutes
        self.strengthen_amount = strengthen_amount
        self._recent_accesses: deque[AccessRecord] = deque(maxlen=100)
        self._memory_store: dict[str, NeuralMemory] = {}

    def register_memory(self, memory: NeuralMemory) -> None:
        self._memory_store[memory.id] = memory

    def record_access(self, memory_id: str) -> list[str]:
        """Record an access and strengthen links to recent accesses."""
        now = datetime.now()
        cutoff = now - timedelta(minutes=self.window_minutes)

        # Find recent accesses within window
        recent_ids = [
            r.memory_id for r in self._recent_accesses
            if r.timestamp > cutoff and r.memory_id != memory_id
        ]

        # Record this access
        self._recent_accesses.append(AccessRecord(memory_id, now))

        # Strengthen links
        memory = self._memory_store.get(memory_id)
        if memory:
            for recent_id in recent_ids:
                self._strengthen_link(memory, recent_id)

        return recent_ids

    def _strengthen_link(self, memory: NeuralMemory, other_id: str) -> None:
        """Strengthen link between two memories."""
        link = memory.get_link(other_id)
        if link:
            link.activate()

        other = self._memory_store.get(other_id)
        if other:
            other_link = other.get_link(memory.id)
            if other_link:
                other_link.activate()

    def get_recent_accesses(self, minutes: float | None = None) -> list[str]:
        """Get recently accessed memory IDs."""
        if minutes is None:
            minutes = self.window_minutes

        cutoff = datetime.now() - timedelta(minutes=minutes)
        return [r.memory_id for r in self._recent_accesses if r.timestamp > cutoff]

    def get_co_access_count(self, mem1_id: str, mem2_id: str) -> int:
        """Count how often two memories were accessed together."""
        count = 0
        accesses = list(self._recent_accesses)

        for i, r1 in enumerate(accesses):
            if r1.memory_id != mem1_id:
                continue

            for r2 in accesses[max(0, i-5):i+5]:
                if r2.memory_id == mem2_id:
                    time_diff = abs((r1.timestamp - r2.timestamp).total_seconds())
                    if time_diff < self.window_minutes * 60:
                        count += 1

        return count


_tracker: StrengthTracker | None = None

def get_tracker() -> StrengthTracker:
    global _tracker
    if _tracker is None:
        _tracker = StrengthTracker()
    return _tracker

def on_co_access(memory_id: str) -> list[str]:
    return get_tracker().record_access(memory_id)

def get_recent_accesses(minutes: float = 5.0) -> list[str]:
    return get_tracker().get_recent_accesses(minutes)
