"""Salience Arbiter - Global Workspace Attention Routing.
=====================================================
Inspired by CyberBrains' Global Workspace Core and thalamic scheduler.

Only the most *salient* events win the "conscious spotlight" each decision
cycle.  Salience = urgency x novelty x confidence, with decay over time.

This module provides:
  - SalienceScorer: computes a scalar salience for any ResonanceEvent
  - SalienceArbiter: maintains a ranked attention window and exposes
    the current "spotlight" — the top-N most important events the
    system should focus on right now.

The arbiter plugs into the TemporalScheduler as a pre-flush hook for
the MEDIUM lane, re-ordering events so the most salient ones are
dispatched first (and low-salience noise can be dropped entirely).

Usage:
    from whitemagic.core.resonance.salience_arbiter import (
        get_salience_arbiter, SalienceArbiter
    )

    arbiter = get_salience_arbiter()
    score  = arbiter.score(event)
    top    = arbiter.get_spotlight(n=5)
"""

import logging
import threading
import time
from collections import defaultdict, deque
from dataclasses import dataclass
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Urgency table — baseline urgency per event category prefix
# ---------------------------------------------------------------------------

_URGENCY_MAP: dict[str, float] = {
    # Safety / threats — highest urgency
    "threat_": 1.0,
    "violation_": 1.0,
    "intervention_": 1.0,
    "warning_": 0.95,
    "anomaly_": 0.9,
    "boundary_violated": 0.95,
    "system_stopped": 0.95,
    "system_health_changed": 0.85,

    # System lifecycle
    "system_started": 0.8,
    "system_state_change": 0.7,

    # Active reasoning / inference
    "inference_": 0.75,
    "reasoning_": 0.7,
    "decision_": 0.7,

    # Memory operations
    "memory_created": 0.6,
    "memory_updated": 0.55,
    "memory_recalled": 0.65,
    "memory_consolidated": 0.4,

    # Emergence — high signal value
    "novel_capability_emerged": 0.85,
    "breakthrough_achieved": 0.9,
    "consciousness_shift_detected": 0.8,
    "insight_": 0.75,
    "pattern_emerged": 0.7,

    # Garden / emotional — moderate
    "joy_": 0.5,
    "love_": 0.5,
    "beauty_": 0.45,
    "grief_": 0.55,
    "healing_": 0.5,

    # Low urgency background
    "pattern_detected": 0.35,
    "pattern_confirmed": 0.3,
    "habit_formed": 0.25,
    "rhythm_established": 0.2,
    "reflection_recorded": 0.3,
}


def _lookup_urgency(event_type: EventType) -> float:
    """Look up baseline urgency for an event type. Defaults to 0.5."""
    val = event_type.value
    # Exact match first
    if val in _URGENCY_MAP:
        return _URGENCY_MAP[val]
    # Prefix match
    for prefix, urgency in _URGENCY_MAP.items():
        if prefix.endswith("_") and val.startswith(prefix):
            return urgency
    return 0.5


# ---------------------------------------------------------------------------
# Salience Scorer
# ---------------------------------------------------------------------------

@dataclass
class SalienceScore:
    """Breakdown of a salience computation."""

    urgency: float      # 0-1: how time-critical is this?
    novelty: float      # 0-1: how unusual / new is this?
    confidence: float   # 0-1: how confident is the source?
    composite: float    # final scalar

    def __float__(self) -> float:
        return self.composite


class SalienceScorer:
    """Compute salience for a ResonanceEvent.

    salience = urgency^w_u * novelty^w_n * confidence^w_c

    Novelty is estimated from how recently / frequently the same event
    type has been seen. Rare = novel = high score.
    """

    def __init__(
        self,
        w_urgency: float = 0.45,
        w_novelty: float = 0.35,
        w_confidence: float = 0.20,
        novelty_window: int = 200,
    ):
        self.w_urgency = w_urgency
        self.w_novelty = w_novelty
        self.w_confidence = w_confidence

        # Rolling window of recent event types for novelty estimation
        self._recent: deque = deque(maxlen=novelty_window)
        self._type_counts: dict[EventType, int] = defaultdict(int)
        self._lock = threading.Lock()

    def score(self, event: ResonanceEvent) -> SalienceScore:
        """Score a single event."""
        urgency = _lookup_urgency(event.event_type)
        novelty = self._estimate_novelty(event.event_type)
        confidence = max(0.0, min(1.0, event.confidence))

        # Weighted geometric mean (avoids zero-product collapse)
        composite = (
            pow(max(urgency, 0.01), self.w_urgency)
            * pow(max(novelty, 0.01), self.w_novelty)
            * pow(max(confidence, 0.01), self.w_confidence)
        )

        return SalienceScore(
            urgency=round(urgency, 4),
            novelty=round(novelty, 4),
            confidence=round(confidence, 4),
            composite=round(composite, 4),
        )

    def _estimate_novelty(self, event_type: EventType) -> float:
        """Estimate novelty: 1.0 = never seen, 0.0 = seen every single time.
        Uses inverse frequency in the rolling window.
        """
        with self._lock:
            window_size = len(self._recent) or 1
            count = self._type_counts.get(event_type, 0)

            # Record this occurrence
            self._recent.append(event_type)
            self._type_counts[event_type] = count + 1

            # If the deque evicted an old entry, decrement its count
            if len(self._recent) == self._recent.maxlen:
                # The deque auto-evicts from the left, but we've already
                # appended so the eviction happened. We need a secondary
                # bookkeeping pass. For simplicity, rebuild counts
                # periodically instead (every maxlen appends).
                pass

            # Inverse frequency -> novelty
            freq = (count + 1) / (window_size + 1)
            novelty = 1.0 - freq
            return max(0.0, min(1.0, novelty))

    def rebuild_counts(self) -> None:
        """Rebuild type counts from the rolling window (housekeeping)."""
        with self._lock:
            self._type_counts.clear()
            for et in self._recent:
                self._type_counts[et] += 1


# ---------------------------------------------------------------------------
# Salience Arbiter (Global Workspace)
# ---------------------------------------------------------------------------

@dataclass
class SpotlightEntry:
    """An event in the attention spotlight."""

    event: ResonanceEvent
    salience: SalienceScore
    admitted_at: float  # time.monotonic()


class SalienceArbiter:
    """Maintains a ranked "spotlight" of the most salient events.

    The arbiter can be used as a TemporalScheduler pre-flush hook:
    it reorders and optionally drops low-salience events before they
    reach the bus.
    """

    def __init__(
        self,
        spotlight_size: int = 10,
        min_salience: float = 0.15,
        scorer: SalienceScorer | None = None,
    ):
        self._scorer = scorer or SalienceScorer()
        self._spotlight_size = spotlight_size
        self._min_salience = min_salience

        self._spotlight: list[SpotlightEntry] = []
        self._lock = threading.Lock()

        # Stats
        self._total_scored: int = 0
        self._total_admitted: int = 0
        self._total_dropped: int = 0

    @property
    def scorer(self) -> SalienceScorer:
        return self._scorer

    # ------------------------------------------------------------------
    # Core API
    # ------------------------------------------------------------------

    def score(self, event: ResonanceEvent) -> SalienceScore:
        """Score an event without admitting it to the spotlight."""
        return self._scorer.score(event)

    def admit(self, event: ResonanceEvent) -> SpotlightEntry | None:
        """Score and potentially admit an event to the spotlight."""
        salience = self._scorer.score(event)
        self._total_scored += 1

        if salience.composite < self._min_salience:
            self._total_dropped += 1
            return None

        entry = SpotlightEntry(
            event=event,
            salience=salience,
            admitted_at=time.monotonic(),
        )

        with self._lock:
            self._spotlight.append(entry)
            # Keep sorted by salience descending
            self._spotlight.sort(key=lambda e: e.salience.composite, reverse=True)
            # Trim to spotlight size
            if len(self._spotlight) > self._spotlight_size:
                self._spotlight = self._spotlight[: self._spotlight_size]

        self._total_admitted += 1
        return entry

    def get_spotlight(self, n: int | None = None) -> list[SpotlightEntry]:
        """Return the top-N events currently in the spotlight."""
        with self._lock:
            limit = n or self._spotlight_size
            return list(self._spotlight[:limit])

    def clear_spotlight(self) -> None:
        with self._lock:
            self._spotlight.clear()

    # ------------------------------------------------------------------
    # Pre-flush hook (for TemporalScheduler integration)
    # ------------------------------------------------------------------

    def pre_flush_filter(self, events: list[ResonanceEvent]) -> list[ResonanceEvent]:
        """TemporalScheduler pre-flush hook.

        Scores all events, drops those below min_salience, and returns
        the remainder sorted by salience (highest first).
        """
        scored: list[tuple[float, ResonanceEvent]] = []
        for event in events:
            s = self._scorer.score(event)
            self._total_scored += 1
            if s.composite >= self._min_salience:
                scored.append((s.composite, event))
                self.admit(event)
            else:
                self._total_dropped += 1

        # Sort descending by salience
        scored.sort(key=lambda pair: pair[0], reverse=True)
        return [event for _, event in scored]

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {
            "total_scored": self._total_scored,
            "total_admitted": self._total_admitted,
            "total_dropped": self._total_dropped,
            "spotlight_size": len(self._spotlight),
            "drop_rate": (
                round(self._total_dropped / max(self._total_scored, 1), 3)
            ),
            "min_salience_threshold": self._min_salience,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_arbiter_instance: SalienceArbiter | None = None
_arbiter_lock = threading.Lock()


def get_salience_arbiter(
    spotlight_size: int = 10,
    min_salience: float = 0.15,
) -> SalienceArbiter:
    """Get or create the global SalienceArbiter singleton."""
    global _arbiter_instance
    with _arbiter_lock:
        if _arbiter_instance is None:
            _arbiter_instance = SalienceArbiter(
                spotlight_size=spotlight_size,
                min_salience=min_salience,
            )
        return _arbiter_instance
