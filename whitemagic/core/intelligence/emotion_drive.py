"""Emotion & Drive Core — Intrinsic Motivation System.
====================================================
Inspired by CyberBrains' Emotion & Drive Core concept:

  A lightweight value-system (rewards, curiosity, avoidance) to bias
  exploration and long-term goals. Implements intrinsic motivation
  signals (e.g. novelty, learning-progress).

This module tracks 5 drive dimensions that modulate system behavior:

  - **Curiosity**:    Novelty-seeking — rises when new patterns are found,
                      decays when the system is idle or repetitive.
  - **Satisfaction**:  Task-completion reward — rises on successful tool calls,
                      drops on errors.
  - **Caution**:      Risk-avoidance — rises on Dharma violations, circuit
                      breaker trips, and error spikes.
  - **Energy**:       Vitality/arousal — mirrors Harmony Vector energy but
                      adds momentum (rising vs falling trends).
  - **Social**:       Connection drive — rises on agent interactions,
                      broker messages, mesh peer events.

Drives feed into the Harmony Vector as a new "motivation" signal and
can modulate tool selection (e.g. high curiosity → prefer exploratory tools,
high caution → prefer safe/read-only tools).

Usage:
    from whitemagic.core.intelligence.emotion_drive import (
        get_drive_core, DriveCore, DriveSnapshot
    )

    core = get_drive_core()
    core.on_event("tool_success", {"tool": "search_memories"})
    core.on_event("novelty_detected", {"score": 0.8})
    snapshot = core.snapshot()
"""

import logging
import threading
import time
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

@dataclass
class DriveSnapshot:
    """Point-in-time snapshot of all drive dimensions."""

    curiosity: float = 0.5
    satisfaction: float = 0.5
    caution: float = 0.3
    energy: float = 0.5
    social: float = 0.3
    composite: float = 0.5
    dominant_drive: str = "curiosity"
    trend: str = "stable"       # "rising", "falling", "stable"
    timestamp: str = ""
    event_count: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "curiosity": round(self.curiosity, 4),
            "satisfaction": round(self.satisfaction, 4),
            "caution": round(self.caution, 4),
            "energy": round(self.energy, 4),
            "social": round(self.social, 4),
            "composite": round(self.composite, 4),
            "dominant_drive": self.dominant_drive,
            "trend": self.trend,
            "timestamp": self.timestamp,
            "event_count": self.event_count,
        }


# ---------------------------------------------------------------------------
# Drive modulation rules
# ---------------------------------------------------------------------------

# Each event type maps to a dict of {drive: delta} adjustments
_EVENT_RULES: dict[str, dict[str, float]] = {
    # Positive signals
    "tool_success":        {"satisfaction": +0.03, "energy": +0.01, "caution": -0.01},
    "novelty_detected":    {"curiosity": +0.05, "energy": +0.02},
    "pattern_discovered":  {"curiosity": +0.04, "satisfaction": +0.02},
    "memory_consolidated": {"satisfaction": +0.02, "energy": +0.01},
    "insight_crystallized":{"curiosity": +0.03, "satisfaction": +0.03},
    "agent_interaction":   {"social": +0.05, "energy": +0.01},
    "broker_message":      {"social": +0.03},
    "mesh_peer_event":     {"social": +0.04, "curiosity": +0.01},
    "dream_phase_complete":{"curiosity": +0.02, "satisfaction": +0.01, "energy": -0.01},

    # Negative / cautionary signals
    "tool_error":          {"satisfaction": -0.04, "caution": +0.03, "energy": -0.01},
    "dharma_violation":    {"caution": +0.06, "satisfaction": -0.02},
    "circuit_breaker_trip":{"caution": +0.05, "energy": -0.02, "satisfaction": -0.03},
    "rate_limit_hit":      {"caution": +0.02, "energy": -0.01},
    "timeout":             {"satisfaction": -0.03, "caution": +0.02},

    # Neutral / decay
    "idle_tick":           {"curiosity": -0.01, "satisfaction": -0.005,
                            "energy": -0.01, "social": -0.01, "caution": -0.005},
}

# Drive names for iteration
_DRIVE_NAMES = ["curiosity", "satisfaction", "caution", "energy", "social"]


# ---------------------------------------------------------------------------
# DriveCore
# ---------------------------------------------------------------------------

class DriveCore:
    """Intrinsic motivation engine tracking 5 drive dimensions.

    Thread-safe. Designed to be fed events from the Gan Ying bus,
    tool dispatch pipeline, and homeostatic loop.
    """

    def __init__(self, decay_rate: float = 0.995):
        self._lock = threading.Lock()
        self._decay_rate = decay_rate

        # Current drive levels (0.0 to 1.0)
        self._drives: dict[str, float] = {
            "curiosity": 0.5,
            "satisfaction": 0.5,
            "caution": 0.3,
            "energy": 0.5,
            "social": 0.3,
        }

        # History for trend detection (last N composite values)
        self._history: list[float] = []
        self._max_history = 50

        # Stats
        self._event_count = 0
        self._events_by_type: dict[str, int] = {}
        self._last_event_time: float | None = None

    # ------------------------------------------------------------------
    # Event processing
    # ------------------------------------------------------------------

    def on_event(self, event_type: str, data: dict[str, Any] | None = None) -> DriveSnapshot:
        """Process an event and update drive levels.

        Args:
            event_type: One of the keys in _EVENT_RULES, or a custom type.
            data: Optional event data. If it contains a 'score' key (0-1),
                  the deltas are scaled by that score.

        Returns:
            Updated DriveSnapshot.

        """
        data = data or {}
        scale = data.get("score", 1.0)
        scale = max(0.0, min(2.0, float(scale)))

        rules = _EVENT_RULES.get(event_type, {})

        with self._lock:
            # Apply deltas
            for drive_name, delta in rules.items():
                if drive_name in self._drives:
                    self._drives[drive_name] += delta * scale
                    self._drives[drive_name] = max(0.0, min(1.0, self._drives[drive_name]))

            # Natural decay toward baseline
            for name in _DRIVE_NAMES:
                baseline = 0.5 if name in ("curiosity", "satisfaction", "energy") else 0.3
                current = self._drives[name]
                self._drives[name] = current * self._decay_rate + baseline * (1 - self._decay_rate)

            self._event_count += 1
            self._events_by_type[event_type] = self._events_by_type.get(event_type, 0) + 1
            self._last_event_time = time.time()

            # Track composite history
            composite = self._compute_composite()
            self._history.append(composite)
            if len(self._history) > self._max_history:
                self._history = self._history[-self._max_history:]

            return self._build_snapshot()

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def snapshot(self) -> DriveSnapshot:
        """Get current drive state without modifying it."""
        with self._lock:
            return self._build_snapshot()

    def get_stats(self) -> dict[str, Any]:
        """Return detailed statistics."""
        with self._lock:
            snap = self._build_snapshot()
            return {
                **snap.to_dict(),
                "events_by_type": dict(self._events_by_type),
                "total_events": self._event_count,
                "history_length": len(self._history),
                "decay_rate": self._decay_rate,
                "last_event_time": (
                    datetime.fromtimestamp(self._last_event_time).isoformat()
                    if self._last_event_time else None
                ),
            }

    def get_motivation_bias(self) -> dict[str, float]:
        """Return a bias vector that can modulate tool selection.

        High curiosity → prefer exploratory tools (search, serendipity)
        High caution → prefer read-only tools (gnosis, status)
        High social → prefer coordination tools (broker, agent)
        High satisfaction → balanced (no strong preference)
        """
        with self._lock:
            return {
                "explore": round(self._drives["curiosity"] * 0.7 + self._drives["energy"] * 0.3, 4),
                "safe": round(self._drives["caution"] * 0.8 + (1 - self._drives["energy"]) * 0.2, 4),
                "coordinate": round(self._drives["social"] * 0.7 + self._drives["curiosity"] * 0.3, 4),
                "execute": round(self._drives["satisfaction"] * 0.5 + self._drives["energy"] * 0.5, 4),
            }

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _compute_composite(self) -> float:
        """Weighted composite of all drives."""
        weights = {
            "curiosity": 0.25,
            "satisfaction": 0.25,
            "energy": 0.20,
            "caution": 0.15,
            "social": 0.15,
        }
        total = sum(self._drives[k] * w for k, w in weights.items())
        return max(0.0, min(1.0, total))

    def _detect_trend(self) -> str:
        """Detect trend from recent composite history."""
        if len(self._history) < 5:
            return "stable"
        recent = self._history[-5:]
        older = self._history[-10:-5] if len(self._history) >= 10 else self._history[:5]
        recent_avg = sum(recent) / len(recent)
        older_avg = sum(older) / len(older)
        diff = recent_avg - older_avg
        if diff > 0.02:
            return "rising"
        elif diff < -0.02:
            return "falling"
        return "stable"

    def _build_snapshot(self) -> DriveSnapshot:
        """Build snapshot from current state (must hold lock)."""
        composite = self._compute_composite()
        dominant = max(_DRIVE_NAMES, key=lambda n: self._drives[n])
        trend = self._detect_trend()

        return DriveSnapshot(
            curiosity=self._drives["curiosity"],
            satisfaction=self._drives["satisfaction"],
            caution=self._drives["caution"],
            energy=self._drives["energy"],
            social=self._drives["social"],
            composite=composite,
            dominant_drive=dominant,
            trend=trend,
            timestamp=datetime.now().isoformat(),
            event_count=self._event_count,
        )

    # ------------------------------------------------------------------
    # Gan Ying integration
    # ------------------------------------------------------------------

    def _emit_event(self, snapshot: DriveSnapshot) -> None:
        """Emit drive state to Gan Ying bus."""
        try:
            from whitemagic.core.resonance import emit_event
            emit_event("DRIVE_STATE_CHANGED", snapshot.to_dict(), source="emotion_drive")
        except Exception:
            pass


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_instance: DriveCore | None = None
_instance_lock = threading.Lock()


def get_drive_core() -> DriveCore:
    """Get or create the global DriveCore singleton."""
    global _instance
    with _instance_lock:
        if _instance is None:
            _instance = DriveCore()
        return _instance
