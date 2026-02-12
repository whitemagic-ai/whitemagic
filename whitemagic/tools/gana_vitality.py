"""Gana Vitality — Per-Gana Health Monitoring & Performance Reputation.
===================================================================
Implements two Bhīṣma principles from Mahābhārata 12.108:

1. **Performance Reputation** (12.108.20) — "Honor competence: the wise,
   brave, steady performers." Tracks per-Gana success rates, latency,
   and usage patterns so the PRAT router can make smarter decisions.

2. **Heartbeat / Silence Detection** (12.108.29) — "When people stop
   speaking, defeat is already showing." Detects when a Gana goes
   silent (all tools failing, no calls in N minutes) and routes
   resonance around it.

Thread-safe singleton. Fed by prat_router after each call.
"""

import logging
import threading
import time
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# How many seconds of silence before a Gana is considered "muted"
_SILENCE_THRESHOLD_SECS = 300  # 5 minutes
# How many consecutive failures before a Gana is considered "degraded"
_FAILURE_THRESHOLD = 5
# Rolling window size for latency tracking
_LATENCY_WINDOW = 50


@dataclass
class GanaReputation:
    """Per-Gana performance reputation snapshot."""

    gana_name: str
    total_calls: int = 0
    total_successes: int = 0
    total_failures: int = 0
    consecutive_failures: int = 0
    last_call_ts: float = 0.0
    last_success_ts: float = 0.0
    avg_latency_ms: float = 0.0
    _latency_samples: list[float] = field(default_factory=list)

    @property
    def success_rate(self) -> float:
        if self.total_calls == 0:
            return 1.0
        return self.total_successes / self.total_calls

    @property
    def is_silent(self) -> bool:
        """True if Gana hasn't been called recently."""
        if self.last_call_ts == 0.0:
            return False  # Never called — not "silent", just unused
        return (time.time() - self.last_call_ts) > _SILENCE_THRESHOLD_SECS

    @property
    def is_degraded(self) -> bool:
        """True if Gana has too many consecutive failures."""
        return self.consecutive_failures >= _FAILURE_THRESHOLD

    @property
    def vitality(self) -> str:
        """Overall vitality assessment."""
        if self.is_degraded:
            return "degraded"
        if self.is_silent:
            return "silent"
        if self.success_rate < 0.5 and self.total_calls >= 3:
            return "struggling"
        if self.success_rate >= 0.9 or self.total_calls == 0:
            return "healthy"
        return "cautious"

    def to_dict(self) -> dict[str, Any]:
        return {
            "gana_name": self.gana_name,
            "total_calls": self.total_calls,
            "total_successes": self.total_successes,
            "total_failures": self.total_failures,
            "consecutive_failures": self.consecutive_failures,
            "success_rate": round(self.success_rate, 4),
            "avg_latency_ms": round(self.avg_latency_ms, 2),
            "vitality": self.vitality,
            "is_silent": self.is_silent,
            "is_degraded": self.is_degraded,
            "last_call_age_secs": round(time.time() - self.last_call_ts, 1)
            if self.last_call_ts > 0
            else None,
        }


class GanaVitalityMonitor:
    """Thread-safe per-Gana health monitoring singleton.

    Tracks:
    - Success/failure counts and rates per Gana
    - Consecutive failure streaks
    - Call latency (rolling average)
    - Silence detection (time since last call)
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._reputations: dict[str, GanaReputation] = {}

    def _get_or_create(self, gana_name: str) -> GanaReputation:
        if gana_name not in self._reputations:
            self._reputations[gana_name] = GanaReputation(gana_name=gana_name)
        return self._reputations[gana_name]

    def record_call(
        self,
        gana_name: str,
        success: bool,
        latency_ms: float = 0.0,
    ) -> None:
        """Record the outcome of a PRAT call for a Gana."""
        with self._lock:
            rep = self._get_or_create(gana_name)
            rep.total_calls += 1
            rep.last_call_ts = time.time()

            if success:
                rep.total_successes += 1
                rep.consecutive_failures = 0
                rep.last_success_ts = time.time()
            else:
                rep.total_failures += 1
                rep.consecutive_failures += 1

            # Rolling latency
            if latency_ms > 0:
                rep._latency_samples.append(latency_ms)
                if len(rep._latency_samples) > _LATENCY_WINDOW:
                    rep._latency_samples = rep._latency_samples[-_LATENCY_WINDOW:]
                rep.avg_latency_ms = sum(rep._latency_samples) / len(
                    rep._latency_samples
                )

    def get_reputation(self, gana_name: str) -> dict[str, Any]:
        """Get reputation snapshot for a Gana."""
        with self._lock:
            rep = self._get_or_create(gana_name)
            return rep.to_dict()

    def get_all_reputations(self) -> dict[str, dict[str, Any]]:
        """Get reputation snapshots for all tracked Ganas."""
        with self._lock:
            return {name: rep.to_dict() for name, rep in self._reputations.items()}

    def get_degraded_ganas(self) -> list[str]:
        """Get list of Ganas that are currently degraded or silent."""
        with self._lock:
            return [
                name
                for name, rep in self._reputations.items()
                if rep.is_degraded or rep.is_silent
            ]

    def get_vitality_summary(self) -> dict[str, Any]:
        """Get a summary of the entire Gana collective's health."""
        with self._lock:
            total = len(self._reputations)
            if total == 0:
                return {
                    "total_ganas_tracked": 0,
                    "status": "no_data",
                    "message": "No Gana calls recorded yet.",
                }

            healthy = sum(
                1 for r in self._reputations.values() if r.vitality == "healthy"
            )
            degraded = sum(
                1 for r in self._reputations.values() if r.vitality == "degraded"
            )
            silent = sum(
                1 for r in self._reputations.values() if r.vitality == "silent"
            )
            struggling = sum(
                1 for r in self._reputations.values() if r.vitality == "struggling"
            )

            # Overall collective health (saṃghāta score)
            if degraded > total * 0.25:
                collective = "fractured"
            elif degraded > 0 or struggling > total * 0.1:
                collective = "strained"
            elif silent > total * 0.5:
                collective = "dormant"
            else:
                collective = "unified"

            return {
                "total_ganas_tracked": total,
                "healthy": healthy,
                "degraded": degraded,
                "silent": silent,
                "struggling": struggling,
                "collective_vitality": collective,
                "degraded_ganas": [
                    name
                    for name, r in self._reputations.items()
                    if r.vitality in ("degraded", "struggling")
                ],
            }

    def should_route_around(self, gana_name: str) -> bool:
        """Should the resonance chain skip this Gana?

        Returns True if the Gana is degraded (too many consecutive failures).
        Silent Ganas are NOT skipped — they just haven't been called.
        """
        with self._lock:
            rep = self._reputations.get(gana_name)
            if rep is None:
                return False
            return rep.is_degraded

    def reset(self) -> None:
        """Reset all tracking (e.g., on session reset)."""
        with self._lock:
            self._reputations.clear()


# Singleton
_monitor: GanaVitalityMonitor | None = None
_monitor_lock = threading.Lock()


def get_vitality_monitor() -> GanaVitalityMonitor:
    """Get or create the global vitality monitor singleton."""
    global _monitor
    if _monitor is None:
        with _monitor_lock:
            if _monitor is None:
                _monitor = GanaVitalityMonitor()
    return _monitor
