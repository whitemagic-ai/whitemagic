"""Harmony Vector — Multi-Dimensional System Balance
===================================================
Inspired by MandalaOS's Lakshmi Subsystem and Tiferet Engine.

The Harmony Vector is a compact, real-time summary of Whitemagic's holistic
health across multiple orthogonal dimensions.  It replaces the simple
Yin/Yang counter with a richer model that any AI caller can read via a
single tool call and use to self-regulate.

Dimensions tracked:
  - **balance**   : Yin/Yang activity ratio (action vs reflection)
  - **throughput** : Tool calls per minute (rolling window)
  - **latency**    : p50/p95 tool response times (rolling window)
  - **error_rate** : Fraction of tool calls returning errors
  - **dharma**     : Ethical score from the Dharma System
  - **karma_debt** : Cumulative declared-vs-actual side-effect mismatch
  - **energy**     : Composite resource pressure (memory, CPU proxy)

Each dimension is normalized to [0.0, 1.0] where 1.0 = perfect health.
The overall ``harmony_score`` is a weighted mean of all dimensions.

The vector is updated on every ``call_tool()`` invocation and can be
queried cheaply at any time.

Usage:
    from whitemagic.harmony.vector import get_harmony_vector
    hv = get_harmony_vector()
    snapshot = hv.snapshot()   # dict safe for JSON / MCP envelope
"""

from __future__ import annotations

import logging
import threading
import time
from collections import deque
from dataclasses import asdict, dataclass
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Guna classification (MandalaOS concept)
# ---------------------------------------------------------------------------

class Guna(str, Enum):
    """Process temperament tags inspired by Vedic gunas."""

    SATTVIC = "sattvic"    # Balanced, interactive, light
    RAJASIC = "rajasic"    # Active, CPU-heavy, write-intensive
    TAMASIC = "tamasic"    # Idle, blocked, read-only

    @classmethod
    def classify_tool(cls, tool_name: str, safety: str = "READ") -> Guna:
        """Auto-classify a tool invocation by name and declared safety."""
        # Write/delete tools are Rajasic (active, mutating)
        if safety in ("WRITE", "DELETE"):
            return cls.RAJASIC
        # Read-only introspection tools are Sattvic (balanced, observing)
        _sattvic_prefixes = (
            "get_", "list_", "search_", "read_", "capabilities",
            "manifest", "state.", "repo.", "ship.", "health",
            "archaeology_find", "archaeology_stats", "archaeology_report",
            "ganying_history", "ganying_listeners", "resonance_trace",
            "watcher_status", "watcher_list", "watcher_stats",
            "governor_stats", "governor_check",
            "get_yin_yang", "get_ethical", "get_dharma",
            "cluster_stats", "pattern_search", "token_report",
            "edge_stats", "rust_status", "sangha_lock_list",
            "scratchpad_", "view_",
        )
        if any(tool_name.startswith(p) for p in _sattvic_prefixes):
            return cls.SATTVIC
        # Default: Rajasic (doing something)
        return cls.RAJASIC


# ---------------------------------------------------------------------------
# Rolling window for time-series metrics
# ---------------------------------------------------------------------------

@dataclass
class _ToolEvent:
    """A single tool invocation record."""

    tool: str
    guna: str
    timestamp: float
    duration_s: float
    success: bool
    declared_safety: str
    actual_writes: int        # count of writes in the response
    karma_mismatch: bool      # declared safety != actual writes pattern


class _RollingWindow:
    """Thread-safe rolling window of ToolEvents."""

    def __init__(self, window_seconds: float = 300.0, max_events: int = 2000):
        self._lock = threading.Lock()
        self._events: deque[_ToolEvent] = deque(maxlen=max_events)
        self._window = window_seconds

    def push(self, event: _ToolEvent) -> None:
        with self._lock:
            self._events.append(event)

    def recent(self) -> list[_ToolEvent]:
        cutoff = time.time() - self._window
        with self._lock:
            return [e for e in self._events if e.timestamp >= cutoff]


# ---------------------------------------------------------------------------
# Harmony Vector
# ---------------------------------------------------------------------------

# Dimension weights for the composite harmony score
_DEFAULT_WEIGHTS: dict[str, float] = {
    "balance": 0.15,
    "throughput": 0.10,
    "latency": 0.15,
    "error_rate": 0.20,
    "dharma": 0.15,
    "karma_debt": 0.15,
    "energy": 0.10,
}


@dataclass
class HarmonySnapshot:
    """Immutable snapshot of the Harmony Vector — safe for JSON serialization."""

    # Per-dimension scores (0.0–1.0, higher is healthier)
    balance: float = 1.0
    throughput: float = 1.0
    latency: float = 1.0
    error_rate: float = 1.0
    dharma: float = 1.0
    karma_debt: float = 1.0
    energy: float = 1.0

    # Composite
    harmony_score: float = 1.0

    # Guna distribution in current window
    guna_sattvic_pct: float = 0.0
    guna_rajasic_pct: float = 0.0
    guna_tamasic_pct: float = 0.0

    # Raw counters (informational)
    tool_calls_in_window: int = 0
    errors_in_window: int = 0
    karma_mismatches_in_window: int = 0
    p50_latency_ms: float = 0.0
    p95_latency_ms: float = 0.0
    window_seconds: float = 300.0

    timestamp: str = ""

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


class HarmonyVector:
    """The living pulse of Whitemagic — a compact, multi-dimensional health metric.

    Updated on every tool call via ``record_call()``.  Query at any time via
    ``snapshot()`` which is a cheap, lock-free read of the latest computation.
    """

    def __init__(
        self,
        window_seconds: float = 300.0,
        weights: dict[str, float] | None = None,
    ):
        self._window = _RollingWindow(window_seconds=window_seconds)
        self._window_seconds = window_seconds
        self._weights = weights or dict(_DEFAULT_WEIGHTS)

        # Normalize weights so they sum to 1.0
        total = sum(self._weights.values())
        if total > 0:
            self._weights = {k: v / total for k, v in self._weights.items()}

        # Accumulated karma debt (persists beyond window)
        self._karma_debt_total: float = 0.0
        self._karma_debt_lock = threading.Lock()

        # Latest snapshot (cheap reads)
        self._latest: HarmonySnapshot = HarmonySnapshot(
            timestamp=datetime.now().isoformat(),
        )

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def record_call(
        self,
        tool_name: str,
        duration_s: float,
        success: bool,
        declared_safety: str = "READ",
        actual_writes: int = 0,
    ) -> HarmonySnapshot:
        """Record a tool invocation and recompute the Harmony Vector."""
        guna = Guna.classify_tool(tool_name, declared_safety)

        # Karma mismatch: declared READ but produced writes, or vice versa
        karma_mismatch = False
        if declared_safety == "READ" and actual_writes > 0:
            karma_mismatch = True
        if declared_safety in ("WRITE", "DELETE") and actual_writes == 0 and success:
            # Declared write but did nothing — not harmful, just noise
            pass

        event = _ToolEvent(
            tool=tool_name,
            guna=guna.value,
            timestamp=time.time(),
            duration_s=duration_s,
            success=success,
            declared_safety=declared_safety,
            actual_writes=actual_writes,
            karma_mismatch=karma_mismatch,
        )
        self._window.push(event)

        if karma_mismatch:
            with self._karma_debt_lock:
                self._karma_debt_total += 1.0

        # Recompute snapshot
        self._latest = self._compute()

        # Push to StateBoard (shared-memory blackboard for Leap 7 Nervous System)
        self._push_to_state_board(self._latest)

        # Feed anomaly detector
        try:
            from whitemagic.harmony.anomaly_detector import get_anomaly_detector
            get_anomaly_detector().ingest({
                "balance": self._latest.balance,
                "throughput": self._latest.throughput,
                "latency": self._latest.latency,
                "error_rate": self._latest.error_rate,
                "dharma": self._latest.dharma,
                "karma_debt": self._latest.karma_debt,
                "energy": self._latest.energy,
            })
        except Exception:
            pass

        return self._latest

    def snapshot(self) -> HarmonySnapshot:
        """Return the most recently computed snapshot (lock-free read)."""
        return self._latest

    def snapshot_dict(self) -> dict[str, Any]:
        """Convenience: snapshot as a plain dict for JSON / envelope."""
        return self._latest.to_dict()

    def decay_karma_debt(self, amount: float = 0.1) -> None:
        """Gradually forgive karma debt (called periodically or on good behavior)."""
        with self._karma_debt_lock:
            self._karma_debt_total = max(0.0, self._karma_debt_total - amount)

    # ------------------------------------------------------------------
    # StateBoard coupling (Leap 7 Nervous System)
    # ------------------------------------------------------------------

    def _push_to_state_board(self, snap: HarmonySnapshot) -> None:
        """Push the 7 harmony dimensions to the shared-memory StateBoard.

        This makes the Harmony Vector readable at <100ns by Zig dispatch,
        Rust EventRing consumers, and any mmap reader — bypassing Python/GIL.
        """
        try:
            from whitemagic.core.acceleration.state_board_bridge import get_state_board
            board = get_state_board()
            board.write_harmony(
                balance=snap.balance,
                throughput=snap.throughput,
                latency=snap.latency,
                error_rate=snap.error_rate,
                dharma=snap.dharma,
                karma_debt=snap.karma_debt,
                energy=snap.energy,
            )
        except Exception:
            pass  # StateBoard is optional — Python path always works

    # ------------------------------------------------------------------
    # Galactic Map coupling (A4 synthesis)
    # ------------------------------------------------------------------

    def _galactic_vitality(self) -> float:
        """Compute memory vitality from Galactic Map zone distribution.

        Vitality is higher when memories are concentrated in inner zones
        (CORE, INNER_RIM) and lower when they drift to outer zones.

        Zone weights:
          CORE=1.0, INNER_RIM=0.8, MID_BAND=0.5, OUTER_RIM=0.2, FAR_EDGE=0.05

        Returns 0.0–1.0 (1.0 = all memories in CORE).
        Falls back to 0.7 (neutral) if the Galactic Map is unavailable.
        """
        try:
            from whitemagic.core.memory.galactic_map import get_galactic_map
            gmap = get_galactic_map()
            zone_counts = gmap.get_zone_counts()
            if not zone_counts:
                return 0.7

            zone_weights = {
                "core": 1.0,
                "inner_rim": 0.8,
                "mid_band": 0.5,
                "outer_rim": 0.2,
                "far_edge": 0.05,
            }

            total = sum(zone_counts.values())
            if total == 0:
                return 0.7

            weighted_sum = sum(
                zone_weights.get(zone, 0.3) * count
                for zone, count in zone_counts.items()
            )
            return min(1.0, max(0.0, weighted_sum / total))
        except Exception:
            return 0.7  # Neutral fallback

    # ------------------------------------------------------------------
    # Internals
    # ------------------------------------------------------------------

    def _compute(self) -> HarmonySnapshot:
        events = self._window.recent()
        now_iso = datetime.now().isoformat()

        if not events:
            return HarmonySnapshot(timestamp=now_iso, window_seconds=self._window_seconds)

        total = len(events)
        errors = sum(1 for e in events if not e.success)
        karma_mismatches = sum(1 for e in events if e.karma_mismatch)

        # --- Guna distribution ---
        guna_counts: dict[str, int] = {"sattvic": 0, "rajasic": 0, "tamasic": 0}
        for e in events:
            guna_counts[e.guna] = guna_counts.get(e.guna, 0) + 1
        sattvic_pct = guna_counts["sattvic"] / total
        rajasic_pct = guna_counts["rajasic"] / total
        tamasic_pct = guna_counts.get("tamasic", 0) / total

        # --- Balance dimension ---
        # Optimal: ~40-60% sattvic.  Extreme skew either way is imbalance.
        sattvic_pct + tamasic_pct  # "yin" side
        rajasic_ratio = rajasic_pct                        # "yang" side
        deviation = abs(rajasic_ratio - 0.5)
        balance_score = max(0.0, 1.0 - deviation * 2.0)

        # --- Throughput dimension ---
        # Healthy: 1-60 calls/min.  Very high = spam, very low = stall.
        window_minutes = self._window_seconds / 60.0
        calls_per_min = total / max(window_minutes, 0.01)
        if calls_per_min < 1.0:
            throughput_score = 0.5  # quiet but okay
        elif calls_per_min <= 60.0:
            throughput_score = 1.0
        else:
            throughput_score = max(0.2, 1.0 - (calls_per_min - 60.0) / 200.0)

        # --- Latency dimension ---
        durations = sorted(e.duration_s for e in events)
        p50 = durations[len(durations) // 2]
        p95_idx = min(int(len(durations) * 0.95), len(durations) - 1)
        p95 = durations[p95_idx]

        # Healthy p95 < 2s, degrade smoothly to 0.2 at 10s
        if p95 <= 0.5:
            latency_score = 1.0
        elif p95 <= 2.0:
            latency_score = 1.0 - (p95 - 0.5) * 0.2
        else:
            latency_score = max(0.2, 0.7 - (p95 - 2.0) * 0.0625)

        # --- Error rate dimension ---
        error_rate = errors / total
        error_score = max(0.0, 1.0 - error_rate * 5.0)  # 20% errors → 0.0

        # --- Dharma dimension ---
        try:
            from whitemagic.dharma import get_dharma_system
            dharma_score = get_dharma_system(with_audit=False).get_ethical_score()
        except Exception:
            dharma_score = 1.0  # No violations is fine

        # --- Karma debt dimension ---
        with self._karma_debt_lock:
            debt = self._karma_debt_total
        # Score degrades as debt accumulates: 10 mismatches → 0.0
        karma_score = max(0.0, 1.0 - debt / 10.0)

        # --- Energy dimension ---
        # Blends two signals:
        #   (1) Slow-call ratio (runtime pressure)
        #   (2) Galactic Map zone vitality (memory health) — A4 synthesis
        slow_calls = sum(1 for e in events if e.duration_s > 1.0)
        slow_ratio = slow_calls / total
        runtime_energy = max(0.0, 1.0 - slow_ratio * 2.0)

        galactic_vitality = self._galactic_vitality()
        # Blend: 60% runtime, 40% galactic vitality
        energy_score = 0.6 * runtime_energy + 0.4 * galactic_vitality

        # --- Composite ---
        dimensions = {
            "balance": balance_score,
            "throughput": throughput_score,
            "latency": latency_score,
            "error_rate": error_score,
            "dharma": dharma_score,
            "karma_debt": karma_score,
            "energy": energy_score,
        }
        harmony = sum(
            dimensions[k] * self._weights.get(k, 0.0) for k in dimensions
        )

        return HarmonySnapshot(
            balance=round(balance_score, 4),
            throughput=round(throughput_score, 4),
            latency=round(latency_score, 4),
            error_rate=round(error_score, 4),
            dharma=round(dharma_score, 4),
            karma_debt=round(karma_score, 4),
            energy=round(energy_score, 4),
            harmony_score=round(harmony, 4),
            guna_sattvic_pct=round(sattvic_pct, 4),
            guna_rajasic_pct=round(rajasic_pct, 4),
            guna_tamasic_pct=round(tamasic_pct, 4),
            tool_calls_in_window=total,
            errors_in_window=errors,
            karma_mismatches_in_window=karma_mismatches,
            p50_latency_ms=round(p50 * 1000, 2),
            p95_latency_ms=round(p95 * 1000, 2),
            window_seconds=self._window_seconds,
            timestamp=now_iso,
        )


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_harmony_vector: HarmonyVector | None = None
_hv_lock = threading.Lock()


def get_harmony_vector() -> HarmonyVector:
    """Get the global Harmony Vector instance."""
    global _harmony_vector
    if _harmony_vector is None:
        with _hv_lock:
            if _harmony_vector is None:
                _harmony_vector = HarmonyVector()
                logger.info("Harmony Vector initialized (5-min rolling window)")
    return _harmony_vector
