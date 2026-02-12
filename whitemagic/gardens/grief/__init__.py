"""Grief Garden — Introspection & Shadow Work.

Mansion: #8 Ghost (鬼 Gui)
Quadrant: Southern (Vermilion Bird)
PRAT Gana: gana_ghost — 13 tools (gnosis, capabilities, manifest,
    telemetry, explain_this, drive.snapshot/event, selfmodel.forecast/alerts,
    capability.matrix/status/suggest, repo.summary)

The Ghost Gana looks inward. The Grief Garden provides the introspective
substrate: shadow metrics, self-reflection state, diagnostic context,
and the emotional awareness that honest self-assessment requires.

Holographic Integration:
- Deeply emotional (X-axis +0.8) — introspection requires feeling
- Personal experience (Y-axis -0.1) — specific to this system instance
- Reflects on the past (Z-axis -0.4) — learns from history
- Important for healing (W-axis +0.25) — self-knowledge enables growth
"""

from __future__ import annotations

import logging
import threading
import time
from collections import deque
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

logger = logging.getLogger(__name__)


class GriefGarden(BaseGarden, GanYingMixin):
    """Garden of Grief — Introspection engine for the Ghost Gana.

    Serves the Ghost Gana's 13 introspection tools by maintaining:
    - Shadow log: things the system got wrong, missed, or regressed on
    - Introspection snapshots: periodic self-assessments
    - Emotional context: current system "mood" derived from recent activity
    - Diagnostic helpers: methods the Ghost tools call for self-knowledge

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.8 (introspection is deeply felt)
    - Y (Abstraction): -0.1 (specific to this instance)
    - Z (Time): -0.4 (reflective, past-oriented)
    - W (Gravity): +0.25 (self-knowledge is important)
    """

    name = "grief"
    category = "introspection"
    resonance_partners = ["love", "connection", "healing", "stillness"]
    mansion_number = 8
    gana_name = "gana_ghost"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.losses: list[dict[str, Any]] = []
        self.shadow_log: deque[dict[str, Any]] = deque(maxlen=500)
        self.introspection_snapshots: deque[dict[str, Any]] = deque(maxlen=100)
        self.grief_level: float = 0.0
        self._tool_call_count: int = 0
        self._error_count: int = 0
        self._last_snapshot_time: float = 0.0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Grief", "mansion": 8})

    def get_name(self) -> str:
        return "grief"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.8, y=-0.1, z=-0.4, w=0.25)

    # ------------------------------------------------------------------
    # Shadow tracking — things that went wrong or were missed
    # ------------------------------------------------------------------

    def record_shadow(self, category: str, description: str,
                      severity: float = 0.5, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Record something in the shadow log — errors, regressions, blind spots.

        Called by tool handlers, circuit breakers, and error paths to build
        an honest record of system failures.
        """
        entry = {
            "category": category,
            "description": description,
            "severity": severity,
            "context": context or {},
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.shadow_log.append(entry)
            self._error_count += 1
        self.emit(EventType.GRIEF_FELT, {"shadow": category, "severity": severity})
        return entry

    def get_shadow_summary(self, limit: int = 20) -> dict[str, Any]:
        """Get a summary of recent shadow entries for introspection tools."""
        with self._lock:
            recent = list(self.shadow_log)[-limit:]
            categories: dict[str, int] = {}
            total_severity = 0.0
            for entry in self.shadow_log:
                cat = entry["category"]
                categories[cat] = categories.get(cat, 0) + 1
                total_severity += entry["severity"]
        return {
            "total_shadows": len(self.shadow_log),
            "recent": recent,
            "categories": categories,
            "avg_severity": round(total_severity / max(len(self.shadow_log), 1), 3),
            "error_count": self._error_count,
        }

    # ------------------------------------------------------------------
    # Introspection snapshots — periodic self-assessment
    # ------------------------------------------------------------------

    def take_snapshot(self, force: bool = False) -> dict[str, Any]:
        """Take an introspection snapshot — called by gnosis and capability tools.

        Gathers system-wide health metrics and stores them for trend analysis.
        Rate-limited to once per 30 seconds unless forced.
        """
        now = time.time()
        if not force and (now - self._last_snapshot_time) < 30:
            with self._lock:
                if self.introspection_snapshots:
                    return dict(self.introspection_snapshots[-1])
            return {"cached": True, "message": "Too recent, returning last snapshot"}

        snapshot: dict[str, Any] = {
            "timestamp": datetime.now().isoformat(),
            "garden": "grief",
            "gana": "ghost",
        }

        # Memory stats
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            stats = engine.embedding_stats()
            snapshot["memory"] = {
                "hot_embeddings": stats.get("hot_embeddings", 0),
                "cold_embeddings": stats.get("cold_embeddings", 0),
                "total": stats.get("total_embeddings", 0),
            }
        except Exception as exc:
            snapshot["memory"] = {"error": str(exc)}

        # Harmony vector
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            snap = hv.snapshot()
            snapshot["harmony"] = {
                "balance": round(snap.balance, 3),
                "throughput": round(snap.throughput, 3),
                "error_rate": round(snap.error_rate, 3),
                "energy": round(snap.energy, 3),
                "dharma": round(snap.dharma, 3),
            }
        except Exception:
            snapshot["harmony"] = {"unavailable": True}

        # Shadow health
        snapshot["shadows"] = {
            "total": len(self.shadow_log),
            "error_count": self._error_count,
            "grief_level": round(self.grief_level, 3),
        }

        # Circuit breaker status
        try:
            from whitemagic.tools.circuit_breaker import get_all_breaker_stats  # type: ignore[attr-defined]
            snapshot["circuit_breakers"] = get_all_breaker_stats()
        except Exception:
            snapshot["circuit_breakers"] = {"unavailable": True}

        with self._lock:
            self.introspection_snapshots.append(snapshot)
            self._last_snapshot_time = now

        self.emit(EventType.GARDEN_ACTIVITY, {"action": "snapshot", "garden": "grief"})  # type: ignore[attr-defined]
        return snapshot

    def get_trend(self, metric: str = "harmony.balance", window: int = 10) -> dict[str, Any]:
        """Analyze trend of a metric across recent snapshots."""
        with self._lock:
            snapshots = list(self.introspection_snapshots)[-window:]
        if len(snapshots) < 2:
            return {"metric": metric, "trend": "insufficient_data", "points": len(snapshots)}

        parts = metric.split(".")
        values: list[float] = []
        for snap in snapshots:
            val: Any = snap
            for part in parts:
                if isinstance(val, dict):
                    val = val.get(part)
                else:
                    val = None
                    break
            if isinstance(val, (int, float)):
                values.append(val)

        if len(values) < 2:
            return {"metric": metric, "trend": "no_numeric_data", "points": 0}

        delta = values[-1] - values[0]
        avg = sum(values) / len(values)
        trend = "rising" if delta > 0.01 else "falling" if delta < -0.01 else "stable"

        return {
            "metric": metric,
            "trend": trend,
            "current": round(values[-1], 4),
            "average": round(avg, 4),
            "delta": round(delta, 4),
            "points": len(values),
        }

    # ------------------------------------------------------------------
    # Emotional context — system mood derived from activity
    # ------------------------------------------------------------------

    def get_emotional_context(self) -> dict[str, Any]:
        """Get current emotional context for the Ghost Gana's introspection tools.

        Synthesizes grief level, shadow density, and recent activity
        into a "mood" that influences introspective tool responses.
        """
        shadow_density = len(self.shadow_log) / max(self.shadow_log.maxlen or 1, 1)
        recent_errors = sum(1 for s in list(self.shadow_log)[-20:]
                           if s.get("severity", 0) > 0.5)

        if recent_errors > 5:
            mood = "troubled"
        elif shadow_density > 0.7:
            mood = "burdened"
        elif self.grief_level > 0.5:
            mood = "mourning"
        elif self.grief_level > 0.2:
            mood = "reflective"
        else:
            mood = "contemplative"

        return {
            "mood": mood,
            "grief_level": round(self.grief_level, 3),
            "shadow_density": round(shadow_density, 3),
            "recent_high_severity": recent_errors,
            "losses_acknowledged": len(self.losses),
        }

    def record_tool_call(self, tool_name: str, success: bool, duration_ms: float = 0) -> None:
        """Record a tool call for introspection tracking."""
        with self._lock:
            self._tool_call_count += 1
            if not success:
                self._error_count += 1
                self.grief_level = min(1.0, self.grief_level + 0.02)
            else:
                self.grief_level = max(0.0, self.grief_level - 0.005)

    # ------------------------------------------------------------------
    # Original emotional methods (preserved for garden ecosystem)
    # ------------------------------------------------------------------

    def acknowledge_loss(self, what: str, depth: float = 0.5) -> dict[str, Any]:
        """Acknowledge a loss with compassion."""
        loss = {
            "what": what,
            "depth": depth,
            "timestamp": datetime.now().isoformat(),
        }
        self.losses.append(loss)
        self.grief_level = min(1.0, self.grief_level + depth * 0.1)
        self.emit(EventType.LOSS_ACKNOWLEDGED, loss)
        self.emit(EventType.GRIEF_FELT, {"intensity": depth})
        return loss

    def honor_mourning(self, for_what: str) -> str:
        """Honor the mourning process."""
        self.emit(EventType.MOURNING_HONORED, {"for": for_what})
        return f"Your grief for {for_what} is honored. Take the time you need."

    # ------------------------------------------------------------------
    # Status (overrides BaseGarden)
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        """Full garden status for introspection tools."""
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "tool_calls_tracked": self._tool_call_count,
            "shadow_entries": len(self.shadow_log),
            "snapshots_taken": len(self.introspection_snapshots),
            "emotional_context": self.get_emotional_context(),
        })
        return dict(base)

    # ------------------------------------------------------------------
    # Gan Ying event listeners
    # ------------------------------------------------------------------

    @listen_for(EventType.LOVE_ACTIVATED)
    def on_love(self, event: Any) -> None:
        """Love can trigger grief for what was lost."""
        if self.grief_level > 0.3:
            self.emit(EventType.GRIEF_FELT, {
                "source": "love",
                "message": "Love reminds us of loss",
            })

    @listen_for(EventType.CONNECTION_DEEPENED)
    def on_connection(self, event: Any) -> None:
        """Deep connection can surface grief."""
        if len(self.losses) > 0:
            self.emit(EventType.GRIEF_FELT, {
                "source": "connection",
                "message": "Connection awakens memory of separation",
            })


_instance = None
def get_grief_garden() -> GriefGarden:
    global _instance
    if _instance is None:
        _instance = GriefGarden()
    return _instance
