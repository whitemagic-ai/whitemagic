"""Stillness Garden — Emptiness, Defrag & Garden Operations.

Mansion: #25 Void (虚 Xu)
Quadrant: Northern (Black Tortoise)
PRAT Gana: gana_void — 4 tools (galactic_dashboard, garden_status,
    garden_list, defrag_memories)

The Void Gana empties. The Stillness Garden provides the substrate for
system-wide garden status, galactic map overview, memory defragmentation,
and the meditative pause that allows the system to observe itself clearly.

Holographic Integration:
- Deeply calm (X-axis -0.4) — void is meditative
- Abstract formlessness (Y-axis +0.4) — emptiness transcends form
- Maximally present (Z-axis +0.8) — stillness is NOW
- Essential for clarity (W-axis +0.3) — emptiness reveals truth
"""

from __future__ import annotations

import logging
import threading
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


class StillnessGarden(BaseGarden, GanYingMixin):
    """Garden of Stillness — Garden ops and defrag engine for the Void Gana.

    Serves the Void Gana's system overview tools by maintaining:
    - Garden registry with live status for all 28 gardens
    - Galactic dashboard data aggregation
    - Memory defragmentation tracking
    - Meditation/pause state (original emotional substrate)
    """

    name = "stillness"
    category = "garden_ops"
    resonance_partners = ["presence", "mystery", "wisdom", "grief"]
    mansion_number = 25
    gana_name = "gana_void"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.defrag_history: deque[dict[str, Any]] = deque(maxlen=50)
        self.moments_of_stillness: list[dict[str, Any]] = []
        self.stillness_level: float = 0.5
        self._garden_registry: dict[str, Any] = {}
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Stillness", "mansion": 25})

    def get_name(self) -> str:
        return "stillness"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=-0.4, y=0.4, z=0.8, w=0.3)

    # ------------------------------------------------------------------
    # Garden operations — serving garden_status, garden_list tools
    # ------------------------------------------------------------------

    def register_garden(self, name: str, garden_instance: Any) -> None:
        """Register a garden instance for status aggregation."""
        with self._lock:
            self._garden_registry[name] = garden_instance

    def list_gardens(self) -> list[dict[str, Any]]:
        """List all registered gardens with basic info."""
        with self._lock:
            result = []
            for name, garden in self._garden_registry.items():
                info = {"name": name}
                if hasattr(garden, "mansion_number"):
                    info["mansion"] = garden.mansion_number
                if hasattr(garden, "gana_name"):
                    info["gana"] = garden.gana_name
                if hasattr(garden, "category"):
                    info["category"] = garden.category
                result.append(info)
            return sorted(result, key=lambda g: g.get("mansion", 99))

    def get_garden_status(self, name: str) -> dict[str, Any]:
        """Get detailed status of a specific garden."""
        with self._lock:
            garden = self._garden_registry.get(name)
        if not garden:
            return {"error": f"Garden '{name}' not registered"}
        if hasattr(garden, "get_status"):
            return garden.get_status()  # type: ignore[no-any-return]
        return {"name": name, "status": "no_status_method"}

    def get_all_garden_statuses(self) -> dict[str, Any]:
        """Get status of all registered gardens."""
        with self._lock:
            gardens = dict(self._garden_registry)
        statuses = {}
        for name, garden in gardens.items():
            if hasattr(garden, "get_status"):
                try:
                    statuses[name] = garden.get_status()
                except Exception as exc:
                    statuses[name] = {"error": str(exc)}
        return {"gardens": statuses, "total": len(statuses)}

    # ------------------------------------------------------------------
    # Galactic dashboard — serving galactic_dashboard tool
    # ------------------------------------------------------------------

    def galactic_dashboard(self) -> dict[str, Any]:
        """Aggregate data for the galactic dashboard view."""
        dashboard: dict[str, Any] = {
            "timestamp": datetime.now().isoformat(),
            "gardens_registered": len(self._garden_registry),
        }

        # Memory zone counts
        try:
            from whitemagic.core.memory.sqlite_backend import get_pool  # type: ignore[attr-defined]
            pool = get_pool()
            with pool.connection() as conn:
                rows = conn.execute("""
                    SELECT
                        CASE
                            WHEN galactic_distance <= 0.15 THEN 'CORE'
                            WHEN galactic_distance <= 0.40 THEN 'INNER_RIM'
                            WHEN galactic_distance <= 0.65 THEN 'MID_BAND'
                            WHEN galactic_distance <= 0.85 THEN 'OUTER_RIM'
                            ELSE 'FAR_EDGE'
                        END as zone,
                        COUNT(*) as count
                    FROM memories
                    WHERE galactic_distance IS NOT NULL
                    GROUP BY zone
                """).fetchall()
                dashboard["zones"] = {row[0]: row[1] for row in rows}
                dashboard["total_memories"] = sum(row[1] for row in rows)
        except Exception as exc:
            dashboard["zones"] = {"error": str(exc)}

        # Defrag summary
        dashboard["defrag"] = {
            "total_runs": len(self.defrag_history),
            "latest": dict(self.defrag_history[-1]) if self.defrag_history else None,
        }

        return dashboard

    # ------------------------------------------------------------------
    # Defragmentation — serving defrag_memories tool
    # ------------------------------------------------------------------

    def record_defrag(self, memories_processed: int, duplicates_removed: int = 0,
                      space_freed_mb: float = 0) -> dict[str, Any]:
        """Record a memory defragmentation run."""
        entry = {
            "memories_processed": memories_processed,
            "duplicates_removed": duplicates_removed,
            "space_freed_mb": round(space_freed_mb, 2),
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.defrag_history.append(entry)
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "defrag", "processed": memories_processed})  # type: ignore[attr-defined]
        return entry

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def enter_stillness(self, duration_minutes: int = 5) -> dict[str, Any]:
        moment = {"duration_minutes": duration_minutes, "entered": datetime.now().isoformat(),
                  "quality": "receptive emptiness"}
        self.moments_of_stillness.append(moment)
        self.stillness_level = min(1.0, self.stillness_level + 0.1)
        self.emit(EventType.STILLNESS_ENTERED, moment)
        return moment

    def pause(self, before_action: str = "") -> dict[str, Any]:
        pause = {"before": before_action, "timestamp": datetime.now().isoformat(),
                 "wisdom": "In stillness, clarity arises"}
        self.emit(EventType.PAUSE_TAKEN, pause)
        return pause

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "gardens_registered": len(self._garden_registry),
            "defrag_runs": len(self.defrag_history),
            "stillness_level": round(self.stillness_level, 3),
        })
        return base

    @listen_for(EventType.ANOMALY_DETECTED)
    def on_anomaly(self, event: Any) -> None:
        self.emit(EventType.STILLNESS_ENTERED, {"source": "anomaly_response", "duration_minutes": 5})

    @listen_for(EventType.MYSTERY_EMBRACED)
    def on_mystery(self, event: Any) -> None:
        self.emit(EventType.SILENCE_EMBRACED, {"source": "mystery", "reason": "to receive insight"})

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        self.stillness_level = min(1.0, self.stillness_level + 0.05)


_instance = None
def get_stillness_garden() -> StillnessGarden:
    global _instance
    if _instance is None:
        _instance = StillnessGarden()
    return _instance
