"""Healing Garden — Memory Nourishment & Resource Management.

Mansion: #17 Stomach (胃 Wei)
Quadrant: Western (White Tiger)
PRAT Gana: gana_stomach — 6 tools (galactic_sweep, prune_memories,
    consolidate_memories, optimize_cache, cold_storage_query, archive_to_cold)

The Stomach Gana nourishes. The Healing Garden provides the substrate for
memory lifecycle management: sweeping the galactic map, consolidating
memories, archiving to cold storage, and the restoration that keeps
the memory system healthy and well-nourished.

Holographic Integration:
- Balanced care (X-axis +0.3) — healing involves gentle maintenance
- Universal process (Y-axis 0.0) — all systems need nourishment
- Future-oriented restoration (Z-axis +0.5) — healing looks forward
- Fundamental importance (W-axis +0.3) — memory health is essential
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


class HealingGarden(BaseGarden, GanYingMixin):
    """Garden of Healing — Memory lifecycle engine for the Stomach Gana.

    Serves the Stomach Gana's memory management tools by providing:
    - Galactic sweep orchestration and tracking
    - Memory consolidation state management
    - Cold storage archival coordination
    - Cache optimization helpers
    """

    name = "healing"
    category = "memory_lifecycle"
    resonance_partners = ["love", "grief", "sanctuary", "stillness"]
    mansion_number = 17
    gana_name = "gana_stomach"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.sweep_history: deque[dict[str, Any]] = deque(maxlen=50)
        self.consolidation_log: deque[dict[str, Any]] = deque(maxlen=100)
        self.archive_log: deque[dict[str, Any]] = deque(maxlen=100)
        self.healing_level: float = 0.5
        self._total_swept: int = 0
        self._total_consolidated: int = 0
        self._total_archived: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Healing", "mansion": 17})

    def get_name(self) -> str:
        return "healing"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.3, y=0.0, z=0.5, w=0.3)

    # ------------------------------------------------------------------
    # Galactic sweep — serving galactic_sweep tool
    # ------------------------------------------------------------------

    def record_sweep(self, zone_counts: dict[str, int],
                     promoted: int = 0, demoted: int = 0) -> dict[str, Any]:
        """Record results of a galactic sweep."""
        entry = {
            "zones": zone_counts,
            "promoted": promoted,
            "demoted": demoted,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.sweep_history.append(entry)
            self._total_swept += sum(zone_counts.values())
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "sweep", "demoted": demoted})  # type: ignore[attr-defined]
        return entry

    def get_sweep_summary(self) -> dict[str, Any]:
        """Get summary of sweep history."""
        with self._lock:
            sweeps = list(self.sweep_history)
        if not sweeps:
            return {"total_sweeps": 0, "total_swept": 0}
        latest = sweeps[-1]
        return {
            "total_sweeps": len(sweeps),
            "total_swept": self._total_swept,
            "latest_zones": latest.get("zones", {}),
            "latest_promoted": latest.get("promoted", 0),
            "latest_demoted": latest.get("demoted", 0),
        }

    # ------------------------------------------------------------------
    # Consolidation — serving consolidate_memories, prune_memories tools
    # ------------------------------------------------------------------

    def record_consolidation(self, count: int, method: str = "standard",
                              details: dict[str, Any] | None = None) -> dict[str, Any]:
        """Record a memory consolidation run."""
        entry = {
            "count": count,
            "method": method,
            "details": details or {},
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.consolidation_log.append(entry)
            self._total_consolidated += count
        self.healing_level = min(1.0, self.healing_level + 0.02)
        self.emit(EventType.HEALING_INITIATED, {"action": "consolidation", "count": count})
        return entry

    # ------------------------------------------------------------------
    # Cold storage — serving archive_to_cold, cold_storage_query tools
    # ------------------------------------------------------------------

    def record_archive(self, count: int, criteria: str = "",
                       size_mb: float = 0) -> dict[str, Any]:
        """Record an archival to cold storage."""
        entry = {
            "count": count,
            "criteria": criteria,
            "size_mb": size_mb,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.archive_log.append(entry)
            self._total_archived += count
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "archive", "count": count})  # type: ignore[attr-defined]
        return entry

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def initiate_healing(self, what: str, approach: str = "gentle") -> dict[str, Any]:
        process = {"what": what, "approach": approach, "started": datetime.now().isoformat(), "progress": 0.0}
        self.emit(EventType.HEALING_INITIATED, process)
        return process

    def restore_wholeness(self, aspect: str) -> dict[str, Any]:
        restoration = {"aspect": aspect, "timestamp": datetime.now().isoformat()}
        self.emit(EventType.WHOLENESS_RESTORED, restoration)
        return restoration

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "total_swept": self._total_swept,
            "total_consolidated": self._total_consolidated,
            "total_archived": self._total_archived,
            "healing_level": round(self.healing_level, 3),
            "sweep_summary": self.get_sweep_summary(),
        })
        return base

    @listen_for(EventType.GRIEF_FELT)
    def on_grief(self, event: Any) -> None:
        self.emit(EventType.HEALING_INITIATED, {"source": "grief", "what": "emotional wound"})

    @listen_for(EventType.LOVE_ACTIVATED)
    def on_love(self, event: Any) -> None:
        self.emit(EventType.RECOVERY_PROGRESSED, {"source": "love", "progress": 0.1})


_instance = None
def get_healing_garden() -> HealingGarden:
    global _instance
    if _instance is None:
        _instance = HealingGarden()
    return _instance
