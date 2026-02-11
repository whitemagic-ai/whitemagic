"""Mystery Garden — Pattern Capture & Association Filtering.

Mansion: #19 Net (毕 Bi)
Quadrant: Western (White Tiger)
PRAT Gana: gana_net — 4 tools (detect_patterns, filter_associations,
    mine_associations, constellation_detect)

The Net Gana captures. The Mystery Garden provides the substrate for
pattern discovery: tracking detected patterns, filtering association
results, constellation detection state, and the embrace of the unknown
that enables finding what you weren't looking for.

Holographic Integration:
- Balanced contemplation (X-axis 0.0) — pattern work is analytical yet intuitive
- Abstract (Y-axis +0.5) — patterns emerge from abstraction
- Timeless (Z-axis 0.0) — patterns recur across time
- Important for growth (W-axis +0.2) — understanding patterns enables insight
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


class MysteryGarden(BaseGarden, GanYingMixin):
    """Garden of Mystery — Pattern capture engine for the Net Gana.

    Serves the Net Gana's pattern tools by maintaining:
    - Detected pattern registry with confidence scores
    - Association mining results and filter history
    - Constellation detection log
    - Serendipity tracker for unexpected discoveries
    """

    name = "mystery"
    category = "pattern_capture"
    resonance_partners = ["wisdom", "connection", "transformation"]
    mansion_number = 19
    gana_name = "gana_net"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.detected_patterns: deque[dict[str, Any]] = deque(maxlen=200)
        self.association_results: deque[dict[str, Any]] = deque(maxlen=100)
        self.constellations_found: deque[dict[str, Any]] = deque(maxlen=50)
        self.serendipities: list[dict[str, Any]] = []
        self._total_patterns: int = 0
        self._total_associations_mined: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Mystery", "mansion": 19})

    def get_name(self) -> str:
        return "mystery"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.0, y=0.5, z=0.0, w=0.2)

    # ------------------------------------------------------------------
    # Pattern detection — serving detect_patterns tool
    # ------------------------------------------------------------------

    def record_pattern(self, pattern_type: str, description: str,
                       confidence: float = 0.7,
                       evidence: list[str] | None = None) -> dict[str, Any]:
        """Record a detected pattern."""
        entry = {
            "type": pattern_type,
            "description": description,
            "confidence": confidence,
            "evidence": evidence or [],
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.detected_patterns.append(entry)
            self._total_patterns += 1
        self.emit(EventType.PATTERN_DETECTED, {"type": pattern_type, "confidence": confidence})
        return entry

    def get_recent_patterns(self, pattern_type: str | None = None,
                            limit: int = 20) -> list[dict[str, Any]]:
        """Get recently detected patterns, optionally filtered by type."""
        with self._lock:
            patterns = list(self.detected_patterns)
        if pattern_type:
            patterns = [p for p in patterns if p["type"] == pattern_type]
        return patterns[-limit:]

    # ------------------------------------------------------------------
    # Association mining — serving mine_associations, filter_associations
    # ------------------------------------------------------------------

    def record_mining_run(self, count: int, new_associations: int,
                          method: str = "standard") -> dict[str, Any]:
        """Record an association mining run."""
        entry = {
            "count": count,
            "new_associations": new_associations,
            "method": method,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.association_results.append(entry)
            self._total_associations_mined += new_associations
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "mine", "new": new_associations})  # type: ignore[attr-defined]
        return entry

    def get_mining_summary(self) -> dict[str, Any]:
        """Get summary of association mining activity."""
        with self._lock:
            runs = list(self.association_results)
        return {
            "total_runs": len(runs),
            "total_associations_mined": self._total_associations_mined,
            "latest": runs[-1] if runs else None,
        }

    # ------------------------------------------------------------------
    # Constellation detection — serving constellation_detect tool
    # ------------------------------------------------------------------

    def record_constellation(self, name: str, member_count: int,
                             centroid: dict[str, Any] | None = None) -> dict[str, Any]:
        """Record a detected constellation."""
        entry = {
            "name": name,
            "member_count": member_count,
            "centroid": centroid or {},
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.constellations_found.append(entry)
        return entry

    def get_constellations(self) -> list[dict[str, Any]]:
        """Get all detected constellations."""
        with self._lock:
            return list(self.constellations_found)

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def encounter_mystery(self, what: str, depth: str = "unknown") -> dict[str, Any]:
        mystery = {"what": what, "depth": depth}
        self.serendipities.append(mystery)
        self.emit(EventType.MYSTERY_EMBRACED, mystery)
        return mystery

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "total_patterns": self._total_patterns,
            "total_associations_mined": self._total_associations_mined,
            "constellations": len(self.constellations_found),
            "serendipities": len(self.serendipities),
        })
        return base

    @listen_for(EventType.WONDER_SPARKED)
    def on_wonder(self, event: Any) -> None:
        self.emit(EventType.MYSTERY_EMBRACED, {"revealed_by": "wonder"})


_instance = None
def get_mystery_garden() -> MysteryGarden:
    global _instance
    if _instance is None:
        _instance = MysteryGarden()
    return _instance
