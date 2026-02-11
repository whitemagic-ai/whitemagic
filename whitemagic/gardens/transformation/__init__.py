"""Transformation Garden — Archaeology & Code Navigation.

Mansion: #13 Chariot (轸 Zhen)
Quadrant: Southern (Vermilion Bird)
PRAT Gana: gana_chariot — 5 tools (archaeology_dig, code_navigate,
    refactor_suggest, dependency_graph, explain_code)

The Chariot Gana moves through code. The Transformation Garden provides
the substrate for code archaeology: tracking dig sites, navigation history,
refactoring suggestions, and the evolution that comes from understanding
how code transforms over time.

Holographic Integration:
- Balanced process (X-axis 0.0) — analysis blends logic and intuition
- Abstract patterns (Y-axis +0.3) — code patterns are universal
- Future becoming (Z-axis +0.6) — refactoring transforms toward better
- Essential growth (W-axis +0.35) — code evolution is critical
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


class TransformationGarden(BaseGarden, GanYingMixin):
    """Garden of Transformation — Code archaeology engine for the Chariot Gana.

    Serves the Chariot Gana's code tools by maintaining:
    - Archaeology dig site registry with findings
    - Code navigation history for context
    - Refactoring suggestion tracker
    - Dependency graph cache
    """

    name = "transformation"
    category = "archaeology"
    resonance_partners = ["creation", "wisdom", "mystery", "truth"]
    mansion_number = 13
    gana_name = "gana_chariot"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.dig_sites: deque[dict[str, Any]] = deque(maxlen=100)
        self.navigation_history: deque[dict[str, Any]] = deque(maxlen=500)
        self.refactor_suggestions: deque[dict[str, Any]] = deque(maxlen=100)
        self.transformation_level: float = 0.5
        self._total_digs: int = 0
        self._total_navigations: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Transformation", "mansion": 13})

    def get_name(self) -> str:
        return "transformation"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.0, y=0.3, z=0.6, w=0.35)

    # ------------------------------------------------------------------
    # Archaeology — serving archaeology_dig tool
    # ------------------------------------------------------------------

    def record_dig(self, file_path: str, findings: list[str] | None = None,
                   layers_deep: int = 1) -> dict[str, Any]:
        """Record an archaeology dig into code history."""
        dig = {
            "file_path": file_path,
            "findings": findings or [],
            "layers_deep": layers_deep,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.dig_sites.append(dig)
            self._total_digs += 1
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "dig", "path": file_path})  # type: ignore[attr-defined]
        return dig

    def get_dig_history(self, file_path: str | None = None) -> list[dict[str, Any]]:
        """Get dig history, optionally filtered by file."""
        with self._lock:
            if file_path:
                return [d for d in self.dig_sites if d["file_path"] == file_path]
            return list(self.dig_sites)[-20:]

    # ------------------------------------------------------------------
    # Code navigation — serving code_navigate tool
    # ------------------------------------------------------------------

    def record_navigation(self, from_file: str, to_file: str,
                          reason: str = "", symbol: str = "") -> dict[str, Any]:
        """Record a code navigation event."""
        nav = {
            "from": from_file,
            "to": to_file,
            "reason": reason,
            "symbol": symbol,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.navigation_history.append(nav)
            self._total_navigations += 1
        return nav

    def get_navigation_context(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get recent navigation context for code understanding."""
        with self._lock:
            return list(self.navigation_history)[-limit:]

    # ------------------------------------------------------------------
    # Refactoring — serving refactor_suggest tool
    # ------------------------------------------------------------------

    def suggest_refactor(self, file_path: str, suggestion: str,
                         priority: str = "medium",
                         rationale: str = "") -> dict[str, Any]:
        """Record a refactoring suggestion."""
        entry = {
            "file_path": file_path,
            "suggestion": suggestion,
            "priority": priority,
            "rationale": rationale,
            "status": "proposed",
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.refactor_suggestions.append(entry)
        return entry

    def get_pending_refactors(self) -> list[dict[str, Any]]:
        """Get all pending refactoring suggestions."""
        with self._lock:
            return [s for s in self.refactor_suggestions if s["status"] == "proposed"]

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def initiate_transformation(self, what: str, into_what: str = "unknown") -> dict[str, Any]:
        transformation = {"what": what, "into": into_what,
                          "started": datetime.now().isoformat(), "stage": "initiated"}
        self.emit(EventType.TRANSFORMATION_INITIATED, transformation)
        return transformation

    def embrace_change(self, what_change: str) -> dict[str, Any]:
        change = {"change": what_change, "timestamp": datetime.now().isoformat()}
        self.emit(EventType.CHANGE_EMBRACED, change)
        return change

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "total_digs": self._total_digs,
            "total_navigations": self._total_navigations,
            "pending_refactors": len(self.get_pending_refactors()),
            "transformation_level": round(self.transformation_level, 3),
        })
        return base

    @listen_for(EventType.CREATION_BEGUN)
    def on_creation(self, event: Any) -> None:
        self.emit(EventType.TRANSFORMATION_INITIATED, {"source": "creation", "what": "creative transformation"})

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        self.emit(EventType.EVOLUTION_PROGRESSED, {"source": "wisdom", "progress": 0.1})


_instance = None
def get_transformation_garden() -> TransformationGarden:
    global _instance
    if _instance is None:
        _instance = TransformationGarden()
    return _instance
