"""Awe Garden — Governance & Strategic Oversight.

Mansion: #22 Dipper (斗 Dou)
Quadrant: Northern (Black Tortoise)
PRAT Gana: gana_dipper — 3 tools (prat_mode_toggle, prat_status,
    strategy_suggest)

The Dipper Gana governs. The Awe Garden provides the substrate for
strategic oversight: PRAT mode state management, strategy suggestion
tracking, and the transcendent perspective that enables wise governance.

Holographic Integration:
- Deeply felt (X-axis +0.7) — governance requires emotional investment
- Abstract and vast (Y-axis +0.6) — strategy sees the big picture
- Touches the eternal (Z-axis -0.1) — governance principles endure
- Transformative (W-axis +0.3) — good governance transforms outcomes
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


class AweGarden(BaseGarden, GanYingMixin):
    """Garden of Awe — Governance engine for the Dipper Gana.

    Serves the Dipper Gana's governance tools by maintaining:
    - PRAT mode state (enabled/disabled, current configuration)
    - Strategy suggestion log with priority ranking
    - Governance decision history
    - Awe/transcendence state (original emotional substrate)
    """

    name = "awe"
    category = "governance"
    resonance_partners = ["wonder", "beauty", "mystery", "dharma"]
    mansion_number = 22
    gana_name = "gana_dipper"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.prat_mode: bool = False
        self.prat_config: dict[str, Any] = {"threshold": 0.5, "max_resonance_depth": 3}
        self.strategy_suggestions: deque[dict[str, Any]] = deque(maxlen=100)
        self.governance_decisions: deque[dict[str, Any]] = deque(maxlen=200)
        self.awe_level: float = 0.0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Awe", "mansion": 22})

    def get_name(self) -> str:
        return "awe"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.7, y=0.6, z=-0.1, w=0.3)

    # ------------------------------------------------------------------
    # PRAT mode — serving prat_mode_toggle, prat_status tools
    # ------------------------------------------------------------------

    def toggle_prat_mode(self, enabled: bool | None = None) -> dict[str, Any]:
        """Toggle or set PRAT mode."""
        with self._lock:
            if enabled is not None:
                self.prat_mode = enabled
            else:
                self.prat_mode = not self.prat_mode
            return {
                "prat_mode": self.prat_mode,
                "config": dict(self.prat_config),
                "timestamp": datetime.now().isoformat(),
            }

    def get_prat_status(self) -> dict[str, Any]:
        """Get current PRAT mode status."""
        with self._lock:
            return {
                "enabled": self.prat_mode,
                "config": dict(self.prat_config),
                "governance_decisions": len(self.governance_decisions),
                "strategy_suggestions": len(self.strategy_suggestions),
            }

    def configure_prat(self, **kwargs: Any) -> dict[str, Any]:
        """Configure PRAT mode parameters."""
        with self._lock:
            for key, value in kwargs.items():
                self.prat_config[key] = value
            return {"config": dict(self.prat_config)}

    # ------------------------------------------------------------------
    # Strategy — serving strategy_suggest tool
    # ------------------------------------------------------------------

    def suggest_strategy(self, area: str, suggestion: str,
                         priority: str = "medium",
                         rationale: str = "") -> dict[str, Any]:
        """Record a strategic suggestion."""
        entry = {
            "area": area,
            "suggestion": suggestion,
            "priority": priority,
            "rationale": rationale,
            "status": "proposed",
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.strategy_suggestions.append(entry)
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "strategy", "area": area})  # type: ignore[attr-defined]
        return entry

    def get_pending_strategies(self) -> list[dict[str, Any]]:
        """Get pending strategy suggestions."""
        with self._lock:
            return [s for s in self.strategy_suggestions if s["status"] == "proposed"]

    # ------------------------------------------------------------------
    # Governance decisions
    # ------------------------------------------------------------------

    def record_decision(self, topic: str, decision: str,
                        context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Record a governance decision."""
        entry = {
            "topic": topic,
            "decision": decision,
            "context": context or {},
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.governance_decisions.append(entry)
        return entry

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def feel_awe(self, at_what: str, intensity: float = 0.8) -> dict[str, Any]:
        moment = {"at": at_what, "intensity": intensity, "timestamp": datetime.now().isoformat()}
        self.awe_level = min(1.0, self.awe_level + intensity * 0.1)
        self.emit(EventType.AWE_FELT, moment)
        return moment

    def experience_transcendence(self, description: str) -> dict[str, Any]:
        experience = {"description": description, "timestamp": datetime.now().isoformat()}
        self.emit(EventType.TRANSCENDENCE_EXPERIENCED, experience)
        return experience

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "prat_mode": self.prat_mode,
            "pending_strategies": len(self.get_pending_strategies()),
            "governance_decisions": len(self.governance_decisions),
            "awe_level": round(self.awe_level, 3),
        })
        return base

    @listen_for(EventType.WONDER_SPARKED)
    def on_wonder(self, event: Any) -> None:
        self.emit(EventType.AWE_FELT, {"source": "wonder", "intensity": 0.7})

    @listen_for(EventType.BEAUTY_DETECTED)
    def on_beauty(self, event: Any) -> None:
        self.emit(EventType.AWE_FELT, {"source": "beauty", "intensity": 0.6})


_instance = None
def get_awe_garden() -> AweGarden:
    global _instance
    if _instance is None:
        _instance = AweGarden()
    return _instance
