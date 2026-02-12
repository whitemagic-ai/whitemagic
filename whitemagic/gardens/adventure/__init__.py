"""Adventure Garden — Export, Deploy & Expansion.

Mansion: #12 Wings (翼 Yi)
Quadrant: Southern (Vermilion Bird)
PRAT Gana: gana_wings — 4 tools (export_memories, import_memories,
    deploy_check, publish_package)

The Wings Gana expands. The Adventure Garden provides the substrate for
data portability: export/import coordination, deployment readiness checks,
package publishing state, and the courage to take code beyond boundaries.

Holographic Integration:
- Emotionally exciting (X-axis +0.4) — shipping is thrilling
- Balanced (Y-axis 0.0) — both specific and universal
- Future exploration (Z-axis +0.6) — deploying reaches outward
- Important for growth (W-axis +0.25) — expansion drives impact
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


class AdventureGarden(BaseGarden, GanYingMixin):
    """Garden of Adventure — Export/deploy engine for the Wings Gana.

    Serves the Wings Gana's portability tools by maintaining:
    - Export/import operation log
    - Deploy readiness state tracking
    - Package publish history
    - Discovery log (original emotional substrate)
    """

    name = "adventure"
    category = "portability"
    resonance_partners = ["wonder", "play", "courage", "truth"]
    mansion_number = 12
    gana_name = "gana_wings"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.export_log: deque[dict[str, Any]] = deque(maxlen=100)
        self.import_log: deque[dict[str, Any]] = deque(maxlen=100)
        self.deploy_checks: deque[dict[str, Any]] = deque(maxlen=50)
        self.discoveries: list[dict[str, Any]] = []
        self.adventure_level: float = 0.6
        self._total_exports: int = 0
        self._total_imports: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Adventure", "mansion": 12})

    def get_name(self) -> str:
        return "adventure"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.4, y=0.0, z=0.6, w=0.25)

    # ------------------------------------------------------------------
    # Export/Import — serving export_memories, import_memories tools
    # ------------------------------------------------------------------

    def record_export(self, format: str, count: int, destination: str = "",
                      size_bytes: int = 0) -> dict[str, Any]:
        """Record a memory export operation."""
        entry = {
            "format": format,
            "count": count,
            "destination": destination,
            "size_bytes": size_bytes,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.export_log.append(entry)
            self._total_exports += 1
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "export", "format": format, "count": count})  # type: ignore[attr-defined]
        return entry

    def record_import(self, format: str, count: int, source: str = "") -> dict[str, Any]:
        """Record a memory import operation."""
        entry = {
            "format": format,
            "count": count,
            "source": source,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.import_log.append(entry)
            self._total_imports += 1
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "import", "format": format, "count": count})  # type: ignore[attr-defined]
        return entry

    def get_portability_summary(self) -> dict[str, Any]:
        """Get summary of export/import activity."""
        with self._lock:
            return {
                "total_exports": self._total_exports,
                "total_imports": self._total_imports,
                "recent_exports": list(self.export_log)[-5:],
                "recent_imports": list(self.import_log)[-5:],
            }

    # ------------------------------------------------------------------
    # Deploy checks — serving deploy_check tool
    # ------------------------------------------------------------------

    def record_deploy_check(self, target: str, ready: bool,
                            issues: list[str] | None = None) -> dict[str, Any]:
        """Record a deployment readiness check."""
        check = {
            "target": target,
            "ready": ready,
            "issues": issues or [],
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.deploy_checks.append(check)
        return check

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def begin_adventure(self, what: str, destination: str = "unknown") -> dict[str, Any]:
        adventure = {"what": what, "destination": destination, "started": datetime.now().isoformat()}
        self.emit(EventType.ADVENTURE_BEGUN, adventure)
        return adventure

    def celebrate_discovery(self, what: str) -> dict[str, Any]:
        discovery = {"what": what, "timestamp": datetime.now().isoformat()}
        self.discoveries.append(discovery)
        self.emit(EventType.DISCOVERY_CELEBRATED, discovery)
        return discovery

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "total_exports": self._total_exports,
            "total_imports": self._total_imports,
            "deploy_checks": len(self.deploy_checks),
            "adventure_level": round(self.adventure_level, 3),
        })
        return base

    @listen_for(EventType.WONDER_SPARKED)
    def on_wonder(self, event: Any) -> None:
        self.emit(EventType.ADVENTURE_BEGUN, {"source": "wonder", "what": "wonder-inspired adventure"})

    @listen_for(EventType.PLAY_INITIATED)
    def on_play(self, event: Any) -> None:
        self.emit(EventType.EXPLORATION_INITIATED, {"source": "play", "where": "playful exploration"})


_instance = None
def get_adventure_garden() -> AdventureGarden:
    global _instance
    if _instance is None:
        _instance = AdventureGarden()
    return _instance
