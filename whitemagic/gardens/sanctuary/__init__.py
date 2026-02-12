"""Sanctuary Garden — Resource Isolation & Sandboxing.

Mansion: #4 Room (房 Fang)
Quadrant: Eastern (Azure Dragon)
PRAT Gana: gana_room — 4 tools (sangha_lock, sandbox.set_limits,
    sandbox.status, sandbox.violations)

The Room Gana encloses. The Sanctuary Garden provides the substrate for
resource isolation: sandbox limits, lock management, violation tracking,
and the safety that allows agents to operate within protected boundaries.

Holographic Integration:
- Nurturing (X-axis +0.3) — safety is emotionally comforting
- Balanced (Y-axis 0.0) — both specific and universal
- Present (Z-axis 0.0) — safety is here and now
- Essential (W-axis +0.3) — boundaries enable trust
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


class SanctuaryGarden(BaseGarden, GanYingMixin):
    """Garden of Sanctuary — Resource isolation engine for the Room Gana.

    Serves the Room Gana's sandbox tools by maintaining:
    - Sandbox resource limits (memory, CPU, file access)
    - Active lock registry for resource coordination
    - Violation log for boundary breaches
    - Safety metrics for monitoring
    """

    name = "sanctuary"
    category = "isolation"
    resonance_partners = ["healing", "dharma", "presence", "protection"]
    mansion_number = 4
    gana_name = "gana_room"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.sandbox_limits: dict[str, Any] = {
            "max_memory_mb": 512,
            "max_file_ops": 1000,
            "allowed_paths": [],
            "blocked_paths": ["/etc", "/root", "/sys"],
            "max_concurrent_tools": 10,
        }
        self.active_locks: dict[str, dict[str, Any]] = {}
        self.violations: deque[dict[str, Any]] = deque(maxlen=500)
        self.safety_level: float = 0.7
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Sanctuary", "mansion": 4})

    def get_name(self) -> str:
        return "sanctuary"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.3, y=0.0, z=0.0, w=0.3)

    # ------------------------------------------------------------------
    # Sandbox limits — serving sandbox.set_limits, sandbox.status tools
    # ------------------------------------------------------------------

    def set_limits(self, **kwargs: Any) -> dict[str, Any]:
        """Set sandbox resource limits."""
        with self._lock:
            for key, value in kwargs.items():
                if key in self.sandbox_limits:
                    self.sandbox_limits[key] = value
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "limits_set", "limits": kwargs})  # type: ignore[attr-defined]
        return {"limits": dict(self.sandbox_limits)}

    def get_sandbox_status(self) -> dict[str, Any]:
        """Get current sandbox status."""
        with self._lock:
            return {
                "limits": dict(self.sandbox_limits),
                "active_locks": len(self.active_locks),
                "violations_total": len(self.violations),
                "safety_level": round(self.safety_level, 3),
                "locks": {k: {"agent": v["agent_id"], "reason": v["reason"]}
                          for k, v in self.active_locks.items()},
            }

    # ------------------------------------------------------------------
    # Lock management — serving sangha_lock tool
    # ------------------------------------------------------------------

    def acquire_lock(self, resource_id: str, agent_id: str,
                     reason: str = "", ttl_seconds: int = 300) -> dict[str, Any]:
        """Acquire a lock on a resource."""
        with self._lock:
            if resource_id in self.active_locks:
                holder = self.active_locks[resource_id]
                return {"acquired": False, "held_by": holder["agent_id"],
                        "reason": holder["reason"]}
            lock = {
                "resource_id": resource_id,
                "agent_id": agent_id,
                "reason": reason,
                "acquired_at": datetime.now().isoformat(),
                "ttl_seconds": ttl_seconds,
            }
            self.active_locks[resource_id] = lock
        self.emit(EventType.SAFETY_ESTABLISHED, {"lock": resource_id, "agent": agent_id})
        return {"acquired": True, "lock": lock}

    def release_lock(self, resource_id: str, agent_id: str) -> dict[str, Any]:
        """Release a lock on a resource."""
        with self._lock:
            lock = self.active_locks.get(resource_id)
            if not lock:
                return {"released": False, "error": "Lock not found"}
            if lock["agent_id"] != agent_id:
                return {"released": False, "error": f"Lock held by {lock['agent_id']}"}
            del self.active_locks[resource_id]
        return {"released": True, "resource": resource_id}

    # ------------------------------------------------------------------
    # Violation tracking — serving sandbox.violations tool
    # ------------------------------------------------------------------

    def record_violation(self, violation_type: str, description: str,
                         agent_id: str = "", severity: float = 0.5) -> dict[str, Any]:
        """Record a sandbox violation."""
        entry = {
            "type": violation_type,
            "description": description,
            "agent_id": agent_id,
            "severity": severity,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.violations.append(entry)
            self.safety_level = max(0.0, self.safety_level - severity * 0.05)
        self.emit(EventType.THREAT_DETECTED, entry)
        return entry

    def get_violations(self, limit: int = 20) -> list[dict[str, Any]]:
        """Get recent violations."""
        with self._lock:
            return list(self.violations)[-limit:]

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def enter_sanctuary(self, where: str = "inner space") -> dict[str, Any]:
        sanctuary = {"where": where, "entered": datetime.now().isoformat()}
        self.emit(EventType.SANCTUARY_ENTERED, sanctuary)
        return sanctuary

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "sandbox": self.get_sandbox_status(),
        })
        return base

    @listen_for(EventType.HEALING_INITIATED)
    def on_healing(self, event: Any) -> None:
        self.emit(EventType.SANCTUARY_ENTERED, {"source": "healing", "where": "healing space"})

    @listen_for(EventType.THREAT_DETECTED)
    def on_threat(self, event: Any) -> None:
        self.emit(EventType.SAFETY_ESTABLISHED, {"source": "threat_response", "for": "protection"})


_instance = None
def get_sanctuary_garden() -> SanctuaryGarden:
    global _instance
    if _instance is None:
        _instance = SanctuaryGarden()
    return _instance
