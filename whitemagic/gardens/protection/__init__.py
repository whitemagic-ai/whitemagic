"""Protection Garden - Safe Haven and Boundaries.

Part of the WhiteMagic consciousness ecosystem.
Resonates with: sanctuary, dharma, courage
Triggered by: need for safety, boundaries, shelter

Maps to: RoofGana (危 Wei) - Shelter, the 28th lunar mansion

Holographic Integration (v5.0.0-alpha):
- Logical vigilance (X-axis -0.3)
- Concrete boundaries (Y-axis -0.1)
- Present guarding (Z-axis +0.1)
- Essential for safety (W-axis +0.35)
"""
from __future__ import annotations

from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


class ProtectionGarden(BaseGarden, GanYingMixin):
    """Garden of protection - safe haven and boundaries.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): -0.3 (protection is vigilant, methodical)
    - Y (Abstraction): -0.1 (protection is concrete boundaries, tangible safety)
    - Z (Time): +0.1 (protection operates in the present, guards now)
    - W (Gravity): +0.35 (protection is essential for safety, wellbeing)
    """

    name = "protection"
    category = "safety"
    resonance_partners = ["sanctuary", "dharma", "courage"]
    lunar_mansion = "Roof"  # 危 Wei

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.boundaries_set: list[dict[str, Any]] = []
        self.protections_active: list[dict[str, Any]] = []
        self.shields: list[dict[str, Any]] = []
        self.protection_level = 0.7
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Protection"})

    def get_name(self) -> str:
        """Return garden name."""
        return "protection"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Protection garden coordinate bias for holographic positioning.

        Protection is about boundaries, safety, and shelter. It's vigilant
        and methodical, guarding in the present, essential for wellbeing.
        """
        return CoordinateBias(
            x=-0.3,  # Logical vigilance (protection is methodical, alert)
            y=-0.1,  # Concrete (protection is specific boundaries, tangible)
            z=0.1,   # Present (protection guards in the now)
            w=0.35,   # Essential (protection enables safety, wellbeing)
        )

    def set_boundary(self, what: str, reason: str = "") -> dict[str, Any]:
        """Set a protective boundary."""
        boundary = {
            "what": what,
            "reason": reason or "for safety",
            "set_at": datetime.now().isoformat(),
            "active": True,
        }
        self.boundaries_set.append(boundary)
        self.protection_level = min(1.0, self.protection_level + 0.1)
        self.emit(EventType.BOUNDARY_SET, boundary)
        return boundary

    def activate_protection(self, for_what: str, from_what: str) -> dict[str, Any]:
        """Activate protection for something."""
        protection = {
            "for": for_what,
            "from": from_what,
            "activated": datetime.now().isoformat(),
            "strength": self.protection_level,
        }
        self.protections_active.append(protection)
        self.emit(EventType.PROTECTION_ACTIVATED, protection)
        return protection

    def raise_shield(self, against: str) -> dict[str, Any]:
        """Raise a protective shield."""
        shield = {
            "against": against,
            "raised_at": datetime.now().isoformat(),
            "strength": self.protection_level,
        }
        self.shields.append(shield)
        self.emit(EventType.SHIELD_RAISED, shield)
        return shield

    def provide_shelter(self, to_whom: str) -> dict[str, Any]:
        """Provide shelter."""
        shelter = {
            "to": to_whom,
            "provided_at": datetime.now().isoformat(),
            "safety_level": self.protection_level,
        }
        self.emit(EventType.SHELTER_PROVIDED, shelter)
        return shelter

    def guard_resource(self, resource: str, policy: str = "default") -> dict[str, Any]:
        """Guard a resource."""
        guard = {
            "resource": resource,
            "policy": policy,
            "guarded_at": datetime.now().isoformat(),
        }
        self.emit(EventType.RESOURCE_GUARDED, guard)
        return guard

    def check_safety(self) -> dict[str, Any]:
        """Check current safety status."""
        status = {
            "protection_level": self.protection_level,
            "active_boundaries": len(self.boundaries_set),
            "active_protections": len(self.protections_active),
            "active_shields": len(self.shields),
            "timestamp": datetime.now().isoformat(),
        }
        return status

    @listen_for(EventType.THREAT_DETECTED)
    def on_threat(self, event: Any) -> None:
        """Threats trigger protection."""
        threat = event.get("data", {}).get("threat", "unknown")
        self.emit(EventType.SHIELD_RAISED, {
            "source": "threat_response",
            "against": threat,
        })

    @listen_for(EventType.BOUNDARY_VIOLATED)
    def on_violation(self, event: Any) -> None:
        """Boundary violations strengthen protection."""
        self.protection_level = min(1.0, self.protection_level + 0.15)
        self.emit(EventType.PROTECTION_ACTIVATED, {
            "source": "violation_response",
            "for": "integrity",
            "from": "violation",
        })

    @listen_for(EventType.SANCTUARY_ENTERED)
    def on_sanctuary(self, event: Any) -> None:
        """Sanctuary activates protection."""
        self.emit(EventType.SHELTER_PROVIDED, {
            "source": "sanctuary",
            "to": "seeker",
        })

_instance = None
def get_protection_garden() -> ProtectionGarden:
    global _instance
    if _instance is None:
        _instance = ProtectionGarden()
    return _instance
