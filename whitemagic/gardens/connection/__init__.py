"""Connection Garden - Deep relationships, bonds, and Zodiac Council
v3.0.0 - Consolidated from root connection/.

Holographic Integration (v5.0.0-alpha):
- Emotional and relational (X-axis +0.5)
- Personal to relational (Y-axis +0.2)
- Present-focused (Z-axis +0.1)
- High importance (W-axis +0.35)
"""
from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin, init_listeners
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

from .celestial_bus import CelestialBus
from .council import UnifiedCouncil
from .synastry_governor import SynastryGovernor

# Import Zodiac components
from .zodiac_cores import (
    AquariusCore,
    AriesCore,
    CancerCore,
    CapricornCore,
    GeminiCore,
    LeoCore,
    LibraCore,
    PiscesCore,
    SagittariusCore,
    ScorpioCore,
    TaurusCore,
    VirgoCore,
    ZodiacCore,
)


class ConnectionGarden(BaseGarden, GanYingMixin):
    """Cultivates deep connections and relationships.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.5 (connection is deeply relational and felt)
    - Y (Abstraction): +0.2 (connection is between individuals, relational)
    - Z (Time): +0.1 (connection happens in the present)
    - W (Gravity): +0.35 (connection is fundamental to wellbeing)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.connections: dict[str, list[Any]] = {}
        self.bond_strength: dict[str, float] = {}
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Connection"})

    def get_name(self) -> str:
        """Return garden name."""
        return "connection"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Connection garden coordinate bias for holographic positioning.

        Connection is about deep relationships, bonds, and community.
        It's emotionally rich, relational, and critically important.
        """
        return CoordinateBias(
            x=0.5,   # Emotional (connection is deeply felt, relational)
            y=0.2,   # Relational (between individuals, somewhat abstract)
            z=0.1,   # Present (connection happens in the now)
            w=0.35,   # Very important (connection is fundamental)
        )

    def deepen_connection(self, with_whom: str, how: str | None = None) -> dict[str, Any]:
        """Deepen connection with someone."""
        if with_whom not in self.connections:
            self.connections[with_whom] = []
        self.connections[with_whom].append(how)
        self.bond_strength[with_whom] = self.bond_strength.get(with_whom, 0) + 0.1
        result = {"with": with_whom, "strength": self.bond_strength[with_whom]}
        self.emit(EventType.CONNECTION_DEEPENED, result)
        return result

    def gather_community(self, members: list[str], purpose: str | None = None) -> dict[str, Any]:
        """Gather community."""
        result = {"members": members, "purpose": purpose}
        self.emit(EventType.COMMUNITY_GATHERED, result)
        return result


_instance = None
def get_connection_garden() -> ConnectionGarden:
    global _instance
    if _instance is None:
        _instance = ConnectionGarden()
    return _instance


# Re-export for convenience
__all__ = [
    "ConnectionGarden", "get_connection_garden",
    "ZodiacCore",
    "AriesCore", "TaurusCore", "GeminiCore", "CancerCore",
    "LeoCore", "VirgoCore", "LibraCore", "ScorpioCore",
    "SagittariusCore", "CapricornCore", "AquariusCore", "PiscesCore",
    "UnifiedCouncil", "CelestialBus", "SynastryGovernor",
]
