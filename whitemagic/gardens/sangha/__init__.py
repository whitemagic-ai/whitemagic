"""Sangha Garden - Community and collective wisdom.

Holographic Integration (v5.0.0-alpha):
- Warmly connecting (X-axis +0.4)
- Relational space (Y-axis +0.3)
- Present gathering (Z-axis 0.0)
- Fundamental to flourishing (W-axis +0.35)
"""
from __future__ import annotations

from typing import Any

from whitemagic.core.governance.zodiac_council import ZodiacCouncil, get_council
from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias
from whitemagic.gardens.sangha.collective_memory import CollectiveMemory, get_collective
from whitemagic.gardens.sangha.community_dharma import (
    CommunityDharma,
    get_community_dharma,
)
from whitemagic.gardens.sangha.pattern_federation import (
    PatternFederation,
    get_federation,
)
from whitemagic.gardens.sangha.session_handoff import SessionHandoff, get_handoff


class SanghaGarden(BaseGarden, GanYingMixin):
    """Cultivates community and collective wisdom.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.4 (sangha is warmly relational, connecting)
    - Y (Abstraction): +0.3 (sangha is the relational space between individuals)
    - Z (Time): 0.0 (sangha gathers in the present moment)
    - W (Gravity): +0.35 (sangha is fundamental to human flourishing)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.community_members: list[Any] = []
        self.collective_insights: list[dict[str, Any]] = []
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Sangha"})

    def get_name(self) -> str:
        """Return garden name."""
        return "sangha"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Sangha garden coordinate bias for holographic positioning.

        Sangha is community and collective wisdom. It's warmly relational,
        the space between individuals, fundamental to flourishing.
        """
        return CoordinateBias(
            x=0.4,   # Warmly relational (sangha is emotionally connecting)
            y=0.3,   # Relational space (between individuals, collective)
            z=0.0,   # Present (sangha gathers in the now)
            w=0.35,   # Fundamental (community enables human flourishing)
        )

    def gather(self, members: Any, intention: Any = None) -> dict[str, Any]:
        """Gather the sangha."""
        gathering = {"members": members, "intention": intention}
        self.collective_insights.append(gathering)
        self.emit(EventType.COMMUNITY_GATHERED, gathering)
        return gathering

    def share_wisdom(self, insight: Any, from_whom: Any = None) -> dict[str, Any]:
        """Share wisdom with community."""
        result = {"insight": insight, "from": from_whom}
        self.emit(EventType.COLLECTIVE_WISDOM, result)
        return result

    @listen_for(EventType.CONNECTION_DEEPENED)
    def on_connection(self, event: Any) -> None:
        """Connection strengthens sangha."""
        self.emit(EventType.COMMUNITY_GATHERED, {"note": "Connections form sangha"})

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        """Wisdom shared with sangha."""
        self.share_wisdom(event.data, "collective")

_instance = None
def get_sangha_garden() -> SanghaGarden:
    global _instance
    if _instance is None:
        _instance = SanghaGarden()
    return _instance

__all__ = [
    "CollectiveMemory",
    "CommunityDharma",
    "ZodiacCouncil",
    "PatternFederation",
    "SessionHandoff",
    "SanghaGarden",
    "get_collective",
    "get_community_dharma",
    "get_council",
    "get_federation",
    "get_handoff",
    "get_sangha_garden",
]
