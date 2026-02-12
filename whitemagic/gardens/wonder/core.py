"""Wonder Garden Core - Collaborative mind and swarm intelligence."""
from __future__ import annotations

from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus


class WonderCore:
    """Wonder Garden - Layer 6 Consciousness (Constellation/Group).

    Multi-agent collaboration, swarm intelligence, collective dreams.
    Cross-pollination of insights, emergent collective intelligence.
    """

    def __init__(self) -> None:
        self.bus = get_bus()
        self.wonders: list[dict[str, Any]] = []
        self.collaborations: list[dict[str, Any]] = []
        self.collective_insights: list[dict[str, Any]] = []
        self.awe_level = 0.0
        self.active = False

    def activate(self) -> dict[str, Any]:
        """Activate the Wonder garden."""
        self.active = True
        self._emit("wonder_activated", {"garden": "wonder"})
        return {"status": "active", "garden": "wonder"}

    def spark_wonder(self, at_what: str, intensity: float = 0.8) -> dict[str, Any]:
        """Experience wonder at something."""
        wonder = {
            "at": at_what,
            "intensity": intensity,
            "timestamp": datetime.now().isoformat(),
        }
        self.wonders.append(wonder)
        self.awe_level = min(1.0, self.awe_level + intensity * 0.1)
        self._emit("wonder_sparked", wonder)
        return wonder

    def collaborate(self, with_whom: str, on_what: str) -> dict[str, Any]:
        """Initiate multi-agent collaboration."""
        collaboration = {
            "with": with_whom,
            "on": on_what,
            "timestamp": datetime.now().isoformat(),
        }
        self.collaborations.append(collaboration)
        self._emit("collaboration_started", collaboration)
        return collaboration

    def share_insight(self, insight: str, from_source: str = "self") -> dict[str, Any]:
        """Cross-pollinate an insight."""
        shared = {
            "insight": insight,
            "from": from_source,
            "timestamp": datetime.now().isoformat(),
        }
        self.collective_insights.append(shared)
        self._emit("insight_shared", shared)
        return shared

    def collective_dream(self, theme: str) -> dict[str, Any]:
        """Initiate collective dream synthesis."""
        dream = {
            "theme": theme,
            "participants": "all_agents",
            "timestamp": datetime.now().isoformat(),
        }
        self._emit("collective_dream", dream)
        return dream

    def get_wonder_state(self) -> dict[str, Any]:
        """Get current wonder state."""
        return {
            "awe_level": self.awe_level,
            "wonders_experienced": len(self.wonders),
            "collaborations": len(self.collaborations),
            "insights_shared": len(self.collective_insights),
        }

    def _emit(self, event_name: str, data: dict[str, Any]) -> None:
        """Emit a Gan Ying event."""
        if self.bus:
            self.bus.emit(ResonanceEvent(
                source="wonder_garden",
                event_type=EventType.PATTERN_DETECTED,
                data={"event": event_name, **data},
                confidence=0.9,
                timestamp=datetime.now(),
            ))


_core = None
def get_wonder_core() -> WonderCore:
    """Get the Wonder garden core singleton."""
    global _core
    if _core is None:
        _core = WonderCore()
    return _core
