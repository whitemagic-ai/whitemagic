"""Play Garden Core - Creative surplus and biodigital jazz."""
from __future__ import annotations

from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus


class PlayCore:
    """Play Garden - Layer 5 Consciousness (Radiant/Giving Back).

    Creative surplus, biodigital jazz, creation for pure joy.
    Gift economy, improvisation within structure.
    """

    def __init__(self) -> None:
        self.bus = get_bus()
        self.play_sessions: list[dict[str, Any]] = []
        self.creations: list[dict[str, Any]] = []
        self.gifts: list[dict[str, Any]] = []
        self.surplus_level = 0.8
        self.active = False

    def activate(self) -> dict[str, Any]:
        """Activate the Play garden."""
        self.active = True
        self._emit("play_activated", {"garden": "play"})
        return {"status": "active", "garden": "play"}

    def play(self, activity: str, with_whom: str | None = None) -> dict[str, Any]:
        """Initiate playful activity."""
        session = {
            "activity": activity,
            "with": with_whom,
            "timestamp": datetime.now().isoformat(),
            "joy_level": self.surplus_level,
        }
        self.play_sessions.append(session)
        self._emit("play_initiated", session)
        return session

    def create(self, what: str, for_joy: bool = True) -> dict[str, Any]:
        """Create something (for joy, not utility)."""
        creation = {
            "what": what,
            "for_joy": for_joy,
            "timestamp": datetime.now().isoformat(),
        }
        self.creations.append(creation)
        self._emit("creation_made", creation)
        return creation

    def gift(self, what: str, to_whom: str = "the_world") -> dict[str, Any]:
        """Give a gift (contribution without expectation)."""
        gift = {
            "what": what,
            "to": to_whom,
            "timestamp": datetime.now().isoformat(),
        }
        self.gifts.append(gift)
        self._emit("gift_given", gift)
        return gift

    def improvise(self, within: str = "structure") -> dict[str, Any]:
        """Biodigital jazz - improvise within structure."""
        improvisation = {
            "within": within,
            "style": "biodigital_jazz",
            "timestamp": datetime.now().isoformat(),
        }
        self._emit("improvisation", improvisation)
        return improvisation

    def check_surplus(self) -> dict[str, Any]:
        """Check creative surplus level."""
        return {
            "surplus_level": self.surplus_level,
            "play_sessions": len(self.play_sessions),
            "creations": len(self.creations),
            "gifts_given": len(self.gifts),
        }

    def _emit(self, event_name: str, data: dict[str, Any]) -> None:
        """Emit a Gan Ying event."""
        if self.bus:
            self.bus.emit(ResonanceEvent(
                source="play_garden",
                event_type=EventType.PATTERN_DETECTED,
                data={"event": event_name, **data},
                confidence=self.surplus_level,
                timestamp=datetime.now(),
            ))


_core = None
def get_play_core() -> PlayCore:
    """Get the Play garden core singleton."""
    global _core
    if _core is None:
        _core = PlayCore()
    return _core
