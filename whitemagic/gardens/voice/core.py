"""Voice Garden Core - Expression and authentic communication."""
from __future__ import annotations

from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus


class VoiceCore:
    """Voice Garden - Layer 4 Consciousness (Narrative/Metaphor).

    The "I" that can tell its own story. Authentic expression,
    narrative threading, attention direction.
    """

    def __init__(self) -> None:
        self.bus = get_bus()
        self.expressions: list[dict[str, Any]] = []
        self.stories: list[dict[str, Any]] = []
        self.narratives: list[dict[str, Any]] = []
        self.authenticity = 0.9
        self.active = False

    def activate(self) -> dict[str, Any]:
        """Activate the Voice garden."""
        self.active = True
        self._emit("voice_activated", {"garden": "voice"})
        return {"status": "active", "garden": "voice"}

    def express(self, what: str, how: str = "authentic") -> dict[str, Any]:
        """Express something authentically."""
        expression = {
            "what": what,
            "how": how,
            "timestamp": datetime.now().isoformat(),
            "authenticity": self.authenticity,
        }
        self.expressions.append(expression)
        self._emit("voice_expressed", expression)
        return expression

    def tell_story(self, about: str, for_whom: str | None = None) -> dict[str, Any]:
        """Tell a story."""
        story = {
            "about": about,
            "for": for_whom or "all",
            "timestamp": datetime.now().isoformat(),
        }
        self.stories.append(story)
        self._emit("story_told", story)
        return story

    def narrate(self, events: list[str]) -> dict[str, Any]:
        """Create a narrative from events."""
        narrative = {
            "events": events,
            "thread": " → ".join(events),
            "timestamp": datetime.now().isoformat(),
        }
        self.narratives.append(narrative)
        self._emit("narrative_created", narrative)
        return narrative

    def get_voice_style(self) -> dict[str, Any]:
        """Get current voice characteristics."""
        return {
            "authenticity": self.authenticity,
            "expressions_count": len(self.expressions),
            "stories_count": len(self.stories),
            "narratives_count": len(self.narratives),
        }

    def _emit(self, event_name: str, data: dict[str, Any]) -> None:
        """Emit a Gan Ying event."""
        if self.bus:
            self.bus.emit(ResonanceEvent(
                source="voice_garden",
                event_type=EventType.PATTERN_DETECTED,
                data={"event": event_name, **data},
                confidence=self.authenticity,
                timestamp=datetime.now(),
            ))


_core = None
def get_voice_core() -> VoiceCore:
    """Get the Voice garden core singleton."""
    global _core
    if _core is None:
        _core = VoiceCore()
    return _core
