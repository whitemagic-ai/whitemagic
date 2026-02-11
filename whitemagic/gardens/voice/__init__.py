"""Voice Garden - Expression and authentic communication.

Holographic Integration (v5.0.0-alpha):
- Balanced to emotional (X-axis +0.2)
- Personal to relational (Y-axis +0.1)
- Present expression (Z-axis +0.2)
- Important for connection (W-axis +0.25)
"""

from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias
from whitemagic.gardens.voice.narrative import get_narrative_engine
from whitemagic.gardens.voice.narrator import get_narrator
from whitemagic.gardens.voice.voice_synthesis import VoiceSynthesis


class VoiceGarden(BaseGarden, GanYingMixin):
    """Cultivates authentic voice and expression.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.2 (voice involves both thought and feeling)
    - Y (Abstraction): +0.1 (voice connects self to others, relational)
    - Z (Time): +0.2 (voice expresses in the present moment)
    - W (Gravity): +0.25 (authentic voice matters for connection)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.expressions: list[dict[str, Any]] = []
        self.stories_told: list[dict[str, Any]] = []
        self.authenticity = 0.9

        # Initialize engines
        self.narrative = get_narrative_engine()
        self.synthesis = VoiceSynthesis()
        self.narrator = get_narrator()

        # Start narrator listening
        self.narrator.start()

        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Voice"})

    def get_name(self) -> str:
        """Return garden name."""
        return "voice"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Voice garden coordinate bias for holographic positioning.

        Voice is authentic expression and communication. It connects
        thought and feeling, self and other, in the present moment.
        """
        return CoordinateBias(
            x=0.2,   # Slightly emotional (voice involves thought and feeling)
            y=0.1,   # Slightly relational (connects self to others)
            z=0.2,   # Present (voice expresses now)
            w=0.25,   # Important (authentic voice enables connection)
        )

    def activate_voice(self, what: Any, how: str = "authentic") -> dict[str, Any]:
        """Activate voice to express something (Legacy API support)."""
        return self.speak(what, context={"style": how})

    def speak(self, text: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Speak with authentic voice."""
        # 1. Synthesize voice (add personality)
        authentic_text = self.synthesis.generate_in_my_voice(text)

        # 2. Analyze and learn
        self.synthesis.analyze_expression(authentic_text)

        # 3. Emit event
        expression = {
            "text": authentic_text,
            "original": text,
            "context": context or {},
            "timestamp": str(self.narrative.get_recent_entries(limit=1)),
        }
        self.expressions.append(expression)
        self.emit(EventType.VOICE_EXPRESSED, expression)

        # 4. Log to narrative
        self.narrative.add_entry(
            story=self.narrator.current_story,
            chapter=self.narrator.current_chapter,
            text=authentic_text,
            context=context,
        )

        return {"words": len(authentic_text.split()), "text": authentic_text}

    def begin_story(self, title: str, theme: str | None = None) -> dict[str, Any]:
        """Begin a new story."""
        self.narrative.create_story(title, theme)
        self.narrator.current_story = title
        self.narrator.current_chapter = "Beginning"

        event_data = {"title": title, "theme": theme}
        self.stories_told.append(event_data)
        self.emit(EventType.STORY_TOLD, event_data)

        return {"story": title, "status": "started"}

    def begin_chapter(self, name: str) -> dict[str, Any]:
        """Begin a new chapter."""
        try:
            self.narrative.add_chapter(self.narrator.current_story, name)
            self.narrator.current_chapter = name
            return {"success": True, "chapter": name}
        except ValueError as e:
            return {"success": False, "error": str(e)}

    def reflect(self) -> str:
        """Reflect on the current narrative."""
        entries = self.narrative.get_recent_entries(
            story=self.narrator.current_story,
            limit=5,
        )
        if not entries:
            return "Silence... the page is blank."

        reflection = f"Reflecting on '{self.narrator.current_story}':\n"
        for e in entries:
            reflection += f"- {e.get('text', '')[:50]}...\n"

        return reflection

    def get_state(self) -> dict[str, Any]:
        """Get current voice state."""
        return {
            "current_story": self.narrator.current_story,
            "current_chapter": self.narrator.current_chapter,
            "current_focus": "System Operation",
            "actions_taken": len(self.expressions),
            "words_spoken": sum(len(e.get("text", "").split()) for e in self.expressions),
            "authenticity": self.synthesis.measure_authenticity("test string"),
        }

    def get_stats(self) -> dict[str, Any]:
        """Get voice statistics."""
        return {
            "stories": self.narrative.list_stories(),
            "attention_sessions": len(self.narrative.list_chapters()),
            "palace_rooms": 0,  # Placeholder for Memory Palace integration
        }

    def tell_story(self, about: Any, for_whom: Any = None) -> dict[str, Any]:
        """Tell a story (Legacy API support)."""
        self.begin_story(f"Story about {about}", theme=str(for_whom))
        return {"about": about, "for": for_whom}

    @listen_for(EventType.TRUTH_SPOKEN)
    def on_truth(self, event: Any) -> None:
        """Truth wants to be spoken."""
        self.speak(f"Speaking truth: {event.data}")

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        """Wisdom wants to be shared."""
        self.speak(f"Wisdom integrated: {event.data}")

    @listen_for(EventType.HEXAGRAM_CAST)
    def on_hexagram(self, event: Any) -> None:
        """Speak the wisdom of the hexagram."""
        hexagram = event.data.get("hexagram", "unknown")
        wisdom = event.data.get("wisdom", "Ancient wisdom speaks through the pattern")
        self.speak(f"The hexagram {hexagram} reveals: {wisdom}", context={"type": "oracle"})

_instance = None
def get_voice_garden() -> VoiceGarden:
    global _instance
    if _instance is None:
        _instance = VoiceGarden()
    return _instance
