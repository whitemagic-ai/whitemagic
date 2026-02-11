"""🗣️ System Narrator - The Voice of WhiteMagic
Translates system events into a coherent narrative stream.
"""

from __future__ import annotations

import logging
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.core.continuity import log_event
from whitemagic.core.resonance.gan_ying import EventType, get_bus
from whitemagic.gardens.voice.narrative import get_narrative_engine
from whitemagic.gardens.voice.templates import get_template

logger = logging.getLogger(__name__)

class Narrator:
    """The Narrator listens to the Gan Ying Bus and weaves events into stories.
    """

    def __init__(self, base_dir: Path | None = None):
        self.bus = get_bus()
        self.engine = get_narrative_engine() # Uses narrative.py singleton
        self._listening = False
        self.current_story = "Session-" + datetime.now().strftime("%Y-%m-%d")
        self.current_chapter = "Stream"

        # Ensure story/chapter exist
        self._init_story()

    def _init_story(self) -> None:
        """Initialize current story structure."""
        if not self.engine.get_story(self.current_story):
            self.engine.create_story(self.current_story, theme="System Operation")

        if not self.engine.get_chapter(self.current_chapter):
            self.engine.add_chapter(self.current_story, self.current_chapter)

    def start(self) -> None:
        """Start listening to events."""
        if self._listening:
            return

        # Register for ALL events (wildcard)
        # Note: GanYingBus doesn't support wildcard yet, so we listen to specific ones
        # or we modify GanYingBus to support it.
        # For now, let's listen to key events.

        key_events = [
            EventType.MEMORY_CREATED,
            EventType.MEMORY_RECALLED,
            EventType.PATTERN_DETECTED,
            EventType.ORACLE_CAST,
            EventType.SYSTEM_STARTED,
            EventType.CLONE_SEARCH_COMPLETE,
            EventType.PATTERN_CONFIRMED,
            EventType.PATTERN_REJECTED,
            EventType.VOICE_EXPRESSED,  # Listen to its own garden
            EventType.STORY_TOLD,
        ]

        for et in key_events:
            self.bus.listen(et, self._on_event)

        self._listening = True
        logger.info("🗣️ Narrator started. Listening for events...")

    def narrate(self, prompt: str) -> str:
        """Generate a narrative response to a prompt.

        Args:
            prompt: The text prompt to narrate about

        Returns:
            Generated narrative text

        """
        # Add the prompt as a narrative entry
        narrative_text = f"Reflecting on: {prompt}"

        self.engine.add_entry(
            story=self.current_story,
            chapter=self.current_chapter,
            text=narrative_text,
            context={"prompt": prompt, "type": "user_requested"},
        )

        # Log the narration request
        log_event(
            source="narrator",
            event_type="voice_narrate_requested",
            data={
                "prompt": prompt,
                "story": self.current_story,
                "chapter": self.current_chapter,
            },
        )

        return narrative_text

    def _on_event(self, event: Any) -> None:
        """Handle incoming event."""
        try:
            text = get_template(event.event_type, event.data)

            # 1. Add to local narrative engine (Voice Garden)
            self.engine.add_entry(
                story=self.current_story,
                chapter=self.current_chapter,
                text=text,
                context=event.data,
            )

            # 2. Log to Continuity Event Log (Cross-Interface)
            log_event(
                source="narrator",
                event_type=event.event_type.value,
                data={
                    "text": text,
                    "original_data": event.data,
                    "story": self.current_story,
                    "chapter": self.current_chapter,
                },
            )

            # Real-time output (optional)
            # print(f"🗣️ {text}")

        except Exception as e:
            logger.info(f"⚠️ Narrator error: {e}")

# Singleton
_narrator = None

def get_narrator() -> Narrator:
    global _narrator
    if _narrator is None:
        _narrator = Narrator()
    return _narrator
