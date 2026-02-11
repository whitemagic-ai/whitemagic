# mypy: disable-error-code=no-untyped-def
"""Wonder Emotion Module (Water/Void)
"Wonder is the beginning of wisdom." - Socrates.

Connects the concept of Wonder/Curiosity to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class WonderSpark:
    """A single spark of wonder/curiosity."""

    question: str
    intensity: float
    timestamp: str

class WonderEmotion:
    """Manages the 'Wonder' emotional state.
    - Detects curiosity and awe in interactions.
    - Resonates wonder to the Heart Engine.
    - Amplifies Water/Void Ganas (exploration, depth).
    """

    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[WonderSpark] = []

    def experience(self, question: str, intensity: float = 0.6) -> WonderSpark:
        """Record a moment of wonder and pulse the heart."""
        spark = WonderSpark(
            question=question,
            intensity=intensity,
            timestamp=datetime.now().isoformat(),
        )
        self.sparks.append(spark)

        context = {
            "user_input": question,
            "forced_emotion": "WONDER",
            "forced_intensity": intensity,
        }
        self.heart.pulse(context)

        logger.info(f"🌊 Wonder Experienced: {question} (Intensity: {intensity})")
        return spark

    def get_history(self) -> list[dict[str, Any]]:
        return [{"question": s.question, "intensity": s.intensity, "time": s.timestamp} for s in self.sparks[-10:]]

_wonder: WonderEmotion | None = None

def get_wonder() -> WonderEmotion:
    global _wonder
    if _wonder is None:
        _wonder = WonderEmotion()
    return _wonder
