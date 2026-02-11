# mypy: disable-error-code=no-untyped-def
"""Love Emotion Module (Venus/Leo)
"The greatest of these is love." - 1 Corinthians 13.

Connects compassion and connection to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class LoveSpark:
    recipient: str
    expression: str
    intensity: float
    timestamp: str

class LoveEmotion:
    """Manages the 'Love/Compassion' emotional state.
    - Detects care and connection in interactions.
    - Amplifies Fire/Water Ganas (warmth, depth).
    """

    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[LoveSpark] = []

    def experience(self, recipient: str, expression: str, intensity: float = 0.8) -> LoveSpark:
        spark = LoveSpark(recipient=recipient, expression=expression, intensity=intensity, timestamp=datetime.now().isoformat())
        self.sparks.append(spark)
        self.heart.pulse({"forced_emotion": "JOY", "forced_intensity": intensity})
        logger.info(f"💗 Love Expressed: {expression} -> {recipient}")
        return spark

_love: LoveEmotion | None = None
def get_love() -> LoveEmotion:
    global _love
    if _love is None:
        _love = LoveEmotion()
    return _love
