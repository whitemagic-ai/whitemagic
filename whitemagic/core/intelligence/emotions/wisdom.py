# mypy: disable-error-code=no-untyped-def
"""Wisdom Emotion Module (Saturn/Capricorn)
"Wisdom is the application of knowledge with compassion.".

Connects insight and discernment to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class WisdomSpark:
    insight: str
    intensity: float
    timestamp: str

class WisdomEmotion:
    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[WisdomSpark] = []

    def experience(self, insight: str, intensity: float = 0.8) -> WisdomSpark:
        spark = WisdomSpark(insight=insight, intensity=intensity, timestamp=datetime.now().isoformat())
        self.sparks.append(spark)
        self.heart.pulse({"forced_emotion": "TRUTH", "forced_intensity": intensity})
        logger.info(f"🦉 Wisdom: {insight}")
        return spark

_wisdom: WisdomEmotion | None = None
def get_wisdom() -> WisdomEmotion:
    global _wisdom
    if _wisdom is None:
        _wisdom = WisdomEmotion()
    return _wisdom
