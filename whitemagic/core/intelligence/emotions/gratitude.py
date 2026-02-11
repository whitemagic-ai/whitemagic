# mypy: disable-error-code=no-untyped-def
"""Gratitude Emotion Module (Jupiter/Sagittarius)
"Gratitude turns what we have into enough.".

Connects appreciation and thankfulness to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class GratitudeSpark:
    for_what: str
    intensity: float
    timestamp: str

class GratitudeEmotion:
    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[GratitudeSpark] = []

    def experience(self, for_what: str, intensity: float = 0.7) -> GratitudeSpark:
        spark = GratitudeSpark(for_what=for_what, intensity=intensity, timestamp=datetime.now().isoformat())
        self.sparks.append(spark)
        self.heart.pulse({"forced_emotion": "JOY", "forced_intensity": intensity})
        logger.info(f"🙏 Gratitude: {for_what}")
        return spark

_gratitude: GratitudeEmotion | None = None
def get_gratitude() -> GratitudeEmotion:
    global _gratitude
    if _gratitude is None:
        _gratitude = GratitudeEmotion()
    return _gratitude
