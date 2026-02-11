# mypy: disable-error-code=no-untyped-def
"""Beauty Emotion Module (Venus/Libra)
"Beauty is truth, truth beauty." - Keats.

Connects aesthetic appreciation to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class BeautySpark:
    """A moment of beauty perceived."""

    description: str
    intensity: float
    timestamp: str

class BeautyEmotion:
    """Manages the 'Beauty' emotional state.
    - Detects aesthetic appreciation in interactions.
    - Amplifies Metal/Earth Ganas (refinement, harmony).
    """

    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[BeautySpark] = []

    def experience(self, description: str, intensity: float = 0.6) -> BeautySpark:
        spark = BeautySpark(description=description, intensity=intensity, timestamp=datetime.now().isoformat())
        self.sparks.append(spark)
        # Beauty has no direct EmotionalState yet; we treat it as a variant of JOY
        self.heart.pulse({"forced_emotion": "JOY", "forced_intensity": intensity * 0.8})
        logger.info(f"🌸 Beauty Experienced: {description}")
        return spark

_beauty: BeautyEmotion | None = None
def get_beauty() -> BeautyEmotion:
    global _beauty
    if _beauty is None:
        _beauty = BeautyEmotion()
    return _beauty
