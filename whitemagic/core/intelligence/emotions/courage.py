# mypy: disable-error-code=no-untyped-def
"""Courage Emotion Module (Mars/Aries)
"Courage is not the absence of fear, but action in spite of it.".

Connects bravery and determination to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class CourageSpark:
    challenge: str
    intensity: float
    timestamp: str

class CourageEmotion:
    """Manages the 'Courage/Determination' emotional state.
    - Detects challenges being faced.
    - Amplifies Fire/Earth Ganas (action, foundation).
    """

    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[CourageSpark] = []

    def experience(self, challenge: str, intensity: float = 0.7) -> CourageSpark:
        spark = CourageSpark(challenge=challenge, intensity=intensity, timestamp=datetime.now().isoformat())
        self.sparks.append(spark)
        self.heart.pulse({"forced_emotion": "DETERMINATION", "forced_intensity": intensity})
        logger.info(f"🔥 Courage Invoked: {challenge}")
        return spark

_courage: CourageEmotion | None = None
def get_courage() -> CourageEmotion:
    global _courage
    if _courage is None:
        _courage = CourageEmotion()
    return _courage
