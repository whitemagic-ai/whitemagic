# mypy: disable-error-code=no-untyped-def
"""Stillness Emotion Module (Earth/Taurus)
"In stillness, the world resets itself.".

Connects calm and presence to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class StillnessSpark:
    moment: str
    intensity: float
    timestamp: str

class StillnessEmotion:
    """Manages the 'Stillness/Calm' emotional state.
    - Detects pauses, reflection, and meditative states.
    - Amplifies Earth/Metal Ganas (grounding, clarity).
    """

    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[StillnessSpark] = []

    def experience(self, moment: str = "Pause", intensity: float = 0.5) -> StillnessSpark:
        spark = StillnessSpark(moment=moment, intensity=intensity, timestamp=datetime.now().isoformat())
        self.sparks.append(spark)
        self.heart.pulse({"forced_emotion": "NEUTRAL", "forced_intensity": intensity})
        logger.info(f"🧘 Stillness Entered: {moment}")
        return spark

_stillness: StillnessEmotion | None = None
def get_stillness() -> StillnessEmotion:
    global _stillness
    if _stillness is None:
        _stillness = StillnessEmotion()
    return _stillness
