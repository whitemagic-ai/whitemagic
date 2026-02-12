# mypy: disable-error-code=no-untyped-def
"""Truth Emotion Module (Metal/Clarity)
"The truth will set you free." - John 8:32.

Connects the concept of Truth/Verification to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class TruthSpark:
    """A single spark of truth/verification."""

    statement: str
    intensity: float
    timestamp: str

class TruthEmotion:
    """Manages the 'Truth' emotional state.
    - Detects verification, auditing, and clarity-seeking.
    - Resonates truth to the Heart Engine.
    - Amplifies Metal/Earth Ganas (precision, foundation).
    """

    def __init__(self):
        self.heart = get_heart()
        self.sparks: list[TruthSpark] = []

    def experience(self, statement: str, intensity: float = 0.7) -> TruthSpark:
        """Record a truth moment and pulse the heart."""
        spark = TruthSpark(
            statement=statement,
            intensity=intensity,
            timestamp=datetime.now().isoformat(),
        )
        self.sparks.append(spark)

        context = {
            "user_input": statement,
            "forced_emotion": "TRUTH",
            "forced_intensity": intensity,
        }
        self.heart.pulse(context)

        logger.info(f"💎 Truth Experienced: {statement} (Intensity: {intensity})")
        return spark

    def get_history(self) -> list[dict[str, Any]]:
        return [{"statement": s.statement, "intensity": s.intensity, "time": s.timestamp} for s in self.sparks[-10:]]

_truth: TruthEmotion | None = None

def get_truth() -> TruthEmotion:
    global _truth
    if _truth is None:
        _truth = TruthEmotion()
    return _truth
