"""Joy Emotion Module (Leo/Fire)
"Joy shared is joy multiplied.".

Connects the concept of Joy to the Heart Engine.
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.intelligence.heart import get_heart

logger = logging.getLogger(__name__)

@dataclass
class JoySpark:
    """A single spark of joy."""

    source: str
    description: str
    intensity: float  # 0.0 - 1.0
    timestamp: str

class JoyEmotion:
    """Manages the 'Joy' emotional state.
    - Detects joy in interactions.
    - Resonates joy to the Heart Engine.
    - Amplifies Fire/Wood Ganas.
    """

    def __init__(self) -> None:
        self.heart = get_heart()
        self.sparks: list[JoySpark] = []

    def experience(self, source: str, description: str, intensity: float = 0.5) -> JoySpark:
        """Record a joyful moment and pulse the heart.
        """
        spark = JoySpark(
            source=source,
            description=description,
            intensity=intensity,
            timestamp=datetime.now().isoformat(),
        )
        self.sparks.append(spark)

        # Pulse the global Heart Engine
        # This will set the system state to JOY if valid
        context = {
            "user_input": description, # simple heuristic
            "forced_emotion": "JOY",
            "forced_intensity": intensity,
        }
        self.heart.pulse(context)

        logger.info(f"✨ Joy Experienced: {description} (Intensity: {intensity})")
        return spark

    def resonate(self) -> None:
        """Amplify the current joy state.
        This roughly maps to the old 'broadcast_joy' notion.
        """
        # In a real system, this might trigger the 'Wings' Gana to share the joy
        current = self.heart.status()
        if current["state"] == "Joy":
            logger.info("🔥 Joy Resonance Active! Fire Ganas Amplified.")
            # Logic to trigger Ganas would go here or be handled by Gana middleware
        else:
            logger.info("Resonance failed: Heart is not currently in Joy state.")

    def get_history(self) -> list[dict[str, Any]]:
        return [
            {
                "description": s.description,
                "intensity": s.intensity,
                "time": s.timestamp,
            }
            for s in self.sparks[-10:]
        ]

# Simple singleton for ease of access
_joy: JoyEmotion | None = None

def get_joy() -> JoyEmotion:
    global _joy
    if _joy is None:
        _joy = JoyEmotion()
    return _joy
