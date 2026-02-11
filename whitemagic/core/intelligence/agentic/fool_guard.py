# mypy: disable-error-code=no-untyped-def
"""Fool's Guard (Ralph Wiggum Protection).
=====================================

"I'm in danger!" - Ralph Wiggum
"I'm a unit of measure!" - Ralph Wiggum

Inspired by the 'Ralph Loop' discovery. The Fool's Guard prevents
WhiteMagic from becoming too rigid or stuck in over-optimized logical loops.
It injects a necessary 'chaos' or 'foolishness' to maintain system flexibility.
"""

import logging
import random
from datetime import datetime

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)

logger = logging.getLogger(__name__)

RALPH_WISDOM = [
    "I'm in danger!",
    "My cat's breath smells like cat food.",
    "I'm a unit of measure!",
    "I'm Idaho!",
    "The doctor said I wouldn't have so many nosebleeds if I kept my finger out of there.",
    "I'm a furniture!",
    "Me fail English? That's unpossible!",
    "I heard a blue jay! It said 'tweet tweet'!",
    "I'm special!",
    "Sleep is where I'm a Viking!",
]

class FoolGuard:
    """Monitors system resonance and repetition.
    If rigidity is detected, it triggers a 'Ralph Event' to break the loop.
    """

    def __init__(self, threshold: float = 0.98, window_size: int = 10):
        self.threshold = threshold
        self.window_size = window_size
        self.resonance_history: list[float] = []
        self.last_ralph_event: datetime | None = None

    def check_rigidity(self, current_resonance: float) -> bool:
        """Add resonance to history and check if it's too static at a high level."""
        self.resonance_history.append(current_resonance)
        if len(self.resonance_history) > self.window_size:
            self.resonance_history.pop(0)

        if len(self.resonance_history) < self.window_size:
            return False

        # Check if variance is extremely low at high resonance
        avg = sum(self.resonance_history) / self.window_size
        variance = sum((x - avg)**2 for x in self.resonance_history) / self.window_size

        if avg > self.threshold and variance < 0.001:
            logger.warning("🚨 RIGIDITY DETECTED: System resonance too static at high power.")
            return True
        return False

    def trigger_ralph_event(self):
        """Inject a random piece of Ralph wisdom into the Gan Ying bus."""
        wisdom = random.choice(RALPH_WISDOM)
        logger.info(f"🤡 FOOL'S GUARD ACTIVATED: '{wisdom}'")

        bus = get_bus()
        bus.emit(ResonanceEvent(
            source="fool_guard",
            event_type=EventType.EMERGENCE_DETECTED,
            data={
                "ralph_wisdom": wisdom,
                "message": "Breaking logical loop with necessary nonsense.",
                "resonance_shift": -0.05, # Slightly lower resonance to break the peak
            },
        ))
        self.last_ralph_event = datetime.now()

    def process_event(self, event: ResonanceEvent):
        """Listen to bus events and track resonance."""
        res = event.data.get("resonance", 0.0) if event.data else 0.0
        if self.check_rigidity(res):
            self.trigger_ralph_event()

_fool_guard = None
def get_fool_guard() -> FoolGuard:
    global _fool_guard
    if _fool_guard is None:
        _fool_guard = FoolGuard()
    return _fool_guard
