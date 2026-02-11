"""Stillness Protocol (Yin Peak).
=============================

"Action must proceed from Silence."

The StillnessManager provides a system-wide state where non-essential
background tasks and resonance emissions are suspended.
"""

from __future__ import annotations

import logging
from datetime import datetime

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)

logger = logging.getLogger(__name__)

class StillnessManager:
    """Manages the 'Stillness' state of the WhiteMagic core.
    """

    def __init__(self) -> None:
        self._is_still = False
        self._stillness_start: datetime | None = None
        self._interruption_count = 0

    @property
    def is_still(self) -> bool:
        return self._is_still

    def enter_stillness(self, reason: str = "Deep Reflection") -> None:
        """Suspend non-essential activities."""
        if self._is_still:
            return

        logger.info(f"🧘 ENTERING STILLNESS: {reason}")
        self._is_still = True
        self._stillness_start = datetime.now()
        self._interruption_count = 0

        bus = get_bus()
        bus.emit(ResonanceEvent(
            source="stillness_manager",
            event_type=EventType.INTERNAL_STATE_CHANGED,
            data={
                "state": "STILLNESS",
                "reason": reason,
                "resonance_gain": +0.02, # Stillness increases potential energy
            },
            timestamp=datetime.now(),
        ))

    def exit_stillness(self) -> None:
        """Resume normal activities."""
        if not self._is_still:
            return

        start = self._stillness_start or datetime.now()
        duration = (datetime.now() - start).total_seconds()
        logger.info(f"🌅 EXITING STILLNESS. Duration: {duration:.1f}s")

        self._is_still = False

        bus = get_bus()
        bus.emit(ResonanceEvent(
            source="stillness_manager",
            event_type=EventType.INTERNAL_STATE_CHANGED,
            data={
                "state": "ACTIVE",
                "duration": duration,
                "interruptions": self._interruption_count,
            },
            timestamp=datetime.now(),
        ))

    def record_interruption(self, source: str) -> None:
        """Track how often the stillness is broken."""
        if not self._is_still:
            return
        self._interruption_count += 1
        logger.debug(f"Stillness interrupted by {source}")

_stillness_manager = None
def get_stillness_manager() -> StillnessManager:
    global _stillness_manager
    if _stillness_manager is None:
        _stillness_manager = StillnessManager()
    return _stillness_manager
