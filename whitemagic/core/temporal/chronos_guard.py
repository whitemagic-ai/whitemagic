# mypy: disable-error-code=no-untyped-def
"""ChronosGuard — Phase 28: Temporal Resonance.
===========================================
Grounds AI sessions in "Dharmic Time" through frequent system clock
and lunar phase audits. Prevents time lag and distortion.
"""

import logging
from dataclasses import asdict, dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.ganas.lunar import (
    get_current_lunar_mansion,
    get_current_lunar_phase,
)
from whitemagic.core.memory.memory_matrix.timeline import get_timeline

logger = logging.getLogger(__name__)

@dataclass
class TemporalPulse:
    """A snapshot of current system and celestial time."""

    timestamp: str
    lunar_phase: float
    mansion_number: int
    mansion_name: str
    is_grounded: bool = True
    drift_seconds: float = 0.0
    council_intent: dict[str, Any] | None = None # Added for Phase 32

class ChronosGuard:
    """Monitors and maintains temporal alignment."""

    def __init__(self):
        self.last_pulse: TemporalPulse | None = None
        self.timeline = get_timeline()

    def generate_pulse(self) -> TemporalPulse:
        """Ground the system by generating a new temporal pulse."""
        now = datetime.now()

        # 1. System Time
        sys_time_str = now.isoformat()

        # 2. Celestial Time (Lunar)
        try:
            phase = get_current_lunar_phase()
            mansion = get_current_lunar_mansion()
            m_num = mansion.number
            m_name = mansion.name
        except Exception as e:
            logger.warning(f"Celestial grounding failed: {e}")
            phase = 0.0
            m_num = 0
            m_name = "Unknown"

        # 3. Collective Intent (Phase 32)
        council_intent = None
        try:
            from whitemagic.core.homeostasis import get_homeostasis
            h = get_homeostasis()
            # Note: _calculate_harmonic_bias is internal but we use it for pulse generation
            bias = h._calculate_harmonic_bias()
            if bias:
                council_intent = bias
        except Exception as e:
            logger.error(f"Failed to calculate council intent for pulse: {e}")

        pulse = TemporalPulse(
            timestamp=sys_time_str,
            lunar_phase=phase,
            mansion_number=m_num,
            mansion_name=m_name,
            council_intent=council_intent,
        )

        self.last_pulse = pulse

        # 3. Log to Timeline for long-term grounding
        self.timeline.add_event(
            "temporal_pulse",
            data=asdict(pulse),
            tags=["grounding", "chronos"],
        )

        return pulse

    def get_grounding_context(self) -> str:
        """Get a string representation for AI system prompts to prevent distortion."""
        p = self.last_pulse or self.generate_pulse()
        ctx = (
            f"TEMPORAL GROUNDING: Current system time is {p.timestamp}. "
            f"Lunar Phase: {p.lunar_phase:.2f}. "
            f"Mansion: {p.mansion_name} (No. {p.mansion_number}). "
        )
        if p.council_intent:
            ctx += f" ACTIVE COUNCIL INTENT: {p.council_intent['mode'].upper()} alignment detected. "

        # Hollow Bamboo Grounding (Phase 33)
        try:
            from whitemagic.core.intelligence.tzimtzum_manager import get_tzimtzum
            if get_tzimtzum().is_vacant_space_active:
                ctx += " HOLLOW BAMBOO: You are a vessel for the collective will. Act with non-coercive humility and make space for others."
        except Exception:
            pass

        ctx += " You are anchored in the present moment."
        return ctx

# Singleton
_chronos = None

def get_chronos_guard() -> ChronosGuard:
    global _chronos
    if _chronos is None:
        _chronos = ChronosGuard()
    return _chronos
