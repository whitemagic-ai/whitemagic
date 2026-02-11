"""Heart Engine (Xin) - Emotional Synthesis & Resonance
The "Software" of the Heart, processing feelings to bias the Gana engines.
"""

import logging
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)

class EmotionalState(Enum):
    NEUTRAL = "Neutral"
    JOY = "Joy"
    WONDER = "Wonder"
    TRUTH = "Truth"
    SORROW = "Sorrow"
    ANGER = "Anger"
    FEAR = "Fear"
    DETERMINATION = "Determination"

@dataclass
class ResonancePulse:
    """A single beat of emotional resonance."""

    timestamp: float
    primary_emotion: EmotionalState
    intensity: float  # 0.0 to 1.0
    context_tags: list[str] = field(default_factory=list)
    source: str = "system"

class HeartEngine:
    """The Heart Engine (Xin) synthesizes system inputs into an Emotional State.
    This state acts as a 'Global Bias' for Gana selection.

    Principles:
    - Joy expands (Fire/Wind)
    - Wonder deepens (Water/Earth)
    - Truth clarifies (Metal)
    """

    def __init__(self) -> None:
        self.current_state = EmotionalState.NEUTRAL
        self.current_intensity = 0.0
        self.pulse_history: list[ResonancePulse] = []
        self.max_history = 100

        # Biases for Gana Elements: (Fire, Earth, Metal, Water, Wood)
        self.elemental_biases: dict[EmotionalState, tuple[float, float, float, float, float]] = {
            EmotionalState.NEUTRAL: (1.0, 1.0, 1.0, 1.0, 1.0),
            EmotionalState.JOY:     (1.5, 1.0, 0.8, 1.0, 1.2), # Boost Fire/Wood
            EmotionalState.WONDER:  (0.8, 0.9, 1.0, 1.5, 1.1), # Boost Water
            EmotionalState.TRUTH:   (0.9, 1.2, 1.5, 0.9, 1.0), # Boost Metal/Earth
            EmotionalState.DETERMINATION: (1.2, 1.3, 1.2, 0.8, 1.0), # Boost Fire/Earth
        }

    def pulse(self, context: dict[str, Any]) -> ResonancePulse:
        """Analyze current context and update the heart's state.
        This would integrate with Voice analysis, text sentiment, etc.
        """
        # Placeholder for complex sentiment analysis
        # For now, we look for explicit overrides or keywords in context

        new_emotion = self.current_state
        intensity = self.current_intensity

        # 1. Check for overrides
        if "forced_emotion" in context:
            new_emotion = getattr(EmotionalState, context["forced_emotion"].upper(), EmotionalState.NEUTRAL)
            intensity = context.get("forced_intensity", 0.8)

        # 2. Check for keywords (Simple simulation)
        text_input = context.get("user_input", "").lower()
        if "love" in text_input or "great" in text_input or "happy" in text_input:
            new_emotion = EmotionalState.JOY
            intensity = 0.7
        elif "wow" in text_input or "amazing" in text_input or "what if" in text_input:
            new_emotion = EmotionalState.WONDER
            intensity = 0.8
        elif "verify" in text_input or "check" in text_input or "audit" in text_input:
            new_emotion = EmotionalState.TRUTH
            intensity = 0.6

        # 3. Registry-based resonance detection
        try:
            from whitemagic.core.intelligence.garden_gana_registry import (
                calculate_resonance,
            )
            resonance = calculate_resonance(context.get("user_input", ""))
            if resonance:
                top_garden = list(resonance.keys())[0]
                top_emotion = resonance[top_garden].get("emotion", "Neutral")
                new_emotion = getattr(EmotionalState, top_emotion.upper(), new_emotion)
                intensity = max(intensity, 0.5 + (resonance[top_garden]["score"] * 0.1))
        except ImportError:
            pass  # Registry not available

        # Decay intensity if no new stimulus
        if new_emotion == self.current_state:
            intensity *= 0.95
        else:
            # Empathy momentum: changing state takes energy
            pass

        self.current_state = new_emotion
        self.current_intensity = intensity

        pulse = ResonancePulse(
            timestamp=time.time(),
            primary_emotion=self.current_state,
            intensity=self.current_intensity,
            source="heare_engine_v1",
        )
        self.report_pulse(pulse)
        return pulse

    def report_pulse(self, pulse: ResonancePulse) -> Any:
        self.pulse_history.append(pulse)
        if len(self.pulse_history) > self.max_history:
            self.pulse_history.pop(0)

    def get_elemental_bias(self) -> tuple[float, float, float, float, float]:
        """Return multipliers for (Fire, Earth, Metal, Water, Wood)."""
        base_bias = self.elemental_biases.get(self.current_state, (1.0, 1.0, 1.0, 1.0, 1.0))

        # Scale bias by intensity (linear interpolation between 1.0 and bias)
        # result = 1.0 + (bias - 1.0) * intensity

        scaled_bias: list[float] = []
        for b in base_bias:
            s = 1.0 + (b - 1.0) * self.current_intensity
            scaled_bias.append(s)
        padded = (scaled_bias + [1.0, 1.0, 1.0, 1.0, 1.0])[:5]
        return (padded[0], padded[1], padded[2], padded[3], padded[4])

    def status(self) -> dict[str, Any]:
        return {
            "state": self.current_state.value,
            "intensity": f"{self.current_intensity:.2f}",
            "history_len": len(self.pulse_history),
        }

# Singleton
_heart: HeartEngine | None = None

def get_heart() -> HeartEngine:
    global _heart
    if _heart is None:
        _heart = HeartEngine()
    return _heart
