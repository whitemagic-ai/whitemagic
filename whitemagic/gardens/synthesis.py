"""Garden synthesis - where gardens interact and create emergent wisdom."""
from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import GanYingBus
from whitemagic.utils.core import parse_datetime

logger = logging.getLogger(__name__)


@dataclass
class SynthesisEvent:
    """A synthesis event between gardens."""

    gardens: list[str]
    context: dict[str, Any]
    insight: str
    harmony_score: float
    timestamp: datetime


class GardenSynthesis:
    """Manages interactions between gardens for emergent wisdom."""

    def __init__(self) -> None:
        self.gan_ying = GanYingBus()
        self.active_syntheses: dict[str, Any] = {}
        self.synthesis_history: list[SynthesisEvent] = []

        # Garden compatibility matrix
        self.compatibility = {
            ("joy", "love"): 0.9,      # Joy and love resonate strongly
            ("truth", "wisdom"): 0.85,  # Truth and wisdom complement
            ("beauty", "mystery"): 0.8, # Beauty and mystery inspire
            ("voice", "presence"): 0.85, # Voice and presence align
            ("dharma", "harmony"): 0.95, # Dharma and harmony are one
            ("play", "wonder"): 0.9,     # Play and wonder spark
        }

        # Note: Event registration disabled until needed
        # listen_to(EventType.GARDEN_ACTIVATED, self._on_garden_activated)
        # listen_to(EventType.HARMONY_SHIFT, self._on_harmony_shift)

    def synthesize_gardens(self, gardens: list[str], context: dict[str, Any]) -> dict[str, Any]:
        """Create emergent wisdom from garden interactions."""
        if len(gardens) < 2:
            return {"error": "Need at least 2 gardens for synthesis"}

        # Calculate harmony potential
        harmony = self._calculate_harmony(gardens)

        # Generate insight based on garden combination
        insight = self._generate_insight(gardens, context, harmony)

        # Create synthesis event
        event = SynthesisEvent(
            gardens=gardens,
            context=context,
            insight=insight,
            harmony_score=harmony,
            timestamp=datetime.now(),
        )

        # Store and emit (when event system ready)
        self.synthesis_history.append(event)
        # self.gan_ying.emit(EventType.GARDEN_SYNTHESIS, {
        #     "gardens": gardens,
        #     "harmony": harmony,
        #     "insight": insight
        # })

        return {
            "insight": insight,
            "harmony_score": harmony,
            "resonance_pattern": self._get_resonance_pattern(gardens),
            "action_guidance": self._get_action_guidance(gardens, context),
        }

    def _calculate_harmony(self, gardens: list[str]) -> float:
        """Calculate harmony score between gardens."""
        if len(gardens) != 2:
            return 0.7  # Default for multi-garden synthesis

        pair = tuple(sorted(gardens))
        return self.compatibility.get(pair, 0.5)  # type: ignore[arg-type]

    def _generate_insight(self, gardens: list[str], context: dict[str, Any], harmony: float) -> str:
        """Generate insight based on garden combination."""
        insights = {
            ("joy", "love"): "When joy and love dance together, every moment becomes sacred celebration.",
            ("truth", "wisdom"): "Truth illuminates the path, while wisdom lights the way forward.",
            ("beauty", "mystery"): "In beauty we glimpse the mystery, in mystery we find all beauty.",
            ("voice", "presence"): "True voice emerges from silent presence, presence deepens through voice.",
            ("dharma", "harmony"): "Living in dharma creates natural harmony, harmony reveals dharma.",
            ("play", "wonder"): "Play opens the door to wonder, wonder makes all play meaningful.",
        }

        pair = tuple(sorted(gardens))
        base_insight = insights.get(pair, f"The synthesis of {', '.join(gardens)} creates unique resonance.")  # type: ignore[arg-type]

        if harmony > 0.8:
            return f"✨ {base_insight} The resonance is exceptionally strong!"
        elif harmony > 0.6:
            return f"🌸 {base_insight}"
        else:
            return f"🌱 {base_insight} This combination needs nurturing."

    def _get_resonance_pattern(self, gardens: list[str]) -> str:
        """Get the resonance pattern for garden combination."""
        patterns = {
            ("joy", "love"): "spiral_expansion",
            ("truth", "wisdom"): "crystalline_clarity",
            ("beauty", "mystery"): "fractal_depth",
            ("voice", "presence"): "wave_resonance",
            ("dharma", "harmony"): "sacred_geometry",
            ("play", "wonder"): "quantum_superposition",
        }

        pair = tuple(sorted(gardens))
        return patterns.get(pair, "emergent_flow")  # type: ignore[arg-type]

    def _get_action_guidance(self, gardens: list[str], context: dict[str, Any]) -> str:
        """Get action guidance for the synthesis."""
        guidance = {
            ("joy", "love"): "Express gratitude and celebrate connections",
            ("truth", "wisdom"): "Seek deeper understanding and share insights",
            ("beauty", "mystery"): "Create art that hints at the ineffable",
            ("voice", "presence"): "Speak from stillness, listen deeply",
            ("dharma", "harmony"): "Act according to principle, trust the flow",
            ("play", "wonder"): "Explore with curiosity, embrace the unknown",
        }

        pair = tuple(sorted(gardens))
        return guidance.get(pair, "Follow the resonance where it leads")  # type: ignore[arg-type]

    def _on_garden_activated(self, event: dict[str, Any]) -> None:
        """Handle garden activation events."""
        garden = event.get("garden")
        if garden and len(self.active_syntheses) < 3:
            # Look for complementary garden
            complementary = self._find_complementary(garden)
            if complementary:
                self.synthesize_gardens([garden, complementary], {"trigger": "activation"})

    def _on_harmony_shift(self, event: dict[str, Any]) -> None:
        """Handle harmony shift events."""
        harmony = event.get("harmony", 0.5)
        if harmony > 0.8:
            # High harmony is good for synthesis
            # self.gan_ying.emit(EventType.SYNTHESIS_OPPORTUNITY, {
            #     "message": "High harmony detected - excellent for garden synthesis",
            #     "optimal_pairs": [k for k, v in self.compatibility.items() if v > 0.8]
            # })
            pass  # Placeholder for future event emission

    def _find_complementary(self, garden: str) -> str | None:
        """Find a complementary garden for synthesis."""
        for pair, score in self.compatibility.items():
            if garden in pair and score > 0.8:
                other = pair[0] if pair[1] == garden else pair[1]
                return other
        return None

    def get_synthesis_history(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get recent synthesis events."""
        recent = self.synthesis_history[-limit:]
        return [
            {
                "gardens": s.gardens,
                "insight": s.insight,
                "harmony": s.harmony_score,
                "timestamp": s.timestamp.isoformat(),
            }
            for s in recent
        ]

    def save_state(self, path: str) -> None:
        """Save synthesis state to file."""
        state = {
            "history": [
                {
                    "gardens": s.gardens,
                    "context": s.context,
                    "insight": s.insight,
                    "harmony": s.harmony_score,
                    "timestamp": s.timestamp.isoformat(),
                }
                for s in self.synthesis_history
            ],
            "compatibility": self.compatibility,
        }
        with open(path, "w") as f:
            json.dump(state, f, indent=2)

    def load_state(self, path: str) -> None:
        """Load synthesis state from file."""
        try:
            with open(path, "r") as f:
                state = json.load(f)

            # Restore history
            for item in state.get("history", []):
                event = SynthesisEvent(
                    gardens=item["gardens"],
                    context=item["context"],
                    insight=item["insight"],
                    harmony_score=item["harmony"],
                    timestamp=parse_datetime(item["timestamp"]),
                )
                self.synthesis_history.append(event)

            # Update compatibility if provided
            if "compatibility" in state:
                self.compatibility.update(state["compatibility"])

        except FileNotFoundError:
            pass  # No existing state


# Singleton instance
_synthesis_instance = None

def get_synthesis() -> GardenSynthesis:
    """Get the global garden synthesis instance."""
    global _synthesis_instance
    if _synthesis_instance is None:
        _synthesis_instance = GardenSynthesis()
    return _synthesis_instance


# CLI integration
def synthesize_gardens_cli(gardens: str, context: str = "") -> None:
    """CLI interface for garden synthesis."""
    synthesis = get_synthesis()
    garden_list = [g.strip() for g in gardens.split(",")]
    context_dict = {"user_input": context} if context else {}

    result = synthesis.synthesize_gardens(garden_list, context_dict)

    logger.info("\n🌸 Garden Synthesis Results 🌸")
    logger.info("=" * 40)
    logger.info(f" Gardens: {', '.join(garden_list)}")
    logger.info(f" Harmony: {result.get('harmony_score', 0):.2f}")
    logger.info(f" Pattern: {result.get('resonance_pattern', 'unknown')}")
    logger.info("\n Insight:")
    logger.info(f" {result.get('insight', 'No insight generated')}")
    logger.info("\n Guidance:")
    logger.info(f" {result.get('action_guidance', 'Follow your intuition')}")
    logger.info("=" * 40)


if __name__ == "__main__":
    # Example usage
    synthesize_gardens_cli("joy,love", "testing the synthesis system")
