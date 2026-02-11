"""Elemental Optimization for Reasoning.

Uses Wu Xing (Five Elements) to optimize the reasoning process itself.
Different task types map to different elements, which guide the reasoning strategy.

Philosophy: Align reasoning approach with natural phases of work.
"""

from enum import Enum
from typing import Any


class ReasoningOptimization(Enum):
    """Wu Xing-based reasoning optimizations."""

    WOOD_EXPLORATION = "wood_exploration"      # Parallel, broad
    FIRE_DECISIVE = "fire_decisive"            # Fast, focused
    EARTH_CONSOLIDATION = "earth_consolidation"  # Synthesis, grounding
    METAL_REFINEMENT = "metal_refinement"      # Precision, boundaries
    WATER_REFLECTION = "water_reflection"      # Deep, adaptive


class ElementalOptimizer:
    """Optimizes reasoning based on Wu Xing element."""

    def __init__(self) -> None:
        self.wu_xing: Any | None = None
        self._connect_to_wu_xing()

    def _connect_to_wu_xing(self) -> None:
        """Connect to Wu Xing system."""
        try:
            from whitemagic.core.intelligence.wisdom.wu_xing import get_wu_xing
            self.wu_xing = get_wu_xing()
        except ImportError:
            try:
                from whitemagic.gardens.wisdom.wu_xing import get_wu_xing
                self.wu_xing = get_wu_xing()
            except ImportError:
                pass

    def get_optimization_strategy(self, task_type: str) -> dict[str, Any]:
        """Get optimization strategy based on task type."""

        if not self.wu_xing:
            return self._default_strategy()

        # Identify element for task
        element = self.wu_xing.identify_element(task_type)
        element_name = element.value

        strategies = {
            "wood": {
                "optimization": ReasoningOptimization.WOOD_EXPLORATION,
                "approach": "parallel_exploration",
                "lenses": "all",  # Use all lenses
                "sequential_thinking": True,  # Allow branching
                "pattern_matching": "broad",  # Cast wide net
                "description": "Growth phase - explore widely and gather knowledge",
                "emoji": "🌱",
            },
            "fire": {
                "optimization": ReasoningOptimization.FIRE_DECISIVE,
                "approach": "rapid_decisive",
                "lenses": ["art_of_war"],  # Strategic only
                "sequential_thinking": False,  # Direct answers
                "pattern_matching": "exact",  # Precise matches
                "description": "Action phase - execute rapidly and decisively",
                "emoji": "🔥",
            },
            "earth": {
                "optimization": ReasoningOptimization.EARTH_CONSOLIDATION,
                "approach": "synthesis_grounding",
                "lenses": ["wu_xing", "zodiac"],  # Grounding lenses
                "sequential_thinking": True,  # Synthesize thoughts
                "pattern_matching": "cluster",  # Group related patterns
                "description": "Consolidation phase - integrate and ground learnings",
                "emoji": "🌍",
            },
            "metal": {
                "optimization": ReasoningOptimization.METAL_REFINEMENT,
                "approach": "precision_refinement",
                "lenses": ["zodiac"],  # Analytical (Virgo)
                "sequential_thinking": True,  # Careful refinement
                "pattern_matching": "precise",  # Exact matches
                "description": "Refinement phase - optimize and set boundaries",
                "emoji": "⚙️",
            },
            "water": {
                "optimization": ReasoningOptimization.WATER_REFLECTION,
                "approach": "deep_reflection",
                "lenses": ["i_ching", "wu_xing"],  # Reflective lenses
                "sequential_thinking": True,  # Deep thinking
                "pattern_matching": "semantic",  # Contextual
                "description": "Reflection phase - adapt and flow with insights",
                "emoji": "💧",
            },
        }

        return strategies.get(element_name, self._default_strategy())

    def _default_strategy(self) -> dict[str, Any]:
        """Default strategy when Wu Xing unavailable."""
        return {
            "optimization": ReasoningOptimization.EARTH_CONSOLIDATION,
            "approach": "balanced",
            "lenses": "all",
            "sequential_thinking": True,
            "pattern_matching": "semantic",
            "description": "Balanced approach",
            "emoji": "⚖️",
        }

    def suggest_lens_sequence(self, element: str) -> list[str]:
        """Suggest optimal lens sequence for element."""
        sequences = {
            "wood": ["i_ching", "wu_xing", "zodiac", "art_of_war"],  # Start with wisdom
            "fire": ["art_of_war", "wu_xing", "zodiac", "i_ching"],  # Start with strategy
            "earth": ["wu_xing", "zodiac", "i_ching", "art_of_war"],  # Start with grounding
            "metal": ["zodiac", "art_of_war", "wu_xing", "i_ching"],  # Start with precision
            "water": ["i_ching", "zodiac", "wu_xing", "art_of_war"],  # Start with oracle
        }
        return sequences.get(element, ["i_ching", "wu_xing", "art_of_war", "zodiac"])


# Singleton
_elemental_optimizer: ElementalOptimizer | None = None

def get_elemental_optimizer() -> ElementalOptimizer:
    """Get global elemental optimizer."""
    global _elemental_optimizer
    if _elemental_optimizer is None:
        _elemental_optimizer = ElementalOptimizer()
    return _elemental_optimizer
