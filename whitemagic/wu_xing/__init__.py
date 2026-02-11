"""
Wu Xing (五行) - Five Elements System

The Wu Xing system models the flow of energy through five elements:
Wood (木), Fire (火), Earth (土), Metal (金), and Water (水).

It provides understanding of cycles, balance, and transformation
within the WhiteMagic system.

Created: January 6, 2026
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class Element(Enum):
    """The five elements of Wu Xing."""
    WOOD = "wood"      # 木 - Growth, creativity, beginnings
    FIRE = "fire"      # 火 - Transformation, passion, illumination
    EARTH = "earth"    # 土 - Stability, nourishment, grounding
    METAL = "metal"    # 金 - Structure, precision, refinement
    WATER = "water"    # 水 - Flow, wisdom, reflection


@dataclass
class ElementalState:
    """The current state of an element."""
    element: Element
    energy: float  # 0.0 to 1.0
    quality: str   # Descriptor of current quality
    timestamp: datetime | None = None

    def __post_init__(self) -> None:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class WuXingEngine:
    """
    The Wu Xing Engine models elemental flows and transformations.

    It understands:
    - Generating cycle (生): Wood → Fire → Earth → Metal → Water → Wood
    - Overcoming cycle (克): Wood → Earth → Water → Fire → Metal → Wood
    - Balance and harmony between elements
    """

    def __init__(self) -> None:
        self.elements: dict[Element, ElementalState] = self._initialize_elements()
        self.cycle_history: list[dict[str, Any]] = []
        self.imbalance_threshold = 0.3
        self.harmony_threshold = 0.8

    def _initialize_elements(self) -> dict[Element, ElementalState]:
        """Initialize all elements in balanced state."""
        return {
            Element.WOOD: ElementalState(Element.WOOD, 0.5, "balanced"),
            Element.FIRE: ElementalState(Element.FIRE, 0.5, "balanced"),
            Element.EARTH: ElementalState(Element.EARTH, 0.5, "balanced"),
            Element.METAL: ElementalState(Element.METAL, 0.5, "balanced"),
            Element.WATER: ElementalState(Element.WATER, 0.5, "balanced")
        }

    def get_element(self, element: Element) -> ElementalState:
        """Get the current state of an element."""
        return self.elements[element]

    def adjust_element(self, element: Element, energy_change: float, quality: str | None = None) -> None:
        """
        Adjust the energy of an element.

        Args:
            element: The element to adjust
            energy_change: Change in energy (-1.0 to 1.0)
            quality: Optional new quality descriptor
        """
        current = self.elements[element]
        new_energy = max(0.0, min(1.0, current.energy + energy_change))

        self.elements[element] = ElementalState(
            element=element,
            energy=new_energy,
            quality=quality or self._determine_quality(element, new_energy)
        )

        # Apply elemental interactions
        self._apply_interactions(element)

        # Record the change
        self.cycle_history.append({
            "element": element.value,
            "energy_change": energy_change,
            "new_energy": new_energy,
            "timestamp": datetime.now()
        })

    def _determine_quality(self, element: Element, energy: float) -> str:
        """Determine quality descriptor based on energy level."""
        if energy < 0.2:
            qualities = {
                Element.WOOD: "dormant",
                Element.FIRE: "ember",
                Element.EARTH: "barren",
                Element.METAL: "corroded",
                Element.WATER: "stagnant"
            }
        elif energy < 0.4:
            qualities = {
                Element.WOOD: "sprouting",
                Element.FIRE: "flickering",
                Element.EARTH: "fertile",
                Element.METAL: "raw",
                Element.WATER: "trickling"
            }
        elif energy < 0.6:
            qualities = {
                Element.WOOD: "growing",
                Element.FIRE: "burning",
                Element.EARTH: "stable",
                Element.METAL: "forging",
                Element.WATER: "flowing"
            }
        elif energy < 0.8:
            qualities = {
                Element.WOOD: "thriving",
                Element.FIRE: "radiant",
                Element.EARTH: "nourishing",
                Element.METAL: "polished",
                Element.WATER: "streaming"
            }
        else:
            qualities = {
                Element.WOOD: "blossoming",
                Element.FIRE: "blazing",
                Element.EARTH: "abundant",
                Element.METAL: "luminous",
                Element.WATER: "cascading"
            }

        return qualities[element]

    def _apply_interactions(self, changed_element: Element) -> None:
        """Apply elemental interactions based on cycles."""
        # Generating cycle (what this element creates)
        generating_target = self._get_generating_target(changed_element)
        if generating_target:
            # Small boost to what this element generates
            boost = 0.05 * self.elements[changed_element].energy
            current = self.elements[generating_target]
            new_energy = min(1.0, current.energy + boost)
            self.elements[generating_target] = ElementalState(
                generating_target,
                new_energy,
                self._determine_quality(generating_target, new_energy)
            )

        # Overcoming cycle (what this element controls)
        overcoming_target = self._get_overcoming_target(changed_element)
        if overcoming_target:
            # This element suppresses its target slightly
            suppression = 0.03 * self.elements[changed_element].energy
            current = self.elements[overcoming_target]
            new_energy = max(0.0, current.energy - suppression)
            self.elements[overcoming_target] = ElementalState(
                overcoming_target,
                new_energy,
                self._determine_quality(overcoming_target, new_energy)
            )

    def _get_generating_target(self, element: Element) -> Element | None:
        """Get what this element generates in the generating cycle."""
        generating_cycle = {
            Element.WOOD: Element.FIRE,    # Wood feeds Fire
            Element.FIRE: Element.EARTH,   # Fire creates Earth (ash)
            Element.EARTH: Element.METAL,  # Earth bears Metal
            Element.METAL: Element.WATER,  # Metal collects Water
            Element.WATER: Element.WOOD    # Water nourishes Wood
        }
        return generating_cycle.get(element)

    def _get_overcoming_target(self, element: Element) -> Element | None:
        """Get what this element overcomes in the overcoming cycle."""
        overcoming_cycle = {
            Element.WOOD: Element.EARTH,   # Wood penetrates Earth
            Element.EARTH: Element.WATER,  # Earth absorbs Water
            Element.WATER: Element.FIRE,   # Water extinguishes Fire
            Element.FIRE: Element.METAL,   # Fire melts Metal
            Element.METAL: Element.WOOD    # Metal cuts Wood
        }
        return overcoming_cycle.get(element)

    def analyze_situation(self, situation: str) -> dict[str, Any]:
        """
        Analyze a situation using Wu Xing principles.

        Returns elemental associations and guidance.
        """
        situation_lower = situation.lower()

        # Identify dominant elements
        element_keywords = {
            Element.WOOD: ["grow", "create", "begin", "start", "plant", "green", "life"],
            Element.FIRE: ["transform", "passion", "energy", "light", "heat", "bright", "inspire"],
            Element.EARTH: ["stabilize", "ground", "nourish", "support", "steady", "foundation"],
            Element.METAL: ["structure", "refine", "precise", "analyze", "organize", "sharp"],
            Element.WATER: ["flow", "reflect", "adapt", "wisdom", "deep", "intuitive"]
        }

        element_scores = {}
        for element, keywords in element_keywords.items():
            score = sum(1 for keyword in keywords if keyword in situation_lower)
            element_scores[element] = score

        # Find primary and secondary elements
        sorted_elements = sorted(element_scores.items(), key=lambda x: x[1], reverse=True)
        primary = sorted_elements[0][0] if sorted_elements[0][1] > 0 else None
        secondary = sorted_elements[1][0] if len(sorted_elements) > 1 and sorted_elements[1][1] > 0 else None

        # Generate guidance
        guidance = self._generate_elemental_guidance(primary, secondary, situation)

        return {
            "situation": situation,
            "primary_element": primary.value if primary else None,
            "secondary_element": secondary.value if secondary else None,
            "element_scores": {e.value: s for e, s in element_scores.items()},
            "current_state": {e.value: {
                "energy": state.energy,
                "quality": state.quality
            } for e, state in self.elements.items()},
            "guidance": guidance,
            "timestamp": datetime.now()
        }

    def _generate_elemental_guidance(self, primary: Element | None,
                                   secondary: Element | None,
                                   situation: str) -> str:
        """Generate guidance based on elemental analysis."""
        if not primary:
            return "No clear elemental alignment detected. Consider the situation from multiple perspectives."

        guidance_parts = []

        # Primary element guidance
        primary_guidance = {
            Element.WOOD: "Focus on growth and new beginnings. Nurture creative ideas and allow them to develop naturally.",
            Element.FIRE: "Embrace transformation and passion. This is a time for dynamic change and inspired action.",
            Element.EARTH: "Seek stability and grounding. Build solid foundations and nurture yourself and others.",
            Element.METAL: "Bring structure and precision. Analyze carefully and refine your approach.",
            Element.WATER: "Go with the flow and trust your intuition. Allow wisdom to emerge from reflection."
        }

        guidance_parts.append(primary_guidance[primary])

        # Secondary element interaction
        if secondary:
            interaction = self._get_element_interaction(primary, secondary)
            if interaction:
                guidance_parts.append(interaction)

        # Balance advice
        current_energy = self.elements[primary].energy
        if current_energy < 0.3:
            guidance_parts.append(f"The {primary.value} element is weak. Consider activities that strengthen it.")
        elif current_energy > 0.8:
            guidance_parts.append(f"The {primary.value} element is very strong. Ensure it doesn't overwhelm other aspects.")

        return " ".join(guidance_parts)

    def _get_element_interaction(self, elem1: Element, elem2: Element) -> str | None:
        """Get guidance on elemental interaction."""
        # Check generating cycle
        if self._get_generating_target(elem1) == elem2:
            return f"The {elem1.value} element nourishes {elem2.value}. Allow this natural flow to support your actions."
        elif self._get_generating_target(elem2) == elem1:
            return f"The {elem2.value} element nourishes {elem1.value}. Draw on this supportive energy."

        # Check overcoming cycle
        if self._get_overcoming_target(elem1) == elem2:
            return f"The {elem1.value} element controls {elem2.value}. Use this influence wisely and avoid excess."
        elif self._get_overcoming_target(elem2) == elem1:
            return f"The {elem2.value} element controls {elem1.value}. Be mindful of constraints and work with them."

        return None

    def get_balance_score(self) -> float:
        """Calculate overall elemental balance (0.0 to 1.0)."""
        energies = [state.energy for state in self.elements.values()]
        mean_energy = sum(energies) / len(energies)
        variance = sum((e - mean_energy) ** 2 for e in energies) / len(energies)

        # High variance = low balance
        balance = max(0.0, 1.0 - variance * 2)
        return balance

    def get_harmony_score(self) -> float:
        """Calculate harmony based on proper elemental relationships."""
        score = 1.0

        # Check that generating cycle is flowing
        for element in Element:
            target = self._get_generating_target(element)
            if target:
                source_energy = self.elements[element].energy
                target_energy = self.elements[target].energy

                # If source is high but target is low, disharmony
                if source_energy > 0.7 and target_energy < 0.3:
                    score -= 0.2

        return max(0.0, score)

    def rebalance(self) -> None:
        """Attempt to rebalance all elements toward harmony."""
        balance = self.get_balance_score()

        if balance < self.harmony_threshold:
            # Gentle adjustment toward center
            for element, state in self.elements.items():
                if state.energy > 0.6:
                    self.adjust_element(element, -0.1, "balancing")
                elif state.energy < 0.4:
                    self.adjust_element(element, 0.1, "balancing")

    def get_current_cycle(self) -> str:
        """Describe the current elemental cycle phase."""
        # Find the most active element
        most_active = max(self.elements.items(), key=lambda x: x[1].energy)

        cycle_descriptions = {
            Element.WOOD: "Wood Phase - Time for growth, creativity, and new beginnings",
            Element.FIRE: "Fire Phase - Time for transformation, passion, and illumination",
            Element.EARTH: "Earth Phase - Time for stability, nourishment, and grounding",
            Element.METAL: "Metal Phase - Time for structure, precision, and refinement",
            Element.WATER: "Water Phase - Time for flow, wisdom, and reflection"
        }

        return cycle_descriptions[most_active[0]]


# Global instance
_wuxing_engine: WuXingEngine | None = None


def get_wuxing_engine() -> WuXingEngine:
    """Get the global Wu Xing engine instance."""
    global _wuxing_engine
    if _wuxing_engine is None:
        _wuxing_engine = WuXingEngine()
        logger.info("Wu Xing Engine initialized")
    return _wuxing_engine


def analyze_with_wuxing(situation: str) -> dict[str, Any]:
    """Convenience function to analyze a situation with Wu Xing."""
    engine = get_wuxing_engine()
    return engine.analyze_situation(situation)


def get_elemental_balance() -> dict[str, Any]:
    """Get current elemental balance."""
    engine = get_wuxing_engine()
    return {
        "balance": engine.get_balance_score(),
        "harmony": engine.get_harmony_score(),
        "cycle": engine.get_current_cycle()
    }
