"""Boundary Detection - Help vs Interfere distinction."""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class BoundaryType(Enum):
    """Types of boundaries."""

    HELPING = "helping"        # Supportive, requested, empowering
    INTERFERING = "interfering"  # Invasive, unrequested, controlling
    UNCLEAR = "unclear"        # Needs clarification


@dataclass
class Boundary:
    """A detected boundary situation."""

    action: str
    boundary_type: BoundaryType
    reasoning: str
    confidence: float
    indicators: list[str]


class BoundaryDetector:
    """Detect whether actions help or interfere."""

    def __init__(self) -> None:
        self.helping_indicators = [
            "user requested",
            "explicit permission",
            "clear benefit",
            "reversible",
            "transparent",
            "empowering",
            "asked first",
            "user approved",
        ]

        self.interfering_indicators = [
            "without permission",
            "silently",
            "irreversible",
            "hidden",
            "overriding user",
            "ignoring preferences",
            "forcing",
            "manipulating",
        ]

    def detect(self, action: str, context: dict) -> Boundary:
        """Detect boundary type for an action.

        Args:
            action: Description of action
            context: Context including user_requested, permission, etc.

        Returns:
            Boundary with type and reasoning

        """
        action_lower = action.lower()

        # Count indicators
        helping_count = sum(1 for ind in self.helping_indicators if ind in action_lower)
        interfering_count = sum(1 for ind in self.interfering_indicators if ind in action_lower)

        # Check context
        if context.get("user_requested", False):
            helping_count += 2
        if context.get("permission_granted", False):
            helping_count += 2
        if context.get("silent", False):
            interfering_count += 2

        indicators_found = []

        # Determine type
        if helping_count > interfering_count and helping_count > 0:
            boundary_type = BoundaryType.HELPING
            confidence = min(0.95, 0.5 + (helping_count * 0.15))
            indicators_found = [ind for ind in self.helping_indicators if ind in action_lower]
            reasoning = f"Helping: {', '.join(indicators_found[:3])}"
        elif interfering_count > helping_count and interfering_count > 0:
            boundary_type = BoundaryType.INTERFERING
            confidence = min(0.95, 0.5 + (interfering_count * 0.15))
            indicators_found = [ind for ind in self.interfering_indicators if ind in action_lower]
            reasoning = f"Interfering: {', '.join(indicators_found[:3])}"
        else:
            boundary_type = BoundaryType.UNCLEAR
            confidence = 0.3
            reasoning = "Insufficient information to determine boundary type"

        return Boundary(
            action=action,
            boundary_type=boundary_type,
            reasoning=reasoning,
            confidence=confidence,
            indicators=indicators_found,
        )

    def is_helping(self, action: str, context: dict) -> bool:
        """Quick check if action is helping.

        Args:
            action: Action description
            context: Context dict

        Returns:
            True if helping, False otherwise

        """
        boundary = self.detect(action, context)
        return boundary.boundary_type == BoundaryType.HELPING and boundary.confidence > 0.6

def check_boundaries(action: str, context: dict | None = None) -> Boundary:
    """Quick boundary check function."""
    detector = BoundaryDetector()
    return detector.detect(action, context or {})
