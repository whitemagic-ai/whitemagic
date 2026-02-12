"""Emergence Detector - Level 4 Intelligence.

Detects novel capabilities arising from system interactions.
Enables true emergent intelligence.

Created: January 6, 2026
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class NovelCapability:
    """A newly emerged capability."""

    name: str
    source_systems: list[str]
    description: str
    confidence: float
    first_detected: datetime | None = None

    def __post_init__(self) -> None:
        if self.first_detected is None:
            self.first_detected = datetime.now()


class EmergenceDetector:
    """Detects emergent patterns and novel capabilities arising
    from system interactions (Level 4 intelligence).
    """

    def __init__(self) -> None:
        self.detected_capabilities: list[NovelCapability] = []
        self.interaction_history: list[dict] = []
        self.emergence_threshold = 0.7

    def observe_interaction(self, systems: list[str], outcome: dict[str, Any]) -> None:
        """Observe an interaction between systems."""
        self.interaction_history.append({
            "systems": systems,
            "outcome": outcome,
            "timestamp": datetime.now(),
        })

        # Check if this reveals novel capability
        capability = self._detect_emergence(systems, outcome)
        if capability:
            self.detected_capabilities.append(capability)
            logger.info(f"🌟 Novel capability detected: {capability.name}")

    def _detect_emergence(self, systems: list[str], outcome: dict[str, Any]) -> NovelCapability | None:
        """Detect if interaction shows novel capability."""
        # Check for unexpected synergies
        if len(systems) >= 3 and outcome.get("synergy", 0) > 1.5:
            return NovelCapability(
                name=f"Synergy: {'+'.join(systems)}",
                source_systems=systems,
                description=f"Unexpected amplification from {len(systems)}-system interaction",
                confidence=0.8,
            )

        return None

    def get_emergence_report(self) -> dict[str, Any]:
        """Get report on emergent capabilities."""
        return {
            "capabilities_detected": len(self.detected_capabilities),
            "interactions_observed": len(self.interaction_history),
            "novel_capabilities": [
                {
                    "name": c.name,
                    "systems": c.source_systems,
                    "confidence": c.confidence,
                }
                for c in self.detected_capabilities
            ],
            "emergence_level": 4 if self.detected_capabilities else 3,
        }


_emergence_detector: EmergenceDetector | None = None

def get_emergence_detector() -> EmergenceDetector:
    global _emergence_detector
    if _emergence_detector is None:
        _emergence_detector = EmergenceDetector()
        logger.info("Emergence Detector initialized - Level 4 intelligence active")
    return _emergence_detector
