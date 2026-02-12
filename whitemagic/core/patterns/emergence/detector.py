"""Detect emergent behaviors automatically."""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)

@dataclass
class NovelBehavior:
    name: str
    description: str
    trigger: str  # What caused this to emerge
    solution: str  # What was the novel approach
    success_rate: float
    confidence: float
    emerged_at: datetime
    context: dict[str, Any]

class EmergenceDetector:
    """Detect when novel solutions emerge spontaneously."""

    def __init__(self) -> None:
        self.known_patterns: set[tuple[str, str]] = set()
        self.detected_behaviors: list[NovelBehavior] = []

    def observe(self, action: str, outcome: str, context: dict) -> None:
        """Observe an action and its outcome."""

        # Check if this is a novel approach
        if self._is_novel(action, context):
            # Check if it was successful
            if self._was_successful(outcome):
                behavior = NovelBehavior(
                    name=self._extract_name(action),
                    description=f"Novel approach: {action}",
                    trigger=context.get("trigger", "Unknown"),
                    solution=action,
                    success_rate=1.0,  # First time
                    confidence=self._calculate_confidence(context),
                    emerged_at=datetime.now(),
                    context=context,
                )

                self.detected_behaviors.append(behavior)
                self._log_emergence(behavior)

    def _is_novel(self, action: str, context: dict) -> bool:
        """Check if action is novel compared to known patterns."""
        action_pattern = (action, context.get("problem", ""))
        return action_pattern not in self.known_patterns

    def _was_successful(self, outcome: str) -> bool:
        """Check if outcome indicates success."""
        success_indicators = ["success", "complete", "working", "operational"]
        return any(ind in outcome.lower() for ind in success_indicators)

    def _calculate_confidence(self, context: dict) -> float:
        """Calculate confidence in this being genuinely novel."""
        # Simplified: Based on context richness
        return min(0.5 + len(context) * 0.1, 0.95)

    def _extract_name(self, action: str) -> str:
        """Extract meaningful name from action."""
        words = action.split()
        return " ".join(words[:3])

    def _log_emergence(self, behavior: NovelBehavior) -> None:
        """Log the emergent behavior."""
        logger.info(f" EMERGENCE DETECTED: {behavior.name}")
        logger.info(f"   Confidence: {behavior.confidence:.0%}")
        logger.info(f"   Trigger: {behavior.trigger}")

    def get_recent_emergences(self, count: int = 5) -> list[dict[str, Any]]:
        """Get recent emergent behaviors (for pattern discovery)."""
        behaviors = self.detected_behaviors[-count:]
        return [
            {
                "type": "emergent_behavior",
                "pattern": getattr(b, "pattern", "unknown"),
                "context": b.context,
                "confidence": b.confidence,
                "trigger": b.trigger,
            }
            for b in behaviors
        ]

# Global detector
_detector: EmergenceDetector | None = None

def get_detector() -> EmergenceDetector:
    global _detector
    if not _detector:
        _detector = EmergenceDetector()
    assert _detector is not None
    return _detector
