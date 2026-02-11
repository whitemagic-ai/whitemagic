"""Base Agent Class
Integrates Continuity (Grounding) and Zodiac Routing.
"""
import logging
from typing import Any, cast

from whitemagic.continuity.grounding import GroundingSystem
from whitemagic.zodiac.zodiac_cores import get_zodiac_cores

logger = logging.getLogger(__name__)

class BaseAgent:
    """Base class for all WhiteMagic Agents.
    Provides:
    1. Grounding (Time/System context)
    2. Zodiac Capability (Astrological capability routing)
    3. Logging and Identity
    """

    def __init__(self, name: str, role: str):
        self.name = name
        self.role = role
        self.grounding = GroundingSystem()
        self.zodiac = get_zodiac_cores()
        self.context: dict[str, Any] = {}
        logger.info(f"Initialized Agent: {self.name} ({self.role})")

    def orient(self) -> dict[str, Any]:
        """Establish current spacetime coordinates."""
        anchor = self.grounding.anchor_self()
        self.context.update(anchor)
        return cast(dict[str, Any], anchor)

    def consult_zodiac(self, operation: str, intention: str, urgency: str = "normal") -> dict[str, Any]:
        """Ask the Zodiac Cores who is best suited for a task.
        Returns the best core name and its score.
        """
        context = {
            "operation": operation,
            "intention": intention,
            "urgency": urgency,
        }

        best_core = None
        best_score = -1.0

        for name, core in self.zodiac.get_all_cores().items():
            score = core.can_handle(context)
            if score > best_score:
                best_score = score
                best_core = name

        return {
            "best_core": best_core,
            "score": best_score,
            "all_scores": {name: core.can_handle(context) for name, core in self.zodiac.get_all_cores().items()},
        }

    def run(self, *args: Any, **kwargs: Any) -> Any:
        """Main execution method to be overridden."""
        self.orient()
        raise NotImplementedError("Agents must implement run()")
