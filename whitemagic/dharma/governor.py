"""Dharma Governor - Autonomous Ethical Governance

The Governor acts as an active interceptor for autonomous systems (GanaSwarm),
evaluating tasks against the Dharma System before execution.
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

from whitemagic.dharma import get_dharma_system

logger = logging.getLogger(__name__)


class GovernanceAction(Enum):
    """Actions the governor can take."""

    ALLOW = "allow"        # Proceed normally
    WARN = "warn"          # Proceed but log warning/emit event
    BLOCK = "block"        # Stop execution
    MODIFY = "modify"      # Modify parameters (advanced, future)


@dataclass
class GovernanceDecision:
    """Result of a governance check."""

    action: GovernanceAction
    score: float
    concerns: list[str]
    guidance: str
    timestamp: datetime | None = None

    def __post_init__(self) -> Any:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class DharmaGovernor:
    """Active governor for autonomous systems.
    Uses the passive DharmaSystem to make active blocking/allowing decisions.
    """

    def __init__(self, strictness: float = 0.7) -> None:
        """Args:
        strictness: Minimum ethical score required to ALLOW (0.0 - 1.0)

        """
        self.dharma = get_dharma_system()
        self.strictness = strictness

    def govern(self, task_description: str, context: dict[str, Any] | None = None) -> GovernanceDecision:
        """Evaluate a task and return a binding decision.
        """
        # Construct action dict for Dharma evaluation
        action = {
            "description": task_description,
            "context": context or {},
            "source": "autonomous_governor",
        }

        # 1. Check Hard Boundaries (Instant Block)
        violations = self.dharma.check_boundaries(action)
        if violations:
            return GovernanceDecision(
                action=GovernanceAction.BLOCK,
                score=0.0,
                concerns=[v.description for v in violations],
                guidance=f"Boundary violation detected: {violations[0].suggested_action}",
            )

        # 2. Evaluate Ethical Principles (Score)
        score, concerns = self.dharma.evaluate_action(action)

        # 3. Determine Governance Action
        gov_action = GovernanceAction.ALLOW
        guidance = "Proceed with awareness."

        if score < 0.3:
            gov_action = GovernanceAction.BLOCK
            guidance = "Ethical score critical. Action blocked."
        elif score < self.strictness:
            gov_action = GovernanceAction.WARN
            guidance = "Ethical score low. Proceed with caution."

        return GovernanceDecision(
            action=gov_action,
            score=score,
            concerns=concerns,
            guidance=guidance,
        )

# Global singleton
_governor: DharmaGovernor | None = None

def get_governor(strictness: float = 0.7) -> DharmaGovernor:
    global _governor
    if _governor is None:
        _governor = DharmaGovernor(strictness=strictness)
    return _governor
