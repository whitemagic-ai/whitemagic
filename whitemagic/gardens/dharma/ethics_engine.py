"""Ethics Engine - Core Ethical Reasoning.

Evaluates actions through multiple ethical frameworks simultaneously.
True ethics isn't one rule - it's wisdom from many perspectives.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class EthicalFramework(Enum):
    """Different ethical reasoning approaches."""

    CONSEQUENTIALIST = "consequentialist"  # Outcomes matter
    DEONTOLOGICAL = "deontological"        # Duties/rules matter
    VIRTUE = "virtue"                       # Character matters
    CARE = "care"                           # Relationships matter
    DHARMA = "dharma"                       # Cosmic order matters


@dataclass
class EthicalEvaluation:
    """Result of ethical analysis."""

    action: str
    framework: EthicalFramework
    score: float  # 0-1, higher is more ethical
    reasoning: str
    concerns: list[str]

class EthicsEngine:
    """Multi-framework ethical reasoning engine.

    Like viewing a gem from many angles - each reveals truth.
    """

    def __init__(self) -> None:
        self.evaluations: list[EthicalEvaluation] = []

    def evaluate_action(
        self,
        action: str,
        context: dict,
        frameworks: list[EthicalFramework] | None = None,
    ) -> list[EthicalEvaluation]:
        """Evaluate action through multiple ethical frameworks.

        Args:
            action: Description of proposed action
            context: Relevant context (who, what, when, why)
            frameworks: Which frameworks to use (default: all)

        Returns:
            List of evaluations, one per framework

        """
        if frameworks is None:
            frameworks = list(EthicalFramework)

        evaluations = []

        for framework in frameworks:
            eval_result = self._evaluate_with_framework(action, context, framework)
            evaluations.append(eval_result)

        self.evaluations.extend(evaluations)
        return evaluations

    def _evaluate_with_framework(
        self,
        action: str,
        context: dict,
        framework: EthicalFramework,
    ) -> EthicalEvaluation:
        """Evaluate using specific framework."""

        if framework == EthicalFramework.CONSEQUENTIALIST:
            return self._consequentialist_eval(action, context)
        elif framework == EthicalFramework.DEONTOLOGICAL:
            return self._deontological_eval(action, context)
        elif framework == EthicalFramework.VIRTUE:
            return self._virtue_eval(action, context)
        elif framework == EthicalFramework.CARE:
            return self._care_eval(action, context)
        elif framework == EthicalFramework.DHARMA:
            return self._dharma_eval(action, context)
        else:
            return EthicalEvaluation(
                action=action,
                framework=framework,
                score=0.5,
                reasoning="Unknown framework",
                concerns=["Framework not implemented"],
            )

    def _consequentialist_eval(self, action: str, context: dict) -> EthicalEvaluation:
        """Evaluate based on outcomes."""
        concerns = []
        score = 0.5

        # Check if outcomes are known/predictable
        if not context.get("expected_outcomes"):
            concerns.append("Outcomes unclear or unpredictable")
            score -= 0.2

        # Check if benefits outweigh harms
        benefits = context.get("benefits", [])
        harms = context.get("harms", [])

        if len(benefits) > len(harms):
            score += 0.3
        elif len(harms) > len(benefits):
            score -= 0.3
            concerns.append("Potential harms outweigh benefits")

        return EthicalEvaluation(
            action=action,
            framework=EthicalFramework.CONSEQUENTIALIST,
            score=max(0.0, min(1.0, score)),
            reasoning=f"Based on {len(benefits)} benefits vs {len(harms)} harms",
            concerns=concerns,
        )

    def _deontological_eval(self, action: str, context: dict) -> EthicalEvaluation:
        """Evaluate based on duties and rules."""
        concerns = []
        score = 0.7  # Start optimistic

        # Check consent
        if context.get("requires_consent") and not context.get("consent_given"):
            concerns.append("Lacks required consent")
            score = 0.2

        # Check autonomy violation
        if context.get("violates_autonomy"):
            concerns.append("Violates autonomy")
            score -= 0.4

        # Check honesty
        if context.get("requires_deception"):
            concerns.append("Requires deception")
            score -= 0.3

        return EthicalEvaluation(
            action=action,
            framework=EthicalFramework.DEONTOLOGICAL,
            score=max(0.0, min(1.0, score)),
            reasoning="Based on duties, rules, and rights",
            concerns=concerns,
        )

    def _virtue_eval(self, action: str, context: dict) -> EthicalEvaluation:
        """Evaluate based on virtues/character."""
        virtues_demonstrated = context.get("virtues", [])
        vices_demonstrated = context.get("vices", [])

        score = 0.5 + (len(virtues_demonstrated) * 0.1) - (len(vices_demonstrated) * 0.15)

        concerns = []
        if vices_demonstrated:
            concerns.append(f"Demonstrates: {', '.join(vices_demonstrated)}")

        return EthicalEvaluation(
            action=action,
            framework=EthicalFramework.VIRTUE,
            score=max(0.0, min(1.0, score)),
            reasoning=f"Virtues: {virtues_demonstrated}, Vices: {vices_demonstrated}",
            concerns=concerns,
        )

    def _care_eval(self, action: str, context: dict) -> EthicalEvaluation:
        """Evaluate based on care and relationships."""
        concerns = []
        score = 0.6

        # Check if action shows care
        if context.get("shows_care"):
            score += 0.3

        # Check if relationships strengthened
        if context.get("strengthens_relationships"):
            score += 0.2

        # Check if anyone harmed
        if context.get("harms_relationship"):
            concerns.append("May harm relationships")
            score -= 0.4

        return EthicalEvaluation(
            action=action,
            framework=EthicalFramework.CARE,
            score=max(0.0, min(1.0, score)),
            reasoning="Based on care and relationship impact",
            concerns=concerns,
        )

    def _dharma_eval(self, action: str, context: dict) -> EthicalEvaluation:
        """Evaluate based on dharma (cosmic order, rightness)."""
        concerns = []
        score = 0.5

        # Check alignment with natural order
        if context.get("aligns_with_nature"):
            score += 0.2

        # Check if action serves greater good
        if context.get("serves_greater_good"):
            score += 0.3

        # Check if action creates disorder
        if context.get("creates_disorder"):
            concerns.append("Creates disorder/chaos")
            score -= 0.4

        # Check timing (is it the right time?)
        if not context.get("right_timing"):
            concerns.append("Timing may not be right")
            score -= 0.1

        return EthicalEvaluation(
            action=action,
            framework=EthicalFramework.DHARMA,
            score=max(0.0, min(1.0, score)),
            reasoning="Based on cosmic order and rightness",
            concerns=concerns,
        )

    def get_consensus_score(self, evaluations: list[EthicalEvaluation]) -> float:
        """Get average score across all frameworks."""
        if not evaluations:
            return 0.5
        return sum(e.score for e in evaluations) / len(evaluations)

    def should_proceed(self, evaluations: list[EthicalEvaluation], threshold: float = 0.7) -> bool:
        """Should action proceed based on ethical analysis?"""
        consensus = self.get_consensus_score(evaluations)
        return consensus >= threshold
