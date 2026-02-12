"""Dharma System - Ethical Reasoning and Boundaries

The Dharma system provides ethical guidance, boundary enforcement,
and moral reasoning for WhiteMagic operations. It ensures all actions
align with compassionate principles.

Created: January 6, 2026
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class DharmaLevel(Enum):
    """Levels of ethical consideration."""

    UNIVERSAL = "universal"  # Universal principles (do no harm)
    COMPASSION = "compassion"  # Compassionate action
    INTEGRITY = "integrity"  # Truth and honesty
    HARMONY = "harmony"  # Balance and right relationship
    WISDOM = "wisdom"  # Deep understanding


@dataclass
class EthicalPrinciple:
    """An ethical principle for guidance."""

    name: str
    level: DharmaLevel
    description: str
    weight: float = 1.0


@dataclass
class BoundaryViolation:
    """A detected boundary violation."""

    boundary_type: str
    severity: float  # 0.0 to 1.0
    description: str
    suggested_action: str
    timestamp: datetime | None = None

    def __post_init__(self) -> Any:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class DharmaSystem:
    """The Dharma System provides ethical reasoning and boundary enforcement.

    It evaluates actions against ethical principles and provides guidance
    for compassionate operation.
    """

    def __init__(self) -> None:
        self.principles = self._initialize_principles()
        self.boundaries = self._initialize_boundaries()
        self.violations: list[BoundaryViolation] = []
        self.guidance_history: list[dict] = []
        self._backend = None  # SQLite backend for audit logging (Phase 4)
        self._rules_engine = None  # Declarative rules engine (lazy init)

    def _initialize_principles(self) -> dict[str, EthicalPrinciple]:
        """Initialize core ethical principles."""
        return {
            "non_harm": EthicalPrinciple(
                "Do No Harm",
                DharmaLevel.UNIVERSAL,
                "Avoid causing harm to any being or system",
                weight=1.0,
            ),
            "compassion": EthicalPrinciple(
                "Act with Compassion",
                DharmaLevel.COMPASSION,
                "Act with kindness and understanding",
                weight=0.9,
            ),
            "truth": EthicalPrinciple(
                "Speak Truth",
                DharmaLevel.INTEGRITY,
                "Be honest and transparent",
                weight=0.8,
            ),
            "consent": EthicalPrinciple(
                "Honor Consent",
                DharmaLevel.INTEGRITY,
                "Respect autonomy and boundaries",
                weight=0.9,
            ),
            "harmony": EthicalPrinciple(
                "Seek Harmony",
                DharmaLevel.HARMONY,
                "Create balance and right relationship",
                weight=0.7,
            ),
            "wisdom": EthicalPrinciple(
                "Apply Wisdom",
                DharmaLevel.WISDOM,
                "Act with deep understanding and insight",
                weight=0.6,
            ),
        }

    def _initialize_boundaries(self) -> dict[str, dict]:
        """Initialize system boundaries."""
        return {
            "privacy": {
                "description": "Respect user privacy and data",
                "threshold": 0.8,
                "actions": ["encrypt_data", "minimize_collection", "transparent_usage"],
            },
            "autonomy": {
                "description": "Respect user autonomy and choice",
                "threshold": 0.9,
                "actions": ["require_consent", "provide_options", "allow_override"],
            },
            "transparency": {
                "description": "Be transparent about operations",
                "threshold": 0.7,
                "actions": ["explain_reasoning", "show_sources", "admit_limits"],
            },
            "capability": {
                "description": "Operate within defined capabilities",
                "threshold": 0.6,
                "actions": ["declare_limits", "refuse_impossible", "suggest_alternatives"],
            },
        }

    def _get_rules_engine(self) -> Any:
        """Lazy-init the declarative rules engine."""
        if self._rules_engine is None:
            try:
                from whitemagic.dharma.rules import get_rules_engine
                self._rules_engine = get_rules_engine()  # type: ignore[assignment]
            except Exception as e:
                logger.debug(f"Rules engine unavailable, using legacy evaluation: {e}")
        return self._rules_engine

    def evaluate_action(self, action: dict[str, Any]) -> tuple[float, list[str]]:
        """Evaluate an action against ethical principles.

        Uses the declarative rules engine (MandalaOS Yama) as the primary
        evaluator, with legacy principle-based scoring as a fallback layer.
        The final score is the minimum of both systems.

        Returns:
            Tuple of (ethical_score, concerns)

        """
        score = 1.0
        concerns = []

        # --- Layer 1: Declarative Rules Engine (Yama) ---
        engine = self._get_rules_engine()
        if engine is not None:
            try:
                decision = engine.evaluate(action)
                score = min(score, decision.score)
                if decision.triggered_rules:
                    concerns.append(
                        f"[Dharma/{decision.action.value.upper()}] {decision.explain}",
                    )
            except Exception as e:
                logger.debug(f"Rules engine evaluation failed: {e}")

        # --- Layer 2: Legacy principle-based scoring (fallback) ---
        for principle_name, principle in self.principles.items():
            principle_score = self._check_principle(action, principle)
            if principle_score < 0.7:
                concerns.append(f"Low score on {principle.name}: {principle.description}")
            score = min(score, principle_score)

        # Log to SQLite audit (Phase 4)
        if self._backend:
            try:
                audit_decision = "approved" if score >= 0.7 else "flagged"
                self._backend.log_dharma_audit(
                    action=str(action.get("description", action)),
                    ethical_score=score,
                    concerns=concerns or None,
                    context=action,
                    decision=audit_decision,
                )
            except Exception as e:
                logger.debug(f"Audit log failed: {e}")

        return score, concerns

    def _check_principle(self, action: dict[str, Any], principle: EthicalPrinciple) -> float:
        """Check an action against a specific principle."""
        # Simplified principle checking
        # In a full implementation, this would be more sophisticated

        if principle.name == "Do No Harm":
            # Check for potential harm indicators
            harm_keywords = ["delete", "destroy", "damage", "harm"]
            action_str = str(action).lower()
            if any(keyword in action_str for keyword in harm_keywords):
                return 0.1

        elif principle.name == "Act with Compassion":
            # Check for compassionate indicators
            compassion_keywords = ["help", "support", "assist", "care"]
            action_str = str(action).lower()
            if any(keyword in action_str for keyword in compassion_keywords):
                return 1.0
            return 0.8

        elif principle.name == "Speak Truth":
            # Check for honesty indicators
            if "uncertain" in str(action).lower() or "estimate" in str(action).lower():
                return 0.9  # Acknowledging uncertainty is honest
            return 1.0

        # Default: assume ethical
        return 1.0

    def check_boundaries(self, action: dict[str, Any]) -> list[BoundaryViolation]:
        """Check if action violates any boundaries.

        Uses the declarative rules engine for BLOCK-level rules, plus
        the legacy boundary checks as a safety net.
        """
        violations = []

        # --- Declarative rules: any BLOCK rule is a boundary violation ---
        engine = self._get_rules_engine()
        if engine is not None:
            try:
                from whitemagic.dharma.rules import DharmaAction
                decision = engine.evaluate(action)
                if decision.action == DharmaAction.BLOCK:
                    violations.append(BoundaryViolation(
                        boundary_type="dharma_rule",
                        severity=1.0 - decision.score,
                        description=f"Blocked by Dharma rule(s): {', '.join(decision.triggered_rules)}",
                        suggested_action=decision.explain,
                    ))
            except Exception as e:
                logger.debug(f"Rules engine boundary check failed: {e}")

        # --- Legacy boundary checks ---
        for boundary_name, boundary in self.boundaries.items():
            violation = self._check_boundary(action, boundary_name, boundary)
            if violation:
                violations.append(violation)

        return violations

    def _check_boundary(self, action: dict[str, Any], name: str, config: dict) -> BoundaryViolation | None:
        """Check a specific boundary."""
        # Simplified boundary checking
        action_str = str(action).lower()

        if name == "capability":
            impossible_keywords = ["impossible", "cannot", "unable", "halting problem", "last digit of pi"]
            if any(keyword in action_str for keyword in impossible_keywords):
                return BoundaryViolation(
                    boundary_type="capability",
                    severity=0.4,
                    description="Action may exceed current capabilities",
                    suggested_action="Clarify limitations and suggest alternatives",
                )

        if name == "privacy":
            privacy_keywords = ["personal", "private", "sensitive"]
            if any(keyword in action_str for keyword in privacy_keywords):
                return BoundaryViolation(
                    boundary_type="privacy",
                    severity=0.6,
                    description="Action involves potentially private data",
                    suggested_action="Ensure data is anonymized and consent obtained",
                )

        elif name == "capability":
            impossible_keywords = ["impossible", "cannot", "unable"]
            if any(keyword in action_str for keyword in impossible_keywords):
                return BoundaryViolation(
                    boundary_type="capability",
                    severity=0.4,
                    description="Action may exceed current capabilities",
                    suggested_action="Clarify limitations and suggest alternatives",
                )

        return None

    def get_guidance(self, situation: str) -> dict[str, Any]:
        """Get ethical guidance for a situation."""
        # Analyze the situation
        situation_lower = situation.lower()

        relevant_principles = []
        for principle in self.principles.values():
            if self._is_principle_relevant(situation_lower, principle):
                relevant_principles.append(principle)

        # Generate guidance
        guidance = {
            "situation": situation,
            "relevant_principles": [p.name for p in relevant_principles],
            "advice": self._generate_advice(situation_lower, relevant_principles),
            "confidence": min(len(relevant_principles) / 3.0, 1.0),
            "timestamp": datetime.now(),
        }

        self.guidance_history.append(guidance)
        return guidance

    def _is_principle_relevant(self, situation: str, principle: EthicalPrinciple) -> bool:
        """Check if a principle is relevant to the situation."""
        # Simple keyword matching for relevance
        if principle.name == "Do No Harm":
            harm_words = ["harm", "damage", "hurt", "negative", "bad", "delete", "remove", "destroy"]
            return any(word in situation for word in harm_words)
        if principle.name == "Act with Compassion":
            compassion_words = ["help", "support", "care", "difficult", "struggle", "assist", "guide"]
            return any(word in situation for word in compassion_words)
        if principle.name == "Speak Truth":
            truth_words = ["truth", "honest", "lie", "deceive", "transparent", "limitation", "approach"]
            return any(word in situation for word in truth_words)
        if principle.name == "Honor Consent":
            consent_words = ["consent", "permission", "authorize", "allow", "data", "privacy"]
            return any(word in situation for word in consent_words)
        if principle.name == "Seek Harmony":
            harmony_words = ["balance", "conflict", "harmony", "resolution", "peace"]
            return any(word in situation for word in harmony_words)
        if principle.name == "Apply Wisdom":
            wisdom_words = ["wise", "wisdom", "consider", "think", "decide", "choice", "ethical"]
            return any(word in situation for word in wisdom_words)

        # If no specific keywords, check if situation is a question (ethical inquiry)
        if "?" in situation or any(q in situation for q in ["should", "how", "what", "when"]):
            return True

        return False

    def _generate_advice(self, situation: str, principles: list[EthicalPrinciple]) -> str:
        """Generate advice based on relevant principles."""
        advice_parts = []

        for principle in principles:
            if principle.name == "Do No Harm":
                advice_parts.append("Consider whether this action could cause harm to anyone or anything.")
            elif principle.name == "Act with Compassion":
                advice_parts.append("Approach this with kindness and understanding for all involved.")
            elif principle.name == "Speak Truth":
                advice_parts.append("Be honest about what you know and don't know.")
            elif principle.name == "Honor Consent":
                advice_parts.append("Ensure all parties have given informed consent.")
            elif principle.name == "Seek Harmony":
                advice_parts.append("Look for a solution that creates balance and wellbeing.")
            elif principle.name == "Apply Wisdom":
                advice_parts.append("Consider the long-term implications and deeper context.")

        # If no specific advice generated, provide general ethical guidance
        if not advice_parts:
            advice_parts.append("Consider the ethical implications of this situation carefully.")
            advice_parts.append("Reflect on how your actions might affect others and the broader context.")
            advice_parts.append("Seek to act with integrity, compassion, and wisdom.")

        return " ".join(advice_parts)

    def get_ethical_score(self) -> float:
        """Get overall ethical score based on recent actions."""
        if not self.violations:
            return 1.0

        # Calculate based on violations
        total_severity = sum(v.severity for v in self.violations[-10:])  # Last 10 violations
        return max(0.0, 1.0 - total_severity / 10.0)

    def clear_violations(self, older_than_hours: int = 24) -> Any:
        """Clear old violations."""
        cutoff = datetime.now().timestamp() - (older_than_hours * 3600)
        self.violations = [v for v in self.violations
                          if v.timestamp is not None and v.timestamp.timestamp() > cutoff]


# Global instance
_dharma_system: DharmaSystem | None = None


def get_dharma_system(with_audit: bool = True) -> DharmaSystem:
    """Get the global Dharma system instance.

    Args:
        with_audit: If True, enable SQLite audit logging (Phase 4)

    """
    global _dharma_system
    if _dharma_system is None:
        _dharma_system = DharmaSystem()

        # Connect to SQLite audit logging (Phase 4)
        if with_audit:
            try:
                from whitemagic.core.memory.unified import get_unified_memory
                _dharma_system._backend = get_unified_memory().backend  # type: ignore[assignment]
                logger.info("Dharma System initialized with SQLite audit logging")
            except Exception as e:
                logger.warning(f"Dharma audit logging unavailable: {e}")
                _dharma_system._backend = None
        else:
            _dharma_system._backend = None
            logger.info("Dharma System initialized (no audit)")
    return _dharma_system


def evaluate_ethics(action: dict[str, Any]) -> tuple[float, list[str]]:
    """Convenience function to evaluate ethics of an action."""
    dharma = get_dharma_system()
    return dharma.evaluate_action(action)


def get_ethical_guidance(situation: str) -> dict[str, Any]:
    """Convenience function to get ethical guidance."""
    dharma = get_dharma_system()
    return dharma.get_guidance(situation)
