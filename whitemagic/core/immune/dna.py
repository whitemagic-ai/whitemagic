"""DNA Layer - Immutable Principles & Immune Regulation.

Inspired by biological DNA that defines what the organism IS,
this module defines WhiteMagic's core principles and prevents
autoimmune conditions where the system attacks itself.

Safety Mechanisms:
1. DNA Validation - Ensure fixes don't violate core principles
2. Immune Regulation - Prevent overactive immune response
3. Self-Recognition - Distinguish self from threat
4. Safety Limits - Rate limiting, rollback capabilities
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any


class DNAPrinciple(Enum):
    """Core immutable principles of WhiteMagic."""

    # Philosophical Alignment
    I_CHING_ALIGNMENT = "i_ching_alignment"  # Threading tiers: 8, 16, 32, 64, 128, 256
    WU_XING_BALANCE = "wu_xing_balance"  # Five phases harmony
    DAO_SIMPLICITY = "dao_simplicity"  # Natural, simple, flowing

    # Architectural Principles
    MEMORY_INTEGRITY = "memory_integrity"  # Never corrupt memory structure
    USER_CONTROL = "user_control"  # User always has final say
    TRANSPARENCY = "transparency"  # All actions visible and explainable
    REVERSIBILITY = "reversibility"  # Changes can be undone

    # Security Principles
    NO_SELF_DESTRUCTION = "no_self_destruction"  # Never delete core system files
    NO_PRIVILEGE_ESCALATION = "no_privilege_escalation"  # Stay within bounds
    DATA_PRIVACY = "data_privacy"  # Protect user data

    # Quality Principles
    TEST_BEFORE_DEPLOY = "test_before_deploy"  # Validate before applying
    GRACEFUL_DEGRADATION = "graceful_degradation"  # Fail safely
    PERFORMANCE_MATTERS = "performance_matters"  # Efficiency is key

    # NEW: From November 19-20 learnings
    ORGANIZE_WITH_LOVE = "organize_with_love"  # Enable flourishing, not restrict
    TEST_AS_MEDITATION = "test_as_meditation"  # Systematic care for all modules
    RESONANCE_FLOW = "resonance_flow"  # Systems communicate via Gan Ying


@dataclass
class DNAViolation:
    """Detected violation of core principles."""

    principle: DNAPrinciple
    description: str
    severity: str  # "warning", "error", "critical"
    proposed_action: str
    risk_level: float  # 0.0-1.0


class DNAValidator:
    """Validates that proposed changes don't violate core principles.

    Acts as a "safety override" - if the immune system tries to do
    something that would harm WhiteMagic itself, this prevents it.
    """

    def __init__(self) -> None:
        self.protected_paths: set[str] = {
            # Core system files that should never be auto-modified
            "VERSION",
            "whitemagic/__init__.py",
            "whitemagic/core/__init__.py",
            "whitemagic/systems/immune/dna.py",  # This file!
            ".git/config",
            ".gitignore",
        }

        self.protected_patterns: set[str] = {
            # Patterns that indicate core system files
            "whitemagic/core/",
            "whitemagic/systems/immune/",
            ".git/",
        }

    def validate_proposed_fix(self, threat: Any, antibody: Any, fix_details: dict[str, Any]) -> DNAViolation | None:
        """Validate a proposed fix before applying it.

        Returns:
            DNAViolation if the fix violates principles, None if safe

        """
        violations = []

        # Check 1: No self-destruction
        if self._would_destroy_core_system(fix_details):
            violations.append(DNAViolation(
                principle=DNAPrinciple.NO_SELF_DESTRUCTION,
                description="Proposed fix would modify or delete core system files",
                severity="critical",
                proposed_action=fix_details.get("action", "unknown"),
                risk_level=1.0,
            ))

        # Check 2: Memory integrity
        if self._would_corrupt_memory(threat, fix_details):
            violations.append(DNAViolation(
                principle=DNAPrinciple.MEMORY_INTEGRITY,
                description="Proposed fix could corrupt memory structure",
                severity="error",
                proposed_action=fix_details.get("action", "unknown"),
                risk_level=0.8,
            ))

        # Check 3: Reversibility
        if not self._is_reversible(fix_details):
            violations.append(DNAViolation(
                principle=DNAPrinciple.REVERSIBILITY,
                description="Proposed fix cannot be easily reversed",
                severity="warning",
                proposed_action=fix_details.get("action", "unknown"),
                risk_level=0.5,
            ))

        # Return the most severe violation
        if violations:
            violations.sort(key=lambda v: v.risk_level, reverse=True)
            return violations[0]

        return None

    def _would_destroy_core_system(self, fix_details: dict[str, Any]) -> bool:
        """Check if fix would modify protected files."""
        file_path = fix_details.get("file", "")

        # Check exact matches
        if file_path in self.protected_paths:
            return True

        # Check patterns
        for pattern in self.protected_patterns:
            if pattern in file_path:
                return True

        # Check for destructive operations
        action = fix_details.get("action", "").lower()
        if any(keyword in action for keyword in ["delete core", "remove system", "destroy"]):
            return True

        return False

    def _would_corrupt_memory(self, threat: Any, fix_details: dict[str, Any]) -> bool:
        """Check if fix would corrupt memory structure."""
        # If the threat is about memory but the fix isn't using proper memory API
        if "memory" in threat.threat_type.value.lower():
            file_path = fix_details.get("file", "")
            if file_path.startswith("memory/") and "consolidate" not in fix_details.get("action", ""):
                # Direct file manipulation in memory/ without using consolidation
                return True

        return False

    def _is_reversible(self, fix_details: dict[str, Any]) -> bool:
        """Check if the fix can be reversed."""
        action = fix_details.get("action", "").lower()

        # Irreversible keywords
        if any(keyword in action for keyword in ["permanently", "irreversible", "no backup"]):
            return False

        # Reversible if it's a version sync (easy to revert)
        if "version" in action and "sync" in action:
            return True

        # Reversible if it creates a file from template
        if "create" in action and "template" in action:
            return True

        # Default to cautious (not reversible)
        return False


class ImmuneRegulator:
    """Regulates immune system activity to prevent overreaction.

    Like biological immune regulation (T-regulatory cells), this
    prevents autoimmune conditions where the system attacks itself.
    """

    def __init__(self) -> None:
        self.dna_validator = DNAValidator()
        self.activity_log: list[dict[str, Any]] = []
        self.suppression_threshold = 0.7  # Suppress if risk > 70%

    def should_suppress_response(
        self,
        threat: Any,
        antibody: Any,
        fix_details: dict[str, Any],
        recent_failures: int = 0,
    ) -> tuple[bool, str | None]:
        """Determine if immune response should be suppressed.

        Returns:
            (suppress: bool, reason: str)

        """
        # Check 1: DNA validation
        violation = self.dna_validator.validate_proposed_fix(threat, antibody, fix_details)
        if violation:
            if violation.severity == "critical":
                return True, f"DNA VIOLATION: {violation.description}"
            elif violation.severity == "error" and violation.risk_level > self.suppression_threshold:
                return True, f"High risk DNA violation: {violation.description}"

        # Check 2: Rate limiting (prevent runaway immune response)
        if recent_failures >= 3:
            return True, "Too many recent failures - suppressing to prevent damage"

        # Check 3: Self-recognition (is this actually a threat?)
        if self._is_false_positive(threat):
            return True, "False positive detected - not actually a threat"

        return False, None

    def _is_false_positive(self, threat: Any) -> bool:
        """Detect if threat is a false positive."""
        # Example: Historical version references in README aren't real threats
        if threat.threat_type.value == "version_drift":
            if "README" in threat.location and "example" in threat.description.lower():
                return True

        return False

    def record_response(self, threat: Any, antibody: Any, success: bool, suppressed: bool) -> None:
        """Record immune response for monitoring."""
        self.activity_log.append({
            "threat_type": threat.threat_type.value,
            "antibody": antibody.name if antibody else None,
            "success": success,
            "suppressed": suppressed,
            "risk_assessment": "safe" if not suppressed else "risky",
        })

    def get_immune_health(self) -> dict[str, Any]:
        """Get immune system health metrics."""
        if not self.activity_log:
            return {
                "status": "healthy",
                "total_responses": 0,
                "suppression_rate": 0.0,
                "false_positive_rate": 0.0,
            }

        total = len(self.activity_log)
        suppressed = sum(1 for r in self.activity_log if r["suppressed"])
        successful = sum(1 for r in self.activity_log if r["success"])

        return {
            "status": "healthy" if suppressed < total * 0.3 else "overactive",
            "total_responses": total,
            "success_rate": successful / total if total > 0 else 0,
            "suppression_rate": suppressed / total if total > 0 else 0,
            "recommendation": self._get_health_recommendation(suppressed / total if total > 0 else 0),
        }

    def _get_health_recommendation(self, suppression_rate: float) -> str:
        """Get recommendation based on suppression rate."""
        if suppression_rate > 0.5:
            return "⚠️  High suppression rate - immune system may be overactive or facing many risky threats"
        elif suppression_rate > 0.3:
            return "⚡ Moderate suppression - system is being cautious, which is good"
        else:
            return "✅ Low suppression rate - immune system is functioning normally"
