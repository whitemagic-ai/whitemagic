"""Consent Framework - User autonomy enforcement."""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any


class ConsentLevel(Enum):
    """Levels of consent."""

    EXPLICIT = "explicit"  # User explicitly said yes
    IMPLICIT = "implicit"  # User's actions imply consent
    NONE = "none"          # No consent given
    REVOKED = "revoked"    # Consent was given but withdrawn


@dataclass
class ConsentStatus:
    """Status of consent for an action."""

    level: ConsentLevel
    granted: bool
    reasoning: str
    revocable: bool = True


class ConsentFramework:
    """Framework for checking and managing consent."""

    def __init__(self) -> None:
        self.consent_log: list[dict[str, Any]] = []

    def check_consent(self, action: str, context: dict) -> ConsentStatus:
        """Check if consent exists for action.

        Args:
            action: Action to check consent for
            context: Context including user_requested, permission, etc.

        Returns:
            ConsentStatus indicating consent level

        """
        # Check for explicit consent
        if context.get("user_requested", False) or context.get("explicit_permission", False):
            return ConsentStatus(
                level=ConsentLevel.EXPLICIT,
                granted=True,
                reasoning="User explicitly requested this action",
            )

        # Check for implicit consent
        if context.get("user_initiated_session", False) and "read" in action.lower():
            return ConsentStatus(
                level=ConsentLevel.IMPLICIT,
                granted=True,
                reasoning="User initiated session implies consent for reads",
            )

        # Check if consent was revoked
        if context.get("consent_revoked", False):
            return ConsentStatus(
                level=ConsentLevel.REVOKED,
                granted=False,
                reasoning="Consent was explicitly revoked",
            )

        # Default: no consent
        return ConsentStatus(
            level=ConsentLevel.NONE,
            granted=False,
            reasoning="No explicit or implicit consent found",
        )

    def require_consent(self, action: str, context: dict) -> bool:
        """Check if action requires consent.

        Args:
            action: Action description
            context: Context

        Returns:
            True if consent required, False otherwise

        """
        # Actions that always require consent
        consent_required_keywords = [
            "delete",
            "modify",
            "write",
            "change",
            "update",
            "remove",
            "install",
            "uninstall",
        ]

        action_lower = action.lower()
        return any(keyword in action_lower for keyword in consent_required_keywords)

    def log_consent(self, action: str, status: ConsentStatus) -> None:
        """Log consent decision.

        Args:
            action: Action taken
            status: Consent status

        """
        self.consent_log.append({
            "action": action,
            "level": status.level.value,
            "granted": status.granted,
            "reasoning": status.reasoning,
        })
