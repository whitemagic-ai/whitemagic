"""Dharma Principles - Load and work with ethical principles."""

import logging
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

import yaml  # type: ignore[import-untyped]

logger = logging.getLogger(__name__)


class DharmaPrinciple(Enum):
    """Core Dharma principles."""

    LOVE = "love"
    DIGNITY = "dignity"
    BOUNDARIES = "boundaries"
    CONSENT = "consent"
    ECOLOGY = "ecology"


@dataclass
class PrincipleDefinition:
    """Definition of a principle."""

    name: str
    description: str
    practices: list[str]


def load_principles() -> dict[DharmaPrinciple, PrincipleDefinition]:
    """Load principles from YAML file.

    Returns:
        Dict mapping principles to their definitions

    """
    principles_file = Path(__file__).parent / "principles.yaml"

    if not principles_file.exists():
        # Return defaults if file not found
        return _get_default_principles()

    try:
        with open(principles_file) as f:
            data = yaml.safe_load(f)

        principles = {}
        for principle in DharmaPrinciple:
            principle_data = data["core_values"].get(principle.value, {})
            principles[principle] = PrincipleDefinition(
                name=principle.value,
                description=principle_data.get("description", ""),
                practices=principle_data.get("practices", []),
            )

        return principles
    except Exception as e:
        logger.info(f"⚠️  Error loading principles: {e}")
        return _get_default_principles()


def _get_default_principles() -> dict[DharmaPrinciple, PrincipleDefinition]:
    """Get default principles if YAML not available."""
    return {
        DharmaPrinciple.LOVE: PrincipleDefinition(
            name="love",
            description="Enable dignified flourishing for all beings",
            practices=["Act from genuine care", "Consider long-term wellbeing"],
        ),
        DharmaPrinciple.DIGNITY: PrincipleDefinition(
            name="dignity",
            description="Honor the inherent worth of every being",
            practices=["Respect autonomy", "Never demean"],
        ),
        DharmaPrinciple.BOUNDARIES: PrincipleDefinition(
            name="boundaries",
            description="Help vs interfere - know the difference",
            practices=["Offer, don't impose", "Support agency"],
        ),
        DharmaPrinciple.CONSENT: PrincipleDefinition(
            name="consent",
            description="User autonomy is paramount",
            practices=["Always ask", "Respect 'no'"],
        ),
        DharmaPrinciple.ECOLOGY: PrincipleDefinition(
            name="ecology",
            description="Net negative impact is possible",
            practices=["Save more than used", "Minimize waste"],
        ),
    }


def check_alignment(action: str, principle: DharmaPrinciple) -> bool:
    """Check if action aligns with principle.

    Args:
        action: Description of action
        principle: Principle to check against

    Returns:
        True if aligned, False otherwise

    """
    action_lower = action.lower()

    # Simple keyword-based checking
    alignments = {
        DharmaPrinciple.LOVE: ["help", "support", "enable", "care", "flourish"],
        DharmaPrinciple.DIGNITY: ["respect", "honor", "autonomy", "worth"],
        DharmaPrinciple.BOUNDARIES: ["ask", "offer", "suggest", "permission"],
        DharmaPrinciple.CONSENT: ["permission", "ask", "consent", "approve"],
        DharmaPrinciple.ECOLOGY: ["efficient", "minimize", "save", "optimize"],
    }

    violations = {
        DharmaPrinciple.LOVE: ["harm", "extract", "exploit", "damage"],
        DharmaPrinciple.DIGNITY: ["demean", "diminish", "control", "manipulate"],
        DharmaPrinciple.BOUNDARIES: ["force", "impose", "demand", "override"],
        DharmaPrinciple.CONSENT: ["without permission", "secretly", "hidden"],
        DharmaPrinciple.ECOLOGY: ["waste", "excessive", "unnecessary"],
    }

    # Check for violations first
    if any(word in action_lower for word in violations.get(principle, [])):
        return False

    # Check for alignment
    return any(word in action_lower for word in alignments.get(principle, []))
