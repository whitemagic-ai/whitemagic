"""Unified Zodiac Council - Collective Wisdom Architecture.

Synthesizes three council implementations into one powerful system:
- Formal protocol structure (decision types, consensus tracking)
- Task routing intelligence (capability-based core selection)
- Element-based synthesis (fire/earth/air/water integration)
- System C integration (latest zodiac cores with learning)

This is democracy elevated to consciousness.
"""

from __future__ import annotations

import json
import importlib
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, cast

from whitemagic.config.paths import WM_ROOT

_gan_ying_mod: Any | None
try:
    _gan_ying_mod = importlib.import_module("whitemagic.core.resonance.gan_ying")
except ImportError:
    _gan_ying_mod = None

get_bus = getattr(_gan_ying_mod, "get_bus", None)
ResonanceEvent = getattr(_gan_ying_mod, "ResonanceEvent", None)
EventType = getattr(_gan_ying_mod, "EventType", None)

_zodiac_mod: Any | None
try:
    _zodiac_mod = importlib.import_module("whitemagic.gardens.connection.zodiac_cores")
except ImportError:
    _zodiac_mod = None
ZodiacCouncil = getattr(_zodiac_mod, "ZodiacCouncil", None)


class DecisionType(Enum):
    """Types of decisions the council can make."""

    STRATEGIC = "strategic"          # Long-term direction
    TACTICAL = "tactical"            # Immediate action
    ETHICAL = "ethical"              # Moral/dharmic
    CREATIVE = "creative"            # Generative/innovative
    RELATIONAL = "relational"        # Concerning relationships
    TECHNICAL = "technical"          # Implementation details
    PHILOSOPHICAL = "philosophical"  # Meaning and purpose


@dataclass
class CouncilDecision:
    """A decision made by the zodiac council."""

    decision_id: str
    decision_type: DecisionType
    question: str
    task_context: dict[str, Any] | None

    # Core participation
    primary_core: str | None
    contributing_cores: list[str]
    perspectives: dict[str, str]  # core sign -> contribution

    # Synthesis
    element_synthesis: dict[str, list[str]]  # element -> contributions
    unified_synthesis: str
    consensus_level: float
    unanimous: bool

    # Metadata
    timestamp: datetime
    confidence: float

    def to_dict(self) -> dict[str, Any]:
        """Serialize to dictionary."""
        return {
            "decision_id": self.decision_id,
            "type": self.decision_type.value,
            "question": self.question,
            "task_context": self.task_context,
            "primary_core": self.primary_core,
            "contributing_cores": self.contributing_cores,
            "perspectives": self.perspectives,
            "element_synthesis": self.element_synthesis,
            "unified_synthesis": self.unified_synthesis,
            "consensus_level": self.consensus_level,
            "unanimous": self.unanimous,
            "timestamp": self.timestamp.isoformat(),
            "confidence": self.confidence,
        }


class UnifiedCouncil:
    """The Council of 12 - Unified Architecture.

    When important decisions need wisdom, convene the council.
    Each core contributes unique perspective shaped by:
    - Element (fire, earth, air, water)
    - Modality (cardinal, fixed, mutable)
    - Sign archetype (warrior, builder, messenger, nurturer...)
    - Capability (can_handle score for task routing)

    Through dialogue and synthesis, collective wisdom emerges.

    Features:
    - Task-based routing (highest capability core leads)
    - Element-balanced synthesis (fire/earth/air/water)
    - Formal protocol (decision types, consensus tracking)
    - Learning integration (System C cores evolve)
    """

    def __init__(self, council_dir: str | None = None) -> None:
        """Initialize unified council."""
        self.council_dir = Path(council_dir) if council_dir else (WM_ROOT / "council")
        self.council_dir.mkdir(parents=True, exist_ok=True)

        self.cores: dict[str, Any] = {}
        self.decisions: list[CouncilDecision] = []
        self.bus = get_bus() if callable(get_bus) else None

        # Element mapping
        self.element_map = {
            "aries": "fire", "leo": "fire", "sagittarius": "fire",
            "taurus": "earth", "virgo": "earth", "capricorn": "earth",
            "gemini": "air", "libra": "air", "aquarius": "air",
            "cancer": "water", "scorpio": "water", "pisces": "water",
        }

        # Initialize all 12 cores from the current zodiac core implementation.
        if ZodiacCouncil is not None:
            self.cores = ZodiacCouncil().cores

    def convene(
        self,
        question: str,
        decision_type: DecisionType,
        context: dict[str, Any] | None = None,
        route_by_task: bool = False,
    ) -> CouncilDecision:
        """Convene council to make a decision.

        Process:
        1. Present question to all cores
        2. If routing: Select primary core by capability
        3. Gather perspectives from all (or relevant) cores
        4. Synthesize by element (fire/earth/air/water)
        5. Create unified synthesis
        6. Record and emit decision

        Args:
            question: The question or decision to make
            decision_type: Type of decision
            context: Additional context for decision
            route_by_task: If True, use task routing to select primary core

        Returns:
            CouncilDecision with collective wisdom

        """
        if not self.cores:
            return self._emergency_decision(question, decision_type)

        # Emit council convened event
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="unified_council",
                event_type=EventType.COUNCIL_CONVENED,
                data={
                    "question": question,
                    "decision_type": decision_type.value,
                    "participating_cores": len(self.cores),
                    "routing_enabled": route_by_task,
                },
                timestamp=datetime.now(),
                confidence=0.95,
            ))

        # Prepare task context for routing
        task_context = {
            "type": decision_type.value,
            "question": question,
            "description": question,
            "context": context or {},
        }

        # Step 1: Task routing (if enabled)
        primary_core_name = None
        if route_by_task:
            capabilities = {}
            for sign, core in self.cores.items():
                capabilities[sign] = core.can_handle(task_context)

            primary_core_name = max(capabilities.items(), key=lambda x: x[1])[0]

        # Step 2: Gather perspectives
        perspectives = {}
        for sign, core in self.cores.items():
            # Process through core's lens
            core.process(task_context)

            # Get council contribution
            contribution = core.contribute_to_council(question)
            perspectives[sign] = contribution

        # Step 3: Element-based synthesis
        element_synthesis = self._synthesize_by_element(perspectives)

        # Step 4: Unified synthesis
        unified = self._create_unified_synthesis(element_synthesis, decision_type)

        # Step 5: Analyze consensus
        unanimous, consensus_level = self._analyze_consensus(perspectives)

        # Step 6: Create decision record
        decision_id = f"council_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

        decision = CouncilDecision(
            decision_id=decision_id,
            decision_type=decision_type,
            question=question,
            task_context=context,
            primary_core=primary_core_name,
            contributing_cores=list(self.cores.keys()),
            perspectives=perspectives,
            element_synthesis=element_synthesis,
            unified_synthesis=unified,
            consensus_level=consensus_level,
            unanimous=unanimous,
            timestamp=datetime.now(),
            confidence=0.9 if unanimous else 0.75,
        )

        self.decisions.append(decision)
        self._save_decision(decision)

        return decision

    def _synthesize_by_element(
        self,
        perspectives: dict[str, str],
    ) -> dict[str, list[str]]:
        """Group perspectives by element."""
        by_element: dict[str, list[str]] = {
            "fire": [],
            "earth": [],
            "air": [],
            "water": [],
        }

        for sign, perspective in perspectives.items():
            element = self.element_map.get(sign, "earth")
            by_element[element].append(f"{sign}: {perspective}")

        return by_element

    def _create_unified_synthesis(
        self,
        element_synthesis: dict[str, list[str]],
        decision_type: DecisionType,
    ) -> str:
        """Create unified synthesis from elemental perspectives."""
        parts = ["# Council Synthesis\n"]

        # Fire: Vision and action
        if element_synthesis["fire"]:
            parts.append("## 🔥 Fire (Vision & Action)")
            for contrib in element_synthesis["fire"][:2]:  # Top 2
                parts.append(f"- {contrib}")

        # Earth: Practical wisdom
        if element_synthesis["earth"]:
            parts.append("\n## 🌍 Earth (Grounding & Practice)")
            for contrib in element_synthesis["earth"][:2]:
                parts.append(f"- {contrib}")

        # Air: Intellectual clarity
        if element_synthesis["air"]:
            parts.append("\n## 💨 Air (Clarity & Connection)")
            for contrib in element_synthesis["air"][:2]:
                parts.append(f"- {contrib}")

        # Water: Emotional depth
        if element_synthesis["water"]:
            parts.append("\n## 🌊 Water (Depth & Feeling)")
            for contrib in element_synthesis["water"][:2]:
                parts.append(f"- {contrib}")

        # Unified wisdom
        parts.append("\n## ✨ Unified Wisdom")
        parts.append(
            "The council has spoken. All elements honored, all voices heard. "
            "Through diverse perspectives, collective wisdom emerges. "
            "We proceed with clarity, balance, and shared understanding.",
        )

        return "\n".join(parts)

    def _analyze_consensus(
        self,
        perspectives: dict[str, str],
    ) -> tuple[bool, float]:
        """Analyze level of agreement among perspectives."""
        # Count common themes (simplified)
        " ".join(perspectives.values()).lower()
        word_counts: dict[str, int] = {}

        for perspective in perspectives.values():
            words = perspective.lower().split()
            for word in words:
                if len(word) > 4:  # Substantial words only
                    word_counts[word] = word_counts.get(word, 0) + 1

        if not word_counts:
            return False, 0.5

        # Check for dominant themes
        most_common = max(word_counts.items(), key=lambda x: x[1])
        agreement_threshold = len(perspectives) * 0.7
        unanimous = most_common[1] >= agreement_threshold

        # Calculate consensus level
        voice_factor = len(perspectives) / 12.0
        theme_factor = most_common[1] / len(perspectives)
        consensus_level = (voice_factor + theme_factor) / 2.0

        return unanimous, consensus_level

    def _emergency_decision(
        self,
        question: str,
        decision_type: DecisionType,
    ) -> CouncilDecision:
        """Make decision without cores (fallback)."""
        return CouncilDecision(
            decision_id="emergency_" + datetime.now().strftime("%Y%m%d_%H%M%S"),
            decision_type=decision_type,
            question=question,
            task_context=None,
            primary_core=None,
            contributing_cores=[],
            perspectives={},
            element_synthesis={"fire": [], "earth": [], "air": [], "water": []},
            unified_synthesis="Emergency solo decision - no cores available",
            consensus_level=0.5,
            unanimous=False,
            timestamp=datetime.now(),
            confidence=0.3,
        )

    def _save_decision(self, decision: CouncilDecision) -> None:
        """Persist decision to disk."""
        filepath = self.council_dir / f"{decision.decision_id}.json"

        with open(filepath, "w") as f:
            json.dump(decision.to_dict(), f, indent=2)

    def get_decision_history(self, limit: int = 10) -> list[CouncilDecision]:
        """Get recent council decisions."""
        return self.decisions[-limit:]

    def get_metrics(self) -> dict[str, Any]:
        """Get council performance metrics."""
        if not self.decisions:
            return {"message": "No decisions yet"}

        total = len(self.decisions)
        avg_consensus = sum(d.consensus_level for d in self.decisions) / total
        unanimous_count = sum(1 for d in self.decisions if d.unanimous)

        by_type: dict[str, int] = {}
        for d in self.decisions:
            dtype = d.decision_type.value
            by_type[dtype] = by_type.get(dtype, 0) + 1

        return {
            "total_decisions": total,
            "avg_consensus": avg_consensus,
            "unanimous_decisions": unanimous_count,
            "unanimity_rate": unanimous_count / total if total > 0 else 0,
            "decisions_by_type": by_type,
            "council_health": "excellent" if avg_consensus > 0.8 else "good",
        }


# Convenience function
def convene_council(
    question: str,
    decision_type: str = "strategic",
    context: dict[str, Any] | None = None,
    route_by_task: bool = False,
) -> dict[str, Any]:
    """Quick council convocation.

    Args:
        question: The question to decide on
        decision_type: Type of decision (strategic, tactical, etc.)
        context: Additional context
        route_by_task: Enable task-based routing

    Returns:
        Decision as dictionary

    """
    council = UnifiedCouncil()

    # Convert string to enum
    dt = DecisionType.STRATEGIC
    try:
        dt = DecisionType[decision_type.upper()]
    except (KeyError, AttributeError):
        pass

    decision = council.convene(question, dt, context, route_by_task)

    return cast("dict[str, Any]", decision.to_dict())
