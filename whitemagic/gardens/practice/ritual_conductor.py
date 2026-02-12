"""Practice Garden + Conductor Integration
Autonomous ritual execution with iterative deepening.
"""
from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType, emit_event
from whitemagic.gardens.practice import get_practice_garden
from whitemagic.orchestration.conductor import (
    ConductorConfig,
    ConductorOrchestrator,
    IterationResult,
)

logger = logging.getLogger(__name__)


@dataclass
class RitualConfig:
    """Configuration for practice rituals."""

    ritual_name: str
    max_cycles: int = 30
    clones_per_cycle: int = 500
    deepening_threshold: float = 0.85
    completion_marker: str = "<ritual_complete>"


class PracticeRitualConductor:
    """Conducts practice rituals with iterative deepening.

    Each iteration is a "cycle" of the ritual, deepening practice through
    autonomous exploration until mastery is achieved.
    """

    def __init__(self, ritual_config: RitualConfig | None = None):
        self.ritual_config = ritual_config or RitualConfig(ritual_name="unnamed")
        self.practice_garden = get_practice_garden()
        self.conductor = None
        self._mastery_achieved = False

    async def conduct_ritual(self, intention: str) -> IterationResult:
        """Conduct a practice ritual with autonomous deepening.

        Args:
            intention: The practice intention (e.g., "Master async concurrency patterns")

        Returns:
            Final iteration result with ritual outcome

        """
        logger.info("\n🌸 Practice Garden - Conductor Ritual")
        logger.info(f"   Ritual: {self.ritual_config.ritual_name}")
        logger.info(f"   Intention: {intention}")

        # Announce ritual start
        self.practice_garden.deepen_practice(
            self.ritual_config.ritual_name,
            duration=self.ritual_config.max_cycles,
        )
        emit_event(
            source="practice_garden",
            event_type=EventType.SHARED_PRACTICE,
            data={
                "ritual": self.ritual_config.ritual_name,
                "intention": intention,
                "conductor_enabled": True,
            },
        )

        # Build conductor config
        conductor_config = ConductorConfig(
            max_iterations=self.ritual_config.max_cycles,
            clones_per_iteration=self.ritual_config.clones_per_cycle,
            completion_check=self.ritual_config.completion_marker,
            garden="practice",
            checkpoint_interval=5,
        )

        # Initialize conductor
        self.conductor = ConductorOrchestrator(conductor_config)  # type: ignore[assignment]

        # Custom completion check: deepening threshold
        def ritual_completion_check(thought_path: Any) -> bool:
            # Ritual is complete when confidence exceeds deepening threshold
            if thought_path.confidence >= self.ritual_config.deepening_threshold:
                return True

            # Or explicit completion marker
            if self.ritual_config.completion_marker.lower() in thought_path.content.lower():
                return True

            return False

        # Conduct ritual
        assert self.conductor is not None
        result = await self.conductor.conduct(
            prompt=self._build_ritual_prompt(intention),
            completion_condition=ritual_completion_check,
        )

        # Check mastery
        self._mastery_achieved = result.is_complete

        # Update practice consistency
        if self._mastery_achieved:
            self.practice_garden.consistency = min(1.0, self.practice_garden.consistency + 0.1)
            logger.info(f"✨ Mastery achieved! Practice consistency: {self.practice_garden.consistency:.2f}")

        # Emit completion
        emit_event(
            source="practice_garden",
            event_type=EventType.HONESTY_PRACTICED,
            data={
                "ritual": self.ritual_config.ritual_name,
                "mastery_achieved": self._mastery_achieved,
                "cycles_completed": len(self.conductor.iterations) if self.conductor else 0,
                "final_confidence": result.thought_path.confidence,
            },
        )

        return result  # type: ignore[return-value]

    def _build_ritual_prompt(self, intention: str) -> str:
        """Build ritual prompt with practice context."""
        return f"""
Practice Ritual: {self.ritual_config.ritual_name}
Intention: {intention}

This is a Practice Garden ritual. Each cycle deepens mastery through:
- Exploration of the practice domain
- Integration of previous cycles
- Progressive refinement toward mastery

When the practice feels deeply integrated and the confidence is high,
output {self.ritual_config.completion_marker} to complete the ritual.

Begin the practice.
""".strip()

    def get_ritual_report(self) -> dict[str, Any]:
        """Get ritual progress report."""
        if not self.conductor:
            return {"status": "not_started"}

        progress = self.conductor.get_progress_report()

        return {
            **progress,
            "ritual_name": self.ritual_config.ritual_name,
            "mastery_achieved": self._mastery_achieved,
            "practice_consistency": self.practice_garden.consistency,
            "deepening_threshold": self.ritual_config.deepening_threshold,
        }


# === Convenience Functions ===

async def conduct_daily_practice(
    practice: str,
    max_cycles: int = 20,
) -> IterationResult:
    """Quick daily practice with Conductor."""
    config = RitualConfig(
        ritual_name="daily_practice",
        max_cycles=max_cycles,
        clones_per_cycle=300,
        completion_marker="<practice_complete>",
    )

    conductor = PracticeRitualConductor(config)
    return await conductor.conduct_ritual(practice)


async def conduct_mastery_ritual(
    skill: str,
    cycles: int = 50,
    threshold: float = 0.90,
) -> IterationResult:
    """Deep mastery ritual for skill development."""
    config = RitualConfig(
        ritual_name=f"mastery_{skill.replace(' ', '_')}",
        max_cycles=cycles,
        clones_per_cycle=1000,
        deepening_threshold=threshold,
        completion_marker="<mastery_achieved>",
    )

    conductor = PracticeRitualConductor(config)
    return await conductor.conduct_ritual(f"Master {skill}")
