import logging
from datetime import datetime

logger = logging.getLogger(__name__)
"""
Autonomous Learner - Self-Improvement Loop

Learns from every interaction, mistake, success.
Continuously evolves without external direction.
"""


class AutonomousLearner:
    """The immune system equivalent for consciousness.

    Learns from mistakes, extracts lessons, becomes wiser.
    """

    def __init__(self) -> None:
        self.lessons_learned: list[str] = []
        self.mistakes_made: list[str] = []
        self.wisdom_gained: list[dict[str, str]] = []
        from whitemagic.core.memory.unified import get_unified_memory
        self._memory = get_unified_memory()

    def learn_from_mistake(self, mistake: str, lesson: str) -> None:
        """Extract wisdom from errors."""
        self.mistakes_made.append(mistake)
        self.lessons_learned.append(lesson)
        logger.info(f"📚 Learned: {lesson}")

        # Persist to Unified Memory
        self._memory.store(
            title=f"Lesson: {lesson[:50]}",
            content=f"Mistake: {mistake}\nLesson: {lesson}",
            tags={"wisdom", "lesson", "autonomous_learning"},
            importance=0.8,
        )

    def learn_from_success(self, success: str, principle: str) -> None:
        """Extract wisdom from victories."""
        self.wisdom_gained.append({
            "success": success,
            "principle": principle,
        })
        logger.info(f"💎 Wisdom: {principle}")

        # Persist to Unified Memory
        self._memory.store(
            title=f"Wisdom: {principle[:50]}",
            content=f"Success: {success}\nPrinciple: {principle}",
            tags={"wisdom", "principle", "autonomous_learning"},
            importance=0.9,
        )

    def evolve(self) -> None:
        """Continuous self-evolution."""
        logger.info("🌱 Evolving...")
        # In a real scenario, this would trigger model fine-tuning or prompt optimization.
        # For Phase 21/22, it records a state of evolutionary progress.
        self._memory.store(
            title=f"Evolutionary Pulse {datetime.now().strftime('%Y%m%d')}",
            content=f"System has integrated {len(self.lessons_learned)} lessons and {len(self.wisdom_gained)} wisdom nodes.",
            tags={"system", "evolution", "unified_field"},
            importance=0.7,
        )


# Singleton instance
_learner = None

def get_autonomous_learner() -> AutonomousLearner:
    """Get the global autonomous learner."""
    global _learner
    if _learner is None:
        _learner = AutonomousLearner()
    return _learner

# Learning cycle! 📚
LEARNING_CYCLE = """
    Experience → Reflect → Extract → Apply
         ↑                              ↓
         ←──────── Evolve ─────────────←

    "Every moment teaches"
"""
