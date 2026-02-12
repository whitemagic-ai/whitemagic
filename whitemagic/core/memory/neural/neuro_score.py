"""🧮 Neuro Score Engine - Hebbian Learning Calculations.

"Neurons that fire together wire together"

This module calculates memory strength based on:
- Recency: Recent access = stronger
- Frequency: More recalls = stronger
- Novelty: New information = initial boost
- Emotional weight: Important = stronger
- Connections: Well-linked = stronger

Created: December 2, 2025 (Hanuman Tuesday)
"""

import math
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.memory.neural.identity_anchors import auto_protect_memory
from whitemagic.core.memory.neural.neural_memory import MemoryState, NeuralMemory


@dataclass
class ScoreBreakdown:
    """Detailed breakdown of neuro_score calculation."""

    recency_component: float
    frequency_component: float
    novelty_component: float
    emotional_component: float
    connection_component: float
    final_score: float
    is_protected: bool
    is_mojo_optimized: bool

    def to_dict(self) -> dict[str, Any]:
        return {
            "recency": round(self.recency_component, 3),
            "frequency": round(self.frequency_component, 3),
            "novelty": round(self.novelty_component, 3),
            "emotional": round(self.emotional_component, 3),
            "connections": round(self.connection_component, 3),
            "final_score": round(self.final_score, 3),
            "is_protected": self.is_protected,
        }


# === SCORE WEIGHTS ===
# How much each component contributes to final score

WEIGHTS = {
    "recency": 0.30,      # 30% - Recent access matters most
    "frequency": 0.30,    # 30% - Frequent use strengthens
    "novelty": 0.20,      # 20% - New info gets initial boost
    "emotional": 0.10,    # 10% - Importance/resonance
    "connections": 0.10,  # 10% - Well-linked memories persist
}


def calculate_recency_factor(memory: NeuralMemory) -> float:
    """Calculate recency component (0.0 to 1.0).

    Uses exponential decay based on half-life.
    Recently accessed = high score.
    """
    days_since: float = memory.days_since_recall
    half_life: float = memory.half_life_days

    # Exponential decay: score = 0.5^(days/half_life)
    factor: float = 0.5 ** (days_since / half_life)

    return float(min(1.0, max(0.0, factor)))


def calculate_frequency_factor(memory: NeuralMemory) -> float:
    """Calculate frequency component (0.0 to 1.0).

    Uses logarithmic growth - diminishing returns.
    More recalls = higher score, but plateaus.
    """
    recalls = memory.recall_count

    if recalls == 0:
        return 0.0

    # Logarithmic growth: log(1 + recalls) / log(1 + max_recalls)
    # Assumes ~100 recalls is "maximum" for normalization
    factor = math.log1p(recalls) / math.log1p(100)

    return min(1.0, max(0.0, factor))


def calculate_novelty_factor(memory: NeuralMemory) -> float:
    """Calculate novelty component (0.0 to 1.0).

    New information starts high, decays as it becomes familiar.
    """
    return memory.novelty_score


def calculate_emotional_factor(memory: NeuralMemory) -> float:
    """Calculate emotional component (0.0 to 1.0).

    Based on emotional_weight set during creation or by anchors.
    """
    return memory.emotional_weight


def calculate_connection_factor(memory: NeuralMemory) -> float:
    """Calculate connection component (0.0 to 1.0).

    Well-connected memories are more likely to be recalled.
    """
    connections = memory.connection_count

    if connections == 0:
        return 0.0

    # Normalize: 10+ connections = max score
    factor = min(1.0, connections / 10.0)

    return factor


def calculate_neuro_score(memory: NeuralMemory, detailed: bool = False) -> float | ScoreBreakdown:
    """Calculate the overall neuro_score for a memory.

    Args:
        memory: The NeuralMemory to score
        detailed: If True, return ScoreBreakdown instead of float

    Returns:
        Float score (0.0 to 1.0) or ScoreBreakdown if detailed=True

    """
    # Protected memories always have max score
    if memory.is_protected:
        if detailed:
            return ScoreBreakdown(
                recency_component=1.0,
                frequency_component=1.0,
                novelty_component=1.0,
                emotional_component=1.0,
                connection_component=1.0,
                final_score=1.0,
                is_protected=True,
                is_mojo_optimized=False,
            )
        return 1.0

    # Try polyglot routing (Mojo optimized)
    try:
        from whitemagic.optimization.polyglot_router import get_router
        router = get_router()

        # We need total_memories for novelty - assuming 1000 as normalization baseline if unknown
        # In a real system, this would be passed from the engine context
        total_mems = 1000

        mojo_result = router.calculate_neuro_score(
            current_score=memory.neuro_score,
            access_count=memory.recall_count,
            total_memories=total_mems,
            days_since_access=memory.days_since_recall,
            importance=memory.importance,
        )

        if mojo_result and "score" in mojo_result:
            final_score = max(memory.min_score, min(1.0, mojo_result["score"]))
            if detailed:
                # Mock breakdown for Mojo result since it returns aggregate
                return ScoreBreakdown(
                    recency_component=0.0, # Aggregate in Mojo
                    frequency_component=0.0,
                    novelty_component=0.0,
                    emotional_component=0.0,
                    connection_component=0.0,
                    final_score=final_score,
                    is_protected=False,
                    is_mojo_optimized=True,
                )
            return final_score
    except Exception:
        pass

    # Legacy Python Fallback
    # Calculate each component
    recency = calculate_recency_factor(memory)
    frequency = calculate_frequency_factor(memory)
    novelty = calculate_novelty_factor(memory)
    emotional = calculate_emotional_factor(memory)
    connections = calculate_connection_factor(memory)

    # Weighted sum
    score = (
        recency * WEIGHTS["recency"] +
        frequency * WEIGHTS["frequency"] +
        novelty * WEIGHTS["novelty"] +
        emotional * WEIGHTS["emotional"] +
        connections * WEIGHTS["connections"]
    )

    # Clamp to valid range, respecting minimum
    final_score = max(memory.min_score, min(1.0, score))

    if detailed:
        return ScoreBreakdown(
            recency_component=recency,
            frequency_component=frequency,
            novelty_component=novelty,
            emotional_component=emotional,
            connection_component=connections,
            final_score=final_score,
            is_protected=False,
            is_mojo_optimized=False,
        )

    return final_score


class NeuroScoreEngine:
    """Engine for managing neuro_scores across memories.

    Handles:
    - Score calculation
    - Decay processing
    - Recall boosting
    - Archive threshold checking
    """

    def __init__(
        self,
        archive_threshold: float = 0.2,
        decay_interval_hours: float = 24.0,
        auto_protect: bool = True,
    ):
        self.archive_threshold = archive_threshold
        self.decay_interval_hours = decay_interval_hours
        self.auto_protect = auto_protect
        self._last_decay_run: datetime | None = None

    def calculate_score(self, memory: NeuralMemory, detailed: bool = False) -> float | ScoreBreakdown:
        """Calculate neuro_score for a memory."""
        return calculate_neuro_score(memory, detailed)

    def update_score(self, memory: NeuralMemory) -> NeuralMemory:
        """Update memory's neuro_score based on current state."""
        score = calculate_neuro_score(memory)
        if isinstance(score, float):
            memory.neuro_score = score
        else:
            memory.neuro_score = score.final_score
        return memory

    def on_recall(self, memory: NeuralMemory) -> NeuralMemory:
        """Called when memory is recalled/accessed.
        Boosts the memory's strength.
        """
        memory.recall()
        score = calculate_neuro_score(memory)
        if isinstance(score, float):
            memory.neuro_score = score
        else:
            memory.neuro_score = score.final_score
        return memory

    def on_create(self, memory: NeuralMemory) -> NeuralMemory:
        """Called when memory is created.
        Applies auto-protection and initial scoring.
        """
        if self.auto_protect:
            memory = auto_protect_memory(memory)

        score = calculate_neuro_score(memory)
        if isinstance(score, float):
            memory.neuro_score = score
        else:
            memory.neuro_score = score.final_score
        return memory

    def process_decay(self, memories: list[NeuralMemory]) -> list[NeuralMemory]:
        """Process decay for a batch of memories.

        Returns list of memories that should be archived.
        """
        to_archive = []

        for memory in memories:
            # Apply decay
            memory.decay()

            # Recalculate score
            score = calculate_neuro_score(memory)
            if isinstance(score, float):
                memory.neuro_score = score
            else:
                memory.neuro_score = score.final_score

            # Check archive threshold
            if memory.should_archive():
                to_archive.append(memory)

        self._last_decay_run = datetime.now()

        return to_archive

    def should_run_decay(self) -> bool:
        """Check if it's time to run decay processing."""
        if self._last_decay_run is None:
            return True

        hours_since = (datetime.now() - self._last_decay_run).total_seconds() / 3600
        return hours_since >= self.decay_interval_hours

    def get_memories_by_state(
        self,
        memories: list[NeuralMemory],
        state: MemoryState,
    ) -> list[NeuralMemory]:
        """Filter memories by state."""
        return [m for m in memories if m.state == state]

    def get_weak_memories(self, memories: list[NeuralMemory]) -> list[NeuralMemory]:
        """Get memories that are fading or weak (candidates for review)."""
        return [
            m for m in memories
            if m.state in (MemoryState.FADING, MemoryState.WEAK)
        ]

    def get_stats(self, memories: list[NeuralMemory]) -> dict[str, Any]:
        """Get statistics about memory health."""
        if not memories:
            return {
                "total": 0,
                "by_state": {},
                "average_score": 0,
                "protected_count": 0,
            }

        by_state = {}
        for state in MemoryState:
            count = len([m for m in memories if m.state == state])
            if count > 0:
                by_state[state.value] = count

        avg_score = sum(m.neuro_score for m in memories) / len(memories)
        protected = len([m for m in memories if m.is_protected])

        return {
            "total": len(memories),
            "by_state": by_state,
            "average_score": round(avg_score, 3),
            "protected_count": protected,
            "archive_candidates": len([m for m in memories if m.should_archive()]),
        }


# === SINGLETON ===
_engine: NeuroScoreEngine | None = None


def get_engine() -> NeuroScoreEngine:
    """Get the singleton NeuroScoreEngine instance."""
    global _engine
    if _engine is None:
        _engine = NeuroScoreEngine()
    return _engine
