"""Multi-Spectral Reasoning Engine - Unified Wisdom System.

Philosophy:
-----------
Like biological organisms, intelligent reasoning requires multiple perspectives:
- I Ching: What is CHANGING? (64 archetypal transformations)
- Wu Xing: What needs BALANCE? (5 elemental cycles)
- Art of War: What is the STRATEGY? (timing, positioning, resources)
- Zodiac: What is the PHASE? (12 specialized modes of consciousness)
- Sequential-Thinking: How do I REASON through this? (structured multi-step)

Together, these form a "Multi-Spectral Scientific Method":
1. Observe from multiple lenses (multi-spectral)
2. Detect patterns from memory (biological learning)
3. Reason step-by-step (scientific method)
4. Synthesize into coherent decision (integration)
"""

from __future__ import annotations

import importlib
import logging
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

class ReasoningLens(Enum):
    """Available reasoning lenses."""

    I_CHING = "i_ching"
    WU_XING = "wu_xing"
    ART_OF_WAR = "art_of_war"
    ZODIAC = "zodiac"
    SEQUENTIAL = "sequential"
    ALL = "all"

@dataclass
class ReasoningContext:
    """Context for reasoning operation."""

    question: str
    task_type: str = "analysis"
    urgency: str = "normal"
    complexity: str = "medium"
    stakes: str = "medium"
    available_resources: dict[str, bool] = field(default_factory=dict)
    constraints: list[str] = field(default_factory=list)
    past_patterns: list[dict[str, Any]] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

@dataclass
class LensPerspective:
    """Perspective from a single reasoning lens."""

    lens: ReasoningLens
    analysis: str
    confidence: float
    guidance: str
    details: dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)

@dataclass
class ReasoningThought:
    """Single thought in sequential reasoning chain."""

    number: int
    content: str
    lens_used: ReasoningLens | None = None
    revises: int | None = None
    branch_id: str | None = None
    timestamp: datetime = field(default_factory=datetime.now)

@dataclass
class ReasoningResult:
    """Final result of multi-spectral reasoning."""

    question: str
    perspectives: list[LensPerspective]
    thoughts: list[ReasoningThought]
    synthesis: str
    recommendation: str
    confidence: float
    reasoning_chain: list[str]
    patterns_matched: list[dict[str, Any]]
    timestamp: datetime = field(default_factory=datetime.now)

class MultiSpectralReasoner:
    """Unified reasoning engine combining all wisdom systems."""

    def __init__(self, base_dir: Path = Path(".")) -> None:
        self.base_dir = base_dir
        self.memory_dir = self.base_dir / "memory" / "reasoning"
        self.memory_dir.mkdir(parents=True, exist_ok=True)

        self.i_ching: Any | None = None
        self.wu_xing: Any | None = None
        self.zodiac_cores: Any | None = None
        self.art_of_war: Callable[[str], Any] | None = None
        self.holographic: Any | None = None
        self.unified_memory: Any | None = None
        self.reasoning_history: list[ReasoningResult] = []
        self.bus: Any | None = None

        self._init_systems()
        # Only print if not silent to avoid CLI spam
        import os
        if not os.getenv("WM_SILENT_INIT"):
            logger.info("🌈 Multi-Spectral Reasoning Engine initialized (Materialized Core)")

    def _init_systems(self) -> None:
        """Initialize all wisdom systems."""
        try:
            from whitemagic.gardens.wisdom.i_ching import get_i_ching
            self.i_ching = get_i_ching()
        except Exception as e:
            logger.debug(f"I Ching system not available: {e}")

        try:
            # Use new path for Wu Xing
            from whitemagic.core.intelligence.wisdom.wu_xing import get_wu_xing
            self.wu_xing = get_wu_xing()
        except Exception as e:
            logger.debug(f"Wu Xing system not available: {e}")

        try:
            from whitemagic.zodiac.zodiac_cores import get_zodiac_cores
            self.zodiac_cores = get_zodiac_cores()
        except Exception as e:
            logger.debug(f"Zodiac cores matching not available: {e}")

        try:
            art_mod = importlib.import_module("whitemagic.gardens.wisdom.art_of_war")
            get_war_wisdom = getattr(art_mod, "get_war_wisdom", None)
            if callable(get_war_wisdom):
                self.art_of_war = get_war_wisdom
        except Exception as e:
            logger.debug(f"Art of War wisdom not available: {e}")

        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            self.bus = get_bus()
        except Exception as e:
            logger.debug(f"Gan Ying resonance bus not available: {e}")

        try:
            from whitemagic.core.memory.holographic import get_holographic_memory
            from whitemagic.core.memory.unified import get_unified_memory
            self.holographic = get_holographic_memory()
            self.unified_memory = get_unified_memory()
        except Exception as e:
            logger.debug(f"Memory systems not available: {e}")

    def reason(self, question: str, lenses: Sequence[ReasoningLens] | None = None,
               context: ReasoningContext | None = None,
               use_sequential_thinking: bool = True) -> ReasoningResult:
        """Main reasoning entry point."""
        if context is None:
            context = ReasoningContext(question=question)

        lenses_list = [ReasoningLens.I_CHING, ReasoningLens.WU_XING,
                       ReasoningLens.ART_OF_WAR, ReasoningLens.ZODIAC] if not lenses or ReasoningLens.ALL in lenses else list(lenses)

        perspectives: list[LensPerspective] = []
        thoughts: list[ReasoningThought] = []

        # 1. I Ching Lens
        if ReasoningLens.I_CHING in lenses_list and self.i_ching:
            try:
                hexagram = self.i_ching.cast_hexagram(question)
                perspectives.append(LensPerspective(
                    lens=ReasoningLens.I_CHING,
                    analysis=hexagram.judgment,
                    confidence=0.9,
                    guidance=hexagram.guidance,
                    details={"hexagram": hexagram.number, "name": hexagram.name, "image": hexagram.image},
                ))
            except Exception as e:
                logger.info(f"⚠️ I Ching reasoning failed: {e}")

        # 2. Wu Xing Lens
        if ReasoningLens.WU_XING in lenses_list and self.wu_xing:
            try:
                element = self.wu_xing.identify_element(context.task_type)
                suggestion = self.wu_xing.suggest_optimization(element)
                perspectives.append(LensPerspective(
                    lens=ReasoningLens.WU_XING,
                    analysis=f"Current phase is {element.value}",
                    confidence=0.85,
                    guidance=suggestion,
                    details={"element": element.value},
                ))
            except Exception as e:
                logger.info(f"⚠️ Wu Xing reasoning failed: {e}")

        # 3. Art of War Lens
        if ReasoningLens.ART_OF_WAR in lenses_list and self.art_of_war:
            try:
                # self.art_of_war is the function get_war_wisdom directly
                wisdom = self.art_of_war(context.task_type)
                perspectives.append(LensPerspective(
                    lens=ReasoningLens.ART_OF_WAR,
                    analysis=wisdom.principle,
                    confidence=0.8,
                    guidance=wisdom.application,
                    details={"chapter": wisdom.chapter},
                ))
            except Exception as e:
                logger.info(f"⚠️ Art of War reasoning failed: {e}")

        # 4. Pattern Matching (Memory Lens)
        patterns_matched = self._match_patterns(context, perspectives)

        # 5. Synthesis
        synthesis = self._synthesize_perspectives(perspectives, patterns_matched)
        recommendation = self._generate_recommendation(context, perspectives, synthesis, patterns_matched)
        confidence = self._calculate_confidence(perspectives, patterns_matched)

        result = ReasoningResult(
            question=question,
            perspectives=perspectives,
            thoughts=thoughts,
            synthesis=synthesis,
            recommendation=recommendation,
            confidence=confidence,
            reasoning_chain=[t.content for t in thoughts],
            patterns_matched=patterns_matched,
        )

        self.reasoning_history.append(result)

        # Emit reasoning result to bus
        if self.bus:
            try:
                from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent
                self.bus.emit(ResonanceEvent(
                    source="multi_spectral_reasoner",
                    event_type=EventType.WISDOM_INTEGRATED,
                    data={
                        "question": question,
                        "synthesis": synthesis,
                        "recommendation": recommendation,
                        "lens_count": len(perspectives),
                    },
                    timestamp=datetime.now(),
                    confidence=confidence,
                ))
            except Exception as e:
                logger.warning(f"Failed to emit reasoning event: {e}")

        return result

    def _calculate_similarity(self, q1: str, q2: str) -> float:
        """Calculate semantic similarity using Rust acceleration (Materialized)."""
        try:
            from whitemagic.core.memory.neural.rust_bridge import fast_similarity
            return float(fast_similarity(q1, q2))
        except (ImportError, Exception):
            # Fallback to keyword overlap
            words1, words2 = set(q1.lower().split()), set(q2.lower().split())
            if not words1 or not words2:
                return 0.0
            overlap = len(words1 & words2)
            total = len(words1 | words2)
            return overlap / total if total > 0 else 0.0

    def _match_patterns(self, context: ReasoningContext, perspectives: list[LensPerspective]) -> list[dict[str, Any]]:
        patterns: list[dict[str, Any]] = []
        for past in self.reasoning_history[-50:]:  # Check last 50
            similarity = self._calculate_similarity(context.question, past.question)
            if similarity > 0.6:
                patterns.append({
                    "past_question": past.question,
                    "similarity": similarity,
                    "lessons": past.synthesis[:100],
                    "outcome": past.recommendation,
                })
        return sorted(patterns, key=lambda x: float(x["similarity"]), reverse=True)[:3]

    def _synthesize_perspectives(self, perspectives: list[LensPerspective], patterns: list[dict[str, Any]]) -> str:
        if not perspectives:
            return "No wisdom perspectives available. Relying on objective analysis."

        synthesis_parts = []

        # Check for consensus or conflict
        [p.guidance for p in perspectives]

        synthesis_parts.append(f"Considered {len(perspectives)} perspectives.")

        for p in perspectives:
            synthesis_parts.append(f"{p.lens.value.replace('_', ' ').title()}: {p.guidance}")

        if patterns:
            synthesis_parts.append(f"Found {len(patterns)} similar past situations.")

        return "\\n".join(synthesis_parts)

    def _generate_recommendation(self, context: ReasoningContext, perspectives: list[LensPerspective],
                                 synthesis: str, patterns: list[dict[str, Any]]) -> str:
        # Simple heuristic synthesis for now
        # In v8.0 this will use LLM synthesis
        if not perspectives:
            return "Proceed with standard operating procedures."

        # Prioritize Art of War for high stakes
        if context.stakes == "high":
            for p in perspectives:
                if p.lens == ReasoningLens.ART_OF_WAR:
                    return f"Strategic Priority: {p.guidance}"

        # Prioritize Wu Xing for complex tasks
        if context.complexity == "high":
            for p in perspectives:
                if p.lens == ReasoningLens.WU_XING:
                    return f"Workflow Priority: {p.guidance}"

        # Default to first available
        return perspectives[0].guidance

    def _calculate_confidence(self, perspectives: list[LensPerspective], patterns: list[dict[str, Any]]) -> float:
        if not perspectives:
            return 0.5
        base_conf = sum(p.confidence for p in perspectives) / len(perspectives)

        # Boost confidence if we have matching patterns
        if patterns:
            base_conf += 0.1

        return min(1.0, base_conf)

_reasoner: MultiSpectralReasoner | None = None


def get_reasoner() -> MultiSpectralReasoner:
    global _reasoner
    if _reasoner is None:
        _reasoner = MultiSpectralReasoner()
    return _reasoner
