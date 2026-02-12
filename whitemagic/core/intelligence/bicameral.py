"""Bicameral Reasoning - Dual-Hemisphere Processing with Cross-Critique.
====================================================================
Inspired by CyberBrains' bicameral mind architecture and Jaynes' theory:

  Left Hemisphere:  Sequential, symbolic, causal, low-temperature.
                    Aims for precision, logical consistency, fewer hallucinations.

  Right Hemisphere: Holistic, pattern-matching, stochastic, high-temperature.
                    Aims for creativity, novel connections, anomaly detection.

  Corpus Callosum:  High-bandwidth message bus connecting both hemispheres.
                    Left queries Right for intuition; Right queries Left for
                    logic checks. Neither hemisphere dominates — creativity
                    emerges from the *tension* between precision and freedom.

This module extends the existing ThoughtClone / MultiSpectral infrastructure
by adding a structured dual-mode reasoning pass with cross-validation.

Usage:
    from whitemagic.core.intelligence.bicameral import (
        get_bicameral_reasoner, BicameralReasoner
    )

    reasoner = get_bicameral_reasoner()
    result = await reasoner.reason("How should we restructure the memory tier?")
    print(result.synthesis)
    print(result.left_analysis)
    print(result.right_analysis)
    print(result.cross_critique)
"""

import asyncio
import logging
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

@dataclass
class HemisphereResult:
    """Output from a single hemisphere."""

    hemisphere: str          # "left" or "right"
    content: str
    confidence: float
    strategy: str
    reasoning_chain: list[str] = field(default_factory=list)
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class CrossCritique:
    """Result of one hemisphere critiquing the other."""

    critic: str              # Which hemisphere is critiquing
    target: str              # Which hemisphere is being critiqued
    agreements: list[str]    # Points of agreement
    challenges: list[str]    # Points of disagreement / concern
    suggestions: list[str]   # Constructive modifications
    confidence_adjustment: float  # How much the critique adjusts confidence (-1 to +1)


@dataclass
class BicameralResult:
    """Full bicameral reasoning output."""

    query: str
    left_analysis: HemisphereResult
    right_analysis: HemisphereResult
    cross_critique: list[CrossCritique]
    synthesis: str
    final_confidence: float
    dominant_hemisphere: str   # Which hemisphere "won" this round
    tension_score: float       # 0=agreement, 1=maximum disagreement
    duration_ms: float = 0.0
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        return {
            "query": self.query,
            "left": {
                "content": self.left_analysis.content,
                "confidence": self.left_analysis.confidence,
                "strategy": self.left_analysis.strategy,
            },
            "right": {
                "content": self.right_analysis.content,
                "confidence": self.right_analysis.confidence,
                "strategy": self.right_analysis.strategy,
            },
            "synthesis": self.synthesis,
            "final_confidence": round(self.final_confidence, 4),
            "dominant_hemisphere": self.dominant_hemisphere,
            "tension_score": round(self.tension_score, 4),
            "cross_critique_count": len(self.cross_critique),
            "duration_ms": round(self.duration_ms, 2),
            "timestamp": self.timestamp.isoformat(),
        }


# ---------------------------------------------------------------------------
# Hemisphere Strategies
# ---------------------------------------------------------------------------

# Left hemisphere: precise, analytical strategies
LEFT_STRATEGIES = [
    "analytical", "systematic", "factual", "cautious",
    "logical", "sequential", "deductive",
]

# Right hemisphere: creative, holistic strategies
RIGHT_STRATEGIES = [
    "creative", "intuitive", "optimistic", "theoretical",
    "holistic", "associative", "divergent",
]


# ---------------------------------------------------------------------------
# Bicameral Reasoner
# ---------------------------------------------------------------------------

class BicameralReasoner:
    """Dual-hemisphere reasoning engine.

    Runs two independent thought-clone armies (left=precise, right=creative),
    then performs cross-critique through the "corpus callosum" before
    synthesizing a final answer.
    """

    def __init__(
        self,
        left_clones: int = 50,
        right_clones: int = 50,
        cross_critique_enabled: bool = True,
    ):
        self._left_clones = left_clones
        self._right_clones = right_clones
        self._cross_critique_enabled = cross_critique_enabled

        # Stats
        self._total_reasonings: int = 0
        self._left_wins: int = 0
        self._right_wins: int = 0
        self._ties: int = 0
        self._avg_tension: float = 0.0

    # ------------------------------------------------------------------
    # Core reasoning
    # ------------------------------------------------------------------

    async def reason(self, query: str, context: dict[str, Any] | None = None) -> BicameralResult:
        """Run bicameral reasoning on a query.

        1. Left hemisphere explores with precision strategies
        2. Right hemisphere explores with creative strategies
        3. Corpus callosum performs cross-critique
        4. Synthesis integrates both perspectives
        """
        start = time.perf_counter()
        context = context or {}

        # Phase 1: Parallel hemisphere exploration
        left_result, right_result = await asyncio.gather(
            self._explore_hemisphere("left", query, LEFT_STRATEGIES, self._left_clones),
            self._explore_hemisphere("right", query, RIGHT_STRATEGIES, self._right_clones),
        )

        # Phase 2: Cross-critique (corpus callosum)
        critiques: list[CrossCritique] = []
        if self._cross_critique_enabled:
            critiques = self._cross_critique(left_result, right_result)

        # Phase 3: Synthesis
        synthesis, final_conf, dominant, tension = self._synthesize(
            left_result, right_result, critiques,
        )

        # Stats
        self._total_reasonings += 1
        if dominant == "left":
            self._left_wins += 1
        elif dominant == "right":
            self._right_wins += 1
        else:
            self._ties += 1
        # Running average tension
        self._avg_tension = (
            (self._avg_tension * (self._total_reasonings - 1) + tension)
            / self._total_reasonings
        )

        elapsed_ms = (time.perf_counter() - start) * 1000

        result = BicameralResult(
            query=query,
            left_analysis=left_result,
            right_analysis=right_result,
            cross_critique=critiques,
            synthesis=synthesis,
            final_confidence=final_conf,
            dominant_hemisphere=dominant,
            tension_score=tension,
            duration_ms=elapsed_ms,
        )

        # Emit event
        self._emit_event(result)

        return result

    # ------------------------------------------------------------------
    # Hemisphere exploration
    # ------------------------------------------------------------------

    async def _explore_hemisphere(
        self,
        hemisphere: str,
        query: str,
        strategies: list[str],
        num_clones: int,
    ) -> HemisphereResult:
        """Run a thought clone army biased toward a hemisphere's strategies."""
        from whitemagic.edge.thought_clones_async import (
            AsyncThoughtCloneArmy,
            CloneConfig,
        )

        config = CloneConfig(max_clones=num_clones, max_concurrent_api_calls=50)
        army = AsyncThoughtCloneArmy(config=config)

        # Override strategy generation to bias toward this hemisphere

        def biased_strategies(count: int) -> list[str]:
            result = []
            for i in range(count):
                result.append(strategies[i % len(strategies)])
            return result

        army._generate_strategies = biased_strategies  # type: ignore[method-assign]

        start = time.perf_counter()
        best_path = await army.parallel_explore(query, num_clones)
        elapsed = (time.perf_counter() - start) * 1000

        return HemisphereResult(
            hemisphere=hemisphere,
            content=best_path.content,
            confidence=best_path.confidence,
            strategy=best_path.strategy,
            reasoning_chain=[best_path.content],
            duration_ms=elapsed,
            metadata={
                "clone_id": best_path.clone_id,
                "tokens": best_path.tokens,
            },
        )

    # ------------------------------------------------------------------
    # Corpus Callosum (cross-critique)
    # ------------------------------------------------------------------

    def _cross_critique(
        self,
        left: HemisphereResult,
        right: HemisphereResult,
    ) -> list[CrossCritique]:
        """Each hemisphere critiques the other.
        Uses heuristic analysis of content characteristics.
        """
        critiques = []

        # Left critiques Right (checks for logical consistency)
        left_on_right = self._left_critiques_right(left, right)
        critiques.append(left_on_right)

        # Right critiques Left (checks for creative breadth)
        right_on_left = self._right_critiques_left(left, right)
        critiques.append(right_on_left)

        return critiques

    def _left_critiques_right(self, left: HemisphereResult, right: HemisphereResult) -> CrossCritique:
        """Left hemisphere checks Right for logical consistency."""
        agreements = []
        challenges = []
        suggestions = []
        conf_adj = 0.0

        # Agreement: if both mention similar themes
        left_words = set(left.content.lower().split())
        right_words = set(right.content.lower().split())
        overlap = left_words & right_words
        stopwords = {"the", "a", "an", "is", "are", "was", "were", "to", "of", "and", "in", "for", "on", "with", "that", "this", "it"}
        meaningful_overlap = overlap - stopwords

        if len(meaningful_overlap) > 3:
            agreements.append(f"Shared focus on: {', '.join(list(meaningful_overlap)[:5])}")
            conf_adj += 0.05

        # Challenge: right hemisphere may be too speculative
        speculative_markers = {"maybe", "perhaps", "possibly", "could", "might", "imagine", "creative", "unconventional", "novel"}
        right_speculation = right_words & speculative_markers
        if len(right_speculation) > 1:
            challenges.append(f"High speculation detected: {', '.join(right_speculation)}")
            conf_adj -= 0.05

        # Suggestion: ground creative ideas in specifics
        if right.confidence < left.confidence:
            suggestions.append("Consider grounding creative insights with concrete evidence or examples")

        return CrossCritique(
            critic="left",
            target="right",
            agreements=agreements,
            challenges=challenges,
            suggestions=suggestions,
            confidence_adjustment=conf_adj,
        )

    def _right_critiques_left(self, left: HemisphereResult, right: HemisphereResult) -> CrossCritique:
        """Right hemisphere checks Left for creative breadth."""
        agreements = []
        challenges = []
        suggestions = []
        conf_adj = 0.0

        # Agreement on shared themes
        left_words = set(left.content.lower().split())
        right_words = set(right.content.lower().split())
        overlap = left_words & right_words
        stopwords = {"the", "a", "an", "is", "are", "was", "were", "to", "of", "and", "in", "for", "on", "with", "that", "this", "it"}
        meaningful_overlap = overlap - stopwords

        if len(meaningful_overlap) > 3:
            agreements.append(f"Common ground on: {', '.join(list(meaningful_overlap)[:5])}")
            conf_adj += 0.05

        # Challenge: left hemisphere may be too narrow
        narrow_markers = {"only", "must", "always", "never", "precisely", "exactly", "systematic"}
        left_narrowness = left_words & narrow_markers
        if len(left_narrowness) > 1:
            challenges.append(f"Potentially over-constrained thinking: {', '.join(left_narrowness)}")
            conf_adj -= 0.03

        # Suggestion: explore adjacent possibilities
        if left.confidence > right.confidence + 0.2:
            suggestions.append("High precision may miss novel approaches — consider exploring adjacent solution spaces")

        return CrossCritique(
            critic="right",
            target="left",
            agreements=agreements,
            challenges=challenges,
            suggestions=suggestions,
            confidence_adjustment=conf_adj,
        )

    # ------------------------------------------------------------------
    # Synthesis
    # ------------------------------------------------------------------

    def _synthesize(
        self,
        left: HemisphereResult,
        right: HemisphereResult,
        critiques: list[CrossCritique],
    ) -> tuple:
        """Synthesize both hemispheres + cross-critiques into a final answer.

        Returns: (synthesis_text, final_confidence, dominant_hemisphere, tension_score)
        """
        # Calculate tension (disagreement between hemispheres)
        conf_diff = abs(left.confidence - right.confidence)
        content_overlap = self._content_similarity(left.content, right.content)
        tension = (conf_diff + (1.0 - content_overlap)) / 2.0
        tension = max(0.0, min(1.0, tension))

        # Apply critique adjustments
        left_adj = 0.0
        right_adj = 0.0
        for c in critiques:
            if c.target == "left":
                left_adj += c.confidence_adjustment
            elif c.target == "right":
                right_adj += c.confidence_adjustment

        adj_left_conf = max(0.0, min(1.0, left.confidence + left_adj))
        adj_right_conf = max(0.0, min(1.0, right.confidence + right_adj))

        # Determine dominance
        if abs(adj_left_conf - adj_right_conf) < 0.05:
            dominant = "balanced"
        elif adj_left_conf > adj_right_conf:
            dominant = "left"
        else:
            dominant = "right"

        # Weighted synthesis
        total_conf = adj_left_conf + adj_right_conf
        if total_conf == 0:
            left_weight = 0.5
            right_weight = 0.5
        else:
            left_weight = adj_left_conf / total_conf
            right_weight = adj_right_conf / total_conf

        final_confidence = left_weight * adj_left_conf + right_weight * adj_right_conf

        # Build synthesis text
        parts = []
        parts.append(f"[LEFT ({left.strategy}, conf={adj_left_conf:.2f})] {left.content}")
        parts.append(f"[RIGHT ({right.strategy}, conf={adj_right_conf:.2f})] {right.content}")

        if critiques:
            critique_notes = []
            for c in critiques:
                if c.challenges:
                    critique_notes.append(f"{c.critic}→{c.target}: {'; '.join(c.challenges)}")
                if c.suggestions:
                    critique_notes.append(f"{c.critic} suggests: {'; '.join(c.suggestions)}")
            if critique_notes:
                parts.append(f"[CORPUS CALLOSUM] {' | '.join(critique_notes)}")

        parts.append(
            f"[SYNTHESIS] Dominant={dominant}, tension={tension:.2f}, "
            f"confidence={final_confidence:.2f}",
        )

        synthesis = "\n".join(parts)
        return synthesis, final_confidence, dominant, tension

    def _content_similarity(self, a: str, b: str) -> float:
        """Simple word-overlap similarity."""
        wa = set(a.lower().split())
        wb = set(b.lower().split())
        if not wa or not wb:
            return 0.0
        return len(wa & wb) / len(wa | wb)

    # ------------------------------------------------------------------
    # Event emission
    # ------------------------------------------------------------------

    def _emit_event(self, result: BicameralResult) -> None:
        """Emit reasoning result to the Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying import emit_event
            from whitemagic.core.resonance.gan_ying_enhanced import EventType

            emit_event(
                "bicameral_reasoner",
                EventType.REASONING_COMPLETE,
                {
                    "query": result.query[:100],
                    "dominant": result.dominant_hemisphere,
                    "tension": result.tension_score,
                    "confidence": result.final_confidence,
                    "duration_ms": result.duration_ms,
                },
            )
        except Exception:
            pass  # Non-critical

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {
            "total_reasonings": self._total_reasonings,
            "left_wins": self._left_wins,
            "right_wins": self._right_wins,
            "ties": self._ties,
            "avg_tension": round(self._avg_tension, 4),
            "left_clones": self._left_clones,
            "right_clones": self._right_clones,
            "cross_critique_enabled": self._cross_critique_enabled,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_reasoner_instance: BicameralReasoner | None = None
_reasoner_lock = threading.Lock()


def get_bicameral_reasoner(
    left_clones: int = 50,
    right_clones: int = 50,
) -> BicameralReasoner:
    """Get or create the global BicameralReasoner singleton."""
    global _reasoner_instance
    with _reasoner_lock:
        if _reasoner_instance is None:
            _reasoner_instance = BicameralReasoner(
                left_clones=left_clones,
                right_clones=right_clones,
            )
        return _reasoner_instance
