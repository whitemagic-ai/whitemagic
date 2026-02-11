# mypy: disable-error-code=no-untyped-def
"""ResonanceEngine — Gana #14 Abundance (豐).

Purpose: Amplify emergent patterns through sympathetic resonance.
Garden: gratitude

When patterns repeat across clusters, this engine detects the convergence
and amplifies the signal. It's the "audience applause" that reinforces good ideas.
"""

import logging
import math
from collections import Counter
from dataclasses import dataclass, field
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType, emit_event

logger = logging.getLogger(__name__)


@dataclass
class ResonancePattern:
    """A detected pattern with resonance score."""

    pattern_id: str
    content: str
    frequency: int
    sources: list[str]
    resonance_score: float = 0.0
    amplification_factor: float = 1.0


@dataclass
class ResonanceEngine:
    """Amplify emergent patterns through sympathetic resonance.

    When the same pattern appears across multiple sources (clusters, memories,
    strategies), it gains "resonance" — a multiplied importance signal.

    Inspired by 感應 (Gan Ying): when one thing vibrates, similar things resonate.
    """

    # Pattern frequency threshold to trigger resonance
    min_frequency: int = 3

    # Maximum amplification factor
    max_amplification: float = 5.0

    # Decay factor for older patterns
    temporal_decay: float = 0.95

    # Active resonance patterns
    patterns: dict[str, ResonancePattern] = field(default_factory=dict)

    # Historical resonance events
    history: list[dict[str, Any]] = field(default_factory=list)

    def __post_init__(self):
        emit_event(
            "resonance_engine",
            EventType.SYSTEM_STARTED,
            {"component": "ResonanceEngine", "garden": "gratitude"},
        )
        logger.info("ResonanceEngine initialized (Garden: gratitude)")

    def detect_resonance(
        self,
        patterns: list[tuple[str, str, str]],  # (pattern_id, content, source)
    ) -> list[ResonancePattern]:
        """Detect resonance across a set of patterns.

        Args:
            patterns: List of (pattern_id, content, source) tuples

        Returns:
            List of patterns that exhibit resonance (freq >= min_frequency)

        """
        # Count frequencies
        content_counts: Counter = Counter()
        content_sources: dict[str, list[str]] = {}
        content_ids: dict[str, str] = {}

        for pid, content, source in patterns:
            # Normalize content for matching
            normalized = content.lower().strip()[:200]
            content_counts[normalized] += 1

            if normalized not in content_sources:
                content_sources[normalized] = []
                content_ids[normalized] = pid
            content_sources[normalized].append(source)

        # Find resonant patterns
        resonant = []
        for content, freq in content_counts.items():
            if freq >= self.min_frequency:
                # Calculate resonance score (log scale to prevent runaway)
                resonance_score = math.log(freq + 1) / math.log(self.min_frequency + 1)

                pattern = ResonancePattern(
                    pattern_id=content_ids[content],
                    content=content,
                    frequency=freq,
                    sources=content_sources[content],
                    resonance_score=resonance_score,
                    amplification_factor=min(resonance_score * 2, self.max_amplification),
                )

                # Store and emit
                self.patterns[pattern.pattern_id] = pattern
                emit_event(
                    "resonance_engine",
                    EventType.RESONANCE_AMPLIFIED,
                    {
                        "pattern_id": pattern.pattern_id,
                        "frequency": freq,
                        "resonance_score": resonance_score,
                        "sources": pattern.sources[:5],  # Limit for event payload
                    },
                )

                resonant.append(pattern)

        logger.info(f"Detected {len(resonant)} resonant patterns from {len(patterns)} inputs")
        return resonant

    def amplify_signal(
        self,
        signal: dict[str, float],
        pattern_id: str | None = None,
    ) -> dict[str, float]:
        """Amplify a signal based on matching resonance patterns.

        Args:
            signal: Dict of signal_name -> value
            pattern_id: Optional specific pattern to match

        Returns:
            Amplified signal dict

        """
        if pattern_id and pattern_id in self.patterns:
            factor = self.patterns[pattern_id].amplification_factor
        else:
            # Use average amplification across all patterns
            if self.patterns:
                factor = sum(p.amplification_factor for p in self.patterns.values()) / len(self.patterns)
            else:
                factor = 1.0

        return {k: v * factor for k, v in signal.items()}

    def emit_gratitude(self, pattern: ResonancePattern) -> None:
        """Emit a gratitude event for a resonant pattern.

        This is the "audience applause" — acknowledging and celebrating
        patterns that resonate strongly.
        """
        emit_event(
            "resonance_engine",
            EventType.GRATITUDE_FELT,
            {
                "pattern_id": pattern.pattern_id,
                "content_preview": pattern.content[:100],
                "resonance_score": pattern.resonance_score,
                "sources_count": len(pattern.sources),
            },
        )

        self.history.append({
            "event": "gratitude",
            "pattern_id": pattern.pattern_id,
            "resonance_score": pattern.resonance_score,
        })

    def decay_patterns(self) -> int:
        """Apply temporal decay to all patterns.

        Returns:
            Number of patterns removed due to decay

        """
        removed = 0
        for pid in list(self.patterns.keys()):
            pattern = self.patterns[pid]
            pattern.resonance_score *= self.temporal_decay
            pattern.amplification_factor *= self.temporal_decay

            # Remove if below threshold
            if pattern.resonance_score < 0.1:
                del self.patterns[pid]
                removed += 1

        return removed

    def get_top_resonant(self, n: int = 10) -> list[ResonancePattern]:
        """Get top N resonant patterns by score."""
        return sorted(
            self.patterns.values(),
            key=lambda p: p.resonance_score,
            reverse=True,
        )[:n]


# === Convenience Functions ===

def get_resonance_engine() -> ResonanceEngine:
    """Get a ResonanceEngine instance."""
    return ResonanceEngine()


def detect_and_amplify(
    patterns: list[tuple[str, str, str]],
    signal: dict[str, float],
) -> tuple[list[ResonancePattern], dict[str, float]]:
    """Detect resonance and amplify signal in one call.

    Returns:
        (resonant_patterns, amplified_signal)

    """
    engine = get_resonance_engine()
    resonant = engine.detect_resonance(patterns)

    # Amplify using strongest pattern
    if resonant:
        strongest = max(resonant, key=lambda p: p.resonance_score)
        amplified = engine.amplify_signal(signal, strongest.pattern_id)
        engine.emit_gratitude(strongest)
    else:
        amplified = signal

    return resonant, amplified
