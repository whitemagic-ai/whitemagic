"""Entropy & Abstraction Scorer (v14.1).
======================================
Computes two information-theoretic signals for each memory:

  1. **Entropy score** (0.0–1.0): How information-dense is the content?
     High = rich, diverse vocabulary. Low = repetitive, sparse.
     Uses normalized Shannon entropy over word-level token distribution.

  2. **Abstraction level** (0.0–1.0): Concrete facts vs abstract concepts.
     High = principles, patterns, strategies, generalizations.
     Low = specific names, dates, numbers, file paths, URLs.

These signals feed into:
  - Retention scoring (mindful forgetting evaluator)
  - Hybrid recall ranking (boost high-entropy results)
  - Lifecycle sweep reporting
  - Memory quality analytics

Usage:
    from whitemagic.core.memory.entropy_scorer import get_entropy_scorer
    scorer = get_entropy_scorer()

    # Score a single text
    result = scorer.score("WhiteMagic uses 28 Ganas...")
    # → EntropyResult(entropy=0.87, abstraction=0.62, ...)

    # Batch score from DB
    report = scorer.sweep(persist=True)
"""

from __future__ import annotations

import logging
import math
import re
import threading
import time
from collections import Counter
from dataclasses import dataclass
from typing import Any, Callable

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Abstraction markers
# ---------------------------------------------------------------------------

# Concrete indicators (lower abstraction)
_CONCRETE_PATTERNS: list[re.Pattern[str]] = [
    re.compile(r"\b\d{4}[-/]\d{2}[-/]\d{2}\b"),        # dates
    re.compile(r"\b\d+\.\d+\.\d+\b"),                    # versions
    re.compile(r"\b(?:https?://|/[\w/]+\.[\w]+)\b"),      # URLs/paths
    re.compile(r"\b[A-Z][a-z]+(?:[A-Z][a-z]+)+\b"),      # CamelCase identifiers
    re.compile(r"\b\w+\.(py|rs|zig|go|ts|js|hs|ex)\b"),  # file extensions
    re.compile(r"\b0x[0-9a-fA-F]+\b"),                    # hex literals
    re.compile(r"\b\d{3,}\b"),                            # large numbers
]

# Abstract indicators (higher abstraction)
_ABSTRACT_WORDS: frozenset[str] = frozenset({
    "concept", "principle", "pattern", "strategy", "architecture",
    "approach", "philosophy", "framework", "methodology", "paradigm",
    "abstract", "general", "universal", "fundamental", "theoretical",
    "system", "design", "model", "structure", "hierarchy",
    "relationship", "connection", "association", "integration", "synthesis",
    "governance", "policy", "rule", "constraint", "boundary",
    "insight", "understanding", "knowledge", "wisdom", "learning",
    "process", "workflow", "pipeline", "lifecycle", "protocol",
    "optimization", "efficiency", "performance", "scalability", "resilience",
    "should", "could", "would", "might", "always", "never", "typically",
    "enables", "facilitates", "provides", "supports", "ensures",
})


# ---------------------------------------------------------------------------
# Result types
# ---------------------------------------------------------------------------

@dataclass
class EntropyResult:
    """Scoring result for a single memory."""
    entropy: float = 0.5          # 0.0–1.0: information density
    abstraction: float = 0.5      # 0.0–1.0: concrete ↔ abstract
    vocab_richness: float = 0.5   # type-token ratio
    word_count: int = 0
    unique_words: int = 0
    concrete_markers: int = 0
    abstract_markers: int = 0

    @property
    def composite(self) -> float:
        """Combined quality signal: weighted blend of entropy + abstraction."""
        return round(0.6 * self.entropy + 0.4 * self.abstraction, 4)

    def to_dict(self) -> dict[str, Any]:
        return {
            "entropy": round(self.entropy, 4),
            "abstraction": round(self.abstraction, 4),
            "composite": self.composite,
            "vocab_richness": round(self.vocab_richness, 4),
            "word_count": self.word_count,
            "unique_words": self.unique_words,
            "concrete_markers": self.concrete_markers,
            "abstract_markers": self.abstract_markers,
        }


@dataclass
class EntropySweepReport:
    """Results from a batch entropy scoring sweep."""
    total_scored: int = 0
    avg_entropy: float = 0.0
    avg_abstraction: float = 0.0
    avg_composite: float = 0.0
    high_entropy_count: int = 0    # > 0.7
    low_entropy_count: int = 0     # < 0.3
    duration_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "total_scored": self.total_scored,
            "avg_entropy": round(self.avg_entropy, 4),
            "avg_abstraction": round(self.avg_abstraction, 4),
            "avg_composite": round(self.avg_composite, 4),
            "high_entropy_count": self.high_entropy_count,
            "low_entropy_count": self.low_entropy_count,
            "duration_ms": round(self.duration_ms, 1),
        }


# ---------------------------------------------------------------------------
# Core scorer
# ---------------------------------------------------------------------------

class EntropyScorer:
    """Computes entropy and abstraction scores for memory content."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._total_scored: int = 0

    # ------------------------------------------------------------------
    # Single-text scoring
    # ------------------------------------------------------------------

    def score(self, text: str) -> EntropyResult:
        """Score a single text for entropy and abstraction."""
        if not text or not text.strip():
            return EntropyResult()

        text_lower = text.lower()
        words = re.findall(r"\b[a-z']+\b", text_lower)

        if len(words) < 3:
            return EntropyResult(word_count=len(words))

        word_count = len(words)
        unique_words = len(set(words))

        # --- Entropy (normalized Shannon entropy over word frequencies) ---
        freq = Counter(words)
        total = sum(freq.values())
        if total <= 1:
            entropy = 0.0
        else:
            raw_entropy = -sum(
                (c / total) * math.log2(c / total)
                for c in freq.values() if c > 0
            )
            max_entropy = math.log2(min(total, unique_words)) if unique_words > 1 else 1.0
            entropy = raw_entropy / max_entropy if max_entropy > 0 else 0.0

        # Clamp and apply length normalization (very short texts get penalized)
        length_factor = min(1.0, word_count / 20.0)
        entropy = min(1.0, entropy * length_factor)

        # --- Vocabulary richness (type-token ratio, adjusted for length) ---
        # Yule's K approximation: less sensitive to text length than raw TTR
        if word_count > 0:
            vocab_richness = unique_words / word_count
            # Adjust: longer texts naturally have lower TTR
            if word_count > 50:
                vocab_richness = min(1.0, vocab_richness * 1.3)
        else:
            vocab_richness = 0.0

        # --- Abstraction level ---
        concrete_count = sum(
            len(pat.findall(text)) for pat in _CONCRETE_PATTERNS
        )
        abstract_count = sum(
            1 for w in words if w in _ABSTRACT_WORDS
        )

        total_markers = concrete_count + abstract_count
        if total_markers == 0:
            abstraction = 0.5  # neutral
        else:
            abstraction = abstract_count / total_markers

        # Blend with vocabulary sophistication (longer avg word length → more abstract)
        avg_word_len = sum(len(w) for w in words) / word_count if word_count > 0 else 4.0
        len_signal = min(1.0, max(0.0, (avg_word_len - 3.0) / 5.0))
        abstraction = 0.7 * abstraction + 0.3 * len_signal

        self._total_scored += 1

        return EntropyResult(
            entropy=round(max(0.0, min(1.0, entropy)), 4),
            abstraction=round(max(0.0, min(1.0, abstraction)), 4),
            vocab_richness=round(max(0.0, min(1.0, vocab_richness)), 4),
            word_count=word_count,
            unique_words=unique_words,
            concrete_markers=concrete_count,
            abstract_markers=abstract_count,
        )

    # ------------------------------------------------------------------
    # Batch sweep
    # ------------------------------------------------------------------

    def sweep(self, persist: bool = True, limit: int = 10000) -> EntropySweepReport:
        """Score all memories in the hot DB and optionally persist to metadata.

        Stores results in memory metadata as:
          metadata.entropy_score, metadata.abstraction_level, metadata.entropy_composite
        """
        start = time.perf_counter()
        report = EntropySweepReport()

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            memories = um.list_recent(limit=limit)
        except Exception as e:
            logger.error(f"Entropy sweep: could not load memories: {e}")
            return report

        backend = um.backend if persist else None

        total_entropy = 0.0
        total_abstraction = 0.0
        total_composite = 0.0

        for mem in memories:
            content_str = str(mem.content) if mem.content else ""
            if mem.title:
                content_str = f"{mem.title} {content_str}"

            result = self.score(content_str)
            report.total_scored += 1
            total_entropy += result.entropy
            total_abstraction += result.abstraction
            total_composite += result.composite

            if result.entropy > 0.7:
                report.high_entropy_count += 1
            elif result.entropy < 0.3:
                report.low_entropy_count += 1

            # Persist scores in metadata
            if persist and backend:
                try:
                    mem.metadata["entropy_score"] = result.entropy
                    mem.metadata["abstraction_level"] = result.abstraction
                    mem.metadata["entropy_composite"] = result.composite
                    backend.store(mem)
                except Exception:
                    pass

        n = max(report.total_scored, 1)
        report.avg_entropy = total_entropy / n
        report.avg_abstraction = total_abstraction / n
        report.avg_composite = total_composite / n
        report.duration_ms = (time.perf_counter() - start) * 1000

        logger.info(
            f"📊 Entropy sweep: {report.total_scored} scored, "
            f"avg_entropy={report.avg_entropy:.3f}, "
            f"avg_abstraction={report.avg_abstraction:.3f}, "
            f"{report.duration_ms:.0f}ms",
        )
        return report

    # ------------------------------------------------------------------
    # Retention evaluator plugin
    # ------------------------------------------------------------------

    def as_retention_evaluator(self) -> Callable[[Any], object]:
        """Return a callable compatible with RetentionEngine evaluators.

        Entropy contributes to retention: high-entropy memories are more
        worth keeping than low-entropy ones.
        """
        from whitemagic.core.memory.mindful_forgetting import RetentionSignal

        def _entropy_evaluator(mem: Any) -> RetentionSignal:
            content_str = str(mem.content) if mem.content else ""
            if mem.title:
                content_str = f"{mem.title} {content_str}"
            result = self.score(content_str)
            return RetentionSignal(
                name="entropy",
                score=result.composite,
                weight=0.15,
                reason=f"entropy={result.entropy:.2f}, abstraction={result.abstraction:.2f}",
            )

        return _entropy_evaluator

    # ------------------------------------------------------------------
    # Stats
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {"total_scored": self._total_scored}


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_scorer_instance: EntropyScorer | None = None
_scorer_lock = threading.Lock()


def get_entropy_scorer() -> EntropyScorer:
    """Get or create the global EntropyScorer singleton."""
    global _scorer_instance
    with _scorer_lock:
        if _scorer_instance is None:
            _scorer_instance = EntropyScorer()
        return _scorer_instance
