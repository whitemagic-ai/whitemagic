"""Novelty Detector - Detects new and emergent patterns.

Part of the emergence system. Identifies what's genuinely new.
"""

import hashlib
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass
class NoveltyEvent:
    """A detected novelty."""

    content: str
    novelty_score: float
    timestamp: datetime = field(default_factory=datetime.now)
    category: str = "unknown"
    hash_id: str = ""

    def __post_init__(self) -> None:
        if not self.hash_id:
            self.hash_id = hashlib.md5(self.content.encode()).hexdigest()[:8]


class NoveltyDetector:
    """Detects novel patterns that haven't been seen before."""

    def __init__(self) -> None:
        self.seen_hashes: set[str] = set()
        self.pattern_counts: dict[str, int] = {}
        self.novelty_history: list[NoveltyEvent] = []

    def detect(self, content: str, context: dict[str, Any] | None = None) -> NoveltyEvent:
        """Detect if content is novel."""
        content_hash = hashlib.md5(content.encode()).hexdigest()[:8]

        # Check if we've seen this exact content
        is_exact_duplicate = content_hash in self.seen_hashes
        self.seen_hashes.add(content_hash)

        # Check pattern similarity (simple word-based)
        words = set(content.lower().split())
        similar_count = 0
        for pattern in self.pattern_counts:
            pattern_words = set(pattern.lower().split())
            overlap = len(words & pattern_words) / max(len(words | pattern_words), 1)
            if overlap > 0.7:
                similar_count += 1

        # Calculate novelty score
        if is_exact_duplicate:
            novelty_score = 0.0
        elif similar_count > 5:
            novelty_score = 0.2
        elif similar_count > 2:
            novelty_score = 0.5
        elif similar_count > 0:
            novelty_score = 0.7
        else:
            novelty_score = 1.0  # Completely new!

        # Store pattern
        self.pattern_counts[content[:100]] = self.pattern_counts.get(content[:100], 0) + 1

        event = NoveltyEvent(
            content=content[:200],
            novelty_score=novelty_score,
            category="pattern" if context and context.get("type") == "pattern" else "content",
            hash_id=content_hash,
        )
        self.novelty_history.append(event)

        return event

    def get_novelty_stats(self) -> dict[str, Any]:
        """Get statistics about detected novelty."""
        if not self.novelty_history:
            return {"total": 0, "avg_novelty": 0.0}

        total = len(self.novelty_history)
        avg_novelty = sum(e.novelty_score for e in self.novelty_history) / total
        highly_novel = sum(1 for e in self.novelty_history if e.novelty_score > 0.8)

        return {
            "total": total,
            "avg_novelty": avg_novelty,
            "highly_novel_count": highly_novel,
            "unique_hashes": len(self.seen_hashes),
        }


class EmergenceScorer:
    """Scores the emergence level of patterns."""

    def __init__(self) -> None:
        self.scores: list[dict[str, Any]] = []

    def score(self, pattern: dict[str, Any]) -> float:
        """Score how emergent a pattern is."""
        # Emergence indicators
        complexity: float = pattern.get("complexity", 0.5)
        novelty: float = pattern.get("novelty", 0.5)
        integration: float = pattern.get("integration", 0.5)
        self_organization: float = pattern.get("self_organization", 0.5)

        # Weighted score
        emergence_score: float = (
            complexity * 0.2 +
            novelty * 0.3 +
            integration * 0.25 +
            self_organization * 0.25
        )

        result = {
            "pattern": pattern.get("name", "unnamed"),
            "score": emergence_score,
            "components": {
                "complexity": complexity,
                "novelty": novelty,
                "integration": integration,
                "self_organization": self_organization,
            },
        }
        self.scores.append(result)

        return emergence_score

    def is_emergent(self, pattern: dict[str, Any], threshold: float = 0.6) -> bool:
        """Check if a pattern qualifies as emergent."""
        return self.score(pattern) >= threshold


# Singletons
_detector: NoveltyDetector | None = None
_scorer: EmergenceScorer | None = None


def get_novelty_detector() -> NoveltyDetector:
    global _detector
    if _detector is None:
        _detector = NoveltyDetector()
    return _detector


def get_emergence_scorer() -> EmergenceScorer:
    global _scorer
    if _scorer is None:
        _scorer = EmergenceScorer()
    return _scorer
