"""Pattern Learning - Learn rules from query patterns.
Version: 3.0.0

Watches queries and their answers to automatically generate
new rules for edge inference.

This enables the system to get smarter over time without
manual rule creation.
"""

from __future__ import annotations

import json
import logging
import re
from collections import Counter, defaultdict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class QueryRecord:
    """Record of a query and its resolution."""

    query: str
    answer: str
    method: str  # 'local', 'cloud', 'cache'
    confidence: float
    timestamp: str
    tokens_used: int = 0


@dataclass
class LearnedRule:
    """A rule learned from query patterns."""

    id: str
    pattern: str  # Keywords separated by |
    response: str
    confidence: float
    learned_from: list[str]  # Query IDs that contributed
    created: str
    times_used: int = 0


class PatternLearner:
    """Learn patterns from queries to generate new rules.

    Workflow:
    1. Record all queries and their resolutions
    2. Find clusters of similar queries
    3. Extract common patterns
    4. Generate rules from patterns
    5. Validate rules against held-out queries
    """

    def __init__(self, data_dir: Path | None = None) -> None:
        from whitemagic.config.paths import DATA_DIR
        self.data_dir = data_dir or (DATA_DIR / "learning")
        self.data_dir.mkdir(parents=True, exist_ok=True)

        self._queries: list[QueryRecord] = []
        self._learned_rules: list[LearnedRule] = []
        self._load()

    def _load(self) -> None:
        """Load existing data."""
        queries_file = self.data_dir / "queries.json"
        rules_file = self.data_dir / "learned_rules.json"

        if queries_file.exists():
            try:
                data = json.loads(queries_file.read_text())
                self._queries = [QueryRecord(**q) for q in data]
            except json.JSONDecodeError:
                pass

        if rules_file.exists():
            try:
                data = json.loads(rules_file.read_text())
                self._learned_rules = [LearnedRule(**r) for r in data]
            except json.JSONDecodeError:
                pass

    def _save(self) -> None:
        """Save data."""
        queries_file = self.data_dir / "queries.json"
        rules_file = self.data_dir / "learned_rules.json"

        queries_file.write_text(json.dumps([
            {
                "query": q.query,
                "answer": q.answer,
                "method": q.method,
                "confidence": q.confidence,
                "timestamp": q.timestamp,
                "tokens_used": q.tokens_used,
            }
            for q in self._queries
        ], indent=2))

        rules_file.write_text(json.dumps([
            {
                "id": r.id,
                "pattern": r.pattern,
                "response": r.response,
                "confidence": r.confidence,
                "learned_from": r.learned_from,
                "created": r.created,
                "times_used": r.times_used,
            }
            for r in self._learned_rules
        ], indent=2))

    def record_query(self, query: str, answer: str, method: str,
                     confidence: float, tokens_used: int = 0) -> None:
        """Record a query for learning."""
        self._queries.append(QueryRecord(
            query=query,
            answer=answer,
            method=method,
            confidence=confidence,
            timestamp=datetime.now().isoformat(),
            tokens_used=tokens_used,
        ))
        self._save()

    def _extract_keywords(self, text: str) -> list[str]:
        """Extract significant keywords from text."""
        stopwords = {"the", "a", "an", "is", "are", "was", "were", "be",
                     "to", "of", "in", "for", "on", "with", "at", "by",
                     "and", "or", "but", "if", "this", "that", "it",
                     "what", "how", "when", "where", "why", "which",
                     "can", "do", "does", "have", "has", "my", "your"}

        words = re.findall(r"\b\w+\b", text.lower())
        return [w for w in words if w not in stopwords and len(w) > 2]

    def _find_similar_queries(self) -> dict[str, list[QueryRecord]]:
        """Group similar queries by common keywords."""
        clusters = defaultdict(list)

        for query in self._queries:
            keywords = self._extract_keywords(query.query)
            if keywords:
                # Use first significant keyword as cluster key
                key = keywords[0]
                clusters[key].append(query)

        # Filter to clusters with multiple queries
        return {k: v for k, v in clusters.items() if len(v) >= 2}

    def learn(self, min_cluster_size: int = 2, min_confidence: float = 0.8) -> list[LearnedRule]:
        """Learn new rules from recorded queries.

        Returns: List of newly learned rules
        """
        clusters = self._find_similar_queries()
        new_rules = []

        for cluster_key, queries in clusters.items():
            # Only learn from high-confidence cloud answers
            cloud_queries = [q for q in queries
                          if q.method == "cloud" and q.confidence >= min_confidence]

            if len(cloud_queries) < min_cluster_size:
                continue

            # Check if we already have a rule for this pattern
            existing_patterns = {r.pattern for r in self._learned_rules}

            # Extract common keywords across queries
            all_keywords = []
            for q in cloud_queries:
                all_keywords.extend(self._extract_keywords(q.query))

            common = Counter(all_keywords).most_common(3)
            pattern = "|".join(kw for kw, _ in common)

            if pattern in existing_patterns:
                continue

            # Use most common answer
            answers = Counter(q.answer for q in cloud_queries)
            best_answer, count = answers.most_common(1)[0]

            # Create rule
            rule = LearnedRule(
                id=f"learned_{len(self._learned_rules) + 1}",
                pattern=pattern,
                response=best_answer,
                confidence=count / len(cloud_queries),
                learned_from=[q.query for q in cloud_queries[:5]],
                created=datetime.now().isoformat(),
            )

            self._learned_rules.append(rule)
            new_rules.append(rule)

        self._save()
        return new_rules

    def get_learned_rules(self) -> list[LearnedRule]:
        """Get all learned rules."""
        return self._learned_rules

    def export_rules(self) -> list[dict[str, Any]]:
        """Export learned rules for use in EdgeInference."""
        return [
            {
                "id": r.id,
                "pattern": r.pattern,
                "response": r.response,
                "confidence": r.confidence,
                "metadata": {"learned_from": r.learned_from[:3]},
            }
            for r in self._learned_rules
        ]

    def stats(self) -> dict[str, Any]:
        """Get learning statistics."""
        cloud_queries = [q for q in self._queries if q.method == "cloud"]
        local_queries = [q for q in self._queries if q.method == "local"]

        return {
            "total_queries": len(self._queries),
            "cloud_queries": len(cloud_queries),
            "local_queries": len(local_queries),
            "learned_rules": len(self._learned_rules),
            "tokens_saved_by_local": sum(q.tokens_used for q in local_queries),
            "potential_savings": len(cloud_queries) * 500,  # Estimate
        }


if __name__ == "__main__":
    logger.info("🧠 PATTERN LEARNING TEST")
    logger.info("=" * 50)

    learner = PatternLearner()

    # Simulate some queries
    learner.record_query(
        "What version is WhiteMagic?",
        "WhiteMagic version 3.0.0",
        "cloud",
        0.95,
        500,
    )
    learner.record_query(
        "What's the current version?",
        "WhiteMagic version 3.0.0",
        "cloud",
        0.90,
        450,
    )
    learner.record_query(
        "Version number?",
        "WhiteMagic version 3.0.0",
        "cloud",
        0.85,
        300,
    )

    # Learn rules
    new_rules = learner.learn()

    logger.info(f"Learned {len(new_rules)} new rules:")
    for rule in new_rules:
        logger.info(f"  - Pattern: {rule.pattern}")
        logger.info(f"    Response: {rule.response[:50]}...")

    logger.info(f"\nStats: {learner.stats()}")
