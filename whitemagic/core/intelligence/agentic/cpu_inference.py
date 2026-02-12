from __future__ import annotations

import logging
import re
from collections import Counter, defaultdict
from collections.abc import Callable
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)
"""
CPU/I/O Inference Engine - Local computation for AI tasks.
Version: 3.0.0

Philosophy: Use CPU and disk I/O instead of cloud GPU tokens.

This module provides inference-like capabilities using only local resources:
1. Pattern matching (regex, fuzzy)
2. Statistical analysis (frequency, correlation)
3. Graph traversal (relationships, dependencies)
4. Rule-based inference (if-then chains)
5. Template-based generation (fill-in-the-blank)

No neural networks, no GPU, no cloud - just smart algorithms.
"""



@dataclass
class InferenceResult:
    """Result of a CPU inference operation."""

    query: str
    answer: str
    confidence: float
    method: str
    evidence: list[str] = field(default_factory=list)
    tokens_equivalent: int = 0  # How many tokens this would cost in cloud


class PatternMatcher:
    """Fast pattern matching using regex and fuzzy search."""

    @lru_cache(maxsize=128)  # type: ignore[misc]
    def __init__(self) -> None:
        self._patterns: dict[str, tuple[str, str]] = {}  # name -> (pattern, template)

    @lru_cache(maxsize=128)
    def add_pattern(self, name: str, pattern: str, response_template: str) -> None:
        """Add a pattern with response template."""
        self._patterns[name] = (pattern, response_template)

    @lru_cache(maxsize=128)
    def match(self, query: str) -> tuple[str, str, dict[str, Any]] | None:
        """Match query against patterns, return (name, response, captures)."""
        query_lower = query.lower()
        for name, (pattern, template) in self._patterns.items():
            match = re.search(pattern, query_lower)
            if match:
                # Fill template with captures
                groups = match.groupdict() if match.groupdict() else {}
                response = template.format(**groups) if groups else template
                return name, response, groups
        return None


class StatisticalAnalyzer:
    """Statistical analysis on local data."""

    @staticmethod
    @lru_cache(maxsize=128)
    def word_frequency(text: str, top_n: int = 10) -> list[tuple[str, int]]:
        """Get most frequent words."""
        words = re.findall(r"\b\w+\b", text.lower())
        # Filter common words
        stopwords = {"the", "a", "an", "is", "are", "was", "were", "be", "been",
                     "being", "have", "has", "had", "do", "does", "did", "will",
                     "would", "could", "should", "may", "might", "must", "shall",
                     "to", "of", "in", "for", "on", "with", "at", "by", "from",
                     "and", "or", "but", "if", "then", "else", "when", "where",
                     "this", "that", "these", "those", "it", "its"}
        words = [w for w in words if w not in stopwords and len(w) > 2]
        return Counter(words).most_common(top_n)

    @staticmethod
    @lru_cache(maxsize=128)
    def extract_numbers(text: str) -> list[float]:
        """Extract all numbers from text."""
        numbers = re.findall(r"-?\d+\.?\d*", text)
        return [float(n) for n in numbers]

    @staticmethod
    def summarize_numbers(numbers: list[float]) -> dict[str, float]:
        """Get statistical summary of numbers."""
        if not numbers:
            return {}
        return {
            "count": len(numbers),
            "sum": sum(numbers),
            "mean": sum(numbers) / len(numbers),
            "min": min(numbers),
            "max": max(numbers),
        }


class DependencyGraph:
    """Graph-based inference for relationships."""

    def __init__(self) -> None:
        self._edges: dict[str, set[str]] = defaultdict(set)
        self._reverse: dict[str, set[str]] = defaultdict(set)

    def add_edge(self, from_node: str, to_node: str) -> None:
        """Add directed edge."""
        self._edges[from_node].add(to_node)
        self._reverse[to_node].add(from_node)

    def get_dependencies(self, node: str) -> set[str]:
        """Get all nodes this node depends on."""
        return self._edges.get(node, set())

    def get_dependents(self, node: str) -> set[str]:
        """Get all nodes that depend on this node."""
        return self._reverse.get(node, set())

    def get_all_dependencies(self, node: str, visited: set[str] | None = None) -> set[str]:
        """Get transitive closure of dependencies."""
        if visited is None:
            visited = set()
        if node in visited:
            return set()
        visited.add(node)
        deps = self._edges.get(node, set())
        all_deps = set(deps)
        for dep in deps:
            all_deps.update(self.get_all_dependencies(dep, visited))
        return all_deps


class RuleEngine:
    """If-then rule-based inference."""

    def __init__(self) -> None:
        self._rules: list[tuple[Callable[..., Any], Callable[..., Any]]] = []
        self._facts: set[str] = set()

    def add_rule(self, condition: Callable[..., Any], action: Callable[..., Any]) -> None:
        """Add inference rule."""
        self._rules.append((condition, action))

    def add_fact(self, fact: str) -> None:
        """Assert a fact."""
        self._facts.add(fact.lower())

    def has_fact(self, fact: str) -> bool:
        """Check if fact is known."""
        return fact.lower() in self._facts

    def infer(self) -> list[str]:
        """Run forward chaining inference."""
        new_facts = []
        changed = True
        while changed:
            changed = False
            for condition, action in self._rules:
                if condition(self._facts):
                    result = action(self._facts)
                    if result and result.lower() not in self._facts:
                        self._facts.add(result.lower())
                        new_facts.append(result)
                        changed = True
        return new_facts


class TemplateGenerator:
    """Template-based text generation."""

    def __init__(self) -> None:
        self._templates: dict[str, str] = {}

    def add_template(self, name: str, template: str) -> None:
        """Add a template."""
        self._templates[name] = template

    def generate(self, name: str, **kwargs: Any) -> str | None:
        """Generate text from template."""
        template = self._templates.get(name)
        if not template:
            return None
        try:
            return template.format(**kwargs)
        except KeyError:
            return None


class CPUInferenceEngine:
    """Main CPU-based inference engine.

    Combines multiple inference methods for complex queries.
    """

    def __init__(self, project_root: Path | None = None) -> None:
        from whitemagic.config import PROJECT_ROOT
        self.project_root = project_root or PROJECT_ROOT

        self.patterns = PatternMatcher()
        self.stats = StatisticalAnalyzer()
        self.graph = DependencyGraph()
        self.rules = RuleEngine()
        self.templates = TemplateGenerator()

        self._setup_default_patterns()
        self._setup_default_rules()
        self._setup_default_templates()

    def _setup_default_patterns(self) -> None:
        """Setup common query patterns."""
        # File queries
        self.patterns.add_pattern(
            "find_file",
            r"(?:find|where is|locate)\s+(?:the\s+)?(?:file\s+)?['\"]?(\w+)['\"]?",
            "Searching for files matching '{0}'...",
        )
        # Count queries
        self.patterns.add_pattern(
            "count",
            r"how many (\w+)",
            "Counting {0}...",
        )
        # Definition queries
        self.patterns.add_pattern(
            "definition",
            r"what is (?:a |the )?(\w+)",
            "Looking up definition of {0}...",
        )

    def _setup_default_rules(self) -> None:
        """Setup inference rules."""
        # If we have gardens, we have a garden architecture
        self.rules.add_rule(
            lambda facts: "has gardens" in facts,
            lambda facts: "has garden architecture",
        )
        # If we have tests passing, the code is stable
        self.rules.add_rule(
            lambda facts: "tests passing" in facts,
            lambda facts: "code is stable",
        )

    def _setup_default_templates(self) -> None:
        """Setup response templates."""
        self.templates.add_template(
            "file_found",
            "Found {count} files matching '{query}':\n{files}",
        )
        self.templates.add_template(
            "count_result",
            "There are {count} {item_type} in the project.",
        )

    def infer(self, query: str) -> InferenceResult:
        """Run inference on a query using CPU-only methods.

        Order:
        1. Count queries (most common)
        2. File search queries
        3. Pattern matching (fallback)
        """
        # 1. Try count queries first (most useful)
        if "how many" in query.lower():
            return self._count_inference(query)

        # 2. Try file search for "find" queries
        if "find" in query.lower() or "where" in query.lower():
            return self._file_search_inference(query)

        # 4. Default: return low confidence
        return InferenceResult(
            query=query,
            answer="Unable to infer locally. May need AI assistance.",
            confidence=0.2,
            method="none",
            tokens_equivalent=0,
        )

    def _file_search_inference(self, query: str) -> InferenceResult:
        """Infer answer for file search queries."""
        # Extract search term
        words = query.lower().split()
        search_terms = [w for w in words if len(w) > 3 and w not in
                       {"find", "where", "file", "files", "the", "is", "are", "locate"}]

        if not search_terms:
            return InferenceResult(
                query=query,
                answer="Please specify what to search for.",
                confidence=0.3,
                method="file_search",
                tokens_equivalent=50,
            )

        # Search for files
        search_term = search_terms[0]
        matches = list(self.project_root.rglob(f"*{search_term}*"))[:20]

        if matches:
            files_str = "\n".join(f"  - {f.relative_to(self.project_root)}" for f in matches[:10])
            answer = self.templates.generate(
                "file_found",
                count=len(matches),
                query=search_term,
                files=files_str,
            )
            return InferenceResult(
                query=query,
                answer=answer or "",
                confidence=0.9,
                method="file_search",
                evidence=[str(m.relative_to(self.project_root)) for m in matches[:5]],
                tokens_equivalent=len(matches) * 20,
            )

        return InferenceResult(
            query=query,
            answer=f"No files found matching '{search_term}'",
            confidence=0.7,
            method="file_search",
            tokens_equivalent=50,
        )

    def _count_inference(self, query: str) -> InferenceResult:
        """Infer answer for count queries."""
        query_lower = query.lower()

        # Count tests (check first - more specific)
        if "test" in query_lower:
            test_files = list((self.project_root / "tests").rglob("test_*.py"))
            return InferenceResult(
                query=query,
                answer=f"There are {len(test_files)} test files in tests/.",
                confidence=0.9,
                method="count",
                tokens_equivalent=100,
            )

        # Count markdown/docs
        if "markdown" in query_lower or "doc" in query_lower or " md " in query_lower:
            count = len([f for f in self.project_root.rglob("*.md")
                        if "node_modules" not in str(f)])
            return InferenceResult(
                query=query,
                answer=f"There are {count} markdown files.",
                confidence=0.95,
                method="count",
                tokens_equivalent=count * 2,
            )

        # Count gardens
        if "garden" in query_lower:
            try:
                from whitemagic.gardens import get_all_gardens
                gardens = get_all_gardens()
                return InferenceResult(
                    query=query,
                    answer=f"There are {len(gardens)} gardens: {', '.join(gardens.keys())}",
                    confidence=1.0,
                    method="count",
                    tokens_equivalent=500,
                )
            except Exception:
                pass

        # Count Python files (general fallback)
        if "python" in query_lower or "file" in query_lower:
            count = len([f for f in self.project_root.rglob("*.py")
                        if "node_modules" not in str(f) and "__pycache__" not in str(f)])
            return InferenceResult(
                query=query,
                answer=f"There are {count} Python files in the project.",
                confidence=0.95,
                method="count",
                evidence=["Counted *.py files excluding node_modules"],
                tokens_equivalent=count * 5,
            )

        return InferenceResult(
            query=query,
            answer="Specify what to count: files, tests, gardens, docs, etc.",
            confidence=0.3,
            method="count",
            tokens_equivalent=50,
        )


# === SINGLETON ===

_engine: CPUInferenceEngine | None = None


def get_cpu_inference() -> CPUInferenceEngine:
    """Get or create CPU inference engine."""
    global _engine
    if _engine is None:
        _engine = CPUInferenceEngine()
    return _engine


def cpu_infer(query: str) -> InferenceResult:
    """Convenience function for CPU inference."""
    return get_cpu_inference().infer(query)


if __name__ == "__main__":
    logger.info("🖥️ CPU INFERENCE ENGINE DEMO")
    logger.info("=" * 50)

    engine = get_cpu_inference()

    queries = [
        "How many Python files are there?",
        "Find the clone army file",
        "How many test files?",
        "How many markdown files?",
    ]

    for q in queries:
        result = engine.infer(q)
        logger.info(f"\nQ: {q}")
        logger.info(f"A: {result.answer}")
        logger.info(f"Confidence: {result.confidence:.0%}")
        logger.info(f"Method: {result.method}")
        logger.info(f"Tokens equivalent: {result.tokens_equivalent}")
