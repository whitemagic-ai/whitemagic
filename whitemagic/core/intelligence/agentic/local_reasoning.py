"""Local Reasoning Engine - Offload AI computation to local CPU/disk.
Version: 4.2.0.

PURPOSE: Reduce token burn by doing heavy lifting locally.

Instead of sending large context to cloud AI:
1. Use local parallel search (CloneArmy) to find relevant info
2. Use local pattern matching (Rust) for analysis
3. Use local embeddings for similarity search
4. Only send DISTILLED results to AI for final reasoning

Token Savings:
- Before: Send 50K tokens of context → AI processes → Response
- After: Local search finds 5 relevant items → Send 2K tokens → Response
- Result: 90%+ token reduction for many operations
"""

from typing import Any
import logging
import re
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class LocalInsight:
    """An insight discovered through local reasoning."""

    source: str
    content: str
    relevance: float
    method: str  # 'clone_search', 'pattern_match', 'embedding', 'rule'
    tokens_saved: int  # Estimated tokens NOT sent to cloud


@dataclass
class ReasoningResult:
    """Result of local reasoning process."""

    query: str
    insights: list[LocalInsight]
    summary: str
    total_tokens_saved: int
    duration_ms: float
    ready_for_ai: bool  # True if needs AI completion, False if fully resolved locally


class LocalReasoningEngine:
    """Offload reasoning to local CPU/disk operations.

    Philosophy: AI should receive DISTILLED context, not raw data.
    The more we process locally, the fewer tokens we burn.
    """

    def __init__(self, project_root: Path | None = None) -> None:
        from whitemagic.config import PROJECT_ROOT
        self.project_root = project_root or PROJECT_ROOT
        self._rules: list[Callable] = []
        self._patterns: dict[str, str] = {}

    def add_rule(self, rule: Callable[[str], LocalInsight | None]) -> Any:
        """Add a local reasoning rule."""
        self._rules.append(rule)

    def add_pattern(self, name: str, pattern: str) -> Any:
        """Add a pattern for local matching."""
        self._patterns[name] = pattern

    def reason_locally(self, query: str, max_results: int = 10) -> ReasoningResult:
        """Attempt to answer query using local resources.

        Order of operations:
        1. Check cached answers
        2. Apply rule-based reasoning
        3. Pattern matching
        4. Clone army search
        5. Embedding similarity (if available)

        Returns distilled results ready for AI (or fully resolved).
        """
        import time
        start = time.time()

        insights: list[LocalInsight] = []

        # 1. Rule-based reasoning (fastest, no I/O)
        for rule in self._rules:
            try:
                insight = rule(query)
                if insight:
                    insights.append(insight)
            except Exception:
                pass

        # 2. Pattern matching (fast, regex)
        pattern_insights = self._match_patterns(query)
        insights.extend(pattern_insights)

        if insights:
            insights.sort(key=lambda x: x.relevance, reverse=True)
            if insights[0].method == "rule" and insights[0].relevance >= 0.95:
                total_saved = sum(i.tokens_saved for i in insights)
                summary = self._generate_summary(query, insights)
                duration_ms = (time.time() - start) * 1000

                return ReasoningResult(
                    query=query,
                    insights=insights[:max_results],
                    summary=summary,
                    total_tokens_saved=total_saved,
                    duration_ms=duration_ms,
                    ready_for_ai=False,
                )

        # 3. Clone army search (parallel, thorough)
        clone_insights = self._clone_search(query, max_results)
        insights.extend(clone_insights)

        # 4. Embedding similarity (if Rust available)
        embedding_insights = self._embedding_search(query, max_results)
        insights.extend(embedding_insights)

        # Sort by relevance
        insights.sort(key=lambda x: x.relevance, reverse=True)
        insights = insights[:max_results]

        # Calculate tokens saved
        total_saved = sum(i.tokens_saved for i in insights)

        # Generate summary for AI
        summary = self._generate_summary(query, insights)

        duration_ms = (time.time() - start) * 1000

        # Determine if AI needed
        ready_for_ai = len(insights) > 0 and insights[0].relevance < 0.9

        return ReasoningResult(
            query=query,
            insights=insights,
            summary=summary,
            total_tokens_saved=total_saved,
            duration_ms=duration_ms,
            ready_for_ai=ready_for_ai,
        )

    def _match_patterns(self, query: str) -> list[LocalInsight]:
        """Match query against registered patterns."""
        insights = []
        query_lower = query.lower()

        for name, pattern in self._patterns.items():
            try:
                if re.search(pattern, query_lower):
                    insights.append(LocalInsight(
                        source=f"pattern:{name}",
                        content=f"Query matches pattern '{name}'",
                        relevance=0.7,
                        method="pattern_match",
                        tokens_saved=100,
                    ))
            except Exception:
                pass

        return insights

    def _clone_search(self, query: str, max_results: int) -> list[LocalInsight]:
        """Use Clone Army for parallel search."""
        try:
            from whitemagic.config.concurrency import CLONE_ARMY_WORKERS
            from whitemagic.core.memory.clones import CloneArmy

            # Reduced max_workers to prevent system freeze during parallel tests
            army = CloneArmy(
                army_size=100,
                base_path=self.project_root,
                max_workers=CLONE_ARMY_WORKERS,
            )
            results = army.search_and_deliberate(query)

            insights = []
            for result in results[:max_results]:
                # Estimate tokens saved (didn't send full file content)
                tokens_saved = len(result.content) // 4  # ~4 chars per token

                insights.append(LocalInsight(
                    source=result.memory_id,
                    content=result.content[:500],
                    relevance=result.consensus_score,
                    method="clone_search",
                    tokens_saved=tokens_saved,
                ))

            return insights
        except Exception:
            return []

    def _embedding_search(self, query: str, max_results: int) -> list[LocalInsight]:
        """Use Rust embeddings for similarity search."""
        try:
            import whitemagic_rs

            if not hasattr(whitemagic_rs, "similarity_search"):
                return []

            # This would use pre-computed embeddings
            results = whitemagic_rs.similarity_search(query, max_results)

            insights = []
            for path, score, snippet in results:
                insights.append(LocalInsight(
                    source=path,
                    content=snippet,
                    relevance=score,
                    method="embedding",
                    tokens_saved=len(snippet) // 4,
                ))

            return insights
        except Exception:
            return []

    def _generate_summary(self, query: str, insights: list[LocalInsight]) -> str:
        """Generate a summary for AI consumption."""
        if not insights:
            return f"No local insights found for: {query}"

        lines = [f"LOCAL REASONING SUMMARY for: {query}", ""]

        for i, insight in enumerate(insights[:5], 1):
            lines.append(f"{i}. [{insight.method}] {insight.source}")
            lines.append(f"   Relevance: {insight.relevance:.2f}")
            lines.append(f"   {insight.content[:200]}...")
            lines.append("")

        total_saved = sum(i.tokens_saved for i in insights)
        lines.append(f"Estimated tokens saved: {total_saved}")

        return "\n".join(lines)


# === PRE-BUILT REASONING RULES ===

def version_rule(query: str) -> LocalInsight | None:
    """Answer version questions locally."""
    if "version" in query.lower():
        from whitemagic.config import VERSION
        return LocalInsight(
            source="whitemagic.config.VERSION",
            content=f"WhiteMagic version is {VERSION}",
            relevance=1.0,
            method="rule",
            tokens_saved=500,  # Didn't need to search
        )
    return None


def garden_count_rule(query: str) -> LocalInsight | None:
    """Answer garden count questions locally."""
    q = query.lower()
    if "garden" in q and ("how many" in q or "count" in q or "number" in q):
        try:
            from whitemagic.gardens import get_all_gardens
            gardens = get_all_gardens()
            return LocalInsight(
                source="whitemagic.gardens",
                content=f"WhiteMagic has {len(gardens)} gardens: {', '.join(gardens.keys())}",
                relevance=1.0,
                method="rule",
                tokens_saved=1000,
            )
        except Exception:
            pass
    return None


def test_count_rule(query: str) -> LocalInsight | None:
    """Answer test count questions locally."""
    q = query.lower()
    if "test" in q and ("how many" in q or "count" in q or "pass" in q):
        return LocalInsight(
            source="pytest_cache",
            content="WhiteMagic has 507 passing tests (as of Dec 17, 2025)",
            relevance=0.9,
            method="rule",
            tokens_saved=500,
        )
    return None


def cpu_inference_rule(query: str) -> LocalInsight | None:
    """Use CPU inference for count/find queries."""
    q = query.lower()
    if "how many" in q or "find" in q or "where" in q or "count" in q:
        try:
            from whitemagic.core.intelligence.agentic.cpu_inference import cpu_infer
            result = cpu_infer(query)
            if result.confidence >= 0.7:
                return LocalInsight(
                    source=f"cpu_inference:{result.method}",
                    content=result.answer,
                    relevance=result.confidence,
                    method="cpu_inference",
                    tokens_saved=result.tokens_equivalent,
                )
        except Exception:
            pass
    return None


# === SINGLETON ===

_engine: LocalReasoningEngine | None = None


def get_local_reasoning() -> LocalReasoningEngine:
    """Get or create local reasoning engine with default rules."""
    global _engine
    if _engine is None:
        _engine = LocalReasoningEngine()
        # Add default rules (order matters - most specific first)
        _engine.add_rule(version_rule)
        _engine.add_rule(garden_count_rule)
        _engine.add_rule(test_count_rule)
        _engine.add_rule(cpu_inference_rule)  # NEW: CPU inference for count/find
        # Add common patterns
        _engine.add_pattern("file_search", r"find|search|locate|where is")
        _engine.add_pattern("definition", r"what is|define|explain")
        _engine.add_pattern("how_to", r"how to|how do|how can")
    return _engine


def reason_locally(query: str, max_results: int = 10) -> ReasoningResult:
    """Convenience function for local reasoning."""
    return get_local_reasoning().reason_locally(query, max_results)


# === CLI INTEGRATION ===

def command_reason_local(manager: Any, args: Any) -> Any:
    """CLI command for local reasoning."""
    query = args.query
    result = reason_locally(query)

    logger.info("\n🧠 LOCAL REASONING RESULT")
    logger.info("=" * 50)
    logger.info(f"Query: {query}")
    logger.info(f"Duration: {result.duration_ms:.1f}ms")
    logger.info(f"Tokens saved: {result.total_tokens_saved}")
    logger.info(f"AI needed: {'Yes' if result.ready_for_ai else 'No (fully resolved locally)'}")
    logger.info("")
    logger.info(result.summary)

    return 0


if __name__ == "__main__":
    # Demo
    logger.info("🧠 LOCAL REASONING ENGINE DEMO")
    logger.info("=" * 50)

    engine = get_local_reasoning()

    # Test 1: Version question (should resolve locally)
    result = engine.reason_locally("What version is WhiteMagic?")
    logger.info("\nQ: What version is WhiteMagic?")
    logger.info(f"A: {result.insights[0].content if result.insights else 'Not found'}")
    logger.info(f"Tokens saved: {result.total_tokens_saved}")

    # Test 2: Garden count (should resolve locally)
    result = engine.reason_locally("How many gardens does WhiteMagic have?")
    logger.info("\nQ: How many gardens?")
    logger.info(f"A: {result.insights[0].content if result.insights else 'Not found'}")

    # Test 3: Search query (uses clone army)
    result = engine.reason_locally("parallel processing capabilities")
    logger.info("\nQ: parallel processing capabilities")
    logger.info(f"Found: {len(result.insights)} insights")
    logger.info(f"Tokens saved: {result.total_tokens_saved}")
    logger.info(f"Duration: {result.duration_ms:.1f}ms")
