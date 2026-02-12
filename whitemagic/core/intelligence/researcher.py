"""JIT Memory Researcher — Iterative Plan-Search-Reflect at Query Time.
====================================================================
Inspired by the General Agentic Memory (GAM) paper's key insight:
memory retrieval should be an *iterative reasoning process*, not a
single-shot vector search.

The Researcher takes a query and runs a multi-step loop:
  1. PLAN   — Decompose the query into sub-questions
  2. SEARCH — Execute hybrid recall for each sub-question
  3. REFLECT — Evaluate retrieved evidence, identify gaps
  4. REPEAT — Refine sub-questions based on gaps (up to max_rounds)
  5. SYNTHESIZE — Compose a final answer from accumulated evidence

This runs at query time (JIT) — no pre-indexing needed beyond the
existing memory store. It leverages hybrid_recall (BM25 + embedding +
graph walk) as its search primitive.

Gana: gana_winnowing_basket (Wisdom & Search)

Usage:
    from whitemagic.core.intelligence.researcher import get_researcher
    researcher = get_researcher()
    result = researcher.research("What patterns exist in our dream cycle data?")
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class ResearchStep:
    """A single step in the research process."""

    round_num: int
    phase: str  # "plan", "search", "reflect"
    sub_questions: list[str] = field(default_factory=list)
    evidence: list[dict[str, Any]] = field(default_factory=list)
    gaps: list[str] = field(default_factory=list)
    duration_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "round": self.round_num,
            "phase": self.phase,
            "sub_questions": self.sub_questions,
            "evidence_count": len(self.evidence),
            "gaps": self.gaps,
            "duration_ms": round(self.duration_ms, 1),
        }


@dataclass
class ResearchResult:
    """Final result of a JIT research session."""

    query: str
    rounds_completed: int = 0
    total_evidence: int = 0
    unique_memories: int = 0
    synthesis: str = ""
    evidence: list[dict[str, Any]] = field(default_factory=list)
    steps: list[ResearchStep] = field(default_factory=list)
    duration_ms: float = 0.0
    saturated: bool = False  # True if no new evidence found

    def to_dict(self) -> dict[str, Any]:
        return {
            "query": self.query,
            "rounds_completed": self.rounds_completed,
            "total_evidence": self.total_evidence,
            "unique_memories": self.unique_memories,
            "synthesis": self.synthesis,
            "evidence": self.evidence[:20],  # cap for response size
            "steps": [s.to_dict() for s in self.steps],
            "duration_ms": round(self.duration_ms, 1),
            "saturated": self.saturated,
        }


class JITResearcher:
    """Iterative Plan-Search-Reflect memory researcher.

    Each research session:
    1. Decomposes the query into sub-questions (keyword extraction + semantic decomposition)
    2. Searches across the memory store for each sub-question
    3. Evaluates coverage gaps and refines sub-questions
    4. Repeats until saturated or max rounds reached
    5. Synthesizes findings into a coherent response
    """

    def __init__(
        self,
        max_rounds: int = 3,
        evidence_per_question: int = 5,
        saturation_threshold: float = 0.8,
    ) -> None:
        self._max_rounds = max_rounds
        self._evidence_per_question = evidence_per_question
        self._saturation_threshold = saturation_threshold
        self._lock = threading.Lock()
        self._total_sessions = 0
        self._total_evidence_found = 0

    # ------------------------------------------------------------------
    # Core research loop
    # ------------------------------------------------------------------

    def research(
        self,
        query: str,
        max_rounds: int | None = None,
        evidence_limit: int | None = None,
        hops: int = 2,
    ) -> ResearchResult:
        """Run iterative research on a query.

        Args:
            query: The research question.
            max_rounds: Override max iteration rounds.
            evidence_limit: Override evidence per sub-question.
            hops: Graph walk depth for hybrid recall.

        Returns:
            ResearchResult with accumulated evidence and synthesis.
        """
        start = time.perf_counter()
        rounds = max_rounds or self._max_rounds
        per_q = evidence_limit or self._evidence_per_question

        result = ResearchResult(query=query)
        seen_ids: set[str] = set()
        all_evidence: list[dict[str, Any]] = []

        # Round 0: Initial plan
        sub_questions = self._plan(query)

        for round_num in range(rounds):
            step = ResearchStep(round_num=round_num + 1, phase="search")
            step.sub_questions = sub_questions
            step_start = time.perf_counter()

            # SEARCH phase
            round_evidence: list[dict[str, Any]] = []
            for sq in sub_questions:
                hits = self._search(sq, limit=per_q, hops=hops)
                for hit in hits:
                    mid = hit.get("memory_id", "")
                    if mid and mid not in seen_ids:
                        seen_ids.add(mid)
                        hit["found_via"] = sq
                        hit["round"] = round_num + 1
                        round_evidence.append(hit)

            step.evidence = round_evidence
            all_evidence.extend(round_evidence)

            # REFLECT phase — identify gaps
            gaps = self._reflect(query, sub_questions, round_evidence, all_evidence)
            step.gaps = gaps

            step.duration_ms = (time.perf_counter() - step_start) * 1000
            result.steps.append(step)
            result.rounds_completed = round_num + 1

            # Check saturation
            if not round_evidence or not gaps:
                result.saturated = True
                break

            # Refine sub-questions from gaps for next round
            sub_questions = gaps

        # SYNTHESIZE phase
        result.evidence = all_evidence
        result.total_evidence = len(all_evidence)
        result.unique_memories = len(seen_ids)
        result.synthesis = self._synthesize(query, all_evidence)
        result.duration_ms = (time.perf_counter() - start) * 1000

        with self._lock:
            self._total_sessions += 1
            self._total_evidence_found += len(all_evidence)

        logger.info(
            "🔬 JIT Research: '%s' → %d rounds, %d evidence, %d unique memories (%.0fms)",
            query[:50], result.rounds_completed, result.total_evidence,
            result.unique_memories, result.duration_ms,
        )
        return result

    # ------------------------------------------------------------------
    # Plan: Decompose query into sub-questions
    # ------------------------------------------------------------------

    def _plan(self, query: str) -> list[str]:
        """Decompose a query into searchable sub-questions.

        Uses keyword extraction + semantic faceting to generate
        diverse search angles.
        """
        sub_questions = [query]  # Always include the original

        # Extract keywords for focused searches
        keywords = self._extract_keywords(query)
        for kw in keywords[:3]:
            sub_questions.append(kw)

        # Generate faceted sub-questions
        facets = self._generate_facets(query)
        sub_questions.extend(facets[:3])

        # Deduplicate while preserving order
        seen: set[str] = set()
        unique: list[str] = []
        for sq in sub_questions:
            sq_lower = sq.lower().strip()
            if sq_lower and sq_lower not in seen:
                seen.add(sq_lower)
                unique.append(sq)

        return unique[:7]  # Cap at 7 sub-questions per round

    def _extract_keywords(self, text: str) -> list[str]:
        """Extract key phrases from text."""
        try:
            from whitemagic.core.acceleration.rust_accelerators import keyword_extract
            return list(keyword_extract(text, max_keywords=5))
        except Exception:
            pass

        # Fallback: simple word extraction
        stop_words = {
            "the", "a", "an", "is", "are", "was", "were", "be", "been",
            "being", "have", "has", "had", "do", "does", "did", "will",
            "would", "could", "should", "may", "might", "can", "shall",
            "in", "on", "at", "to", "for", "of", "with", "by", "from",
            "as", "into", "through", "during", "before", "after", "above",
            "below", "between", "out", "off", "over", "under", "again",
            "further", "then", "once", "here", "there", "when", "where",
            "why", "how", "all", "both", "each", "few", "more", "most",
            "other", "some", "such", "no", "nor", "not", "only", "own",
            "same", "so", "than", "too", "very", "what", "which", "who",
            "this", "that", "these", "those", "it", "its", "and", "but",
            "or", "if", "our", "we", "my", "me", "i", "you", "your",
        }
        words = text.lower().split()
        keywords = [w.strip(".,!?;:\"'()[]{}") for w in words if len(w) > 2]
        keywords = [w for w in keywords if w and w not in stop_words]

        # Return unique keywords preserving order
        seen: set[str] = set()
        result: list[str] = []
        for w in keywords:
            if w not in seen:
                seen.add(w)
                result.append(w)
        return result[:5]

    def _generate_facets(self, query: str) -> list[str]:
        """Generate diverse search angles for a query.

        Adds temporal, causal, and contextual facets.
        """
        facets: list[str] = []
        q_lower = query.lower()

        # Temporal facet
        if not any(t in q_lower for t in ["when", "history", "timeline", "recent"]):
            facets.append(f"history timeline {query}")

        # Causal facet
        if not any(t in q_lower for t in ["why", "cause", "reason", "because"]):
            facets.append(f"cause reason {query}")

        # Related patterns facet
        facets.append(f"related patterns {query}")

        return facets

    # ------------------------------------------------------------------
    # Search: Execute hybrid recall
    # ------------------------------------------------------------------

    def _search(
        self, sub_question: str, limit: int = 5, hops: int = 2,
    ) -> list[dict[str, Any]]:
        """Search the memory store using hybrid recall."""
        try:
            from whitemagic.core.memory.graph_walker import get_graph_walker
            walker = get_graph_walker()
            results = walker.hybrid_recall(
                query=sub_question,
                hops=hops,
                anchor_limit=limit,
                walk_top_k=limit * 2,
                final_limit=limit,
            )
            return results
        except Exception:
            pass

        # Fallback to basic search
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            memories = um.search(query=sub_question, limit=limit)
            return [
                {
                    "memory_id": m.id,
                    "title": m.title,
                    "content": str(m.content)[:500],
                    "importance": m.importance,
                    "source": "basic_search",
                }
                for m in memories
            ]
        except Exception as e:
            logger.debug(f"Research search failed: {e}")
            return []

    # ------------------------------------------------------------------
    # Reflect: Evaluate coverage and identify gaps
    # ------------------------------------------------------------------

    def _reflect(
        self,
        original_query: str,
        sub_questions: list[str],
        round_evidence: list[dict[str, Any]],
        all_evidence: list[dict[str, Any]],
    ) -> list[str]:
        """Reflect on accumulated evidence and identify knowledge gaps.

        Returns refined sub-questions targeting identified gaps.
        """
        if not round_evidence:
            return []  # No new evidence = saturated

        # Check if we're finding the same things over and over
        new_ratio = len(round_evidence) / max(1, len(all_evidence))
        if new_ratio < (1.0 - self._saturation_threshold):
            return []  # Diminishing returns

        # Extract topics covered by evidence
        covered_topics: set[str] = set()
        for ev in all_evidence:
            title = ev.get("title", "") or ""
            content = ev.get("content", "") or ""
            # Simple topic extraction
            words = (title + " " + content).lower().split()
            for w in words:
                w = w.strip(".,!?;:\"'()[]{}").strip()
                if len(w) > 3:
                    covered_topics.add(w)

        # Identify aspects of the original query not yet covered
        query_words = set(
            w.strip(".,!?;:\"'()[]{}").strip()
            for w in original_query.lower().split()
            if len(w.strip(".,!?;:\"'()[]{}").strip()) > 3
        )
        uncovered = query_words - covered_topics

        # Generate gap-filling sub-questions
        gaps: list[str] = []
        if uncovered:
            gap_terms = " ".join(sorted(uncovered)[:5])
            gaps.append(f"{gap_terms} {original_query}")

        # Add a broadening search if we have limited evidence
        if len(all_evidence) < 3:
            gaps.append(f"examples applications {original_query}")

        # Add a deepening search based on most relevant evidence
        if round_evidence:
            best = max(round_evidence, key=lambda e: e.get("importance", 0) or 0)
            best_title = best.get("title", "")
            if best_title:
                gaps.append(f"related to {best_title}")

        return gaps[:3]  # Cap at 3 gap-filling questions per round

    # ------------------------------------------------------------------
    # Synthesize: Compose findings
    # ------------------------------------------------------------------

    def _synthesize(
        self, query: str, evidence: list[dict[str, Any]],
    ) -> str:
        """Synthesize accumulated evidence into a coherent summary.

        Uses Ollama if available, otherwise falls back to template synthesis.
        """
        if not evidence:
            return f"No relevant memories found for: {query}"

        # Try Ollama-based synthesis
        synthesis = self._ollama_synthesize(query, evidence)
        if synthesis:
            return synthesis

        # Fallback: template-based synthesis
        return self._template_synthesize(query, evidence)

    def _ollama_synthesize(
        self, query: str, evidence: list[dict[str, Any]],
    ) -> str | None:
        """Attempt LLM-based synthesis via Ollama."""
        try:
            from whitemagic.tools.handlers.ollama import handle_ollama_generate as _ollama_generate
            evidence_text = "\n".join(
                f"- [{e.get('title', 'untitled')}]: {e.get('content', '')[:200]}"
                for e in evidence[:10]
            )
            prompt = (
                f"Based on the following memory evidence, synthesize a concise answer "
                f"to the question: {query}\n\nEvidence:\n{evidence_text}\n\n"
                f"Synthesize in 2-3 sentences:"
            )
            result = _ollama_generate(prompt=prompt, model=None)
            if isinstance(result, dict) and result.get("response"):
                return str(result["response"]).strip()
        except Exception:
            pass
        return None

    def _template_synthesize(
        self, query: str, evidence: list[dict[str, Any]],
    ) -> str:
        """Template-based synthesis when LLM is unavailable."""
        # Sort by importance
        sorted_ev = sorted(
            evidence,
            key=lambda e: e.get("importance", 0) or 0,
            reverse=True,
        )

        lines = [f"Research findings for: {query}"]
        lines.append(f"Found {len(evidence)} relevant memories across search.")
        lines.append("")

        for i, ev in enumerate(sorted_ev[:5], 1):
            title = ev.get("title", "untitled")
            content = ev.get("content", "")[:150]
            source = ev.get("source", "unknown")
            lines.append(f"{i}. [{title}] ({source}): {content}")

        if len(evidence) > 5:
            lines.append(f"\n... and {len(evidence) - 5} more relevant memories.")

        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        """Get researcher statistics."""
        with self._lock:
            return {
                "total_sessions": self._total_sessions,
                "total_evidence_found": self._total_evidence_found,
                "max_rounds": self._max_rounds,
                "evidence_per_question": self._evidence_per_question,
                "saturation_threshold": self._saturation_threshold,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_researcher: JITResearcher | None = None
_researcher_lock = threading.Lock()


def get_researcher(**kwargs: Any) -> JITResearcher:
    """Get the global JIT Researcher singleton."""
    global _researcher
    if _researcher is None:
        with _researcher_lock:
            if _researcher is None:
                _researcher = JITResearcher(**kwargs)
    return _researcher
