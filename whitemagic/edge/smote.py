"""Speculative Mixture of Thought Experts (SMoTE)
Version: 4.0.0

A novel architecture combining:
- Speculative decoding (parallel draft/verify)
- Mixture of experts (specialized thought paths)
- Early exit (skip computation for easy cases)
- Ensemble consensus (multiple perspectives)

This is the unified inference engine for WhiteMagic 4.0.
"""

import time
from collections.abc import Callable
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import Any

from .inference import get_edge_inference
from .thought_clones import ThoughtClone, ThoughtPath


@dataclass
class SMoTEResult:
    """Result from SMoTE inference."""

    query: str
    answer: str
    path: str  # 'early_exit', 'expert_consensus', 'self_speculative'
    confidence: float
    latency_ms: float
    experts_consulted: int
    tokens_saved: int
    speculation_accepted: bool = True


class ExpertRouter:
    """MoE-style router that decides which experts to consult.

    Routes based on query characteristics:
    - Factual queries → factual expert
    - Creative queries → creative expert
    - Technical queries → all experts
    """

    QUERY_PATTERNS = {
        "factual": ["what is", "who is", "when", "where", "how many", "define"],
        "creative": ["write", "create", "imagine", "story", "poem", "design"],
        "technical": ["code", "function", "error", "debug", "implement", "algorithm"],
        "philosophical": ["why", "meaning", "purpose", "should", "ethics", "moral"],
    }

    def route(self, query: str) -> list[str]:
        """Determine which experts to consult."""
        query_lower = query.lower()

        matched = []
        for expert_type, patterns in self.QUERY_PATTERNS.items():
            if any(p in query_lower for p in patterns):
                matched.append(expert_type)

        # Default: consult balanced and one other
        if not matched:
            return ["balanced", "factual"]

        return matched[:3]  # Max 3 experts


class SelfSpeculativeLayer:
    """Implements self-speculative decoding concept.

    For queries that need full LLM:
    1. Draft: Quick generation with lower quality settings
    2. Verify: Check against edge rules and patterns
    3. Accept or regenerate
    """

    def __init__(self, llm_generator: Callable | None = None):
        self.generator = llm_generator
        self.edge = get_edge_inference()
        self._acceptance_rate = 0.0
        self._total_speculations = 0

    def speculate(self, query: str, max_tokens: int = 50) -> tuple[str, bool]:
        """Generate with speculation.

        Returns: (response, was_accepted)
        """
        self._total_speculations += 1

        if not self.generator:
            # Fallback: use edge rules as "draft"
            edge_result = self.edge.infer(query)
            if edge_result.confidence > 0.5:
                return edge_result.answer, True
            return "I need more context to answer that accurately.", False

        # Draft generation (quick, lower quality)
        draft = self.generator(query, max_tokens=max_tokens // 2)

        # Verify: Check if draft makes sense
        # (In full implementation, would use remaining layers)
        verified = self._verify_draft(query, draft)

        if verified:
            self._acceptance_rate = (
                self._acceptance_rate * (self._total_speculations - 1) + 1
            ) / self._total_speculations
            return draft, True

        # Regenerate with full quality
        full_response = self.generator(query, max_tokens=max_tokens)
        return full_response, False

    def _verify_draft(self, query: str, draft: str) -> bool:
        """Verify if draft response is acceptable."""
        # Simple heuristics for now
        if len(draft.strip()) < 5:
            return False
        if draft.count("?") > 3:  # Too uncertain
            return False
        if "error" in draft.lower() and "error" not in query.lower():
            return False
        return True


class SMoTE:
    """Speculative Mixture of Thought Experts.

    The unified inference engine combining all research discoveries.
    """

    def __init__(
        self,
        num_experts: int = 4,
        early_exit_threshold: float = 0.8,
        consensus_threshold: float = 0.6,
        llm_generator: Callable | None = None,
    ):
        # Early exit layer (edge rules)
        self.edge = get_edge_inference()
        self.early_exit_threshold = early_exit_threshold

        # Expert layer (thought clones)
        self.experts = {
            "factual": ThoughtClone("factual"),
            "balanced": ThoughtClone("balanced"),
            "creative": ThoughtClone("creative"),
            "cautious": ThoughtClone("cautious"),
        }
        self.router = ExpertRouter()
        self.consensus_threshold = consensus_threshold

        # Self-speculative layer
        self.speculator = SelfSpeculativeLayer(llm_generator)

        # Stats
        self._stats = {
            "total": 0,
            "early_exits": 0,
            "expert_consensus": 0,
            "speculative": 0,
            "tokens_saved": 0,
        }

    def infer(self, query: str, max_tokens: int = 100) -> SMoTEResult:
        """Process query through the SMoTE pipeline.

        1. Early Exit: Check edge rules
        2. Expert Consultation: Parallel thought experts
        3. Self-Speculation: Full LLM with draft/verify
        """
        start = time.time()
        self._stats["total"] += 1

        # === Layer 1: Early Exit (Edge Rules) ===
        edge_result = self.edge.infer(query)

        if edge_result.confidence >= self.early_exit_threshold:
            self._stats["early_exits"] += 1
            self._stats["tokens_saved"] += 100

            return SMoTEResult(
                query=query,
                answer=edge_result.answer,
                path="early_exit",
                confidence=edge_result.confidence,
                latency_ms=(time.time() - start) * 1000,
                experts_consulted=0,
                tokens_saved=100,
            )

        # === Layer 2: Expert Consultation (Parallel) ===
        expert_types = self.router.route(query)

        paths: list[ThoughtPath] = []
        with ThreadPoolExecutor(max_workers=len(expert_types)) as executor:
            futures = {
                executor.submit(self.experts[et].explore, query): et
                for et in expert_types if et in self.experts
            }
            for future in as_completed(futures):
                try:
                    paths.append(future.result())
                except Exception:
                    pass

        if paths:
            # Consensus vote
            best_path = max(paths, key=lambda p: p.confidence)

            if best_path.confidence >= self.consensus_threshold:
                self._stats["expert_consensus"] += 1
                self._stats["tokens_saved"] += 50

                return SMoTEResult(
                    query=query,
                    answer=best_path.content,
                    path="expert_consensus",
                    confidence=best_path.confidence,
                    latency_ms=(time.time() - start) * 1000,
                    experts_consulted=len(paths),
                    tokens_saved=50,
                )

        # === Layer 3: Self-Speculative LLM ===
        self._stats["speculative"] += 1

        answer, accepted = self.speculator.speculate(query, max_tokens)

        return SMoTEResult(
            query=query,
            answer=answer,
            path="self_speculative",
            confidence=0.7 if accepted else 0.5,
            latency_ms=(time.time() - start) * 1000,
            experts_consulted=len(paths),
            tokens_saved=0,
            speculation_accepted=accepted,
        )

    def get_stats(self) -> dict[str, Any]:
        """Get inference statistics."""
        total = self._stats["total"]
        if total == 0:
            return self._stats

        return {
            **self._stats,
            "early_exit_rate": f"{self._stats['early_exits']/total*100:.1f}%",
            "expert_rate": f"{self._stats['expert_consensus']/total*100:.1f}%",
            "speculative_rate": f"{self._stats['speculative']/total*100:.1f}%",
        }


# Singleton
_smote: SMoTE | None = None

def get_smote() -> SMoTE:
    """Get singleton SMoTE engine."""
    global _smote
    if _smote is None:
        _smote = SMoTE()
    return _smote
