"""Parallel Thought Threading - Shadow Clone Jutsu for LLM Inference
Version: 4.0.0

Explores multiple response paths in parallel, then uses consensus
to pick the best. Combines lookahead decoding concepts with
WhiteMagic's Shadow Clone architecture.

This handles the 20% of queries that don't hit edge rules.
"""

import hashlib
import time
from collections.abc import Callable
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import Any

from whitemagic.config.concurrency import IO_WORKERS


@dataclass
class ThoughtPath:
    """A single explored thought path."""

    content: str
    strategy: str
    confidence: float
    tokens_used: int
    latency_ms: float


@dataclass
class ConsensusResult:
    """Result after thought clone deliberation."""

    answer: str
    consensus_score: float
    paths_explored: int
    winning_strategy: str
    latency_ms: float


class ThoughtClone:
    """A single thought exploration clone.

    Each clone uses a different "strategy" (temperature, style)
    to explore the response space differently.
    """

    STRATEGIES = {
        "factual": {"temperature": 0.1, "style": "precise and factual"},
        "balanced": {"temperature": 0.5, "style": "balanced and helpful"},
        "creative": {"temperature": 0.8, "style": "creative and exploratory"},
        "cautious": {"temperature": 0.3, "style": "careful and thorough"},
    }

    def __init__(self, strategy: str = "balanced", generator: Callable | None = None):
        self.strategy = strategy
        self.config = self.STRATEGIES.get(strategy, self.STRATEGIES["balanced"])
        self.generator = generator  # External LLM generator function
        self._cache: dict[str, ThoughtPath] = {}

    def explore(self, prompt: str, max_tokens: int = 50) -> ThoughtPath:
        """Explore a response path for the given prompt.

        If no external generator is provided, uses pattern-based generation.
        """
        start = time.time()
        cache_key = hashlib.md5(f"{prompt}:{self.strategy}".encode()).hexdigest()[:16]

        # Check cache
        if cache_key in self._cache:
            cached = self._cache[cache_key]
            cached.latency_ms = 0.01  # Cache hit
            return cached

        # Generate response based on strategy
        if self.generator:
            # Use external LLM with strategy-specific temperature
            content = self.generator(
                prompt,
                max_tokens=max_tokens,
                temperature=self.config["temperature"],
            )
        else:
            # Fallback: pattern-based response
            content = self._pattern_generate(prompt)

        path = ThoughtPath(
            content=content,
            strategy=self.strategy,
            confidence=self._estimate_confidence(content),
            tokens_used=len(content.split()),
            latency_ms=(time.time() - start) * 1000,
        )

        self._cache[cache_key] = path
        return path

    def _pattern_generate(self, prompt: str) -> str:
        """Simple pattern-based generation as fallback."""
        prompt_lower = prompt.lower()

        # Strategy-specific responses
        if self.strategy == "factual":
            if "what is" in prompt_lower:
                return "Based on available information, the answer requires specific knowledge."
            return "I would need to verify this information before providing a definitive answer."

        if self.strategy == "creative":
            return "That's an interesting question! There are multiple ways to approach this..."

        if self.strategy == "cautious":
            return "Let me carefully consider the implications before answering..."

        # balanced
        return "I can help with that. Here's what I understand..."

    def _estimate_confidence(self, content: str) -> float:
        """Estimate confidence based on response characteristics."""
        # Longer, more specific responses tend to be more confident
        words = len(content.split())

        if words < 5:
            return 0.3
        if words < 15:
            return 0.6
        if words < 50:
            return 0.8
        return 0.9


class ThoughtArmy:
    """Deploy multiple thought clones for parallel response exploration.

    Like the Shadow Clone Jutsu, but for thinking!
    Each clone explores a different path, then consensus picks the best.
    """

    def __init__(
        self,
        num_clones: int = 4,
        max_workers: int | None = None,
        generator: Callable | None = None,
    ):
        self.max_workers = min(max_workers or IO_WORKERS, num_clones)
        self.generator = generator

        # Deploy clones with different strategies
        strategies = ["factual", "balanced", "creative", "cautious"]
        self.clones = [
            ThoughtClone(
                strategy=strategies[i % len(strategies)],
                generator=generator,
            )
            for i in range(num_clones)
        ]

        self._stats = {
            "explorations": 0,
            "consensus_reached": 0,
            "total_paths": 0,
        }

    def parallel_explore(
        self,
        prompt: str,
        max_tokens: int = 50,
        timeout: float = 30.0,
    ) -> ConsensusResult:
        """All clones explore in parallel, consensus picks the best response.

        This is the Shadow Clone Jutsu for thought!
        """
        start = time.time()
        self._stats["explorations"] += 1

        paths: list[ThoughtPath] = []

        # Parallel exploration using ThreadPoolExecutor
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            futures = {
                executor.submit(clone.explore, prompt, max_tokens): clone
                for clone in self.clones
            }

            for future in as_completed(futures, timeout=timeout):
                try:
                    path = future.result(timeout=timeout)
                    paths.append(path)
                except Exception:
                    # Clone failed, continue with others
                    pass

        self._stats["total_paths"] += len(paths)

        if not paths:
            return ConsensusResult(
                answer="Unable to generate response.",
                consensus_score=0.0,
                paths_explored=0,
                winning_strategy="none",
                latency_ms=(time.time() - start) * 1000,
            )

        # Consensus: pick response with highest combined score
        winner = self._consensus_vote(paths)
        self._stats["consensus_reached"] += 1

        return ConsensusResult(
            answer=winner.content,
            consensus_score=winner.confidence,
            paths_explored=len(paths),
            winning_strategy=winner.strategy,
            latency_ms=(time.time() - start) * 1000,
        )

    def _consensus_vote(self, paths: list[ThoughtPath]) -> ThoughtPath:
        """Vote on the best response path.

        Scoring:
        - Higher confidence = better
        - Balanced strategy gets slight boost (more reliable)
        - Faster responses get slight boost
        """
        def score(path: ThoughtPath) -> float:
            base = path.confidence

            # Strategy bonus
            if path.strategy == "balanced":
                base += 0.1
            elif path.strategy == "factual":
                base += 0.05

            # Speed bonus (normalized)
            if path.latency_ms < 100:
                base += 0.05

            return base

        return max(paths, key=score)

    @property
    def stats(self) -> dict[str, Any]:
        return {
            **self._stats,
            "clones": len(self.clones),
            "strategies": [c.strategy for c in self.clones],
        }


# Singleton
_thought_army: ThoughtArmy | None = None

def get_thought_army(generator: Callable | None = None) -> ThoughtArmy:
    """Get singleton thought army."""
    global _thought_army
    if _thought_army is None:
        _thought_army = ThoughtArmy(generator=generator)
    return _thought_army


def parallel_think(prompt: str, max_tokens: int = 50) -> ConsensusResult:
    """Quick function for parallel thought exploration."""
    return get_thought_army().parallel_explore(prompt, max_tokens)
