"""Context Window Optimizer
========================
Intelligently packs context for LLM calls by scoring memories/items
using the Salience Arbiter, then fitting the highest-value items into
a token budget. Prevents the "lost in the middle" problem by placing
the most salient items at the start and end of the context window.

Usage:
    from whitemagic.ai.context_optimizer import get_context_optimizer
    opt = get_context_optimizer()
    packed = opt.pack(items=memories, token_budget=4000)
"""

from __future__ import annotations

import logging
import threading
from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class ContextItem:
    """A single item to consider for context packing."""

    id: str
    content: str
    source: str = ""          # e.g. "memory", "session", "tool_result"
    importance: float = 0.5   # 0.0 - 1.0
    recency: float = 0.5     # 0.0 (old) - 1.0 (fresh)
    relevance: float = 0.5   # 0.0 - 1.0 (query-specific)
    tokens: int = 0           # estimated token count (auto-calculated if 0)

    def salience_score(self) -> float:
        """Combined salience: weighted blend of importance, recency, relevance."""
        return (self.importance * 0.3) + (self.recency * 0.2) + (self.relevance * 0.5)


@dataclass
class PackedContext:
    """Result of context packing."""

    items: list[ContextItem]
    total_tokens: int
    budget: int
    utilization: float      # 0.0 - 1.0
    dropped_count: int
    strategy: str


def estimate_tokens(text: str) -> int:
    """Rough token estimate: ~4 chars per token for English."""
    return max(1, len(text) // 4)


class ContextOptimizer:
    """Packs context items into a token budget using salience scoring.

    Strategy:
      1. Score all items by salience (importance × recency × relevance)
      2. Sort by salience descending
      3. Greedily fit into budget
      4. Reorder: highest-salience at START and END (primacy/recency effect)
    """

    def __init__(self, default_budget: int = 8000):
        self._default_budget = default_budget

    def pack(
        self,
        items: Sequence[ContextItem],
        token_budget: int | None = None,
        query: str | None = None,
    ) -> PackedContext:
        """Pack items into the token budget."""
        budget = token_budget or self._default_budget

        # Auto-estimate tokens if needed
        scored = []
        for item in items:
            if item.tokens == 0:
                item.tokens = estimate_tokens(item.content)
            scored.append(item)

        # Score relevance against query if provided
        if query:
            query_lower = query.lower()
            query_words = set(query_lower.split())
            for item in scored:
                content_lower = item.content.lower()
                # Simple keyword overlap relevance
                content_words = set(content_lower.split())
                overlap = len(query_words & content_words)
                item.relevance = min(1.0, overlap / max(len(query_words), 1))

        # Sort by salience descending
        scored.sort(key=lambda x: x.salience_score(), reverse=True)

        # Greedy packing
        selected = []
        total_tokens = 0
        dropped = 0

        for item in scored:
            if total_tokens + item.tokens <= budget:
                selected.append(item)
                total_tokens += item.tokens
            else:
                dropped += 1

        # Primacy-recency reorder: best at start and end
        if len(selected) >= 4:
            selected = self._primacy_recency_reorder(selected)

        utilization = total_tokens / budget if budget > 0 else 0.0

        return PackedContext(
            items=selected,
            total_tokens=total_tokens,
            budget=budget,
            utilization=round(utilization, 3),
            dropped_count=dropped,
            strategy="salience_primacy_recency",
        )

    def pack_memories(
        self,
        query: str,
        token_budget: int | None = None,
        limit: int = 50,
    ) -> PackedContext:
        """Pack memories from the unified memory system."""
        items = []
        try:
            import time

            from whitemagic.core.memory.unified import get_unified_memory

            um = get_unified_memory()
            results = um.search(query, limit=limit)

            for mem in results:
                created = getattr(mem, "created_at", None)
                recency = 0.5
                if created:
                    age_days = (time.time() - created.timestamp()) / 86400
                    recency = max(0.0, 1.0 - (age_days / 365))

                items.append(ContextItem(
                    id=mem.id,
                    content=f"[{mem.title or 'untitled'}] {mem.content[:2000]}",
                    source="memory",
                    importance=getattr(mem, "importance", 0.5) or 0.5,
                    recency=recency,
                    relevance=0.5,
                ))
        except Exception as e:
            logger.debug("Memory pack failed: %s", e)

        return self.pack(items, token_budget=token_budget, query=query)

    @staticmethod
    def _primacy_recency_reorder(items: list[ContextItem]) -> list[ContextItem]:
        """Place highest-salience items at start and end of the list.
        This exploits the primacy and recency effects in LLM attention.
        """
        if len(items) <= 2:
            return items

        # Re-sort by salience
        by_salience = sorted(items, key=lambda x: x.salience_score(), reverse=True)

        # Top 25% go to front, next 25% go to back, rest in middle
        quarter = max(1, len(by_salience) // 4)
        front = by_salience[:quarter]
        back = by_salience[quarter:quarter * 2]
        middle = by_salience[quarter * 2:]

        return front + middle + list(reversed(back))

    def render(self, packed: PackedContext, separator: str = "\n\n---\n\n") -> str:
        """Render packed context into a single string."""
        return separator.join(item.content for item in packed.items)

    def status(self) -> dict[str, Any]:
        return {
            "default_budget": self._default_budget,
            "strategy": "salience_primacy_recency",
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_optimizer: ContextOptimizer | None = None
_opt_lock = threading.Lock()


def get_context_optimizer() -> ContextOptimizer:
    global _optimizer
    if _optimizer is None:
        with _opt_lock:
            if _optimizer is None:
                _optimizer = ContextOptimizer()
    return _optimizer
