"""Memory Reconsolidation — Labile state updates on retrieval.

Source: LIVING_MEMORY_GAP_ANALYSIS.md (Neuroscience Mapping)

When a memory is retrieved, it enters a brief "labile state" where it can
be updated with new context before being re-stored. This mirrors the
neuroscience finding that recalled memories are temporarily destabilized
and can be modified during reconsolidation.

In WhiteMagic, this means:
1. When a memory is recalled during search/hybrid_recall, it enters labile state
2. If the agent provides new context within the reconsolidation window, the
   memory is updated (content enriched, tags merged, associations added)
3. After the window closes, the memory is re-stabilized with updated metadata

This enables memories to evolve with each retrieval rather than being
static snapshots frozen at creation time.
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# How long (seconds) a memory stays labile after retrieval
DEFAULT_LABILE_WINDOW = 300  # 5 minutes

# Maximum number of memories that can be labile simultaneously
MAX_LABILE = 20


@dataclass
class LabileMemory:
    """A memory in labile (modifiable) state after retrieval."""

    memory_id: str
    original_content: str
    original_tags: list[str]
    retrieved_at: float = field(default_factory=time.time)
    query_context: str = ""
    updates: list[dict[str, Any]] = field(default_factory=list)
    reconsolidated: bool = False

    @property
    def is_expired(self) -> bool:
        """Check if the labile window has closed."""
        return (time.time() - self.retrieved_at) > DEFAULT_LABILE_WINDOW

    @property
    def age_seconds(self) -> float:
        return time.time() - self.retrieved_at


class ReconsolidationEngine:
    """Manages memory reconsolidation — updating memories on retrieval.

    Usage:
        engine = ReconsolidationEngine()

        # When a memory is retrieved:
        engine.mark_labile("mem_123", content="...", tags=[...], query="search query")

        # When new context arrives:
        engine.update_labile("mem_123", new_context="additional info", new_tags=["updated"])

        # Periodically or at session end:
        changes = engine.reconsolidate_all(memory_store)
    """

    def __init__(self, labile_window: float = DEFAULT_LABILE_WINDOW):
        self.labile_window = labile_window
        self._labile: dict[str, LabileMemory] = {}
        self._stats = {
            "total_marked": 0,
            "total_updated": 0,
            "total_reconsolidated": 0,
            "total_expired": 0,
        }

    def mark_labile(
        self,
        memory_id: str,
        content: str,
        tags: list[str],
        query: str = "",
    ) -> LabileMemory:
        """Mark a memory as labile (modifiable) after retrieval.

        Args:
            memory_id: The memory's unique ID
            content: Current content of the memory
            tags: Current tags
            query: The query that triggered retrieval (provides context)

        Returns:
            The LabileMemory entry

        """
        # Expire old entries first
        self._expire_old()

        # If already labile, just refresh the window
        if memory_id in self._labile:
            existing = self._labile[memory_id]
            existing.retrieved_at = time.time()
            existing.query_context = query or existing.query_context
            return existing

        # Enforce max labile limit
        if len(self._labile) >= MAX_LABILE:
            self._evict_oldest()

        labile = LabileMemory(
            memory_id=memory_id,
            original_content=content,
            original_tags=list(tags),
            query_context=query,
        )
        self._labile[memory_id] = labile
        self._stats["total_marked"] += 1
        logger.debug(f"Memory {memory_id} entered labile state (query: {query[:50]})")
        return labile

    def update_labile(
        self,
        memory_id: str,
        new_context: str | None = None,
        new_tags: list[str] | None = None,
        annotation: str | None = None,
    ) -> bool:
        """Update a labile memory with new context.

        Args:
            memory_id: The memory to update
            new_context: Additional context to append
            new_tags: New tags to merge
            annotation: A note about why the update happened

        Returns:
            True if the memory was updated, False if not labile or expired

        """
        labile = self._labile.get(memory_id)
        if labile is None or labile.is_expired:
            return False

        update: dict[str, Any] = {"timestamp": time.time()}
        if new_context:
            update["context"] = new_context
        if new_tags:
            update["tags"] = new_tags
        if annotation:
            update["annotation"] = annotation

        labile.updates.append(update)
        self._stats["total_updated"] += 1
        logger.debug(f"Labile memory {memory_id} updated ({len(labile.updates)} updates)")
        return True

    def is_labile(self, memory_id: str) -> bool:
        """Check if a memory is currently in labile state."""
        labile = self._labile.get(memory_id)
        if labile is None:
            return False
        if labile.is_expired:
            del self._labile[memory_id]
            return False
        return True

    def reconsolidate(self, memory_id: str, memory_store: Any = None) -> dict[str, Any] | None:
        """Reconsolidate a single labile memory — apply updates and re-stabilize.

        Args:
            memory_id: The memory to reconsolidate
            memory_store: Optional memory store to persist changes

        Returns:
            Dict with reconsolidation details, or None if not labile

        """
        labile = self._labile.get(memory_id)
        if labile is None:
            return None

        if not labile.updates:
            # No updates — just remove from labile state
            del self._labile[memory_id]
            return {"memory_id": memory_id, "action": "no_changes", "expired": labile.is_expired}

        # Build updated content
        content_additions = []
        tag_additions: set[str] = set()
        annotations = []

        for update in labile.updates:
            if "context" in update:
                content_additions.append(update["context"])
            if "tags" in update:
                tag_additions.update(update["tags"])
            if "annotation" in update:
                annotations.append(update["annotation"])

        # Construct reconsolidated content
        new_content = labile.original_content
        if content_additions:
            reconsolidation_note = "\n\n[Reconsolidated: " + "; ".join(content_additions) + "]"
            new_content += reconsolidation_note

        new_tags = list(set(labile.original_tags) | tag_additions | {"reconsolidated"})

        # Persist if store available
        if memory_store is not None:
            try:
                memory_store.update_memory(
                    memory_id,
                    content=new_content,
                    tags=new_tags,
                )
                logger.info(f"Reconsolidated memory {memory_id}: +{len(content_additions)} contexts, +{len(tag_additions)} tags")
            except Exception as e:
                logger.warning(f"Failed to persist reconsolidation for {memory_id}: {e}")

        labile.reconsolidated = True
        del self._labile[memory_id]
        self._stats["total_reconsolidated"] += 1

        return {
            "memory_id": memory_id,
            "action": "reconsolidated",
            "updates_applied": len(labile.updates),
            "content_additions": len(content_additions),
            "tag_additions": list(tag_additions),
            "annotations": annotations,
            "labile_duration_s": round(labile.age_seconds, 1),
        }

    def reconsolidate_all(self, memory_store: Any = None) -> list[dict[str, Any]]:
        """Reconsolidate all labile memories that have updates.

        Typically called at session end or periodically.

        Args:
            memory_store: Memory store to persist changes

        Returns:
            List of reconsolidation reports

        """
        results = []
        # Snapshot IDs to avoid mutation during iteration
        ids = list(self._labile.keys())
        for memory_id in ids:
            result = self.reconsolidate(memory_id, memory_store)
            if result:
                results.append(result)
        return results

    def get_labile_ids(self) -> list[str]:
        """Get IDs of all currently labile memories."""
        self._expire_old()
        return list(self._labile.keys())

    def _expire_old(self) -> None:
        """Remove expired labile entries."""
        expired = [mid for mid, lm in self._labile.items() if lm.is_expired]
        for mid in expired:
            del self._labile[mid]
            self._stats["total_expired"] += 1

    def _evict_oldest(self) -> None:
        """Evict the oldest labile memory to make room."""
        if not self._labile:
            return
        oldest_id = min(self._labile, key=lambda k: self._labile[k].retrieved_at)
        del self._labile[oldest_id]
        self._stats["total_expired"] += 1

    def get_status(self) -> dict[str, Any]:
        """Get reconsolidation engine status."""
        self._expire_old()
        return {
            "labile_count": len(self._labile),
            "labile_window_s": self.labile_window,
            "max_labile": MAX_LABILE,
            **self._stats,
            "labile_memories": [
                {
                    "memory_id": lm.memory_id,
                    "age_s": round(lm.age_seconds, 1),
                    "updates": len(lm.updates),
                    "query": lm.query_context[:50],
                }
                for lm in self._labile.values()
            ],
        }


# Global singleton
_engine: ReconsolidationEngine | None = None


def get_reconsolidation_engine() -> ReconsolidationEngine:
    """Get the global reconsolidation engine."""
    global _engine
    if _engine is None:
        _engine = ReconsolidationEngine()
    return _engine
