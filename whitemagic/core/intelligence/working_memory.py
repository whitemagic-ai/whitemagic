"""Working Memory Capacity Model — Bounded attentional bottleneck.

Source: LIVING_MEMORY_GAP_ANALYSIS.md (GAP 8)

Models the cognitive science concept of working memory: a bounded set of
4-7 active "chunks" that represent the agent's current focus. Uses LRU
eviction to manage the bottleneck, with importance-weighted retention.

This provides context windowing for agent reasoning — instead of dumping
all retrieved memories into a prompt, the working memory filters to only
the most relevant active chunks, mimicking human attentional capacity.

Key concepts:
- Capacity: Default 7 chunks (Miller's Law: 7±2)
- Chunks: Memory references with activation scores
- Decay: Unused chunks lose activation over time
- Rehearsal: Accessing a chunk refreshes its activation
- Chunking: Related memories can be grouped into a single slot
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# Default capacity follows Miller's Law (7±2)
DEFAULT_CAPACITY = 7
MIN_CAPACITY = 3
MAX_CAPACITY = 12

# Activation decay per second of non-use
DECAY_RATE = 0.01

# Minimum activation before eviction
EVICTION_THRESHOLD = 0.1


@dataclass
class WorkingChunk:
    """A single item in working memory."""

    memory_id: str
    content: str
    title: str = ""
    activation: float = 1.0
    importance: float = 0.5
    last_accessed: float = field(default_factory=time.time)
    access_count: int = 1
    tags: list[str] = field(default_factory=list)
    grouped_ids: list[str] = field(default_factory=list)

    @property
    def effective_activation(self) -> float:
        """Activation adjusted for time decay and importance."""
        elapsed = time.time() - self.last_accessed
        decayed = self.activation * max(0.0, 1.0 - DECAY_RATE * elapsed)
        # Importance provides a floor — important items resist decay
        return max(decayed, self.importance * 0.3)

    def rehearse(self) -> None:
        """Refresh activation (simulates rehearsal in working memory)."""
        self.activation = min(1.0, self.activation + 0.2)
        self.last_accessed = time.time()
        self.access_count += 1

    def to_dict(self) -> dict[str, Any]:
        return {
            "memory_id": self.memory_id,
            "title": self.title,
            "content_preview": self.content[:100] + "..." if len(self.content) > 100 else self.content,
            "activation": round(self.effective_activation, 3),
            "importance": self.importance,
            "access_count": self.access_count,
            "grouped_ids": self.grouped_ids,
            "tags": self.tags,
        }


class WorkingMemory:
    """Bounded working memory with LRU eviction and importance weighting.

    Usage:
        wm = WorkingMemory(capacity=7)
        wm.attend(memory_id="abc", content="...", importance=0.8)
        wm.attend(memory_id="def", content="...", importance=0.5)
        context = wm.get_context()  # Returns top chunks for prompt injection
    """

    def __init__(self, capacity: int = DEFAULT_CAPACITY):
        self.capacity = max(MIN_CAPACITY, min(MAX_CAPACITY, capacity))
        self._chunks: dict[str, WorkingChunk] = {}
        self._eviction_count = 0
        self._total_attended = 0

    def attend(
        self,
        memory_id: str,
        content: str,
        title: str = "",
        importance: float = 0.5,
        tags: list[str] | None = None,
    ) -> WorkingChunk:
        """Bring a memory into working memory focus.

        If already present, rehearses it. If at capacity, evicts
        the lowest-activation chunk.

        Args:
            memory_id: Unique identifier for the memory
            content: The memory content
            title: Optional title
            importance: 0.0-1.0 importance weight
            tags: Optional tags

        Returns:
            The working chunk (new or refreshed)

        """
        self._total_attended += 1

        # Already in working memory — rehearse it
        if memory_id in self._chunks:
            chunk = self._chunks[memory_id]
            chunk.rehearse()
            logger.debug(f"Rehearsed chunk {memory_id}: activation={chunk.effective_activation:.3f}")
            return chunk

        # Apply decay and evict dead chunks first
        self._apply_decay()

        # At capacity — evict lowest activation
        while len(self._chunks) >= self.capacity:
            self._evict_lowest()

        # Create new chunk
        chunk = WorkingChunk(
            memory_id=memory_id,
            content=content,
            title=title,
            activation=1.0,
            importance=importance,
            tags=tags or [],
        )
        self._chunks[memory_id] = chunk
        logger.debug(f"Attended to {memory_id}: {len(self._chunks)}/{self.capacity} slots used")
        return chunk

    def group(self, chunk_ids: list[str], group_id: str, group_title: str = "") -> WorkingChunk | None:
        """Group multiple chunks into a single slot (chunking).

        This frees up working memory capacity by combining related
        memories into one chunk, mirroring human chunking behavior.

        Args:
            chunk_ids: IDs of chunks to group
            group_id: New ID for the grouped chunk
            group_title: Title for the group

        Returns:
            The new grouped chunk, or None if no valid chunks found

        """
        valid_chunks = [self._chunks[cid] for cid in chunk_ids if cid in self._chunks]
        if not valid_chunks:
            return None

        # Combine content
        combined_content = "\n---\n".join(c.content for c in valid_chunks)
        max_importance = max(c.importance for c in valid_chunks)
        all_tags = list({t for c in valid_chunks for t in c.tags})
        all_ids = [c.memory_id for c in valid_chunks]

        # Remove individual chunks
        for cid in chunk_ids:
            self._chunks.pop(cid, None)

        # Create grouped chunk
        chunk = WorkingChunk(
            memory_id=group_id,
            content=combined_content,
            title=group_title or f"Group of {len(valid_chunks)} memories",
            activation=1.0,
            importance=max_importance,
            tags=all_tags,
            grouped_ids=all_ids,
        )
        self._chunks[group_id] = chunk
        logger.info(f"Grouped {len(valid_chunks)} chunks into {group_id}: freed {len(valid_chunks) - 1} slots")
        return chunk

    def forget(self, memory_id: str) -> bool:
        """Explicitly remove a chunk from working memory."""
        if memory_id in self._chunks:
            del self._chunks[memory_id]
            return True
        return False

    def get_context(self, max_tokens: int | None = None) -> list[dict[str, Any]]:
        """Get current working memory contents, sorted by activation.

        This is the primary method for injecting working memory into
        agent prompts — returns the most active chunks.

        Args:
            max_tokens: Optional approximate token budget (chars / 4)

        Returns:
            List of chunk dicts sorted by activation (highest first)

        """
        self._apply_decay()
        chunks = sorted(self._chunks.values(), key=lambda c: c.effective_activation, reverse=True)

        result = []
        token_count = 0
        for chunk in chunks:
            chunk_dict = chunk.to_dict()
            if max_tokens is not None:
                est_tokens = len(chunk.content) // 4
                if token_count + est_tokens > max_tokens:
                    break
                token_count += est_tokens
            result.append(chunk_dict)

        return result

    def get_active_ids(self) -> list[str]:
        """Get IDs of all chunks currently in working memory."""
        return list(self._chunks.keys())

    def clear(self) -> None:
        """Clear all working memory."""
        self._chunks.clear()

    def _apply_decay(self) -> None:
        """Apply time-based decay and remove dead chunks."""
        to_remove = []
        for memory_id, chunk in self._chunks.items():
            if chunk.effective_activation < EVICTION_THRESHOLD:
                to_remove.append(memory_id)

        for memory_id in to_remove:
            del self._chunks[memory_id]
            self._eviction_count += 1

    def _evict_lowest(self) -> None:
        """Evict the chunk with the lowest effective activation."""
        if not self._chunks:
            return

        lowest_id = min(self._chunks, key=lambda k: self._chunks[k].effective_activation)
        evicted = self._chunks.pop(lowest_id)
        self._eviction_count += 1
        logger.debug(f"Evicted {lowest_id} (activation={evicted.effective_activation:.3f})")

    def get_status(self) -> dict[str, Any]:
        """Get working memory status."""
        self._apply_decay()
        return {
            "capacity": self.capacity,
            "used": len(self._chunks),
            "available": self.capacity - len(self._chunks),
            "total_attended": self._total_attended,
            "total_evicted": self._eviction_count,
            "chunks": [c.to_dict() for c in sorted(
                self._chunks.values(),
                key=lambda c: c.effective_activation,
                reverse=True,
            )],
        }


# Global singleton
_working_memory: WorkingMemory | None = None


def get_working_memory(capacity: int = DEFAULT_CAPACITY) -> WorkingMemory:
    """Get the global working memory instance."""
    global _working_memory
    if _working_memory is None:
        _working_memory = WorkingMemory(capacity=capacity)
    return _working_memory
