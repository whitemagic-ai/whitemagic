"""🔗 Auto Linker - Automatic Memory Connection Discovery
Created: December 2, 2025 (Hanuman Tuesday).
"""

import logging
import re
from dataclasses import dataclass
from datetime import datetime

from whitemagic.core.memory.unified_types import LinkType, Memory, MemoryLink

logger = logging.getLogger(__name__)


@dataclass
class SimilarityResult:
    """Result of similarity comparison."""

    memory: Memory
    similarity: float
    match_type: str


class AutoLinker:
    """Automatically discovers and creates links between memories."""

    def __init__(self, min_similarity: float = 0.3, max_links: int = 10):
        self.min_similarity = min_similarity
        self.max_links = max_links
        self._memory_store: dict[str, Memory] = {}
        self._word_index: dict[str, set[str]] = {}

    def register_memory(self, memory: Memory) -> None:
        """Register a memory for linking."""
        self._memory_store[memory.id] = memory
        content_str = str(memory.content)
        title_str = memory.title or ""
        for word in self._tokenize(content_str + " " + title_str):
            if word not in self._word_index:
                self._word_index[word] = set()
            self._word_index[word].add(memory.id)

    def _tokenize(self, text: str) -> list[str]:
        """Simple tokenization."""
        text = re.sub(r"[^\w\s]", " ", text.lower())
        stopwords = {"the", "a", "an", "is", "are", "was", "to", "of", "in", "for", "on", "with", "and", "or", "it", "this", "that"}
        return [w for w in text.split() if len(w) > 2 and w not in stopwords]

    def calculate_similarity(self, mem1: Memory, mem2: Memory) -> float:
        """Calculate combined similarity score."""
        # Tag similarity (Jaccard)
        tag_sim = 0.0
        if mem1.tags and mem2.tags:
            s1, s2 = set(mem1.tags), set(mem2.tags)
            tag_sim = len(s1 & s2) / len(s1 | s2) if s1 | s2 else 0

        # Content similarity (word overlap)
        w1 = set(self._tokenize(str(mem1.content) + " " + (mem1.title or "")))
        w2 = set(self._tokenize(str(mem2.content) + " " + (mem2.title or "")))
        content_sim = len(w1 & w2) / len(w1 | w2) if w1 | w2 else 0

        return tag_sim * 0.3 + content_sim * 0.7

    def find_similar(self, memory: Memory) -> list[SimilarityResult]:
        """Find memories similar to the given memory."""
        results = []
        for other_id, other in self._memory_store.items():
            if other_id == memory.id:
                continue
            sim = self.calculate_similarity(memory, other)
            if sim >= self.min_similarity:
                results.append(SimilarityResult(other, sim, "content"))
        results.sort(key=lambda r: r.similarity, reverse=True)
        return results[:self.max_links]

    def auto_link_all(self) -> int:
        """Auto-link ALL memories using parallel Rust implementation.
        Returns number of links created.
        """
        try:
            import whitemagic_rs
            if hasattr(whitemagic_rs, "build_memory_graph"):
                # Prepare data for Rust
                memories_data = []
                for m in self._memory_store.values():
                    content_str = str(m.content)
                    title_str = m.title or ""
                    memories_data.append((m.id, content_str + " " + title_str, list(m.tags)))

                logger.info(f"🔗 AutoLinker: Sending {len(memories_data)} memories to Rust graph builder...")

                # Build graph in Rust
                connections = whitemagic_rs.build_memory_graph(
                    memories_data,
                    self.min_similarity,
                    self.max_links,
                )

                links_count = 0
                for source_id, targets in connections.items():
                    if not targets:
                        continue

                    source_mem = self._memory_store.get(source_id)
                    if not source_mem:
                        continue

                    for target_id, similarity in targets:
                        # Check if link exists
                        if target_id not in source_mem.links:
                            source_mem.add_link(target_id, LinkType.RELATED, similarity)

                            # Add reverse link
                            target_mem = self._memory_store.get(target_id)
                            if target_mem:
                                target_mem.add_link(source_id, LinkType.RELATED, similarity)

                            links_count += 1

                return links_count

        except ImportError:
            pass
        except Exception as e:
            logger.info(f"⚠️ Rust graph build failed: {e}")

        # Fallback to Python (iterative)
        count = 0
        for memory in self._memory_store.values():
            links = self.auto_link(memory)
            count += len(links)
        return count

    def auto_link(self, memory: Memory) -> list[MemoryLink]:
        """Create links from memory to similar memories."""
        similar = self.find_similar(memory)
        links = []
        for result in similar:
            link = memory.add_link(result.memory.id, LinkType.RELATED, result.similarity)
            result.memory.add_link(memory.id, LinkType.RELATED, result.similarity)
            links.append(link)
        return links


_linker: AutoLinker | None = None

def get_linker() -> AutoLinker:
    global _linker
    if _linker is None:
        _linker = AutoLinker()
    return _linker

def find_similar_memories(memory: Memory) -> list[SimilarityResult]:
    return get_linker().find_similar(memory)

def create_memory_with_links(content: str, title: str = "", tags: list[str] | None = None) -> Memory:
    """Create a memory and auto-link it."""
    import hashlib

    from whitemagic.core.memory.neural.identity_anchors import auto_protect_memory
    from whitemagic.core.memory.unified_types import MemoryType

    content_str = str(content)[:1000]
    timestamp = datetime.now().isoformat()
    memory_id = hashlib.sha256(f"{content_str}{timestamp}".encode()).hexdigest()[:16]

    memory = Memory(id=memory_id, content=content, title=title, tags=set(tags) if tags else set(), memory_type=MemoryType.SHORT_TERM)
    memory = auto_protect_memory(memory)

    linker = get_linker()
    linker.register_memory(memory)
    linker.auto_link(memory)

    return memory
