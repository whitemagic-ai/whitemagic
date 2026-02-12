"""Memory-Augmented Generation (MAG) for WhiteMagic 4.0
Version: 4.0.0

Injects relevant context from WhiteMagic's memory system into LLM prompts,
dramatically reducing hallucinations and improving accuracy.

Architecture:
1. Query arrives
2. Search memory for relevant context
3. Inject context into prompt
4. Send to LLM with grounded knowledge
"""

import time
from dataclasses import dataclass
from pathlib import Path

from whitemagic.config.paths import MEMORY_DIR as CONFIG_MEMORY_DIR
from whitemagic.config.paths import WM_ROOT

from .local_llm import CascadeResult, LocalLLM


@dataclass
class MemoryChunk:
    """A chunk of relevant memory."""

    content: str
    source: str
    relevance: float


class MemoryAugmentedGenerator:
    """Augments LLM generation with WhiteMagic memory context.

    This is the key to reducing hallucinations - the model
    gets grounded facts instead of making things up.
    """

    def __init__(
        self,
        llm: LocalLLM | None = None,
        memory_dir: Path | None = None,
        max_context_tokens: int = 500,
    ):
        self.llm = llm or LocalLLM()
        self.memory_dir = memory_dir or WM_ROOT
        self.max_context_tokens = max_context_tokens
        self._cache: dict[str, list[MemoryChunk]] = {}

    def search_memory(self, query: str, top_k: int = 3) -> list[MemoryChunk]:
        """Search WhiteMagic memory for relevant context."""
        results = []
        query_lower = query.lower()
        keywords = query_lower.split()

        # Search memory files (use configured paths, avoid duplicates)
        memory_paths = [self.memory_dir / "memory"]
        if self.memory_dir / "memory" != CONFIG_MEMORY_DIR:
            memory_paths.append(CONFIG_MEMORY_DIR)

        for mem_path in memory_paths:
            if not mem_path.exists():
                continue

            for md_file in mem_path.glob("**/*.md"):
                try:
                    content = md_file.read_text(errors="ignore")[:2000]

                    # Simple keyword relevance scoring
                    score = sum(1 for kw in keywords if kw in content.lower())
                    if score > 0:
                        results.append(MemoryChunk(
                            content=content[:500],
                            source=str(md_file.name),
                            relevance=score / len(keywords),
                        ))
                except OSError:
                    continue

        # Sort by relevance and return top_k
        results.sort(key=lambda x: x.relevance, reverse=True)
        return results[:top_k]

    def build_augmented_prompt(
        self,
        query: str,
        memory_chunks: list[MemoryChunk],
    ) -> str:
        """Build a prompt with injected memory context."""
        if not memory_chunks:
            return query

        context_parts = []
        for chunk in memory_chunks:
            context_parts.append(f"[From {chunk.source}]: {chunk.content[:200]}...")

        context_str = "\n".join(context_parts)

        prompt = f"""You are WhiteMagic, a local AI assistant. Use this context to answer accurately:

CONTEXT:
{context_str}

QUESTION: {query}

Answer based on the context above. If unsure, say so."""

        return prompt

    def generate(
        self,
        query: str,
        use_memory: bool = True,
        max_tokens: int = 100,
    ) -> CascadeResult:
        """Generate with optional memory augmentation."""
        start = time.time()

        # Search memory
        chunks = []
        if use_memory:
            chunks = self.search_memory(query)

        # Build prompt
        if chunks:
            prompt = self.build_augmented_prompt(query, chunks)
            method = f"mag:{len(chunks)}_chunks"
        else:
            prompt = query
            method = "direct"

        # Generate
        answer = self.llm.generate(prompt, max_tokens=max_tokens)  # type: ignore[attr-defined]

        return CascadeResult(
            query=query,
            answer=answer,
            method=method,
            confidence=0.85 if chunks else 0.7,
            latency_ms=(time.time() - start) * 1000,
        )


_mag_instance: MemoryAugmentedGenerator | None = None

def get_mag() -> MemoryAugmentedGenerator:
    """Get singleton MAG instance."""
    global _mag_instance
    if _mag_instance is None:
        _mag_instance = MemoryAugmentedGenerator()
    return _mag_instance
