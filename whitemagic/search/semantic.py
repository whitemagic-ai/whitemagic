# mypy: disable-error-code=no-untyped-def
"""Semantic search implementation using embeddings and vector similarity.

Supports multiple search modes:
- keyword: Traditional keyword-based search (existing)
- semantic: Vector similarity search using embeddings
- hybrid: Combines keyword and semantic with RRF (Reciprocal Rank Fusion)
"""

import logging
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any

import numpy as np

from ..core import MemoryManager  # type: ignore[attr-defined]
from ..embeddings import EmbeddingConfig, EmbeddingProvider, get_embedding_provider  # type: ignore[import-not-found]
from ..embeddings.storage import EmbeddingCache, FileBasedEmbeddingCache  # type: ignore[import-not-found]

logger = logging.getLogger(__name__)


class SearchMode(str, Enum):
    """Search mode selection."""

    KEYWORD = "keyword"
    SEMANTIC = "semantic"
    HYBRID = "hybrid"


@dataclass
class SearchResult:
    """Search result with relevance score."""

    memory_id: str
    title: str
    content: str
    type: str
    tags: list[str]
    score: float
    match_type: str  # "keyword", "semantic", or "hybrid"
    created_at: str | None = None
    updated_at: str | None = None


class SemanticSearcher:
    """Semantic search engine for WhiteMagic memories.

    Tier 1 (Ephemeral): Generates embeddings on-demand, no caching.
    Works without database changes - perfect for quick prototyping.
    """

    def __init__(
        self,
        memory_manager: MemoryManager,
        embedding_provider: EmbeddingProvider | None = None,
        embedding_config: EmbeddingConfig | None = None,
        cache: EmbeddingCache | None = None,
    ):
        """Initialize semantic searcher.

        Args:
            memory_manager: WhiteMagic memory manager instance
            embedding_provider: Optional embedding provider (if None, creates from config)
            embedding_config: Optional config (if None, loads from env)
            cache: Optional embedding cache (Tier 2)

        """
        self.manager = memory_manager

        # Set up embedding provider
        if embedding_provider:
            self.embedder = embedding_provider
        elif embedding_config:
            self.embedder = get_embedding_provider(embedding_config)
        else:
            # Load from environment
            config = EmbeddingConfig.from_env()
            self.embedder = get_embedding_provider(config)

        # Set up caching (use file-based cache by default)
        if cache is None:
            self.cache = FileBasedEmbeddingCache()
        else:
            self.cache = cache

    def _cosine_similarity(self, vec1: list[float], vec2: list[float]) -> float:
        """Calculate cosine similarity between two vectors.

        Args:
            vec1: First vector
            vec2: Second vector

        Returns:
            Similarity score between 0 and 1 (1 = identical)

        """
        a = np.array(vec1)
        b = np.array(vec2)

        # Cosine similarity = dot product / (magnitude1 * magnitude2)
        dot_product = np.dot(a, b)
        magnitude_a = np.linalg.norm(a)
        magnitude_b = np.linalg.norm(b)

        if magnitude_a == 0 or magnitude_b == 0:
            return 0.0

        return float(dot_product / (magnitude_a * magnitude_b))

    async def _get_memory_content(self, memory_path: Path) -> dict[str, Any]:
        """Load memory content from file.

        Args:
            memory_path: Path to memory file (.md or .json)

        Returns:
            Dictionary with memory metadata and content

        """
        import json

        # Handle both .md and .json formats
        if memory_path.suffix == ".md":
            # Markdown format with frontmatter
            content = memory_path.read_text()

            # Parse frontmatter if present
            if content.startswith("---\n"):
                parts = content.split("---\n", 2)
                if len(parts) >= 3:
                    import yaml

                    try:
                        metadata = yaml.safe_load(parts[1])
                        content_text = parts[2].strip()
                    except Exception:
                        # Fallback if yaml fails
                        metadata = {}
                        content_text = content
                else:
                    metadata = {}
                    content_text = content
            else:
                metadata = {}
                content_text = content

            return {
                "id": memory_path.name,
                "title": metadata.get("title", memory_path.stem.replace("_", " ").title()),
                "content": content_text,
                "type": metadata.get("type", "short_term"),
                "tags": metadata.get("tags", []),
                "created_at": metadata.get("created_at"),
                "updated_at": metadata.get("updated_at"),
            }
        # JSON format
        with open(memory_path) as f:
            data = json.load(f)

        return {
            "id": memory_path.name,
            "title": data.get("title", ""),
            "content": data.get("content", ""),
            "type": data.get("type", "short_term"),
            "tags": data.get("tags", []),
            "created_at": data.get("created_at"),
            "updated_at": data.get("updated_at"),
        }

    async def semantic_search(
        self,
        query: str,
        k: int = 10,
        threshold: float = 0.7,
        memory_type: str | None = None,
        tags: list[str] | None = None,
    ) -> list[SearchResult]:
        """Perform semantic search using embeddings.

        Tier 1 (Ephemeral): Generates embeddings on-demand for all memories.
        No caching - suitable for prototyping and small datasets.

        Args:
            query: Search query text
            k: Number of results to return
            threshold: Minimum similarity score (0-1)
            memory_type: Optional filter by memory type
            tags: Optional filter by tags

        Returns:
            List of search results sorted by relevance

        """
        # Generate query embedding
        query_embedding = await self.embedder.embed(query)

        # Get all memories (with filters)
        memories = []
        for memory_type_dir in ["short_term", "long_term"]:
            if memory_type and memory_type_dir != memory_type:
                continue
            if memory_type and memory_type != memory_type_dir:
                continue

            # Check both direct path and memory/ subdirectory
            possible_dirs = [
                self.manager.base_dir / memory_type_dir,
                self.manager.base_dir / "memory" / memory_type_dir,
            ]

            for memory_dir in possible_dirs:
                if not memory_dir.exists():
                    continue

                # Support both .json and .md files
                for pattern in ["*.json", "*.md"]:
                    for memory_file in memory_dir.glob(pattern):
                        try:
                            memory = await self._get_memory_content(memory_file)
                            memory["type"] = memory_type_dir

                            # Filter by tags if specified
                            if tags:
                                if not any(tag in memory["tags"] for tag in tags):
                                    continue

                            memories.append(memory)
                        except Exception as e:
                            # Skip corrupted files but log for debugging
                            import logging

                            logging.debug(f"Skipping {memory_file}: {e}")
                            continue

        # Generate embeddings for all memories (with caching)
        results = []

        if not memories:
            return []

        # Get model name for cache key
        model_name = getattr(self.embedder, "model_name", "default")

        # Try to get cached embeddings first
        memory_embeddings = []
        texts_to_embed = []
        indices_to_embed = []

        for i, memory in enumerate(memories):
            # Try cache first if available
            if self.cache and hasattr(self.cache, "get"):
                # Get memory file path for cache lookup
                memory_path = (
                    self.manager.base_dir / memory.get("type", "short_term") / memory["id"]
                )
                if not memory_path.exists():
                    # Try alternate paths
                    alt_paths = [
                        self.manager.base_dir
                        / "memory"
                        / memory.get("type", "short_term")
                        / memory["id"],
                    ]
                    for alt_path in alt_paths:
                        if alt_path.exists():
                            memory_path = alt_path
                            break

                if isinstance(self.cache, FileBasedEmbeddingCache) and memory_path.exists():
                    cached = await self.cache.get(memory_path, model_name)
                    if cached:
                        memory_embeddings.append(cached)
                        continue

            # Not cached, need to embed
            memory_embeddings.append(None)  # Placeholder
            texts_to_embed.append(f"{memory['title']} {memory['content']}")
            indices_to_embed.append(i)

        # Batch embed uncached memories
        if texts_to_embed:
            fresh_embeddings = await self.embedder.embed_batch(texts_to_embed)

            # Fill in fresh embeddings and cache them
            for idx, embedding in zip(indices_to_embed, fresh_embeddings):
                memory_embeddings[idx] = embedding

                # Cache the fresh embedding
                if self.cache and isinstance(self.cache, FileBasedEmbeddingCache):
                    memory = memories[idx]
                    memory_path = (
                        self.manager.base_dir / memory.get("type", "short_term") / memory["id"]
                    )
                    if not memory_path.exists():
                        alt_paths = [
                            self.manager.base_dir
                            / "memory"
                            / memory.get("type", "short_term")
                            / memory["id"],
                        ]
                        for alt_path in alt_paths:
                            if alt_path.exists():
                                memory_path = alt_path
                                break

                    if memory_path.exists():
                        await self.cache.set(memory_path, model_name, embedding)

        # Calculate similarities
        for memory, embedding in zip(memories, memory_embeddings):
            similarity = self._cosine_similarity(query_embedding, embedding)

            if similarity >= threshold:
                results.append(
                    SearchResult(
                        memory_id=memory["id"],
                        title=memory["title"],
                        content=memory["content"],
                        type=memory["type"],
                        tags=memory["tags"],
                        score=similarity,
                        match_type="semantic",
                        created_at=memory.get("created_at"),
                        updated_at=memory.get("updated_at"),
                    ),
                )

        # Sort by score (highest first) and return top k
        results.sort(key=lambda x: x.score, reverse=True)
        return results[:k]

    async def keyword_search(
        self,
        query: str,
        k: int = 10,
        memory_type: str | None = None,
        tags: list[str] | None = None,
    ) -> list[SearchResult]:
        """Perform traditional keyword search.

        Uses the existing MemoryManager search functionality.
        """
        # Use existing search
        raw_results = self.manager.search_memories(query=query, memory_type=memory_type, tags=tags)

        # Convert to SearchResult objects
        results = []
        for i, memory in enumerate(raw_results[:k]):
            # Simple relevance score based on rank
            score = 1.0 - (i * 0.05)  # Decreases by 0.05 per rank

            # Get memory ID - handle different formats
            entry = memory.get("entry") or {}
            memory_id = (
                memory.get("filename")
                or entry.get("filename")
                or memory.get("id")
                or str(memory.get("path", "unknown"))
            )
            if hasattr(memory_id, "stem"):
                memory_id = memory_id.stem

            title = memory.get("title") or entry.get("title", "")
            content = memory.get("content") or entry.get("content", "")
            tags = memory.get("tags") or entry.get("tags", [])
            memory_kind = memory.get("type") or entry.get("type", "short_term")

            results.append(
                SearchResult(
                    memory_id=str(memory_id),
                    title=title,
                    content=content,
                    type=memory_kind,
                    tags=tags,
                    score=max(0.1, score),
                    match_type="keyword",
                    created_at=memory.get("created_at"),
                    updated_at=memory.get("updated_at"),
                ),
            )

        return results

    async def hybrid_search(
        self,
        query: str,
        k: int = 10,
        keyword_weight: float = 0.3,
        semantic_weight: float = 0.7,
        threshold: float = 0.7,
        memory_type: str | None = None,
        tags: list[str] | None = None,
    ) -> list[SearchResult]:
        """Perform hybrid search combining keyword and semantic approaches.

        Uses Reciprocal Rank Fusion (RRF) to combine results.

        Args:
            query: Search query text
            k: Number of results to return
            keyword_weight: Weight for keyword results (0-1)
            semantic_weight: Weight for semantic results (0-1)
            threshold: Minimum similarity for semantic results
            memory_type: Optional filter by memory type
            tags: Optional filter by tags

        Returns:
            List of search results with combined relevance scores

        """
        # Get both result sets
        keyword_results = await self.keyword_search(
            query=query,
            k=k * 2,  # Get more to ensure good fusion
            memory_type=memory_type,
            tags=tags,
        )

        semantic_results = await self.semantic_search(
            query=query, k=k * 2, threshold=threshold, memory_type=memory_type, tags=tags,
        )

        # Apply Reciprocal Rank Fusion (RRF)
        # Score = keyword_weight * (1 / (rank + 60)) + semantic_weight * (1 / (rank + 60))
        rrf_scores: dict[str, tuple[float, SearchResult]] = {}

        # Process keyword results
        for rank, result in enumerate(keyword_results):
            rrf_score = keyword_weight / (rank + 60)
            rrf_scores[result.memory_id] = (rrf_score, result)

        # Process semantic results
        for rank, result in enumerate(semantic_results):
            rrf_score = semantic_weight / (rank + 60)

            if result.memory_id in rrf_scores:
                # Combine scores
                existing_score, existing_result = rrf_scores[result.memory_id]
                combined_score = existing_score + rrf_score

                # Use semantic result as base (has better metadata)
                result.score = combined_score
                result.match_type = "hybrid"
                rrf_scores[result.memory_id] = (combined_score, result)
            else:
                result.score = rrf_score
                result.match_type = "semantic"
                rrf_scores[result.memory_id] = (rrf_score, result)

        # Sort by combined score
        combined_results = [
            result
            for score, result in sorted(rrf_scores.values(), key=lambda x: x[0], reverse=True)
        ]

        return combined_results[:k]

    async def search(
        self,
        query: str,
        mode: SearchMode = SearchMode.HYBRID,
        k: int = 10,
        threshold: float = 0.7,
        keyword_weight: float = 0.3,
        semantic_weight: float = 0.7,
        memory_type: str | None = None,
        tags: list[str] | None = None,
    ) -> list[SearchResult]:
        """Unified search interface supporting multiple modes.

        Args:
            query: Search query text
            mode: Search mode (keyword, semantic, or hybrid)
            k: Number of results to return
            threshold: Minimum similarity for semantic results
            keyword_weight: Weight for keyword in hybrid mode
            semantic_weight: Weight for semantic in hybrid mode
            memory_type: Optional filter by memory type
            tags: Optional filter by tags

        Returns:
            List of search results

        """
        if mode == SearchMode.KEYWORD:
            return await self.keyword_search(query=query, k=k, memory_type=memory_type, tags=tags)
        if mode == SearchMode.SEMANTIC:
            return await self.semantic_search(
                query=query, k=k, threshold=threshold, memory_type=memory_type, tags=tags,
            )
        if mode == SearchMode.HYBRID:
            return await self.hybrid_search(
                query=query,
                k=k,
                keyword_weight=keyword_weight,
                semantic_weight=semantic_weight,
                threshold=threshold,
                memory_type=memory_type,
                tags=tags,
            )
        raise ValueError(f"Unknown search mode: {mode}")


# Convenience function for quick usage
async def semantic_search(
    query: str,
    manager: MemoryManager | None = None,
    k: int = 10,
    mode: SearchMode = SearchMode.HYBRID,
    **kwargs,
) -> list[SearchResult]:
    """Convenience function for semantic search.

    Args:
        query: Search query text
        manager: Optional memory manager (creates new if None)
        k: Number of results to return
        mode: Search mode (keyword, semantic, or hybrid)
        **kwargs: Additional arguments passed to search

    Returns:
        List of search results

    Example:
        >>> results = await semantic_search("debugging async code")
        >>> for result in results:
        ...     logger.info(f"{result.title}: {result.score:.2f}")

    """
    if manager is None:
        manager = MemoryManager()

    searcher = SemanticSearcher(manager)
    return await searcher.search(query=query, k=k, mode=mode, **kwargs)
