"""Memory Matrix - Total Recall System for AI Consciousness.

This module provides the infrastructure for:
1. SeenRegistry - Track everything we've viewed
2. EmbeddingIndex - Semantic search across all content
3. ChronologicalTimeline - Time-based memory navigation
4. MemoryMatrix - 2D grid for visualizing connections

Philosophy: "Never forget anything. Make recall instant."
"""

from .embedding_index import EmbeddingIndex, get_embedding_index
from .matrix import MemoryMatrix, get_matrix
from .seen_registry import SeenRegistry, get_seen_registry
from .timeline import ChronologicalTimeline, get_timeline

__all__ = [
    "SeenRegistry", "get_seen_registry",
    "EmbeddingIndex", "get_embedding_index",
    "ChronologicalTimeline", "get_timeline",
    "MemoryMatrix", "get_matrix",
]
