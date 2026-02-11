"""Embedding Index - Semantic Search for Memories.

Uses sentence embeddings to enable semantic search across all memories.
Find memories by meaning, not just keywords.

Inspired by how the brain creates distributed representations
where similar concepts cluster together in neural space.
"""

import hashlib
import json
import math
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import CACHE_DIR
from whitemagic.utils.fileio import atomic_write, file_lock


class SimpleEmbedding:
    """Simple TF-IDF-like embedding for semantic similarity.

    This is a lightweight implementation that doesn't require
    external ML libraries. For production, consider using
    sentence-transformers or similar.
    """

    def __init__(self) -> None:
        self.vocabulary: dict[str, int] = {}
        self.idf: dict[str, float] = {}
        self.doc_count = 0

    def _tokenize(self, text: str) -> list[str]:
        """Simple tokenization."""
        # Lowercase and split on non-alphanumeric
        import re
        tokens = re.findall(r"\b\w+\b", text.lower())
        # Remove very short tokens and stopwords
        stopwords = {"the", "a", "an", "is", "are", "was", "were", "be", "been",
                     "being", "have", "has", "had", "do", "does", "did", "will",
                     "would", "could", "should", "may", "might", "must", "shall",
                     "can", "to", "of", "in", "for", "on", "with", "at", "by",
                     "from", "as", "into", "through", "during", "before", "after",
                     "above", "below", "between", "under", "again", "further",
                     "then", "once", "here", "there", "when", "where", "why",
                     "how", "all", "each", "few", "more", "most", "other", "some",
                     "such", "no", "nor", "not", "only", "own", "same", "so",
                     "than", "too", "very", "just", "and", "but", "if", "or",
                     "because", "until", "while", "this", "that", "these", "those",
                     "it", "its", "i", "me", "my", "we", "our", "you", "your",
                     "he", "him", "his", "she", "her", "they", "them", "their"}
        return [t for t in tokens if len(t) > 2 and t not in stopwords]

    def _compute_tf(self, tokens: list[str]) -> dict[str, float]:
        """Compute term frequency."""
        tf: dict[str, int] = {}
        for token in tokens:
            tf[token] = tf.get(token, 0) + 1
        # Normalize
        max_freq = float(max(tf.values())) if tf else 1.0
        return {k: v / max_freq for k, v in tf.items()}

    def embed(self, text: str) -> dict[str, float]:
        """Create a sparse embedding vector."""
        tokens = self._tokenize(text)
        tf = self._compute_tf(tokens)

        # Apply IDF weighting if available
        embedding = {}
        for token, freq in tf.items():
            idf = self.idf.get(token, 1.0)
            embedding[token] = freq * idf

        return embedding

    def similarity(self, emb1: dict[str, float], emb2: dict[str, float]) -> float:
        """Compute cosine similarity between embeddings."""
        # Get all keys
        all_keys = set(emb1.keys()) | set(emb2.keys())
        if not all_keys:
            return 0.0

        # Compute dot product and magnitudes
        dot = sum(emb1.get(k, 0) * emb2.get(k, 0) for k in all_keys)
        mag1 = math.sqrt(sum(v ** 2 for v in emb1.values()))
        mag2 = math.sqrt(sum(v ** 2 for v in emb2.values()))

        if mag1 == 0 or mag2 == 0:
            return 0.0

        return dot / (mag1 * mag2)

    def update_idf(self, documents: list[str]) -> None:
        """Update IDF values from a corpus of documents."""
        self.doc_count = len(documents)
        doc_freq: dict[str, int] = {}

        for doc in documents:
            tokens = set(self._tokenize(doc))
            for token in tokens:
                doc_freq[token] = doc_freq.get(token, 0) + 1

        # Compute IDF
        for token, freq in doc_freq.items():
            self.idf[token] = math.log(self.doc_count / (1 + freq)) + 1


class EmbeddingIndex:
    """Index for semantic search across memories.

    Stores embeddings and enables similarity-based retrieval.
    """

    def __init__(self, index_dir: Path | None = None):
        self.index_dir = index_dir or (CACHE_DIR / "embeddings_simple")
        self.index_dir.mkdir(parents=True, exist_ok=True)

        self.embedder = SimpleEmbedding()
        self.index: dict[str, dict[str, Any]] = {}  # id -> {content, embedding, metadata}

        self._load_index()

    def _get_id(self, content: str) -> str:
        """Generate unique ID for content."""
        return hashlib.sha256(content.encode()).hexdigest()[:16]

    def _load_index(self) -> None:
        """Load existing index from disk."""
        index_file = self.index_dir / "index.json"
        if index_file.exists():
            try:
                with file_lock(index_file):
                    data: dict[str, Any] = json.loads(index_file.read_text())
                self.index = data.get("index", {})
                self.embedder.idf = data.get("idf", {})
                self.embedder.doc_count = data.get("doc_count", 0)
            except json.JSONDecodeError:
                pass

    def _save_index(self) -> None:
        """Persist index to disk."""
        index_file = self.index_dir / "index.json"
        data = {
            "index": self.index,
            "idf": self.embedder.idf,
            "doc_count": self.embedder.doc_count,
            "last_updated": datetime.now().isoformat(),
        }
        with file_lock(index_file):
            atomic_write(index_file, json.dumps(data))

    def add(self, content: str, metadata: dict[str, Any] | None = None) -> str:
        """Add content to the index."""
        content_id = self._get_id(content)
        embedding = self.embedder.embed(content)

        self.index[content_id] = {
            "content": content,
            "embedding": embedding,
            "metadata": metadata or {},
            "indexed_at": datetime.now().isoformat(),
        }

        # Update IDF periodically
        if len(self.index) % 10 == 0:
            self._update_idf()

        self._save_index()
        return content_id

    def _update_idf(self) -> None:
        """Update IDF values from indexed documents."""
        documents = [entry["content"] for entry in self.index.values()]
        self.embedder.update_idf(documents)

    def search(self, query: str, top_k: int = 10) -> list[tuple[str, float, dict[str, Any]]]:
        """Search for similar content.

        Returns list of (content_id, similarity_score, entry) tuples.
        """
        query_embedding = self.embedder.embed(query)

        results = []
        for content_id, entry in self.index.items():
            similarity = self.embedder.similarity(query_embedding, entry["embedding"])
            if similarity > 0.1:  # Threshold
                results.append((content_id, similarity, entry))

        # Sort by similarity
        results.sort(key=lambda x: x[1], reverse=True)
        return results[:top_k]

    def find_similar(self, content_id: str, top_k: int = 5) -> list[tuple[str, float, dict[str, Any]]]:
        """Find content similar to a given indexed item."""
        if content_id not in self.index:
            return []

        source_embedding = self.index[content_id]["embedding"]

        results = []
        for cid, entry in self.index.items():
            if cid == content_id:
                continue
            similarity = self.embedder.similarity(source_embedding, entry["embedding"])
            if similarity > 0.1:
                results.append((cid, similarity, entry))

        results.sort(key=lambda x: x[1], reverse=True)
        return results[:top_k]

    def get_stats(self) -> dict[str, Any]:
        """Get index statistics."""
        return {
            "total_documents": len(self.index),
            "vocabulary_size": len(self.embedder.idf),
            "index_size_bytes": sum(
                len(json.dumps(e)) for e in self.index.values()
            ),
        }


# Singleton
_index: EmbeddingIndex | None = None

def get_embedding_index() -> EmbeddingIndex:
    """Get the embedding index singleton."""
    global _index
    if _index is None:
        _index = EmbeddingIndex()
    return _index

def semantic_search(query: str, top_k: int = 10) -> list[dict[str, Any]]:
    """Convenience function for semantic search.

    Returns list of {content, similarity, metadata} dicts.
    """
    index = get_embedding_index()
    results = index.search(query, top_k)
    return [
        {
            "id": r[0],
            "similarity": r[1],
            "content": r[2]["content"],
            "metadata": r[2]["metadata"],
        }
        for r in results
    ]

def index_memory(content: str, metadata: dict[str, Any] | None = None) -> str:
    """Index a memory for semantic search."""
    return get_embedding_index().add(content, metadata)
