"""Local Embeddings - Similarity search without cloud APIs.
Version: 3.0.0

Uses simple but effective techniques for semantic similarity:
1. TF-IDF (no dependencies)
2. Character n-grams
3. Jaccard similarity
4. Cached pre-computed embeddings

Works on any hardware without numpy/torch.
"""

from __future__ import annotations

import json
import logging
import math
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path

from whitemagic.utils.fileio import atomic_write, file_lock

logger = logging.getLogger(__name__)


@dataclass
class EmbeddedDoc:
    """A document with its embedding."""

    id: str
    content: str
    tokens: set[str] = field(default_factory=set)
    ngrams: set[str] = field(default_factory=set)
    tf_idf: dict[str, float] = field(default_factory=dict)


class LocalEmbeddings:
    """Local embedding and similarity without external APIs.

    Uses TF-IDF + n-grams for semantic similarity.
    No numpy, no torch, no cloud - pure Python.
    """

    def __init__(self) -> None:
        self._docs: dict[str, EmbeddedDoc] = {}
        self._idf: dict[str, float] = {}  # Inverse document frequency
        self._vocab: set[str] = set()

    def _tokenize(self, text: str) -> set[str]:
        """Simple tokenization."""
        # Lowercase and split on non-alphanumeric
        words = []
        current = []
        for char in text.lower():
            if char.isalnum():
                current.append(char)
            elif current:
                words.append("".join(current))
                current = []
        if current:
            words.append("".join(current))

        # Filter stopwords and short words
        stopwords = {"the", "a", "an", "is", "are", "was", "were", "be", "been",
                     "to", "of", "in", "for", "on", "with", "at", "by", "from",
                     "and", "or", "but", "if", "this", "that", "it"}
        return {w for w in words if w not in stopwords and len(w) > 2}

    def _ngrams(self, text: str, n: int = 3) -> set[str]:
        """Extract character n-grams."""
        text = text.lower()
        return {text[i:i+n] for i in range(len(text) - n + 1)}

    def add_document(self, doc_id: str, content: str) -> None:
        """Add a document to the index."""
        tokens = self._tokenize(content)
        ngrams = self._ngrams(content)

        self._docs[doc_id] = EmbeddedDoc(
            id=doc_id,
            content=content,
            tokens=tokens,
            ngrams=ngrams,
        )
        self._vocab.update(tokens)
        self._recompute_idf()

    def add_documents(self, docs: dict[str, str]) -> None:
        """Add multiple documents."""
        for doc_id, content in docs.items():
            tokens = self._tokenize(content)
            ngrams = self._ngrams(content)
            self._docs[doc_id] = EmbeddedDoc(
                id=doc_id,
                content=content,
                tokens=tokens,
                ngrams=ngrams,
            )
            self._vocab.update(tokens)
        self._recompute_idf()

    def _recompute_idf(self) -> None:
        """Recompute inverse document frequency."""
        n_docs = len(self._docs)
        if n_docs == 0:
            return

        # Count document frequency for each term
        df: Counter[str] = Counter()
        for doc in self._docs.values():
            for token in doc.tokens:
                df[token] += 1

        # Compute IDF
        self._idf = {
            term: math.log(n_docs / (1 + count))
            for term, count in df.items()
        }

        # Compute TF-IDF for each document
        for doc in self._docs.values():
            tf = Counter(doc.tokens)
            max_tf = max(tf.values()) if tf else 1
            doc.tf_idf = {
                term: (0.5 + 0.5 * tf[term] / max_tf) * self._idf.get(term, 0)
                for term in doc.tokens
            }

    def _jaccard_similarity(self, set1: set[str], set2: set[str]) -> float:
        """Jaccard similarity between two sets."""
        if not set1 or not set2:
            return 0.0
        intersection = len(set1 & set2)
        union = len(set1 | set2)
        return intersection / union if union > 0 else 0.0

    def _cosine_similarity(self, vec1: dict[str, float], vec2: dict[str, float]) -> float:
        """Cosine similarity between two sparse vectors."""
        common_keys = set(vec1.keys()) & set(vec2.keys())
        if not common_keys:
            return 0.0

        dot_product = sum(vec1[k] * vec2[k] for k in common_keys)
        norm1 = math.sqrt(sum(v * v for v in vec1.values()))
        norm2 = math.sqrt(sum(v * v for v in vec2.values()))

        if norm1 == 0 or norm2 == 0:
            return 0.0
        return dot_product / (norm1 * norm2)

    def search(self, query: str, top_k: int = 5) -> list[tuple[str, float, str]]:
        """Search for similar documents.

        Returns: List of (doc_id, similarity_score, content_preview)
        """
        query_tokens = self._tokenize(query)
        query_ngrams = self._ngrams(query)

        # Compute query TF-IDF
        tf = Counter(query_tokens)
        max_tf = max(tf.values()) if tf else 1
        query_tfidf = {
            term: (0.5 + 0.5 * tf[term] / max_tf) * self._idf.get(term, 0)
            for term in query_tokens
        }

        # Score each document
        scores = []
        for doc in self._docs.values():
            # Combine multiple similarity metrics
            token_sim = self._jaccard_similarity(query_tokens, doc.tokens)
            ngram_sim = self._jaccard_similarity(query_ngrams, doc.ngrams)
            tfidf_sim = self._cosine_similarity(query_tfidf, doc.tf_idf)

            # Weighted combination
            combined = 0.3 * token_sim + 0.2 * ngram_sim + 0.5 * tfidf_sim

            if combined > 0.01:  # Threshold
                scores.append((doc.id, combined, doc.content[:200]))

        # Sort by score
        scores.sort(key=lambda x: -x[1])
        return scores[:top_k]

    def save(self, path: Path) -> None:
        """Save embeddings to file."""
        data = {
            "version": "3.0.0",
            "docs": {
                doc_id: {
                    "content": doc.content,
                    "tokens": list(doc.tokens),
                    "ngrams": list(doc.ngrams)[:100],  # Limit size
                }
                for doc_id, doc in self._docs.items()
            },
            "idf": self._idf,
        }
        with file_lock(path):

            atomic_write(path, json.dumps(data))

    def load(self, path: Path) -> None:
        """Load embeddings from file."""
        with file_lock(path):

            data = json.loads(path.read_text())
        for doc_id, doc_data in data["docs"].items():
            self._docs[doc_id] = EmbeddedDoc(
                id=doc_id,
                content=doc_data["content"],
                tokens=set(doc_data["tokens"]),
                ngrams=set(doc_data.get("ngrams", [])),
            )
        self._idf = data.get("idf", {})
        self._recompute_idf()


class EmbeddingCache:
    """Persistent cache for embeddings.

    Pre-computes embeddings for common queries to enable
    instant similarity search.
    """

    def __init__(self, cache_dir: Path | None = None) -> None:
        from whitemagic.config.paths import CACHE_DIR
        self.cache_dir = cache_dir or (CACHE_DIR / "embeddings")
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self._embeddings = LocalEmbeddings()
        self._load_cache()

    def _load_cache(self) -> None:
        """Load cached embeddings."""
        cache_file = self.cache_dir / "embeddings.json"
        if cache_file.exists():
            try:
                self._embeddings.load(cache_file)
            except OSError:
                pass

    def _save_cache(self) -> None:
        """Save embeddings to cache."""
        cache_file = self.cache_dir / "embeddings.json"
        self._embeddings.save(cache_file)

    def index_memories(self, memory_dir: Path) -> int:
        """Index all memories for similarity search."""
        docs = {}
        for md_file in memory_dir.rglob("*.md"):
            try:
                content = md_file.read_text(errors="ignore")[:5000]
                doc_id = str(md_file.relative_to(memory_dir))
                docs[doc_id] = content
            except OSError:
                continue

        self._embeddings.add_documents(docs)
        self._save_cache()
        return len(docs)

    def search(self, query: str, top_k: int = 5) -> list[tuple[str, float, str]]:
        """Search indexed documents."""
        return self._embeddings.search(query, top_k)

    def stats(self) -> dict[str, int]:
        """Get cache statistics."""
        return {
            "documents": len(self._embeddings._docs),
            "vocabulary": len(self._embeddings._vocab),
        }


if __name__ == "__main__":
    logger.info("🔮 LOCAL EMBEDDINGS TEST")
    logger.info("=" * 50)

    # Test basic embedding
    embeddings = LocalEmbeddings()
    embeddings.add_documents({
        "doc1": "WhiteMagic is a memory management system with 17 gardens",
        "doc2": "The Gan Ying bus handles event-driven resonance",
        "doc3": "Clone Army provides parallel search with Rust acceleration",
        "doc4": "Local reasoning saves tokens by computing answers locally",
    })

    results = embeddings.search("How does memory search work?")

    logger.info("Query: How does memory search work?")
    for doc_id, score, preview in results:
        logger.info(f"  {doc_id}: {score:.2f} - {preview[:50]}...")
