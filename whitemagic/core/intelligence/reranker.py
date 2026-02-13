"""Cross-Encoder Reranking — Optional precision layer after hybrid search.

Source: LIVING_MEMORY_GAP_ANALYSIS.md (Zep comparison)

After hybrid_recall returns top-K candidates via FTS + vector + graph walk,
this module re-scores each candidate against the original query using a
cross-encoder model for higher precision. Cross-encoders jointly encode
(query, document) pairs, producing more accurate relevance scores than
bi-encoder cosine similarity alone.

Graceful degradation: if no cross-encoder model is available (no torch,
no sentence-transformers), falls back to a lightweight lexical reranker
using BM25-style term overlap scoring.
"""

from __future__ import annotations

import logging
import math
import re
from collections import Counter
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)

# Lazy-loaded model singleton
_cross_encoder: Any | None = None
_cross_encoder_available: bool | None = None


@dataclass
class RankedResult:
    """A search result with a reranking score."""

    memory_id: str
    title: str
    content: str
    original_score: float
    rerank_score: float
    metadata: dict[str, Any] | None = None

    @property
    def combined_score(self) -> float:
        """Weighted combination of original and rerank scores."""
        return 0.4 * self.original_score + 0.6 * self.rerank_score


def _check_cross_encoder() -> bool:
    """Check if cross-encoder model is available."""
    global _cross_encoder_available
    if _cross_encoder_available is not None:
        return _cross_encoder_available
    try:
        from sentence_transformers import CrossEncoder  # noqa: F401
        _cross_encoder_available = True
    except ImportError:
        _cross_encoder_available = False
    return _cross_encoder_available


def _get_cross_encoder() -> Any:
    """Get or initialize the cross-encoder model (lazy singleton)."""
    global _cross_encoder
    if _cross_encoder is not None:
        return _cross_encoder
    try:
        from sentence_transformers import CrossEncoder
        _cross_encoder = CrossEncoder(
            "cross-encoder/ms-marco-MiniLM-L-6-v2",
            max_length=512,
        )
        logger.info("Cross-encoder model loaded: ms-marco-MiniLM-L-6-v2")
        return _cross_encoder
    except Exception as e:
        logger.warning(f"Cross-encoder unavailable: {e}")
        return None


def _tokenize(text: str) -> list[str]:
    """Simple whitespace + punctuation tokenizer."""
    return re.findall(r"\b\w+\b", text.lower())


def _bm25_score(query_tokens: list[str], doc_tokens: list[str], k1: float = 1.5, b: float = 0.75, avgdl: float = 100.0) -> float:
    """BM25 score for a single document against query tokens."""
    doc_len = len(doc_tokens)
    if doc_len == 0:
        return 0.0

    tf = Counter(doc_tokens)
    score = 0.0
    for term in query_tokens:
        if term not in tf:
            continue
        freq = tf[term]
        numerator = freq * (k1 + 1)
        denominator = freq + k1 * (1 - b + b * doc_len / avgdl)
        # IDF approximation (assume term appears in ~10% of docs)
        idf = math.log(1 + 10.0)
        score += idf * numerator / denominator

    return score


def lexical_rerank(
    query: str,
    results: list[dict[str, Any]],
    top_k: int = 10,
) -> list[RankedResult]:
    """Lightweight lexical reranker using BM25-style scoring.

    Used as fallback when cross-encoder model is unavailable.

    Args:
        query: The search query
        results: List of search result dicts with at least 'id', 'title', 'content'
        top_k: Number of results to return

    Returns:
        Reranked results sorted by combined score

    """
    query_tokens = _tokenize(query)
    if not query_tokens:
        return [
            RankedResult(
                memory_id=r.get("id", ""),
                title=r.get("title", ""),
                content=r.get("content", ""),
                original_score=r.get("score", 0.0),
                rerank_score=r.get("score", 0.0),
                metadata=r.get("metadata"),
            )
            for r in results[:top_k]
        ]

    # Calculate average document length for BM25
    all_tokens = [_tokenize(r.get("content", "") + " " + r.get("title", "")) for r in results]
    avgdl = sum(len(t) for t in all_tokens) / max(len(all_tokens), 1)

    ranked = []
    max_bm25 = 0.0
    for r, doc_tokens in zip(results, all_tokens):
        bm25 = _bm25_score(query_tokens, doc_tokens, avgdl=avgdl)
        max_bm25 = max(max_bm25, bm25)
        ranked.append((r, bm25, doc_tokens))

    # Normalize BM25 scores to [0, 1]
    reranked = []
    for r, bm25, _doc_tokens in ranked:
        norm_score = bm25 / max_bm25 if max_bm25 > 0 else 0.0
        reranked.append(RankedResult(
            memory_id=r.get("id", ""),
            title=r.get("title", ""),
            content=r.get("content", ""),
            original_score=r.get("score", 0.0),
            rerank_score=norm_score,
            metadata=r.get("metadata"),
        ))

    reranked.sort(key=lambda x: x.combined_score, reverse=True)
    return reranked[:top_k]


def cross_encoder_rerank(
    query: str,
    results: list[dict[str, Any]],
    top_k: int = 10,
) -> list[RankedResult]:
    """Rerank results using a cross-encoder model for maximum precision.

    Args:
        query: The search query
        results: List of search result dicts with at least 'id', 'title', 'content'
        top_k: Number of results to return

    Returns:
        Reranked results sorted by cross-encoder score

    """
    model = _get_cross_encoder()
    if model is None:
        logger.debug("Cross-encoder unavailable, falling back to lexical reranker")
        return lexical_rerank(query, results, top_k)

    # Prepare (query, document) pairs for cross-encoder
    pairs = []
    for r in results:
        doc_text = r.get("title", "") + " " + r.get("content", "")
        # Truncate to avoid exceeding model max_length
        doc_text = doc_text[:1024]
        pairs.append([query, doc_text])

    try:
        scores = model.predict(pairs)

        # Normalize to [0, 1]
        min_s = float(min(scores)) if len(scores) > 0 else 0.0
        max_s = float(max(scores)) if len(scores) > 0 else 1.0
        score_range = max_s - min_s if max_s > min_s else 1.0

        reranked = []
        for r, score in zip(results, scores):
            norm_score = (float(score) - min_s) / score_range
            reranked.append(RankedResult(
                memory_id=r.get("id", ""),
                title=r.get("title", ""),
                content=r.get("content", ""),
                original_score=r.get("score", 0.0),
                rerank_score=norm_score,
                metadata=r.get("metadata"),
            ))

        reranked.sort(key=lambda x: x.combined_score, reverse=True)
        return reranked[:top_k]

    except Exception as e:
        logger.warning(f"Cross-encoder inference failed: {e}, falling back to lexical")
        return lexical_rerank(query, results, top_k)


def rerank(
    query: str,
    results: list[dict[str, Any]],
    top_k: int = 10,
    strategy: str = "auto",
) -> list[RankedResult]:
    """Rerank search results using the best available strategy.

    Args:
        query: The search query
        results: List of search result dicts
        top_k: Number of results to return
        strategy: 'auto' (best available), 'cross_encoder', or 'lexical'

    Returns:
        Reranked results sorted by combined score

    """
    if not results:
        return []

    if strategy == "cross_encoder" or (strategy == "auto" and _check_cross_encoder()):
        return cross_encoder_rerank(query, results, top_k)
    else:
        return lexical_rerank(query, results, top_k)


def get_status() -> dict[str, Any]:
    """Get reranker status."""
    return {
        "cross_encoder_available": _check_cross_encoder(),
        "model": "cross-encoder/ms-marco-MiniLM-L-6-v2" if _check_cross_encoder() else None,
        "fallback": "lexical_bm25",
        "strategies": ["auto", "cross_encoder", "lexical"],
    }
