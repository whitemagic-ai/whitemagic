"""Rust Accelerator Bridge — Try Rust, Fall Back to Python
========================================================
Provides unified access to the Rust accelerators with
automatic fallback to pure-Python implementations when the
Rust extension (whitemagic_rs) is not available.

Accelerators (v12.3):
  - Galactic batch scoring (7-signal retention + distance)
  - Association mining (keyword extraction + N² Jaccard overlap)
  - 5D Spatial Index (KD-tree with V dimension)

Accelerators (v13.1):
  - 5D Holographic encoding (batch Rayon, garden/element blending)
  - MinHash LSH (128-hash near-duplicate detection)
  - SQLite batch operations (galactic updates, FTS5, zone stats)

Usage:
    from whitemagic.optimization.rust_accelerators import (
        galactic_batch_score,
        association_mine,
        get_spatial_index_5d,
    )
"""

import json
import logging
from typing import Any, cast

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Rust availability check
# ---------------------------------------------------------------------------

_RUST_AVAILABLE = False
_RUST_V131 = False
_rs: Any = None

try:
    import whitemagic_rs as _rs_mod

    _rs = _rs_mod
    # Check for v12.3 accelerator functions
    if hasattr(_rs, "galactic_batch_score"):
        _RUST_AVAILABLE = True
        logger.debug("Rust v12.3 accelerators loaded")
    else:
        logger.debug("Rust extension found but missing v12.3 accelerators")
    # Check for v13.1 accelerator functions
    if hasattr(_rs, "holographic_encode_batch"):
        _RUST_V131 = True
        logger.debug("Rust v13.1 accelerators loaded")
except ImportError:
    logger.debug("Rust extension not available — using Python fallback")


def rust_available() -> bool:
    """Check if Rust accelerators are available."""
    return _RUST_AVAILABLE


def rust_v131_available() -> bool:
    """Check if v13.1 Rust accelerators (holographic, minhash, sqlite) are available."""
    return _RUST_V131


# ---------------------------------------------------------------------------
# Galactic Batch Scoring
# ---------------------------------------------------------------------------

def galactic_batch_score(
    memories: list[dict[str, Any]],
    quick: bool = False,
) -> list[dict[str, Any]]:
    """Score a batch of memories for galactic distance.

    Args:
        memories: List of dicts with keys: id, importance, neuro_score,
                  emotional_valence, recall_count, is_protected, etc.
        quick: If True, use 4-signal heuristic (faster, less precise).

    Returns:
        List of dicts with: id, retention_score, galactic_distance, zone.

    """
    if _RUST_AVAILABLE:
        try:
            memories_json = json.dumps(memories)
            if quick:
                result_json = _rs.galactic_batch_score_quick(memories_json)
            else:
                result_json = _rs.galactic_batch_score(memories_json)
            parsed: list[dict[str, Any]] = json.loads(result_json)
            return parsed
        except Exception as e:
            logger.debug(f"Rust galactic scoring failed, using Python: {e}")

    # Python fallback
    return _galactic_batch_score_python(memories, quick)


def _galactic_batch_score_python(
    memories: list[dict[str, Any]], quick: bool,
) -> list[dict[str, Any]]:
    """Pure-Python fallback for galactic batch scoring."""
    results = []
    for m in memories:
        if m.get("is_protected") or m.get("is_core_identity") or m.get("is_sacred") or m.get("is_pinned"):
            distance = 0.0
            retention = 1.0
        elif quick:
            # 4-signal heuristic
            s1 = m.get("importance", 0.5) * 1.0
            s2 = m.get("neuro_score", 0.5) * 0.9
            s3 = abs(m.get("emotional_valence", 0.0)) * 0.6
            s4 = min(1.0, m.get("recall_count", 0) / 20.0) * 0.5
            retention = (s1 + s2 + s3 + s4) / 3.0
            distance = round(1.0 - max(0.0, min(1.0, retention)), 4)
        else:
            # 7-signal weighted
            weights = [0.35, 0.20, 0.10, 0.10, 0.10, 0.05, 0.10]
            values = [
                m.get("memory_type_weight", 0.5),
                m.get("richness", 0.3),
                m.get("activity", 0.0),
                m.get("recency", 0.5),
                m.get("importance", 0.5),
                m.get("emotion", 0.0),
                m.get("protection", 0.0),
            ]
            weighted_sum = sum(v * w for v, w in zip(values, weights))
            total_weight = sum(weights)
            retention = max(0.0, min(1.0, weighted_sum / total_weight))
            distance = round(1.0 - retention, 4)

        # Zone classification
        if distance < 0.15:
            zone = "core"
        elif distance < 0.40:
            zone = "inner_rim"
        elif distance < 0.65:
            zone = "mid_band"
        elif distance < 0.85:
            zone = "outer_rim"
        else:
            zone = "far_edge"

        results.append({
            "id": m.get("id", ""),
            "retention_score": round(retention, 4),
            "galactic_distance": distance,
            "zone": zone,
        })

    return results


# ---------------------------------------------------------------------------
# Association Mining
# ---------------------------------------------------------------------------

def association_mine(
    texts: list[tuple[str, str]],
    max_keywords: int = 50,
    min_score: float = 0.3,
    max_results: int = 500,
) -> dict[str, Any]:
    """Extract keywords from texts and compute pairwise overlaps.

    Args:
        texts: List of (memory_id, text_content) tuples.
        max_keywords: Max keywords to extract per text.
        min_score: Minimum overlap score to include.
        max_results: Maximum overlap results to return.

    Returns:
        Dict with memory_count, pair_count, overlaps.

    """
    if _RUST_AVAILABLE:
        try:
            texts_json = json.dumps(texts)
            result_json = _rs.association_mine_fast(
                texts_json, max_keywords, min_score, max_results,
            )
            parsed: dict[str, Any] = json.loads(result_json)
            return parsed
        except Exception as e:
            logger.debug(f"Rust association mining failed, using Python: {e}")

    # Python fallback
    return _association_mine_python(texts, max_keywords, min_score, max_results)


def _association_mine_python(
    texts: list[tuple[str, str]],
    max_keywords: int,
    min_score: float,
    max_results: int,
) -> dict[str, Any]:
    """Pure-Python fallback for association mining."""
    import re
    from collections import defaultdict

    stop_words = {
        "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
        "of", "with", "by", "from", "is", "it", "this", "that", "was", "are",
        "were", "be", "been", "being", "have", "has", "had", "do", "does",
        "did", "will", "would", "could", "should", "may", "might", "shall",
        "can", "not", "no", "nor", "so", "if", "then", "than", "too", "very",
        "just", "about", "up", "out", "into", "over", "after", "before",
    }
    word_re = re.compile(r"\w+")

    # Extract keywords
    fingerprints = []
    for _, text in texts:
        words = word_re.findall(text.lower())
        kws = {w for w in words if w not in stop_words and len(w) > 2}
        if len(kws) > max_keywords:
            freq: dict[str, int] = defaultdict(int)
            for w in words:
                if w in kws:
                    freq[w] += 1
            sorted_kw = sorted(kws, key=lambda k: freq[k], reverse=True)
            kws = set(sorted_kw[:max_keywords])
        fingerprints.append(kws)

    # Pairwise overlap
    overlaps = []
    n = len(fingerprints)
    for i in range(n):
        for j in range(i + 1, n):
            kw_a, kw_b = fingerprints[i], fingerprints[j]
            if not kw_a or not kw_b:
                continue
            shared = kw_a & kw_b
            union_size = len(kw_a | kw_b)
            if union_size == 0:
                continue
            raw_jaccard = len(shared) / union_size
            count_bonus = min(1.0, len(shared) / 5.0) * 0.3
            score = min(1.0, raw_jaccard + count_bonus)
            if score >= min_score:
                overlaps.append({
                    "source_id": texts[i][0],
                    "target_id": texts[j][0],
                    "overlap_score": round(score, 4),
                    "shared_count": len(shared),
                    "shared_keywords": sorted(shared)[:5],
                })

    overlaps.sort(key=lambda x: cast(float, x["overlap_score"]), reverse=True)
    overlaps = overlaps[:max_results]

    return {
        "memory_count": len(texts),
        "pair_count": len(overlaps),
        "overlaps": overlaps,
    }


# ---------------------------------------------------------------------------
# 5D Spatial Index
# ---------------------------------------------------------------------------

_index_5d: Any = None


def get_spatial_index_5d() -> Any:
    """Get or create the global 5D spatial index (Rust or Python fallback)."""
    global _index_5d
    if _index_5d is None:
        if _RUST_AVAILABLE:
            try:
                _index_5d = _rs.SpatialIndex5D()
                logger.debug("Using Rust 5D spatial index")
                return _index_5d
            except Exception:
                pass
        # Python fallback — thin wrapper over the 4D index
        _index_5d = PythonSpatialIndex5D()
        logger.debug("Using Python 5D spatial index fallback")
    return _index_5d


class PythonSpatialIndex5D:
    """Pure-Python fallback for 5D spatial index using brute-force search."""

    def __init__(self) -> None:
        self._points: list[tuple[str, list[float]]] = []

    def add(self, memory_id: str, vector: list[float]) -> int:
        idx = len(self._points)
        self._points.append((memory_id, list(vector)))
        return idx

    def add_batch(self, items: list[tuple[str, list[float]]]) -> int:
        for mid, vec in items:
            self._points.append((mid, list(vec)))
        return len(items)

    def query_nearest(self, vector: list[float], n: int) -> list[tuple[str, float]]:
        if not self._points:
            return []
        dists = []
        for mid, pt in self._points:
            d = sum((a - b) ** 2 for a, b in zip(vector, pt))
            dists.append((mid, d))
        dists.sort(key=lambda x: x[1])
        return dists[:n]

    def query_within_radius(self, vector: list[float], radius_sq: float) -> list[tuple[str, float]]:
        results = []
        for mid, pt in self._points:
            d = sum((a - b) ** 2 for a, b in zip(vector, pt))
            if d <= radius_sq:
                results.append((mid, d))
        return results

    def size(self) -> int:
        return len(self._points)

    def get_snapshot(self) -> list[tuple[str, list[float]]]:
        return list(self._points)

    def clear(self) -> None:
        self._points.clear()


# ---------------------------------------------------------------------------
# v13.1 — 5D Holographic Encoder (Rust Rayon batch)
# ---------------------------------------------------------------------------

def _prepare_memory_for_rust(memory: dict[str, Any]) -> dict[str, Any]:
    """Normalize a memory dict to the shape expected by Rust MemoryInput."""
    from datetime import datetime
    content = str(memory.get("content", ""))
    title = str(memory.get("title", ""))
    combined = f"{title} {content}" if title else content

    # Calculate age_days from created_at
    age_days = 0.0
    ts = memory.get("created_at") or memory.get("timestamp")
    if ts and isinstance(ts, str):
        for fmt in ("%Y-%m-%dT%H:%M:%S.%f", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d"):
            try:
                dt = datetime.strptime(ts[:26], fmt)
                age_days = max(0.0, (datetime.now() - dt).total_seconds() / 86400.0)
                break
            except ValueError:
                continue

    # Extract garden from metadata or tags
    metadata = memory.get("metadata") or {}
    garden = metadata.get("garden", "")
    if not garden:
        tags = memory.get("tags") or []
        for t in tags:
            if t in ("wood", "fire", "earth", "metal", "water"):
                garden = t
                break

    return {
        "id": memory.get("id", "unknown"),
        "content": combined,
        "importance": memory.get("importance") or 0.5,
        "access_count": memory.get("access_count") or memory.get("recall_count") or 0,
        "age_days": age_days,
        "galactic_distance": memory.get("galactic_distance") or 0.0,
        "garden": garden,
        "tags": list(memory.get("tags") or []),
    }


def holographic_encode_batch(
    memories: list[dict[str, Any]],
) -> list[dict[str, float]] | None:
    """Batch-encode memories into 5D holographic coordinates using Rust.

    Args:
        memories: List of memory dicts with keys: content, title, tags,
                  emotional_valence, importance, neuro_score, memory_type,
                  created_at, galactic_distance, retention_score, etc.

    Returns:
        List of {"x", "y", "z", "w", "v"} coordinate dicts, or None if
        Rust is not available.

    """
    if not _RUST_V131:
        return None
    try:
        prepared = [_prepare_memory_for_rust(m) for m in memories]
        memories_json = json.dumps(prepared)
        result_json = _rs.holographic_encode_batch(memories_json)
        parsed: list[dict[str, float]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust holographic batch encoding failed: {e}")
        return None


def holographic_encode_single(
    memory: dict[str, Any],
) -> dict[str, float] | None:
    """Encode a single memory into 5D holographic coordinates using Rust.

    Returns:
        {"x", "y", "z", "w", "v"} coordinate dict, or None if Rust
        is not available.

    """
    if not _RUST_V131:
        return None
    try:
        prepared = _prepare_memory_for_rust(memory)
        memory_json = json.dumps(prepared)
        result_json = _rs.holographic_encode_single(memory_json)
        parsed: dict[str, float] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust holographic single encoding failed: {e}")
        return None


def holographic_nearest_5d(
    query: list[float],
    coords: list[dict[str, Any]],
    k: int = 10,
    weights: list[float] | None = None,
) -> list[dict[str, Any]] | None:
    """Find k-nearest neighbors in 5D holographic space using Rust.

    Args:
        query: [x, y, z, w, v] query vector.
        coords: List of {"id", "x", "y", "z", "w", "v"} dicts.
        k: Number of nearest neighbors.
        weights: Optional [wx, wy, wz, ww, wv] axis weights.

    Returns:
        List of {"id", "distance"} dicts, or None if Rust not available.

    """
    if not _RUST_V131:
        return None
    try:
        query_json = json.dumps(query)
        coords_json = json.dumps(coords)
        weights_json = json.dumps(weights) if weights else ""
        result_json = _rs.holographic_nearest_5d(query_json, coords_json, k, weights_json)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust holographic nearest 5D failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v13.1 — MinHash LSH (near-duplicate detection)
# ---------------------------------------------------------------------------

def minhash_find_duplicates(
    keyword_sets: list[list[str]],
    threshold: float = 0.5,
    max_results: int = 500,
) -> list[dict[str, Any]] | None:
    """Find near-duplicate memory pairs using MinHash LSH.

    Args:
        keyword_sets: List of keyword lists (one per memory).
        threshold: Minimum estimated Jaccard similarity.
        max_results: Maximum candidate pairs to return.

    Returns:
        List of {"i", "j", "estimated_jaccard"} dicts, or None.

    """
    if not _RUST_V131:
        return None
    try:
        keywords_json = json.dumps(keyword_sets)
        result_json = _rs.minhash_find_duplicates(keywords_json, threshold, max_results)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust MinHash find_duplicates failed: {e}")
        return None


def minhash_signatures(
    keyword_sets: list[list[str]],
) -> list[list[int]] | None:
    """Compute MinHash signatures for keyword sets.

    Args:
        keyword_sets: List of keyword lists (one per memory).

    Returns:
        List of 128-element signature vectors, or None.

    """
    if not _RUST_V131:
        return None
    try:
        keywords_json = json.dumps(keyword_sets)
        result_json = _rs.minhash_signatures(keywords_json)
        parsed: list[list[int]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust MinHash signatures failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v13.1 — SQLite Accelerator (batch operations)
# ---------------------------------------------------------------------------

def sqlite_batch_update_galactic(
    db_path: str,
    updates: list[dict[str, Any]],
) -> dict[str, Any] | None:
    """Batch-update galactic distances in SQLite using Rust.

    Args:
        db_path: Path to the SQLite database.
        updates: List of {"id", "galactic_distance", "retention_score"} dicts.

    Returns:
        Result dict with "updated" count, or None if Rust not available.

    """
    if not _RUST_V131:
        return None
    try:
        updates_json = json.dumps(updates)
        result_json = _rs.sqlite_batch_update_galactic(db_path, updates_json)
        parsed: dict[str, Any] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust SQLite batch update failed: {e}")
        return None


def sqlite_decay_drift(
    db_path: str,
    drift_amount: float = 0.005,
    max_distance: float = 0.95,
) -> dict[str, Any] | None:
    """Apply decay drift to inactive memories using Rust SQLite accelerator.

    Returns:
        Result dict with "drifted" count, or None.

    """
    if not _RUST_V131:
        return None
    try:
        result_json = _rs.sqlite_decay_drift(db_path, drift_amount, max_distance)
        parsed: dict[str, Any] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust SQLite decay drift failed: {e}")
        return None


def sqlite_fts_search(
    db_path: str,
    query: str,
    limit: int = 50,
    min_importance: float = 0.0,
) -> list[dict[str, Any]] | None:
    """FTS5 search with galactic weighting using Rust SQLite accelerator.

    Returns:
        List of matching memory dicts, or None.

    """
    if not _RUST_V131:
        return None
    try:
        result_json = _rs.sqlite_fts_search(db_path, query, limit, min_importance)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust SQLite FTS search failed: {e}")
        return None


def sqlite_zone_stats(
    db_path: str,
) -> dict[str, Any] | None:
    """Get galactic zone statistics using Rust SQLite accelerator.

    Returns:
        Dict with zone counts and stats, or None.

    """
    if not _RUST_V131:
        return None
    try:
        result_json = _rs.sqlite_zone_stats(db_path)
        parsed: dict[str, Any] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust SQLite zone stats failed: {e}")
        return None


def sqlite_export_for_mining(
    db_path: str,
    max_distance: float = 0.85,
    min_importance: float = 0.3,
    limit: int = 2000,
) -> list[dict[str, Any]] | None:
    """Export memories for association mining using Rust SQLite accelerator.

    Returns:
        List of memory dicts suitable for mining, or None.

    """
    if not _RUST_V131:
        return None
    try:
        result_json = _rs.sqlite_export_for_mining(db_path, max_distance, min_importance, limit)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust SQLite export for mining failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v13.2 — BM25 Full-Text Search Engine
# ---------------------------------------------------------------------------

_RUST_SEARCH = False
try:
    if _rs is not None and hasattr(_rs, "search_build_index"):
        _RUST_SEARCH = True
        logger.debug("Rust BM25 search engine available")
except Exception:
    pass


def rust_search_available() -> bool:
    """Check if Rust BM25 search engine is available."""
    return _RUST_SEARCH


def search_build_index(
    documents: list[dict[str, str]],
) -> str | None:
    """Build a BM25 inverted index from documents.

    Args:
        documents: List of dicts with keys: id, title, content.

    Returns:
        JSON string of index handle/stats, or None if Rust unavailable.

    """
    if not _RUST_SEARCH:
        return None
    try:
        docs_json = json.dumps(documents)
        result: str = _rs.search_build_index(docs_json)
        return result
    except Exception as e:
        logger.debug(f"Rust search_build_index failed: {e}")
        return None


def search_query(
    query: str,
    limit: int = 10,
) -> list[dict[str, Any]] | None:
    """Query the BM25 global index with a text query.
    Call search_build_index() first to populate the index.

    Returns:
        List of {id, score} dicts sorted by relevance, or None.

    """
    if not _RUST_SEARCH:
        return None
    try:
        result_json = _rs.search_query(query, limit)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust search_query failed: {e}")
        return None


def search_fuzzy(
    query: str,
    limit: int = 10,
    max_distance: int = 2,
) -> list[dict[str, Any]] | None:
    """Fuzzy search the global BM25 index with Levenshtein edit distance tolerance.
    Call search_build_index() first to populate the index.

    Returns:
        List of {id, score} dicts, or None.

    """
    if not _RUST_SEARCH:
        return None
    try:
        result_json = _rs.search_fuzzy(query, limit, max_distance)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust search_fuzzy failed: {e}")
        return None


def search_and_query(
    query: str,
    limit: int = 10,
) -> list[dict[str, Any]] | None:
    """Boolean AND query the global BM25 index — all terms must appear.
    Call search_build_index() first to populate the index.

    Returns:
        List of {id, score} dicts, or None.

    """
    if not _RUST_SEARCH:
        return None
    try:
        result_json = _rs.search_and_query(query, limit)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust search_and_query failed: {e}")
        return None


def search_stats() -> dict[str, Any] | None:
    """Get global index statistics (doc count, vocab size, avg doc length).
    Call search_build_index() first to populate the index.

    Returns:
        Dict with index stats, or None.

    """
    if not _RUST_SEARCH:
        return None
    try:
        result_json = _rs.search_stats()
        parsed: dict[str, Any] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust search_stats failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v13.2 — Atomic Rate Limiter
# ---------------------------------------------------------------------------

_RUST_RATE_LIMITER = False
try:
    if _rs is not None and hasattr(_rs, "rate_check"):
        _RUST_RATE_LIMITER = True
        logger.debug("Rust atomic rate limiter available")
except Exception:
    pass


def rust_rate_limiter_available() -> bool:
    """Check if Rust atomic rate limiter is available."""
    return _RUST_RATE_LIMITER


def rate_check(tool_name: str) -> dict[str, Any] | None:
    """Check rate limit for a tool using Rust atomic sliding windows.

    Returns:
        Dict with {allowed: bool, retry_after_ms: int|None} or None.

    """
    if not _RUST_RATE_LIMITER:
        return None
    try:
        # v14: Native tuple return — no JSON serialization
        if hasattr(_rs, "rate_check_native"):
            allowed, retry_ms = _rs.rate_check_native(tool_name)
            return {"allowed": allowed, "retry_after_ms": retry_ms}
        # Fallback to JSON path
        result_json = _rs.rate_check(tool_name)
        parsed: dict[str, Any] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust rate_check failed: {e}")
        return None


def rate_set_override(tool_name: str, rpm: int) -> bool:
    """Set a per-tool RPM override in the Rust rate limiter.

    Returns:
        True if set successfully, False otherwise.

    """
    if not _RUST_RATE_LIMITER:
        return False
    try:
        _rs.rate_set_override(tool_name, rpm)
        return True
    except Exception as e:
        logger.debug(f"Rust rate_set_override failed: {e}")
        return False


def rate_stats() -> dict[str, Any] | None:
    """Get rate limiter statistics from Rust.

    Returns:
        Dict with stats per tool and global, or None.

    """
    if not _RUST_RATE_LIMITER:
        return None
    try:
        # v14: Native dict return — no JSON serialization
        if hasattr(_rs, "rate_stats_native"):
            native_stats = _rs.rate_stats_native()
            if isinstance(native_stats, dict):
                return native_stats
        # Fallback to JSON path
        result_json = _rs.rate_stats()
        parsed: dict[str, Any] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust rate_stats failed: {e}")
        return None


def rate_check_batch(tool_names: list[str]) -> list[dict[str, Any]] | None:
    """Batch check rate limits for N tools in a single FFI call.

    Amortizes Python→Rust crossing overhead: N checks for the cost of 1
    FFI round-trip. At 2-3μs per check, a batch of 100 tools completes
    in ~5μs total vs ~300μs for 100 individual calls.

    Returns:
        List of {tool, allowed, retry_after_ms} dicts, or None.

    """
    if not _RUST_RATE_LIMITER:
        return None
    try:
        # v14: Native tuple-list return — no JSON serialization
        if hasattr(_rs, "rate_check_batch_native"):
            results = _rs.rate_check_batch_native(tool_names)
            return [
                {"tool": tool, "allowed": allowed, "retry_after_ms": retry_ms}
                for tool, allowed, retry_ms in results
            ]
        # Fallback to JSON path
        if not hasattr(_rs, "rate_check_batch"):
            return None
        result_json = _rs.rate_check_batch(tool_names)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust rate_check_batch failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v13.3.2 — Multi-pass Retrieval Pipeline
# ---------------------------------------------------------------------------

_RUST_PIPELINE = False
try:
    if _rs and hasattr(_rs, "retrieval_pipeline"):
        _RUST_PIPELINE = True
        logger.debug("Rust retrieval pipeline available")
except Exception:
    pass


def retrieval_pipeline(
    candidates: list[dict[str, Any]],
    config: dict[str, Any] | None = None,
) -> list[dict[str, Any]] | None:
    """Execute a multi-pass retrieval pipeline in a single FFI call.

    Chains: text scoring → type filter → tag filter → importance rerank
    → holographic proximity boost → deduplication → finalize.

    All stages execute in Rust without returning to Python, eliminating
    N FFI round-trips. 10 chained passes complete in <20μs.

    Args:
        candidates: List of memory dicts with keys:
            id, score, importance, memory_type, tags, age_days, coords
        config: Pipeline config dict with keys:
            query, limit, enable_tag_filter, required_tags, excluded_tags,
            enable_importance_rerank, importance_weight, recency_weight,
            enable_holographic_boost, query_coords, proximity_weight,
            enable_dedup, dedup_threshold, memory_types, min_importance

    Returns:
        List of {id, score, importance} dicts ranked by composite score,
        or None if Rust is unavailable.

    """
    if not _RUST_PIPELINE:
        return None
    try:
        # v14: Native PyList/PyDict — no JSON serialization on either side
        if hasattr(_rs, "retrieval_pipeline_native"):
            native_results = _rs.retrieval_pipeline_native(candidates, config or {})
            if isinstance(native_results, list):
                return [item for item in native_results if isinstance(item, dict)]
        # Fallback to JSON path
        input_data = json.dumps({
            "candidates": candidates,
            "config": config or {},
        })
        result_json = _rs.retrieval_pipeline(input_data)
        parsed: list[dict[str, Any]] = json.loads(result_json)
        return parsed
    except Exception as e:
        logger.debug(f"Rust retrieval_pipeline failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v13.3.2 — Keyword Extraction (replaces Zig ctypes path)
# ---------------------------------------------------------------------------

_RUST_KEYWORDS = False
try:
    if _rs and hasattr(_rs, "keyword_extract"):
        _RUST_KEYWORDS = True
        logger.debug("Rust keyword extraction available")
except Exception:
    pass


def rust_keywords_available() -> bool:
    """Check if Rust keyword extraction is available."""
    return _RUST_KEYWORDS


def keyword_extract(text: str, max_keywords: int = 50) -> set[str] | None:
    """Extract keywords from text using Rust.

    Returns a set of keywords, or None if Rust is unavailable.
    PyO3 zero-copy string borrowing makes this 5-20× faster than
    the Zig ctypes path and competitive with Python for small texts.
    """
    if not _RUST_KEYWORDS:
        return None
    try:
        result = _rs.keyword_extract(text, max_keywords)
        return cast(set[str], result)
    except Exception as e:
        logger.debug(f"Rust keyword_extract failed: {e}")
        return None


def keyword_extract_batch(texts: list[str], max_keywords: int = 50) -> list[set[str]] | None:
    """Batch extract keywords from multiple texts using Rust.

    Returns a list of keyword sets, or None if Rust is unavailable.
    """
    if not _RUST_KEYWORDS:
        return None
    try:
        result = _rs.keyword_extract_batch(texts, max_keywords)
        return cast(list[set[str]], result)
    except Exception as e:
        logger.debug(f"Rust keyword_extract_batch failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v14.5 — Arrow IPC Bridge (zero-copy columnar interchange)
# ---------------------------------------------------------------------------

_RUST_ARROW = False
try:
    if _rs is not None and hasattr(_rs, "arrow_encode_memories"):
        _RUST_ARROW = True
        logger.debug("Rust Arrow IPC bridge available")
except Exception:
    pass


def arrow_available() -> bool:
    """Check if Rust Arrow IPC bridge is available."""
    return _RUST_ARROW


def arrow_encode_memories(memories_json: str) -> bytes | None:
    """Encode memory JSON to Arrow IPC bytes (zero-copy columnar format).

    Input: JSON array of memory objects with fields:
        id, title, content, importance, memory_type, x, y, z, w, v, tags.
    Returns: Arrow IPC file bytes, or None if Rust/Arrow unavailable.
    """
    if not _RUST_ARROW:
        return None
    try:
        return cast(bytes, _rs.arrow_encode_memories(memories_json))
    except Exception as e:
        logger.debug(f"Rust arrow_encode_memories failed: {e}")
        return None


def arrow_decode_memories(ipc_bytes: bytes) -> str | None:
    """Decode Arrow IPC bytes back to memory JSON string.

    Returns: JSON string of memory objects, or None if unavailable.
    """
    if not _RUST_ARROW:
        return None
    try:
        return cast(str, _rs.arrow_decode_memories(ipc_bytes))
    except Exception as e:
        logger.debug(f"Rust arrow_decode_memories failed: {e}")
        return None


def arrow_schema_info() -> dict[str, Any] | None:
    """Get Arrow schema metadata as a dict."""
    if not _RUST_ARROW:
        return None
    try:
        return cast(dict[str, Any], json.loads(_rs.arrow_schema_info()))
    except Exception as e:
        logger.debug(f"Rust arrow_schema_info failed: {e}")
        return None


def arrow_roundtrip_bench(n: int = 1000) -> tuple[int, int, int] | None:
    """Benchmark: encode N memories to Arrow IPC and back.

    Returns (encode_ns, decode_ns, ipc_size_bytes), or None.
    """
    if not _RUST_ARROW:
        return None
    try:
        return cast(tuple[int, int, int], _rs.arrow_roundtrip_bench(n))
    except Exception as e:
        logger.debug(f"Rust arrow_roundtrip_bench failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v14.5 — Tokio Clone Army (massively parallel exploration)
# ---------------------------------------------------------------------------

_RUST_TOKIO_CLONES = False
try:
    if _rs is not None and hasattr(_rs, "tokio_deploy_clones"):
        _RUST_TOKIO_CLONES = True
        logger.debug("Rust Tokio Clone Army available")
except Exception:
    pass


def tokio_clones_available() -> bool:
    """Check if Rust Tokio Clone Army is available."""
    return _RUST_TOKIO_CLONES


def tokio_deploy_clones(
    prompt: str,
    num_clones: int = 100,
    strategies: list[str] | None = None,
) -> dict[str, Any] | None:
    """Deploy a Rust tokio clone army for parallel exploration.

    Args:
        prompt: The exploration prompt.
        num_clones: Number of clones to deploy (1-100,000).
        strategies: List of strategy names. Default: mixed
            (direct, chain_of_thought, analytical, creative, synthesis).

    Returns:
        Dict with keys: winner, total_clones, strategy_votes,
        avg_confidence, total_tokens, elapsed_ms. Or None.
    """
    if not _RUST_TOKIO_CLONES:
        return None
    try:
        result_json = _rs.tokio_deploy_clones(prompt, num_clones, strategies or [])
        return cast(dict[str, Any], json.loads(result_json))
    except Exception as e:
        logger.debug(f"Rust tokio_deploy_clones failed: {e}")
        return None


def tokio_clone_bench(num_clones: int = 1000) -> tuple[float, float] | None:
    """Benchmark: deploy N clones and return (elapsed_ms, clones_per_sec)."""
    if not _RUST_TOKIO_CLONES:
        return None
    try:
        return cast(tuple[float, float], _rs.tokio_clone_bench(num_clones))
    except Exception as e:
        logger.debug(f"Rust tokio_clone_bench failed: {e}")
        return None


def tokio_clone_stats() -> dict[str, Any] | None:
    """Get global Tokio clone army statistics."""
    if not _RUST_TOKIO_CLONES:
        return None
    try:
        return cast(dict[str, Any], json.loads(_rs.tokio_clone_stats()))
    except Exception as e:
        logger.debug(f"Rust tokio_clone_stats failed: {e}")
        return None


# ---------------------------------------------------------------------------
# v14.5 — IPC Bridge (Iceoryx2 shared memory)
# ---------------------------------------------------------------------------

_RUST_IPC = False
try:
    if _rs is not None and hasattr(_rs, "ipc_bridge_status"):
        _RUST_IPC = True
        logger.debug("Rust IPC bridge available")
except Exception:
    pass


def ipc_available() -> bool:
    """Check if Rust IPC bridge is available."""
    return _RUST_IPC


def ipc_status() -> dict[str, Any] | None:
    """Get IPC bridge status (backend, channels, stats)."""
    if not _RUST_IPC:
        return None
    try:
        return cast(dict[str, Any], json.loads(_rs.ipc_bridge_status()))
    except Exception as e:
        logger.debug(f"Rust ipc_bridge_status failed: {e}")
        return None
