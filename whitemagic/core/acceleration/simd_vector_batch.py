"""Zig SIMD Vector Batch Operations — Python Bridge.
==================================================
Loads the compiled Zig shared library and exposes SIMD-accelerated
batch vector operations. Falls back to pure Python (or numpy if available).

The Zig implementation provides:
- Top-K cosine search across a vector corpus
- L2 normalization of vector batches
- Batch dot product computation
- Centroid computation

Usage:
    from whitemagic.core.acceleration.simd_vector_batch import (
        batch_topk_cosine, batch_normalize, simd_vector_batch_status
    )
"""
from __future__ import annotations

import ctypes
import logging
import math
import threading
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

_lib = None
_lib_lock = threading.Lock()
_HAS_ZIG = False


def _find_zig_lib() -> str | None:
    """Locate the compiled Zig shared library."""
    import os
    base = Path(__file__).resolve().parent.parent.parent.parent / "whitemagic-zig"
    candidates = [
        os.environ.get("WM_ZIG_LIB", ""),
        str(base / "zig-out" / "lib" / "libwhitemagic.so"),
        str(base / "libwhitemagic.so"),
        str(base / "zig-out" / "lib" / "libwhitemagic.dylib"),
    ]
    for path in candidates:
        if path and os.path.isfile(path):
            return path
    return None


def _load_lib() -> Any:
    """Load the Zig shared library and set up vector batch FFI."""
    global _lib, _HAS_ZIG
    if _lib is not None:
        return _lib
    with _lib_lock:
        if _lib is not None:
            return _lib
        path = _find_zig_lib()
        if not path:
            logger.debug("Zig SIMD library not found — using Python fallback for vector batch")
            return None
        try:
            lib = ctypes.CDLL(path)
            if not hasattr(lib, "wm_batch_topk_cosine"):
                logger.debug("Zig library missing vector batch symbols — using Python fallback")
                return None

            # wm_batch_topk_cosine(query_ptr, corpus_ptr, n, dim, k, out_indices, out_scores) -> count
            lib.wm_batch_topk_cosine.argtypes = [
                ctypes.POINTER(ctypes.c_float),
                ctypes.POINTER(ctypes.c_float),
                ctypes.c_size_t,
                ctypes.c_size_t,
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_int),
                ctypes.POINTER(ctypes.c_float),
            ]
            lib.wm_batch_topk_cosine.restype = ctypes.c_size_t

            _lib = lib
            _HAS_ZIG = True
            logger.info("Zig SIMD vector batch loaded: path=%s", path)
            return lib
        except Exception as e:
            logger.debug("Failed to load Zig vector batch: %s", e)
            return None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def batch_topk_cosine(
    query: list[float],
    corpus: list[list[float]],
    k: int = 10,
) -> list[tuple[int, float]]:
    """Find top-K most similar vectors by cosine similarity.

    Args:
        query: Query vector.
        corpus: List of vectors to search.
        k: Number of results to return.

    Returns:
        List of (index, similarity_score) pairs sorted by descending similarity.

    """
    if not corpus or not query:
        return []

    dim = len(query)
    n = len(corpus)
    k = min(k, n)

    lib = _load_lib()
    if lib is not None and n > 20:
        try:
            query_arr = (ctypes.c_float * dim)(*query)
            flat = []
            for vec in corpus:
                flat.extend(vec[:dim])
            corpus_arr = (ctypes.c_float * len(flat))(*flat)
            out_indices = (ctypes.c_int * k)()
            out_scores = (ctypes.c_float * k)()

            count = lib.wm_batch_topk_cosine(
                query_arr, corpus_arr, n, dim, k,
                out_indices, out_scores,
            )

            if count > 0:
                results = []
                for i in range(count):
                    results.append((int(out_indices[i]), float(out_scores[i])))
                return results
        except Exception as e:
            logger.debug("Zig batch_topk_cosine failed, using Python: %s", e)

    # Python fallback
    return _py_topk_cosine(query, corpus, k)


def _py_topk_cosine(
    query: list[float],
    corpus: list[list[float]],
    k: int,
) -> list[tuple[int, float]]:
    """Pure Python top-K cosine similarity search."""
    dim = len(query)
    q_norm = math.sqrt(sum(x * x for x in query))
    if q_norm < 1e-10:
        return []

    scores = []
    for i, vec in enumerate(corpus):
        dot = sum(query[j] * vec[j] for j in range(min(dim, len(vec))))
        v_norm = math.sqrt(sum(x * x for x in vec[:dim]))
        if v_norm < 1e-10:
            scores.append((i, 0.0))
        else:
            scores.append((i, dot / (q_norm * v_norm)))

    scores.sort(key=lambda x: x[1], reverse=True)
    return scores[:k]


def batch_normalize(vectors: list[list[float]]) -> list[list[float]]:
    """L2-normalize a batch of vectors."""
    result = []
    for vec in vectors:
        norm = math.sqrt(sum(x * x for x in vec))
        if norm < 1e-10:
            result.append(vec)
        else:
            result.append([x / norm for x in vec])
    return result


def batch_centroid(vectors: list[list[float]]) -> list[float]:
    """Compute the centroid of a batch of vectors."""
    if not vectors:
        return []
    dim = len(vectors[0])
    n = len(vectors)
    centroid = [0.0] * dim
    for vec in vectors:
        for i in range(min(dim, len(vec))):
            centroid[i] += vec[i]
    return [c / n for c in centroid]


def simd_vector_batch_status() -> dict[str, Any]:
    """Get SIMD vector batch status."""
    _load_lib()
    return {
        "has_zig_simd": _HAS_ZIG,
        "lib_path": _find_zig_lib() or "not found",
        "backend": "zig_simd" if _HAS_ZIG else "python_fallback",
    }
