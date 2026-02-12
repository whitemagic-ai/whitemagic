"""MCP handlers for Zig SIMD Acceleration."""

from typing import Any


def handle_simd_cosine(**kwargs: Any) -> dict[str, Any]:
    """Compute cosine similarity between two vectors using SIMD acceleration."""
    from whitemagic.core.acceleration.simd_cosine import cosine_similarity
    a = kwargs.get("a", [])
    b = kwargs.get("b", [])
    if not a or not b:
        return {"status": "error", "error": "a and b vectors are required"}
    score = cosine_similarity(a, b)
    return {"status": "success", "similarity": round(score, 6), "dim": len(a)}


def handle_simd_batch(**kwargs: Any) -> dict[str, Any]:
    """Batch cosine similarity — compare query against multiple vectors."""
    from whitemagic.core.acceleration.simd_cosine import batch_cosine
    query = kwargs.get("query", [])
    vectors = kwargs.get("vectors", [])
    if not query or not vectors:
        return {"status": "error", "error": "query and vectors are required"}
    scores = batch_cosine(query, vectors)
    return {"status": "success", "scores": [round(s, 6) for s in scores], "count": len(scores)}


def handle_simd_status(**kwargs: Any) -> dict[str, Any]:
    """Get SIMD acceleration status — Zig library, lane width, backend."""
    from whitemagic.core.acceleration.simd_cosine import simd_status
    return {"status": "success", **simd_status()}
