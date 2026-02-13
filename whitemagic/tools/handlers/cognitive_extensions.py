"""Handlers for v15.6 cognitive extension tools.

New modules:
- Cross-encoder reranking
- Working memory capacity model
- Memory reconsolidation (labile state)
- Incremental community maintenance
"""

from __future__ import annotations

from typing import Any


# ---------------------------------------------------------------------------
# Cross-Encoder Reranking
# ---------------------------------------------------------------------------

def handle_rerank(
    query: str = "",
    results: list[dict[str, Any]] | None = None,
    top_k: int = 10,
    strategy: str = "auto",
    **_kwargs: Any,
) -> dict[str, Any]:
    """Rerank search results using cross-encoder or lexical fallback."""
    from whitemagic.core.intelligence.reranker import rerank, get_status

    if not query:
        return {"status": "ok", **get_status()}

    if not results:
        return {"status": "error", "error": "No results to rerank"}

    ranked = rerank(query, results, top_k=top_k, strategy=strategy)
    return {
        "status": "ok",
        "reranked": [
            {
                "memory_id": r.memory_id,
                "title": r.title,
                "original_score": round(r.original_score, 4),
                "rerank_score": round(r.rerank_score, 4),
                "combined_score": round(r.combined_score, 4),
            }
            for r in ranked
        ],
        "count": len(ranked),
        "strategy": strategy,
    }


def handle_rerank_status(**_kwargs: Any) -> dict[str, Any]:
    """Get reranker status."""
    from whitemagic.core.intelligence.reranker import get_status

    return {"status": "ok", **get_status()}


# ---------------------------------------------------------------------------
# Working Memory
# ---------------------------------------------------------------------------

def handle_working_memory_attend(
    memory_id: str = "",
    content: str = "",
    title: str = "",
    importance: float = 0.5,
    **_kwargs: Any,
) -> dict[str, Any]:
    """Bring a memory into working memory focus."""
    from whitemagic.core.intelligence.working_memory import get_working_memory

    if not memory_id:
        return {"status": "error", "error": "memory_id required"}

    wm = get_working_memory()
    chunk = wm.attend(memory_id, content=content, title=title, importance=importance)
    return {
        "status": "ok",
        "chunk": chunk.to_dict(),
        "slots_used": len(wm.get_active_ids()),
        "capacity": wm.capacity,
    }


def handle_working_memory_context(
    max_tokens: int | None = None,
    **_kwargs: Any,
) -> dict[str, Any]:
    """Get current working memory contents for prompt injection."""
    from whitemagic.core.intelligence.working_memory import get_working_memory

    wm = get_working_memory()
    context = wm.get_context(max_tokens=max_tokens)
    return {
        "status": "ok",
        "chunks": context,
        "count": len(context),
        "capacity": wm.capacity,
    }


def handle_working_memory_status(**_kwargs: Any) -> dict[str, Any]:
    """Get working memory status."""
    from whitemagic.core.intelligence.working_memory import get_working_memory

    return {"status": "ok", **get_working_memory().get_status()}


# ---------------------------------------------------------------------------
# Memory Reconsolidation
# ---------------------------------------------------------------------------

def handle_reconsolidation_mark(
    memory_id: str = "",
    content: str = "",
    tags: list[str] | None = None,
    query: str = "",
    **_kwargs: Any,
) -> dict[str, Any]:
    """Mark a memory as labile (modifiable) after retrieval."""
    from whitemagic.core.intelligence.reconsolidation import get_reconsolidation_engine

    if not memory_id:
        return {"status": "error", "error": "memory_id required"}

    engine = get_reconsolidation_engine()
    lm = engine.mark_labile(memory_id, content=content, tags=tags or [], query=query)
    return {
        "status": "ok",
        "memory_id": lm.memory_id,
        "labile": True,
        "updates_pending": len(lm.updates),
    }


def handle_reconsolidation_update(
    memory_id: str = "",
    new_context: str | None = None,
    new_tags: list[str] | None = None,
    annotation: str | None = None,
    **_kwargs: Any,
) -> dict[str, Any]:
    """Update a labile memory with new context."""
    from whitemagic.core.intelligence.reconsolidation import get_reconsolidation_engine

    if not memory_id:
        return {"status": "error", "error": "memory_id required"}

    engine = get_reconsolidation_engine()
    updated = engine.update_labile(memory_id, new_context=new_context, new_tags=new_tags, annotation=annotation)
    return {
        "status": "ok" if updated else "error",
        "updated": updated,
        "error": None if updated else "Memory not labile or expired",
    }


def handle_reconsolidation_status(**_kwargs: Any) -> dict[str, Any]:
    """Get reconsolidation engine status."""
    from whitemagic.core.intelligence.reconsolidation import get_reconsolidation_engine

    return {"status": "ok", **get_reconsolidation_engine().get_status()}


# ---------------------------------------------------------------------------
# Community Maintenance
# ---------------------------------------------------------------------------

def handle_community_propagate(
    memory_id: str = "",
    neighbors: list[list[Any]] | None = None,
    memory_tags: list[str] | None = None,
    **_kwargs: Any,
) -> dict[str, Any]:
    """Propagate community label from neighbors to a new memory."""
    from whitemagic.core.intelligence.community_maintenance import get_community_maintainer

    if not memory_id:
        return {"status": "error", "error": "memory_id required"}

    maintainer = get_community_maintainer()
    # Convert [[id, weight], ...] to [(id, weight), ...]
    neighbor_tuples = [(n[0], float(n[1])) for n in (neighbors or []) if len(n) >= 2]
    result = maintainer.propagate_label(memory_id, neighbor_tuples, memory_tags=memory_tags)
    return {
        "status": "ok",
        "memory_id": result.memory_id,
        "action": result.action,
        "assigned_community": result.assigned_community,
        "confidence": round(result.confidence, 3),
        "neighbor_count": result.neighbor_count,
    }


def handle_community_status(**_kwargs: Any) -> dict[str, Any]:
    """Get community maintenance status."""
    from whitemagic.core.intelligence.community_maintenance import get_community_maintainer

    return {"status": "ok", **get_community_maintainer().get_status()}


def handle_community_health(**_kwargs: Any) -> dict[str, Any]:
    """Check community health — detect oversized or orphaned communities."""
    from whitemagic.core.intelligence.community_maintenance import get_community_maintainer

    return {"status": "ok", **get_community_maintainer().check_health()}
