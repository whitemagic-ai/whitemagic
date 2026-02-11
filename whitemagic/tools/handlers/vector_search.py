"""MCP handlers for Vector Memory Search."""

from typing import Any


def handle_vector_search(**kwargs: Any) -> dict[str, Any]:
    """Search memories by semantic similarity using embeddings."""
    from whitemagic.core.memory.vector_search import get_vector_search
    query = kwargs.get("query", "")
    if not query:
        return {"status": "error", "error": "query is required"}
    limit = int(kwargs.get("limit", 10))
    vs = get_vector_search()
    results = vs.search(query, limit=limit)
    return {
        "status": "success",
        "results": [r.to_dict() for r in results],
        "count": len(results),
        "index_size": vs.index_count(),
    }


def handle_vector_index(**kwargs: Any) -> dict[str, Any]:
    """Index a memory for vector search."""
    from whitemagic.core.memory.vector_search import get_vector_search
    memory_id = kwargs.get("memory_id", "")
    content = kwargs.get("content", "")
    title = kwargs.get("title", "")
    if not memory_id or not content:
        return {"status": "error", "error": "memory_id and content are required"}
    vs = get_vector_search()
    vs.index_memory(memory_id, content, title=title)
    return {"status": "success", "indexed": memory_id, "index_size": vs.index_count()}


def handle_vector_status(**kwargs: Any) -> dict[str, Any]:
    """Get vector search engine status."""
    from whitemagic.core.memory.vector_search import get_vector_search
    vs = get_vector_search()
    return {"status": "success", **vs.status()}
