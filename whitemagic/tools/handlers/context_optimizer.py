"""MCP handlers for the Context Window Optimizer."""

from typing import Any


def handle_context_pack(**kwargs: Any) -> dict[str, Any]:
    """Pack memories into an optimized context window for LLM calls."""
    from whitemagic.ai.context_optimizer import get_context_optimizer

    query = kwargs.get("query", "")
    token_budget = int(kwargs.get("token_budget", 8000))
    limit = int(kwargs.get("limit", 50))

    if not query:
        return {"status": "error", "error": "query is required"}

    opt = get_context_optimizer()
    packed = opt.pack_memories(query, token_budget=token_budget, limit=limit)

    return {
        "status": "success",
        "total_tokens": packed.total_tokens,
        "budget": packed.budget,
        "utilization": packed.utilization,
        "items_packed": len(packed.items),
        "dropped": packed.dropped_count,
        "strategy": packed.strategy,
        "context": opt.render(packed),
        "item_ids": [i.id for i in packed.items],
    }


def handle_context_status(**kwargs: Any) -> dict[str, Any]:
    """Get Context Optimizer status."""
    from whitemagic.ai.context_optimizer import get_context_optimizer
    opt = get_context_optimizer()
    return {"status": "success", **opt.status()}
