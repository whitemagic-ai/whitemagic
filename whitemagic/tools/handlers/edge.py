"""Edge inference tool handlers."""
from typing import Any


def handle_edge_infer(**kwargs: Any) -> dict[str, Any]:
    query = kwargs.get("query", "")
    if not query:
        return {"status": "error", "message": "query is required"}
    from whitemagic.edge.inference import get_edge_inference
    result = get_edge_inference().infer(query)
    return {
        "status": "success",
        "query": result.query,
        "answer": result.answer,
        "confidence": result.confidence,
        "method": result.method,
        "latency_ms": result.latency_ms,
        "tokens_equivalent": result.tokens_equivalent,
        "from_cache": getattr(result, "from_cache", False),
    }


def handle_edge_add_rule(**kwargs: Any) -> dict[str, Any]:
    rule_id = kwargs.get("id", "")
    pattern = kwargs.get("pattern", "")
    response = kwargs.get("response", "")
    confidence = kwargs.get("confidence", 0.9)
    if not rule_id or not pattern or not response:
        return {"status": "error", "message": "id, pattern, and response are required"}
    from whitemagic.edge.inference import CompiledRule, get_edge_inference
    get_edge_inference().add_rule(
        CompiledRule(
            id=str(rule_id),
            pattern=str(pattern),
            response=str(response),
            confidence=float(confidence) if confidence is not None else 0.9,
        ),
    )
    return {"status": "success", "rule_added": rule_id}


def handle_edge_batch_infer(**kwargs: Any) -> dict[str, Any]:
    queries = kwargs.get("queries", [])
    if not isinstance(queries, list) or not queries:
        return {"status": "error", "message": "queries must be a non-empty list"}
    from whitemagic.edge.inference import get_edge_inference
    engine = get_edge_inference()
    results = []
    for q in queries:
        item = engine.infer(str(q))
        results.append({
            "query": item.query,
            "answer": item.answer,
            "confidence": item.confidence,
            "method": item.method,
            "latency_ms": item.latency_ms,
            "tokens_equivalent": item.tokens_equivalent,
            "from_cache": getattr(item, "from_cache", False),
        })
    return {"status": "success", "results": results}


def handle_edge_stats(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.edge.inference import get_edge_inference
    return {"status": "success", **get_edge_inference().stats}
