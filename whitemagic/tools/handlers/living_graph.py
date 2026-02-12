"""Living Graph tool handlers — v14.0 graph topology, hybrid recall, surprise gate."""
import logging
from typing import Any

logger = logging.getLogger(__name__)


def handle_hybrid_recall(**kwargs: Any) -> dict[str, Any]:
    """Multi-hop graph-aware memory recall combining anchor search + graph walk."""
    query = kwargs.get("query", "")
    if not query:
        return {"status": "error", "message": "query is required"}

    hops = int(kwargs.get("hops", 2))
    anchor_limit = int(kwargs.get("anchor_limit", 5))
    final_limit = int(kwargs.get("final_limit", 10))

    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        results = um.hybrid_recall(
            query=query,
            hops=hops,
            anchor_limit=anchor_limit,
            final_limit=final_limit,
        )

        # Track context reuse in telemetry
        try:
            from whitemagic.core.monitoring.telemetry import get_telemetry
            get_telemetry().record_context_reuse(hit=len(results) > 0)
        except Exception:
            pass

        return {
            "status": "success",
            "query": query,
            "result_count": len(results),
            "results": results,
        }
    except Exception as e:
        return {"status": "error", "message": str(e)}


def handle_graph_topology(**kwargs: Any) -> dict[str, Any]:
    """Graph topology introspection — centrality, communities, bridges."""
    action = kwargs.get("action", "summary")

    try:
        from whitemagic.core.memory.graph_engine import get_graph_engine
        engine = get_graph_engine()

        if action == "rebuild":
            sample_limit = int(kwargs.get("sample_limit", 50000))
            result = engine.rebuild(sample_limit=sample_limit)
            return {"status": "success", "action": "rebuild", **result}

        elif action == "centrality":
            snapshot = engine.centrality_snapshot()
            return {"status": "success", "action": "centrality", "snapshot": snapshot.to_dict()}

        elif action == "communities":
            communities = engine.detect_communities()
            return {
                "status": "success",
                "action": "communities",
                "count": len(communities),
                "communities": [c.to_dict() for c in communities[:20]],
            }

        elif action == "bridges":
            top_n = int(kwargs.get("top_n", 10))
            bridges = engine.find_bridge_nodes(top_n=top_n)
            return {
                "status": "success",
                "action": "bridges",
                "count": len(bridges),
                "bridges": bridges,
            }

        elif action == "echo_chambers":
            sigma = float(kwargs.get("sigma_threshold", 2.0))
            chambers = engine.detect_echo_chambers(sigma_threshold=sigma)
            return {
                "status": "success",
                "action": "echo_chambers",
                "count": len(chambers),
                "echo_chambers": [ec.to_dict() for ec in chambers],
            }

        else:
            # Default: summary
            return {"status": "success", "action": "summary", **engine.summary()}

    except Exception as e:
        return {"status": "error", "message": str(e)}


def handle_surprise_stats(**kwargs: Any) -> dict[str, Any]:
    """Surprise gate statistics — novelty detection metrics."""
    try:
        from whitemagic.core.memory.surprise_gate import get_surprise_gate
        gate = get_surprise_gate()
        return {"status": "success", **gate.get_stats()}
    except Exception as e:
        return {"status": "error", "message": str(e)}


def handle_graph_walk(**kwargs: Any) -> dict[str, Any]:
    """Execute a multi-hop graph walk from seed memory IDs."""
    seed_ids = kwargs.get("seed_ids", [])
    if not seed_ids:
        return {"status": "error", "message": "seed_ids is required (list of memory IDs)"}

    if isinstance(seed_ids, str):
        seed_ids = [s.strip() for s in seed_ids.split(",")]

    hops = int(kwargs.get("hops", 2))
    top_k = int(kwargs.get("top_k", 10))

    try:
        from whitemagic.core.memory.graph_walker import get_graph_walker
        walker = get_graph_walker()
        result = walker.walk(seed_ids=seed_ids, hops=hops, top_k=top_k)
        return {
            "status": "success",
            "walk": result.to_dict(),
        }
    except Exception as e:
        return {"status": "error", "message": str(e)}


def handle_entity_resolve(**kwargs: Any) -> dict[str, Any]:
    """Run embedding-based entity resolution (dedup) on the memory store."""
    similarity_threshold = float(kwargs.get("similarity_threshold", 0.92))
    batch_limit = int(kwargs.get("batch_limit", 500))

    try:
        from whitemagic.core.memory.consolidation import get_consolidator
        consolidator = get_consolidator()
        result = consolidator.resolve_entities(
            similarity_threshold=similarity_threshold,
            batch_limit=batch_limit,
        )
        return {"status": "success", **result}
    except Exception as e:
        return {"status": "error", "message": str(e)}
