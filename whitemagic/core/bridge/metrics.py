
from typing import Any


def track_metric(category: str, metric: str, value: float, context: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Track a quantitative metric."""
    from datetime import datetime

    # Store metric in memory system for persistence
    try:
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager()

        metric_data = {
            "category": category,
            "metric": metric,
            "value": value,
            "context": context or "default",
            "timestamp": datetime.now().isoformat(),
        }

        # Create a memory entry for the metric
        manager.create_memory(
            title=f"Metric: {category}/{metric}",
            content=f"Value: {value}\nContext: {context or 'N/A'}",
            memory_type="short_term",
            tags=["metric", category, metric],
        )

        return {
            "tracked": True,
            "stored": True,
            **metric_data,
        }
    except Exception as e:
        # Fallback if memory system not available
        return {
            "tracked": True,
            "stored": False,
            "category": category,
            "metric": metric,
            "value": value,
            "context": context,
            "timestamp": datetime.now().isoformat(),
            "error": str(e),
            "note": "Metric logged but not persisted to memory",
        }


def get_metrics_summary(
    time_range: str = "last_7_days",
    categories: list[str] | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Get metrics summary."""
    try:
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager()

        # Search for metric memories
        results = manager.search_memories("metric")[:100]

        metrics_by_category: dict[str, list[dict[str, Any]]] = {}
        for result in results:
            if "tags" in result.get("entry", {}):
                tags = result["entry"]["tags"]
                if "metric" in tags:
                    # Parse category from tags
                    for tag in tags:
                        if tag != "metric":
                            if tag not in metrics_by_category:
                                metrics_by_category[tag] = []
                            metrics_by_category[tag].append(result)
                            break

        return {
            "time_range": time_range,
            "categories": list(metrics_by_category.keys()),
            "metrics": metrics_by_category,
            "total_metrics": len(results),
            "summary": f"Found {len(results)} metrics across {len(metrics_by_category)} categories",
        }
    except Exception as e:
        return {
            "time_range": time_range,
            "categories": categories or [],
            "metrics": {},
            "error": str(e),
            "summary": "Metrics retrieval failed - memory system may not be initialized",
        }
