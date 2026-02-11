"""Simplified Tool Aliases — Human-friendly shorthand for common operations.

Maps intuitive verbs to their underlying Gana tools:
  remember  → gana_neck → create_memory
  recall    → gana_winnowing_basket → hybrid_recall
  think     → gana_three_stars → reasoning.bicameral
  check     → gana_root → health_report
  dream     → gana_abundance → dream
"""

from typing import Any


def handle_remember(**kwargs: Any) -> dict[str, Any]:
    """Shorthand: create a memory. Equivalent to gana_neck → create_memory."""
    from whitemagic.tools.unified_api import call_tool
    content = kwargs.get("content")
    if not content:
        return {"status": "error", "error": "content is required"}
    return call_tool(
        "create_memory",
        content=content,
        title=kwargs.get("title", ""),
        tags=kwargs.get("tags", []),
        importance=kwargs.get("importance", 0.5),
    )


def handle_recall(**kwargs: Any) -> dict[str, Any]:
    """Shorthand: search memories. Equivalent to gana_winnowing_basket → hybrid_recall."""
    from whitemagic.tools.unified_api import call_tool
    query = kwargs.get("query") or kwargs.get("content") or kwargs.get("q")
    if not query:
        return {"status": "error", "error": "query is required"}
    return call_tool(
        "hybrid_recall",
        query=query,
        limit=kwargs.get("limit", 5),
    )


def handle_think(**kwargs: Any) -> dict[str, Any]:
    """Shorthand: bicameral reasoning. Equivalent to gana_three_stars → reasoning.bicameral."""
    from whitemagic.tools.unified_api import call_tool
    topic = kwargs.get("topic") or kwargs.get("question") or kwargs.get("content")
    if not topic:
        return {"status": "error", "error": "topic is required"}
    perspectives = kwargs.get("perspectives", ["analytical", "creative", "critical"])
    return call_tool(
        "reasoning.bicameral",
        topic=topic,
        perspectives=perspectives,
    )


def handle_check(**kwargs: Any) -> dict[str, Any]:
    """Shorthand: system health check. Equivalent to gana_root → health_report."""
    from whitemagic.tools.unified_api import call_tool
    return call_tool("health_report")


def handle_dream(**kwargs: Any) -> dict[str, Any]:
    """Shorthand: run dream cycle. Equivalent to gana_abundance → dream."""
    from whitemagic.tools.unified_api import call_tool
    return call_tool("dream")
