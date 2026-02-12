"""MCP handlers for Knowledge Graph Extraction."""

from typing import Any


def handle_kg_extract(**kwargs: Any) -> dict[str, Any]:
    """Extract entities and relations from text into the knowledge graph."""
    from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
    source_id = kwargs.get("source_id", "")
    text = kwargs.get("text", "")
    if not text:
        return {"status": "error", "error": "text is required"}
    kg = get_knowledge_graph()
    result = kg.extract_from_text(source_id or "manual", text)
    return {"status": "success", **result}


def handle_kg_query(**kwargs: Any) -> dict[str, Any]:
    """Query an entity and its connections in the knowledge graph."""
    from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
    name = kwargs.get("name", "")
    if not name:
        return {"status": "error", "error": "name is required"}
    kg = get_knowledge_graph()
    return {"status": "success", **kg.query_entity(name)}


def handle_kg_top(**kwargs: Any) -> dict[str, Any]:
    """Get top entities by mention count."""
    from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
    limit = int(kwargs.get("limit", 20))
    kg = get_knowledge_graph()
    return {"status": "success", "entities": kg.top_entities(limit=limit)}


def handle_kg_status(**kwargs: Any) -> dict[str, Any]:
    """Get knowledge graph status."""
    from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
    kg = get_knowledge_graph()
    return {"status": "success", **kg.status()}
