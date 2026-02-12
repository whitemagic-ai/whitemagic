"""Windsurf conversation reader tool handlers."""
from typing import Any


def handle_windsurf_list_conversations(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import WindsurfConversationReader
    reader = WindsurfConversationReader()
    return {"status": "success", "conversations": reader.list_conversations()}


def handle_windsurf_read_conversation(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import WindsurfConversationReader
    reader = WindsurfConversationReader()
    conv = reader.read_conversation(kwargs.get("path", ""))
    return {"status": "success", **conv.to_dict()}


def handle_windsurf_export_conversation(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import WindsurfConversationReader
    reader = WindsurfConversationReader()
    content = reader.export_conversation(kwargs.get("path", ""), kwargs.get("format", "markdown"))
    return {"status": "success", "content": content}


def handle_windsurf_search_conversations(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import WindsurfConversationReader
    reader = WindsurfConversationReader()
    results = reader.search_conversations(kwargs.get("query", ""))
    return {"status": "success", "results": results}


def handle_windsurf_stats(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.archaeology import WindsurfConversationReader
    reader = WindsurfConversationReader()
    return {"status": "success", **reader.stats()}
