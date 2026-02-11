from collections.abc import Iterable
from typing import Any, cast

from whitemagic.core.memory.unified import recall, remember

from .unified_api import call_tool


def create_memory(
    title: str,
    content: str,
    memory_type: str = "short_term",
    tags: Iterable[str] | None = None,
    **kwargs: Any,
) -> Any:
    """Create a memory entry using the unified memory backend."""
    return remember(content=content, title=title, type=memory_type, tags=set(tags or []), **kwargs)


def search_memories(
    query: str,
    limit: int = 20,
    memory_type: str | None = None,
    tags: Iterable[str] | None = None,
    include_archived: bool = False,
    **kwargs: Any,
) -> list[Any]:
    """Search memories using the unified memory backend."""
    return cast("list[Any]", recall(
        query=query,
        limit=limit,
        type=memory_type,
        tags=set(tags or []),
        include_archived=include_archived,
        **kwargs,
    ))


__all__ = ["call_tool", "create_memory", "search_memories"]
