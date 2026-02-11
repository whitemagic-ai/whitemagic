
from typing import Any

from whitemagic.core.bridge.utils import ensure_string


def memory_create(
    title: str,
    content: str,
    memory_type: str = "long_term",
    tags: list[str] | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Create a new memory."""
    from pathlib import PosixPath

    from whitemagic.core.memory.manager import MemoryManager

    # Handle parameter aliases from MCP schema
    if "type" in kwargs:
        memory_type = kwargs.pop("type")

    # Clean up kwargs
    kwargs.pop("operation", None)

    manager = MemoryManager()

    memory = manager.create_memory(
        title=ensure_string(title) or "Untitled",
        content=ensure_string(content) or "",
        memory_type=ensure_string(memory_type) or "long_term",
        tags=tags or [],
        extra_fields=kwargs,  # Pass remaining kwargs as metadata
    )

    # Memory might be returned as a PosixPath or dict
    if isinstance(memory, dict):
        return {
            "id": memory.get("id", memory.get("slug", "unknown")),
            "title": memory.get("title", title),
            "created": memory.get("created", "unknown"),
            "path": str(memory.get("path", "")),
        }
    elif isinstance(memory, PosixPath):
        return {
            "id": "unknown",
            "title": title,
            "created": "unknown",
            "path": str(memory),
            "message": "Memory created, path returned",
        }
    else:
        return {
            "id": "unknown",
            "title": title,
            "created": "unknown",
            "path": "",
            "message": "Memory created but format unexpected, details unavailable",
        }


def memory_search(
    query: str | None = None,
    memory_type: str | None = None,
    tags: list[str] | None = None,
    limit: int = 10,
    mode: str | None = None,
    use_rust: bool = True,
    **kwargs: Any,
) -> dict[str, Any]:
    """Search memories."""
    from whitemagic.core.memory.manager import MemoryManager
    manager = MemoryManager()

    # Handle parameter aliases
    if "type" in kwargs:
        memory_type = kwargs.pop("type")

    kwargs.pop("operation", None)

    # Try Rust acceleration for high-frequency search if query is provided
    if use_rust and query and not tags and not memory_type:
        try:
            import whitemagic_rs
            # Get all memories for Rust search (this is the current limitation of this bridge)
            # Future enhancement: maintain a Rust-side index
            all_mems = manager.unified.list_recent(limit=1000)
            mem_tuples = [(m.id, m.content) for m in all_mems if m.content]

            if mem_tuples:
                rust_results = whitemagic_rs.rust_search_memories(
                    query,
                    mem_tuples,
                    threshold=kwargs.get("threshold", 0.1),
                    limit=limit,
                )

                # Convert back to legacy format
                results = []
                for mem_id, score in rust_results:
                    mem = manager.unified.recall(mem_id)
                    if mem:
                        results.append({
                            "entry": manager._memory_to_dict(mem),
                            "preview": str(mem.content)[:200] if mem.content else "",
                            "score": score,
                        })

                return {"results": results, "count": len(results), "query": query, "method": "rust_simd"}
        except (ImportError, AttributeError):
            pass

    # Fallback to standard search
    # Filter kwargs to only pass what MemoryManager accepts
    search_kwargs = {}
    if "include_archived" in kwargs:
        search_kwargs["include_archived"] = kwargs["include_archived"]
    if "include_content" in kwargs:
        search_kwargs["include_content"] = kwargs["include_content"]

    result = manager.search_memories(
        query=query,
        memory_type=memory_type,
        tags=tags,
        **search_kwargs,
    )

    # Ensure consistent return type (dict with results key)
    if isinstance(result, list):
        # Truncate content to prevent token overflow (max 500 chars per memory)
        truncated_results = []
        for entry in result[:limit]:  # Respect limit parameter
            if isinstance(entry, dict):
                truncated_mem = entry.copy()
                if "content" in truncated_mem and len(truncated_mem["content"]) > 500:
                    truncated_mem["content"] = truncated_mem["content"][:500] + "... (truncated)"
                truncated_results.append(truncated_mem)
            else:
                truncated_results.append(entry)
        return {"results": truncated_results, "count": len(truncated_results), "query": query}
    elif isinstance(result, dict):
        # Also truncate if result dict contains results array
        if "results" in result and isinstance(result["results"], list):
            truncated_results = []
            for entry in result["results"][:limit]:
                if isinstance(entry, dict):
                    truncated_mem = entry.copy()
                    if "content" in truncated_mem and len(truncated_mem["content"]) > 500:
                        truncated_mem["content"] = truncated_mem["content"][:500] + "... (truncated)"
                    truncated_results.append(truncated_mem)
                else:
                    truncated_results.append(entry)
            result["results"] = truncated_results
            result["count"] = len(truncated_results)
        return result
    else:
        return {"results": [], "count": 0, "query": query, "error": "Unexpected result type"}


def memory_read(memory_id: str | None = None, filename: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Read a memory by id/filename."""
    from whitemagic.core.memory.manager import MemoryManager
    manager = MemoryManager()

    target = memory_id or filename
    if not target:
        return {"error": "memory_id or filename required"}

    result = manager.get_memory(target, include_metadata=True)
    return result


def memory_update(
    memory_id: str | None = None,
    filename: str | None = None,  # Add filename parameter for compatibility
    title: str | None = None,
    content: str | None = None,
    tags: list[str] | None = None,
    add_tags: list[str] | None = None,
    remove_tags: list[str] | None = None,
    memory_type: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Update a memory by id/filename."""
    from whitemagic.core.memory.manager import MemoryManager
    manager = MemoryManager()
    # Use filename if provided, otherwise use memory_id
    target_filename = filename or memory_id
    if not target_filename:
        return {"error": "Either memory_id or filename must be provided"}
    # Don't pass memory_type to update_memory if MemoryManager doesn't accept it
    return manager.update_memory(
        filename=target_filename,
        title=title,
        content=content,
        tags=tags,
        add_tags=add_tags,
        remove_tags=remove_tags,
    )


def memory_delete(
    memory_id: str | None = None,
    filename: str | None = None,
    permanent: bool = False,
    **kwargs: Any,
) -> dict[str, Any]:
    """Delete a memory by id/filename."""
    from whitemagic.core.memory.manager import MemoryManager
    manager = MemoryManager()

    target = memory_id or filename
    if not target:
        return {"error": "memory_id or filename required"}

    return manager.delete_memory(filename=target, permanent=permanent)


def memory_list(limit: int = 20, memory_type: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """List recent memories."""
    from whitemagic.core.memory.manager import MemoryManager

    # Handle parameter aliases
    if not memory_type and "type" in kwargs:
        memory_type = kwargs["type"]

    manager = MemoryManager()
    results = manager.read_recent_memories(memory_type=memory_type or "short_term", limit=limit)
    return {"results": results, "count": len(results)}


def manage_memories(operation: str = "search", **kwargs: Any) -> dict[str, Any]:
    """Unified memory management for MCP."""
    op = (operation or "search").lower()
    if op == "create":
        # Map 'type' to 'memory_type' for the underlying function
        if "type" in kwargs:
            kwargs["memory_type"] = kwargs.pop("type")
        return memory_create(**kwargs)
    if op == "search":
        return memory_search(**kwargs)
    if op == "read":
        return memory_read(**kwargs)
    if op == "update":
        return memory_update(**kwargs)
    if op == "delete":
        return memory_delete(**kwargs)
    if op == "list":
        return memory_list(**kwargs)
    return {"error": f"Unknown memory operation: {operation}"}


def parallel_search(
    query: str | None = None,
    path: str | None = None,
    extensions: list[str] | None = None,
    max_results: int = 20,
    **kwargs: Any,
) -> dict[str, Any]:
    """Perform parallel search across files/memories.
    Delegates to Rust acceleration if available, otherwise standard search.
    """
    try:
        from whitemagic.core.bridge.rust import rust_parallel_grep
        # If parameters align with rust_parallel_grep (path, patterns/query)
        if query:
            return rust_parallel_grep(
                patterns=[query],
                paths=[path] if path else ["./whitemagic"], # Default to core
                extensions=extensions,
                **kwargs,
            )
    except (ImportError, Exception):
        pass

    # Fallback to memory search if no path specified
    if not path:
        return memory_search(query=query, limit=max_results, **kwargs)

    return {"error": "parallel_search requires Rust acceleration for filesystem search or valid path"}
