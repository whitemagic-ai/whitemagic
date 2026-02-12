"""MCP handlers for memory export/import."""

from typing import Any


def handle_export_memories(**kwargs: Any) -> dict[str, Any]:
    """Export memories in JSON, CSV, Markdown, or ZIP format."""
    from whitemagic.core.memory.unified import get_unified_memory
    from whitemagic.tools.export.manager import (
        ExportImportManager,
        ExportRequest,
        MemoryExport,
    )

    fmt = kwargs.get("format", "json")
    tags = kwargs.get("tags")
    memory_type = kwargs.get("memory_type")
    search = kwargs.get("search")
    limit = int(kwargs.get("limit", 100))
    include_metadata = kwargs.get("include_metadata", True)

    # Build filters
    filters = {}
    if tags:
        filters["tags"] = tags if isinstance(tags, list) else [tags]
    if memory_type:
        filters["memory_type"] = memory_type.upper()
    if search:
        filters["search"] = search

    # Load memories
    um = get_unified_memory()
    raw = um.list_recent(limit=limit)
    memories = [
        MemoryExport(
            id=m.id,
            title=m.title or "",
            content=str(m.content),
            memory_type=m.memory_type.name,
            tags=list(m.tags),
            metadata=m.metadata,
            created_at=m.created_at.isoformat(),
            updated_at=m.accessed_at.isoformat(),
        )
        for m in raw
    ]

    mgr = ExportImportManager()
    request = ExportRequest(
        format=fmt,
        filters=filters or None,
        include_metadata=include_metadata,
        compress=bool(kwargs.get("compress", False)),
    )
    result = mgr.export_memories(memories, request)
    return {"status": "success", **result}


def handle_import_memories(**kwargs: Any) -> dict[str, Any]:
    """Import memories from JSON, CSV, or Markdown data."""
    from whitemagic.tools.export.manager import ExportImportManager, ImportRequest

    fmt = kwargs.get("format", "json")
    data = kwargs.get("data", "")
    merge_strategy = kwargs.get("merge_strategy", "skip")
    validate_only = kwargs.get("validate_only", False)

    if not data:
        return {"status": "error", "message": "data is required"}

    mgr = ExportImportManager()
    request = ImportRequest(
        format=fmt,
        data=data,
        merge_strategy=merge_strategy,
        validate_only=validate_only,
    )
    result = mgr.import_memories(request)
    return {"status": "success" if result.get("success") else "error", **result}
