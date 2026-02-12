
from typing import Any


def garden_list(**kwargs: Any) -> dict[str, Any]:
    """List all gardens."""
    from whitemagic.gardens import list_gardens
    gardens = list_gardens()
    return {"gardens": gardens, "count": len(gardens)}


def garden_activate(garden_name: str, **kwargs: Any) -> dict[str, Any]:
    """Activate a specific garden."""
    from whitemagic.gardens import get_garden
    garden = get_garden(garden_name)
    return {
        "name": garden_name,
        "activated": True,
        "description": str(garden)[:100] if garden else None,
    }


def garden_resonance_map(**kwargs: Any) -> dict[str, Any]:
    """Get garden resonance map."""
    from whitemagic.gardens.garden_resonance import get_garden_resonance_map
    resonance_map = get_garden_resonance_map()
    return {"resonance_map": resonance_map}


def manage_gardens(
    action: str = "list",
    garden_name: str | None = None,
    limit: int = 50,
    depth: int = 2,
    **kwargs: Any,
) -> dict[str, Any]:
    """Unified garden management for MCP.

    Actions: list, activate, get_memories, resonance_map, trigger_cascade.
    """
    action = (action or "list").lower()

    if action == "list":
        return garden_list(**kwargs)

    if action == "activate":
        if not garden_name:
            return {"error": "garden_name is required for activate"}
        return garden_activate(garden_name, **kwargs)

    if action == "resonance_map":
        return garden_resonance_map(**kwargs)

    if action == "trigger_cascade":
        if not garden_name:
            return {"error": "garden_name is required for trigger_cascade"}
        from whitemagic.gardens.garden_resonance import trigger_garden_cascade
        cascaded = trigger_garden_cascade(garden_name, f"Cascade triggered from {garden_name}")
        return {
            "starting_garden": garden_name,
            "cascaded_gardens": cascaded,
            "count": len(cascaded),
            "depth_requested": depth,
        }

    if action == "get_memories":
        if not garden_name:
            return {"error": "garden_name is required for get_memories"}
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager()
        results = manager.search_memories(query=None, tags=[garden_name])
        limited_results = results[:limit] if results else []
        return {
            "garden": garden_name,
            "results": limited_results,
            "count": len(limited_results),
            "total": len(results) if results else 0,
        }

    return {"error": f"Unknown garden action: {action}"}


def validate_integrations(quick_check: bool = True, **kwargs: Any) -> dict[str, Any]:
    """Validate WhiteMagic system integrations."""
    from whitemagic.core.bridge.system import check_integrations_health
    return check_integrations_health(component="integrations", quick_check=quick_check)

def protect_context(
    operation: str = "update_memory",
    filename: str | None = None,
    memory_id: str | None = None,
    title: str | None = None,
    content: str | None = None,
    tags: list[str] | None = None,
    permanent: bool = False,
    backupPath: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Memory context protection, updates, and restoration."""
    from whitemagic.core.bridge.memory import memory_delete, memory_update

    operation = (operation or "update_memory").lower()
    target_filename = filename or memory_id

    if operation == "update_memory":
        if not target_filename:
            return {"error": "filename or memory_id required for update_memory"}
        return memory_update(filename=target_filename, title=title, content=content, tags=tags)
    elif operation == "delete_memory":
        if not target_filename:
            return {"error": "filename or memory_id required for delete_memory"}
        return memory_delete(memory_id=target_filename, permanent=permanent)
    elif operation == "windsurf_restore":
        return {"status": "restore_not_implemented", "message": "Windsurf restore feature coming soon"}
    return {"error": f"Unknown protect_context operation: {operation}"}

def garden_garden_status(**kwargs: Any) -> dict[str, Any]:
    """Bridge alias for garden status."""
    return garden_list(**kwargs)

def garden_garden_activate(**kwargs: Any) -> dict[str, Any]:
    """Bridge alias for garden activation."""
    return garden_activate(**kwargs)
