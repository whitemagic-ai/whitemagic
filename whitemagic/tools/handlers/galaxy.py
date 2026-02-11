"""Galaxy management tool handlers.

Provides MCP tools for creating, switching, listing, and ingesting
into project-scoped memory galaxies.
"""

from typing import Any


def handle_galaxy_create(**kwargs: Any) -> dict[str, Any]:
    """Create a new galaxy (project-scoped memory database)."""
    name = kwargs.get("name")
    if not name:
        return {"status": "error", "error": "name is required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        info = gm.create_galaxy(
            name=name,
            project_path=kwargs.get("path"),
            description=kwargs.get("description", ""),
            tags=kwargs.get("tags", []),
        )
        return {
            "status": "success",
            "message": f"Galaxy '{name}' created",
            **info.to_dict(),
        }
    except ValueError as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_switch(**kwargs: Any) -> dict[str, Any]:
    """Switch the active galaxy."""
    name = kwargs.get("name")
    if not name:
        return {"status": "error", "error": "name is required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        info = gm.switch_galaxy(name)
        return {
            "status": "success",
            "message": f"Switched to galaxy '{name}'",
            **info.to_dict(),
        }
    except ValueError as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_list(**kwargs: Any) -> dict[str, Any]:
    """List all known galaxies."""
    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    gm = get_galaxy_manager()
    galaxies = gm.list_galaxies()
    return {
        "status": "success",
        "active": gm.get_active().name,
        "count": len(galaxies),
        "galaxies": galaxies,
    }


def handle_galaxy_status(**kwargs: Any) -> dict[str, Any]:
    """Get galaxy manager status."""
    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    gm = get_galaxy_manager()
    return {"status": "success", **gm.status()}


def handle_galaxy_ingest(**kwargs: Any) -> dict[str, Any]:
    """Ingest files from a directory into a galaxy."""
    name = kwargs.get("name") or kwargs.get("galaxy")
    source_path = kwargs.get("source_path") or kwargs.get("path")

    if not name:
        return {"status": "error", "error": "name (galaxy name) is required"}
    if not source_path:
        return {"status": "error", "error": "source_path is required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        result = gm.ingest_files(
            galaxy_name=name,
            source_path=source_path,
            pattern=kwargs.get("pattern", "**/*.md"),
            max_files=kwargs.get("max_files", 500),
            tags=kwargs.get("tags", []),
        )
        return {"status": "success", **result}
    except (ValueError, FileNotFoundError) as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_delete(**kwargs: Any) -> dict[str, Any]:
    """Remove a galaxy from the registry (database file is preserved)."""
    name = kwargs.get("name")
    if not name:
        return {"status": "error", "error": "name is required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        gm.delete_galaxy(name)
        return {"status": "success", "message": f"Galaxy '{name}' removed from registry"}
    except ValueError as e:
        return {"status": "error", "error": str(e)}
