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


# ── v15.3 Galactic Telepathy ────────────────────────────────────


def handle_galaxy_transfer(**kwargs: Any) -> dict[str, Any]:
    """Transfer memories between galaxies with coordinate re-mapping and dedup."""
    source = kwargs.get("source") or kwargs.get("source_galaxy")
    target = kwargs.get("target") or kwargs.get("target_galaxy")

    if not source:
        return {"status": "error", "error": "source galaxy name is required"}
    if not target:
        return {"status": "error", "error": "target galaxy name is required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        result = gm.transfer_memories(
            source_galaxy=source,
            target_galaxy=target,
            query=kwargs.get("query"),
            tags=kwargs.get("tags"),
            min_importance=float(kwargs.get("min_importance", 0.0)),
            max_galactic_distance=float(kwargs.get("max_galactic_distance", 1.0)),
            limit=int(kwargs.get("limit", 500)),
            copy=kwargs.get("copy", True),
        )
        return {"status": "success", **result}
    except ValueError as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_merge(**kwargs: Any) -> dict[str, Any]:
    """Merge all memories from a source galaxy into a target galaxy."""
    source = kwargs.get("source") or kwargs.get("source_galaxy")
    target = kwargs.get("target") or kwargs.get("target_galaxy") or "default"

    if not source:
        return {"status": "error", "error": "source galaxy name is required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        result = gm.merge_galaxy(
            source_galaxy=source,
            target_galaxy=target,
            delete_after=kwargs.get("delete_after", False),
        )
        return {"status": "success", **result}
    except ValueError as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_sync(**kwargs: Any) -> dict[str, Any]:
    """Bidirectional sync between two galaxies (content-hash dedup)."""
    galaxy_a = kwargs.get("galaxy_a") or kwargs.get("source")
    galaxy_b = kwargs.get("galaxy_b") or kwargs.get("target")

    if not galaxy_a or not galaxy_b:
        return {"status": "error", "error": "galaxy_a and galaxy_b are required"}

    from whitemagic.core.memory.galaxy_manager import get_galaxy_manager

    try:
        gm = get_galaxy_manager()
        result = gm.sync_galaxies(
            galaxy_a=galaxy_a,
            galaxy_b=galaxy_b,
            tags=kwargs.get("tags"),
            min_importance=float(kwargs.get("min_importance", 0.0)),
        )
        return {"status": "success", **result}
    except ValueError as e:
        return {"status": "error", "error": str(e)}


# ── v15.4 Phylogenetic Lineage ──────────────────────────────────────


def handle_galaxy_lineage(**kwargs: Any) -> dict[str, Any]:
    """Build the phylogenetic lineage tree for a memory (ancestors + descendants)."""
    memory_id = kwargs.get("memory_id")
    if not memory_id:
        return {"status": "error", "error": "memory_id is required"}

    from whitemagic.core.memory.phylogenetics import get_phylogenetics

    try:
        pg = get_phylogenetics()
        tree = pg.build_lineage_tree(
            memory_id=memory_id,
            max_depth=int(kwargs.get("max_depth", 10)),
        )
        return {"status": "success", **tree}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_taxonomy(**kwargs: Any) -> dict[str, Any]:
    """Classify a memory using taxonomic ranks (species, genus, family, order, kingdom)."""
    memory_id = kwargs.get("memory_id")
    if not memory_id:
        return {"status": "error", "error": "memory_id is required"}

    from whitemagic.core.memory.phylogenetics import get_phylogenetics

    try:
        pg = get_phylogenetics()
        rank = pg.classify_memory(
            memory_id=memory_id,
            galaxy_name=kwargs.get("galaxy", "default"),
        )
        return {"status": "success", "full_name": rank.full_name, **rank.to_dict()}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def handle_galaxy_lineage_stats(**kwargs: Any) -> dict[str, Any]:
    """Return statistics about the phylogenetic lineage graph."""
    from whitemagic.core.memory.phylogenetics import get_phylogenetics

    try:
        pg = get_phylogenetics()
        stats = pg.get_stats()
        return {"status": "success", **stats}
    except Exception as e:
        return {"status": "error", "error": str(e)}
