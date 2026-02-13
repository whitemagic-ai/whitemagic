"""Handlers for galaxy.backup and galaxy.restore MCP tools."""
from __future__ import annotations

import logging
import tarfile
from datetime import datetime
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


def handle_galaxy_backup(**kwargs: Any) -> dict[str, Any]:
    """Backup a galaxy (or all memories) to a .tar.gz archive."""
    from whitemagic.config.paths import WM_ROOT

    galaxy = kwargs.get("galaxy")
    output_path = kwargs.get("output_path")

    state_root = WM_ROOT
    memory_dir = state_root / "memory"

    if not memory_dir.exists():
        return {"status": "error", "error": "No memory directory found"}

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    if galaxy:
        galaxy_dir = memory_dir / "galaxies" / galaxy
        if not galaxy_dir.exists():
            return {"status": "error", "error": f"Galaxy '{galaxy}' not found"}
        source = galaxy_dir
        label = f"galaxy:{galaxy}"
        default_name = f"wm_backup_{galaxy}_{timestamp}.tar.gz"
    else:
        source = memory_dir
        label = "all_memories"
        default_name = f"wm_backup_all_{timestamp}.tar.gz"

    if output_path:
        out = Path(output_path)
    else:
        backup_dir = state_root / "backups"
        backup_dir.mkdir(parents=True, exist_ok=True)
        out = backup_dir / default_name

    try:
        with tarfile.open(str(out), "w:gz") as tar:
            tar.add(str(source), arcname=source.name)
        size_mb = out.stat().st_size / (1024 * 1024)
        return {
            "status": "success",
            "backup_path": str(out),
            "size_mb": round(size_mb, 2),
            "scope": label,
            "timestamp": timestamp,
        }
    except Exception as e:
        return {"status": "error", "error": f"Backup failed: {e}"}


def handle_galaxy_restore(**kwargs: Any) -> dict[str, Any]:
    """Restore a galaxy from a .tar.gz backup archive."""
    from whitemagic.config.paths import WM_ROOT

    archive_path = kwargs.get("archive_path", "")
    force = kwargs.get("force", False)

    if not archive_path:
        return {"status": "error", "error": "archive_path is required"}

    archive = Path(archive_path)
    if not archive.exists():
        return {"status": "error", "error": f"Archive not found: {archive}"}

    state_root = WM_ROOT
    memory_dir = state_root / "memory"

    if memory_dir.exists() and not force:
        return {
            "status": "error",
            "error": "Memory directory already exists. Set force=true to overwrite.",
        }

    try:
        memory_dir.mkdir(parents=True, exist_ok=True)
        with tarfile.open(str(archive), "r:gz") as tar:
            tar.extractall(str(memory_dir.parent))
        return {
            "status": "success",
            "restored_to": str(memory_dir),
            "archive": str(archive),
        }
    except Exception as e:
        return {"status": "error", "error": f"Restore failed: {e}"}
