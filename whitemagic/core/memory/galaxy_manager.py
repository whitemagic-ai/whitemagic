"""Multi-Galaxy Memory Manager
================================
Enables project-scoped memory databases ("galaxies") so WhiteMagic can
maintain separate knowledge bases for different projects, archives, or
domains while sharing the same cognitive infrastructure.

Architecture:
- Each galaxy is a separate SQLite database with its own holographic index
- A galaxy registry (JSON) tracks all known galaxies
- One galaxy is "active" at a time for tool dispatch
- The "core" galaxy ships with WhiteMagic (quickstart/tutorial memories)
- Users can create galaxies for any project folder

Usage via MCP:
  gana_void → tool: galaxy.create   args: {name, path, description}
  gana_void → tool: galaxy.switch   args: {name}
  gana_void → tool: galaxy.list
  gana_void → tool: galaxy.status
  gana_void → tool: galaxy.ingest   args: {name, source_path, pattern}
"""

from __future__ import annotations

import json
import logging
import os
import threading
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any

from whitemagic.config.paths import MEMORY_DIR, WM_ROOT

logger = logging.getLogger(__name__)

# Registry file location
_REGISTRY_PATH = WM_ROOT / "galaxies.json"


@dataclass
class GalaxyInfo:
    """Metadata for a single galaxy."""

    name: str
    db_path: str
    description: str = ""
    project_path: str | None = None
    created_at: float = field(default_factory=time.time)
    memory_count: int = 0
    last_accessed: float = field(default_factory=time.time)
    tags: list[str] = field(default_factory=list)
    is_core: bool = False

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)

    @classmethod
    def from_dict(cls, d: dict[str, Any]) -> GalaxyInfo:
        return cls(**{k: v for k, v in d.items() if k in cls.__dataclass_fields__})


class GalaxyManager:
    """Manages multiple memory galaxies (project-scoped databases).

    Thread-safe singleton that maintains a registry of galaxies and
    provides switching between them.
    """

    _instance: GalaxyManager | None = None
    _lock = threading.Lock()

    def __init__(self) -> None:
        self._galaxies: dict[str, GalaxyInfo] = {}
        self._active_galaxy: str = "default"
        self._memory_instances: dict[str, Any] = {}  # Lazy UnifiedMemory per galaxy
        self._load_registry()

    @classmethod
    def get_instance(cls) -> GalaxyManager:
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = cls()
        return cls._instance

    # ── Registry persistence ────────────────────────────────────────

    def _load_registry(self) -> None:
        """Load galaxy registry from disk."""
        if _REGISTRY_PATH.exists():
            try:
                data = json.loads(_REGISTRY_PATH.read_text(encoding="utf-8"))
                for name, info_dict in data.get("galaxies", {}).items():
                    self._galaxies[name] = GalaxyInfo.from_dict(info_dict)
                self._active_galaxy = data.get("active", "default")
            except Exception as e:
                logger.warning(f"Failed to load galaxy registry: {e}")

        # Ensure "default" galaxy always exists
        if "default" not in self._galaxies:
            default_db = str(MEMORY_DIR / "whitemagic.db")
            self._galaxies["default"] = GalaxyInfo(
                name="default",
                db_path=default_db,
                description="Primary WhiteMagic galaxy — system knowledge and personal memories",
                is_core=False,
            )
            self._save_registry()

    def _save_registry(self) -> None:
        """Persist galaxy registry to disk."""
        data = {
            "active": self._active_galaxy,
            "galaxies": {name: info.to_dict() for name, info in self._galaxies.items()},
        }
        try:
            _REGISTRY_PATH.parent.mkdir(parents=True, exist_ok=True)
            _REGISTRY_PATH.write_text(json.dumps(data, indent=2, default=str), encoding="utf-8")
        except Exception as e:
            logger.error(f"Failed to save galaxy registry: {e}")

    # ── Galaxy CRUD ─────────────────────────────────────────────────

    def create_galaxy(
        self,
        name: str,
        project_path: str | None = None,
        description: str = "",
        tags: list[str] | None = None,
    ) -> GalaxyInfo:
        """Create a new galaxy with its own database."""
        if name in self._galaxies:
            raise ValueError(f"Galaxy '{name}' already exists")

        # Sanitize name for filesystem
        safe_name = "".join(c if c.isalnum() or c in "-_" else "_" for c in name)

        # Galaxy DB lives in its own subdirectory
        galaxy_dir = MEMORY_DIR / "galaxies" / safe_name
        galaxy_dir.mkdir(parents=True, exist_ok=True)
        db_path = str(galaxy_dir / "whitemagic.db")

        info = GalaxyInfo(
            name=name,
            db_path=db_path,
            description=description or f"Galaxy for {project_path or name}",
            project_path=project_path,
            tags=tags or [],
        )

        self._galaxies[name] = info
        self._save_registry()

        # Pre-initialize the database
        um = self._get_memory(name)
        info.memory_count = 0

        logger.info(f"Created galaxy '{name}' at {db_path}")
        return info

    def delete_galaxy(self, name: str) -> bool:
        """Remove a galaxy from the registry (does NOT delete the database file)."""
        if name == "default":
            raise ValueError("Cannot delete the default galaxy")
        if name not in self._galaxies:
            raise ValueError(f"Galaxy '{name}' not found")
        if self._active_galaxy == name:
            self._active_galaxy = "default"

        # Remove from memory cache
        self._memory_instances.pop(name, None)
        del self._galaxies[name]
        self._save_registry()
        return True

    def list_galaxies(self) -> list[dict[str, Any]]:
        """List all known galaxies with their metadata."""
        result = []
        for name, info in sorted(self._galaxies.items()):
            d = info.to_dict()
            d["is_active"] = name == self._active_galaxy
            # Try to get live memory count
            try:
                um = self._get_memory(name)
                stats = um.backend.get_stats()
                d["memory_count"] = stats.get("total_memories", 0)
                info.memory_count = d["memory_count"]
            except Exception:
                pass
            result.append(d)
        return result

    def switch_galaxy(self, name: str) -> GalaxyInfo:
        """Switch the active galaxy."""
        if name not in self._galaxies:
            raise ValueError(f"Galaxy '{name}' not found. Available: {list(self._galaxies.keys())}")

        self._active_galaxy = name
        self._galaxies[name].last_accessed = time.time()
        self._save_registry()

        # Reset the global singleton so next get_unified_memory() uses the new galaxy
        self._reset_global_memory(name)

        logger.info(f"Switched to galaxy '{name}'")
        return self._galaxies[name]

    def get_active(self) -> GalaxyInfo:
        """Get the currently active galaxy."""
        return self._galaxies.get(self._active_galaxy, self._galaxies["default"])

    def get_galaxy(self, name: str) -> GalaxyInfo | None:
        """Get galaxy info by name."""
        return self._galaxies.get(name)

    # ── Memory instance management ──────────────────────────────────

    def _get_memory(self, name: str) -> Any:
        """Get or create a UnifiedMemory instance for a galaxy."""
        if name not in self._memory_instances:
            info = self._galaxies.get(name)
            if not info:
                raise ValueError(f"Galaxy '{name}' not found")

            from whitemagic.core.memory.unified import UnifiedMemory

            db_path = Path(info.db_path)
            base_path = db_path.parent
            self._memory_instances[name] = UnifiedMemory(base_path=base_path)

        return self._memory_instances[name]

    def get_active_memory(self) -> Any:
        """Get the UnifiedMemory instance for the active galaxy."""
        return self._get_memory(self._active_galaxy)

    def _reset_global_memory(self, galaxy_name: str) -> None:
        """Reset the global get_unified_memory() singleton to use a different galaxy."""
        try:
            import whitemagic.core.memory.unified as um_module

            info = self._galaxies[galaxy_name]
            db_path = Path(info.db_path)
            base_path = db_path.parent

            # Replace the singleton
            new_um = um_module.UnifiedMemory(base_path=base_path)
            um_module._unified_memory_instance = new_um  # type: ignore[attr-defined]

            # Cache it locally too
            self._memory_instances[galaxy_name] = new_um
        except Exception as e:
            logger.error(f"Failed to reset global memory to galaxy '{galaxy_name}': {e}")

    # ── Galaxy status ───────────────────────────────────────────────

    def status(self) -> dict[str, Any]:
        """Get overall galaxy manager status."""
        galaxies = self.list_galaxies()
        return {
            "active_galaxy": self._active_galaxy,
            "total_galaxies": len(self._galaxies),
            "galaxies": galaxies,
            "registry_path": str(_REGISTRY_PATH),
        }

    # ── Ingestion ───────────────────────────────────────────────────

    def ingest_files(
        self,
        galaxy_name: str,
        source_path: str,
        pattern: str = "**/*.md",
        max_files: int = 500,
        tags: list[str] | None = None,
    ) -> dict[str, Any]:
        """Ingest files from a directory into a galaxy's memory store.

        Reads text files matching the glob pattern and stores each as a memory.
        """
        if galaxy_name not in self._galaxies:
            raise ValueError(f"Galaxy '{galaxy_name}' not found")

        um = self._get_memory(galaxy_name)
        source = Path(source_path).expanduser().resolve()

        if not source.exists():
            raise FileNotFoundError(f"Source path not found: {source}")

        files = list(source.glob(pattern))[:max_files]
        ingested = 0
        errors = 0
        skipped = 0

        base_tags = set(tags or [])
        base_tags.add(f"galaxy:{galaxy_name}")
        base_tags.add("ingested")

        for f in files:
            try:
                content = f.read_text(encoding="utf-8", errors="replace")
                if len(content.strip()) < 10:
                    skipped += 1
                    continue

                # Truncate very large files
                if len(content) > 50_000:
                    content = content[:50_000] + "\n\n[... truncated ...]"

                file_tags = base_tags | {f"source:{f.suffix.lstrip('.')}"}
                relative = str(f.relative_to(source)) if f.is_relative_to(source) else f.name

                um.store(
                    content=content,
                    title=relative,
                    tags=file_tags,
                    importance=0.4,
                    metadata={
                        "source_path": str(f),
                        "relative_path": relative,
                        "file_size": f.stat().st_size,
                        "galaxy": galaxy_name,
                    },
                )
                ingested += 1
            except Exception as e:
                logger.warning(f"Failed to ingest {f}: {e}")
                errors += 1

        # Update memory count
        try:
            stats = um.backend.get_stats()
            self._galaxies[galaxy_name].memory_count = stats.get("total_memories", 0)
            self._save_registry()
        except Exception:
            pass

        return {
            "galaxy": galaxy_name,
            "source_path": str(source),
            "pattern": pattern,
            "files_found": len(files),
            "ingested": ingested,
            "skipped": skipped,
            "errors": errors,
        }


def get_galaxy_manager() -> GalaxyManager:
    """Get the global GalaxyManager singleton."""
    return GalaxyManager.get_instance()
