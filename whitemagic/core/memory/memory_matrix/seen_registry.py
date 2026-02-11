"""Seen Registry - Track Everything We've Viewed.

Every time we read a file, look at an image, or process a document,
we record it here. This prevents the cycle of re-reading and forgetting.

Usage:
    from whitemagic.core.memory.memory_matrix import get_seen_registry

    registry = get_seen_registry()
    registry.mark_seen("/path/to/file.md", context="Phase 1 scan")

    if registry.have_seen("/path/to/file.md"):
        last_seen = registry.when_seen("/path/to/file.md")
        logger.info(f"Already seen on {last_seen}")
"""

from __future__ import annotations

import hashlib
import json
import logging
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from threading import Lock
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.fileio import atomic_write, file_lock

logger = logging.getLogger(__name__)

# Singleton instance
_registry_instance: "SeenRegistry" | None = None
_registry_lock = Lock()


@dataclass
class SeenEntry:
    """Record of something we've seen."""

    path: str
    first_seen: str
    last_seen: str
    times_seen: int = 1
    content_hash: str | None = None
    contexts: list[str] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)
    file_type: str = "unknown"
    line_count: int | None = None

    def to_dict(self) -> dict:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict) -> "SeenEntry":
        return cls(**data)


class SeenRegistry:
    """Track everything we've ever seen.

    Persists to disk so we remember across sessions.
    Uses file hashes to detect when content changes.
    """

    def __init__(self, storage_path: Path | None = None):
        self.storage_path = (
            storage_path or (WM_ROOT / "matrix" / "seen_registry.json")
        )
        self.storage_path.parent.mkdir(parents=True, exist_ok=True)
        self._entries: dict[str, SeenEntry] = {}
        self._load()

    def _load(self) -> None:
        """Load registry from disk."""
        if self.storage_path.exists():
            try:
                with file_lock(self.storage_path):
                    data = json.loads(self.storage_path.read_text())
                self._entries = {
                    k: SeenEntry.from_dict(v)
                    for k, v in data.get("entries", {}).items()
                }
            except Exception:
                self._entries = {}

    def _save(self) -> None:
        """Save registry to disk."""
        data = {
            "version": "1.0",
            "updated": datetime.now(timezone.utc).isoformat(),
            "total_entries": len(self._entries),
            "entries": {k: v.to_dict() for k, v in self._entries.items()},
        }
        with file_lock(self.storage_path):
            atomic_write(self.storage_path, json.dumps(data, indent=2))

    def _hash_content(self, path: str) -> str | None:
        """Generate hash of file content."""
        try:
            content = Path(path).read_bytes()
            return hashlib.sha256(content).hexdigest()[:16]
        except Exception:
            return None

    def _detect_type(self, path: str) -> str:
        """Detect file type from extension."""
        ext = Path(path).suffix.lower()
        type_map = {
            ".md": "markdown",
            ".py": "python",
            ".js": "javascript",
            ".ts": "typescript",
            ".json": "json",
            ".yaml": "yaml",
            ".yml": "yaml",
            ".html": "html",
            ".css": "css",
            ".rs": "rust",
            ".hs": "haskell",
            ".png": "image",
            ".jpg": "image",
            ".jpeg": "image",
            ".gif": "image",
            ".svg": "image",
            ".pdf": "pdf",
        }
        return type_map.get(ext, "unknown")

    def _count_lines(self, path: str) -> int | None:
        """Count lines in text file."""
        try:
            return len(Path(path).read_text().splitlines())
        except IOError:
            return None

    def mark_seen(
        self, path: str, context: str | None = None, note: str | None = None,
    ) -> SeenEntry:
        """Mark a file/document as seen.

        Args:
            path: Path to the file
            context: Why we're looking at it (e.g., "Phase 1 scan")
            note: Any notes about what we found

        Returns:
            The SeenEntry record

        """
        now = datetime.now(timezone.utc).isoformat()
        abs_path = str(Path(path).resolve())

        if abs_path in self._entries:
            # Update existing entry
            entry = self._entries[abs_path]
            entry.last_seen = now
            entry.times_seen += 1
            entry.content_hash = self._hash_content(abs_path)
            if context and context not in entry.contexts:
                entry.contexts.append(context)
            if note:
                entry.notes.append(f"[{now}] {note}")
        else:
            # Create new entry
            entry = SeenEntry(
                path=abs_path,
                first_seen=now,
                last_seen=now,
                content_hash=self._hash_content(abs_path),
                contexts=[context] if context else [],
                notes=[f"[{now}] {note}"] if note else [],
                file_type=self._detect_type(abs_path),
                line_count=self._count_lines(abs_path),
            )
            self._entries[abs_path] = entry

        self._save()
        return entry

    def have_seen(self, path: str) -> bool:
        """Check if we've seen a file before."""
        abs_path = str(Path(path).resolve())
        return abs_path in self._entries

    def when_seen(self, path: str) -> str | None:
        """Get when we last saw a file."""
        abs_path = str(Path(path).resolve())
        entry = self._entries.get(abs_path)
        return entry.last_seen if entry else None

    def get_entry(self, path: str) -> SeenEntry | None:
        """Get full entry for a file."""
        abs_path = str(Path(path).resolve())
        return self._entries.get(abs_path)

    def has_changed(self, path: str) -> bool:
        """Check if file content has changed since we last saw it."""
        abs_path = str(Path(path).resolve())
        entry = self._entries.get(abs_path)
        if not entry or not entry.content_hash:
            return True
        current_hash = self._hash_content(abs_path)
        return current_hash != entry.content_hash

    def get_recent(self, limit: int = 50) -> list[SeenEntry]:
        """Get most recently seen entries."""
        sorted_entries = sorted(
            self._entries.values(), key=lambda e: e.last_seen, reverse=True,
        )
        return sorted_entries[:limit]

    def get_by_type(self, file_type: str) -> list[SeenEntry]:
        """Get all entries of a specific type."""
        return [e for e in self._entries.values() if e.file_type == file_type]

    def get_by_context(self, context: str) -> list[SeenEntry]:
        """Get all entries with a specific context."""
        return [e for e in self._entries.values() if context in e.contexts]

    def search(self, query: str) -> list[SeenEntry]:
        """Search entries by path or notes."""
        query_lower = query.lower()
        results = []
        for entry in self._entries.values():
            if query_lower in entry.path.lower():
                results.append(entry)
            elif any(query_lower in note.lower() for note in entry.notes):
                results.append(entry)
        return results

    def stats(self) -> dict[str, Any]:
        """Get registry statistics."""
        by_type: dict[str, int] = {}
        total_views = 0
        for entry in self._entries.values():
            by_type[entry.file_type] = by_type.get(entry.file_type, 0) + 1
            total_views += entry.times_seen

        return {
            "total_files": len(self._entries),
            "total_views": total_views,
            "by_type": by_type,
            "storage_path": str(self.storage_path),
        }

    def clear(self) -> None:
        """Clear all entries (use with caution!)."""
        self._entries = {}
        self._save()


def get_seen_registry() -> SeenRegistry:
    """Get singleton SeenRegistry instance."""
    global _registry_instance
    with _registry_lock:
        if _registry_instance is None:
            _registry_instance = SeenRegistry()
        return _registry_instance
