"""Semantic File System Watcher.

Purpose: Monitor file system changes and integrate with archaeology/memory.
Based on the insight: "Files organized by content, not folders"

Features:
- Watch configurable directories for changes
- Extract metadata on file save (type, size, modified)
- Generate text summaries for embedding/search
- Store in archaeology registry
- Auto-tag with detected themes
- Emit Gan Ying events for file changes

Integration points:
- whitemagic.archaeology.FileArchaeologist
- whitemagic.archaeology.WisdomExtractor
- whitemagic.core.resonance (emit events)

Requires: pip install watchdog (optional dependency)
"""
from __future__ import annotations

import hashlib
import json
import logging
import re
import threading
from collections.abc import Callable
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from enum import Enum, auto
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)

# Singleton instance
_watcher_instance: "SemanticFileWatcher" | None = None
_watcher_lock = threading.Lock()


class FileEventType(Enum):
    """Types of file system events."""

    CREATED = auto()
    MODIFIED = auto()
    DELETED = auto()
    MOVED = auto()


@dataclass
class FileEvent:
    """Represents a file system event."""

    event_type: FileEventType
    path: str
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    old_path: str | None = None  # For MOVED events
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return {
            "event_type": self.event_type.name,
            "path": self.path,
            "timestamp": self.timestamp,
            "old_path": self.old_path,
            "metadata": self.metadata,
        }


@dataclass
class WatchConfig:
    """Configuration for a watched directory."""

    path: str = ""
    recursive: bool = True
    patterns: list[str] = field(default_factory=lambda: ["*.py", "*.md", "*.json", "*.yaml", "*.yml", "*.ts", "*.js"])
    ignore_patterns: list[str] = field(default_factory=lambda: ["__pycache__", ".git", "node_modules", ".pytest_cache", "*.pyc"])
    auto_track_archaeology: bool = True
    emit_gan_ying: bool = True
    extract_themes: bool = True

    # Default patterns for static access
    DEFAULT_PATTERNS = ["*.py", "*.md", "*.json", "*.yaml", "*.yml", "*.ts", "*.js"]
    DEFAULT_IGNORE = ["__pycache__", ".git", "node_modules", ".pytest_cache", "*.pyc"]

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


class SemanticFileWatcher:
    """Semantic file system watcher.

    Monitors directories for changes and integrates with
    the archaeology and memory systems.
    """

    # File type mappings for metadata extraction
    FILE_TYPES = {
        ".py": "python",
        ".md": "markdown",
        ".json": "json",
        ".yaml": "yaml",
        ".yml": "yaml",
        ".ts": "typescript",
        ".js": "javascript",
        ".rs": "rust",
        ".go": "go",
        ".html": "html",
        ".css": "css",
        ".sql": "sql",
        ".sh": "shell",
        ".toml": "toml",
        ".txt": "text",
    }

    def __init__(self, config_path: Path | None = None) -> None:
        self.config_path = config_path or (WM_ROOT / "watcher_config.json")
        self.config_path.parent.mkdir(parents=True, exist_ok=True)

        self._watches: dict[str, WatchConfig] = {}
        self._event_history: list[FileEvent] = []
        self._callbacks: list[Callable[[FileEvent], None]] = []
        self._running = False
        self._observer: Any | None = None
        self._thread: threading.Thread | None = None

        # Load saved config
        self._load_config()

        # Check for watchdog availability
        self._watchdog_available = self._check_watchdog()

    def _check_watchdog(self) -> bool:
        """Check if watchdog is available."""
        try:
            from importlib.util import find_spec
            return find_spec("watchdog") is not None
        except ImportError:
            return False

    def _load_config(self) -> None:
        """Load configuration from disk."""
        if self.config_path.exists():
            try:
                data = json.loads(self.config_path.read_text())
                for path, config in data.get("watches", {}).items():
                    self._watches[path] = WatchConfig(**config)
            except Exception:
                self._watches = {}

    def _save_config(self) -> None:
        """Save configuration to disk."""
        data = {
            "version": "1.0",
            "updated": datetime.now(timezone.utc).isoformat(),
            "watches": {path: config.to_dict() for path, config in self._watches.items()},
        }
        self.config_path.write_text(json.dumps(data, indent=2))

    # =========================================================================
    # Watch Management
    # =========================================================================

    def add_watch(
        self,
        path: str,
        recursive: bool = True,
        patterns: list[str] | None = None,
        ignore_patterns: list[str] | None = None,
        **kwargs: Any,
    ) -> bool:
        """Add a directory to watch.

        Args:
            path: Directory path to watch
            recursive: Watch subdirectories
            patterns: File patterns to watch (e.g., ["*.py", "*.md"])
            ignore_patterns: Patterns to ignore (e.g., ["__pycache__"])
            **kwargs: Additional WatchConfig options

        Returns:
            True if watch was added successfully

        """
        abs_path = str(Path(path).resolve())

        if not Path(abs_path).is_dir():
            return False

        config = WatchConfig(
            path=abs_path,
            recursive=recursive,
            patterns=patterns or WatchConfig.DEFAULT_PATTERNS,
            ignore_patterns=ignore_patterns or WatchConfig.DEFAULT_IGNORE,
            **kwargs,
        )

        self._watches[abs_path] = config
        self._save_config()

        # If running, restart to pick up new watch
        if self._running:
            self.stop()
            self.start()

        return True

    def remove_watch(self, path: str) -> bool:
        """Remove a directory from watching."""
        abs_path = str(Path(path).resolve())

        if abs_path in self._watches:
            del self._watches[abs_path]
            self._save_config()

            # Restart if running
            if self._running:
                self.stop()
                self.start()

            return True
        return False

    def list_watches(self) -> list[dict[str, Any]]:
        """List all configured watches."""
        return [
            {"path": path, **config.to_dict()}
            for path, config in self._watches.items()
        ]

    # =========================================================================
    # Event Handling
    # =========================================================================

    def _extract_metadata(self, path: str) -> dict[str, Any]:
        """Extract metadata from a file."""
        try:
            p = Path(path)
            stat = p.stat()

            metadata = {
                "filename": p.name,
                "extension": p.suffix.lower(),
                "file_type": self.FILE_TYPES.get(p.suffix.lower(), "unknown"),
                "size_bytes": stat.st_size,
                "modified": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                "created": datetime.fromtimestamp(stat.st_ctime).isoformat(),
            }

            # Add content hash for text files
            if metadata["file_type"] != "unknown":
                try:
                    content = p.read_bytes()
                    metadata["content_hash"] = hashlib.sha256(content).hexdigest()[:16]
                    metadata["line_count"] = len(content.decode("utf-8", errors="ignore").splitlines())
                except Exception:
                    pass

            return metadata
        except Exception:
            return {"error": "Could not extract metadata"}

    def _detect_themes(self, path: str) -> list[str]:
        """Detect themes/topics from file content."""
        try:
            from whitemagic.archaeology import get_wisdom_extractor
            extractor = get_wisdom_extractor()

            # Read file content
            content = Path(path).read_text(encoding="utf-8", errors="ignore")

            # Extract themes using wisdom extractor patterns
            themes = []
            theme_patterns = getattr(extractor, "THEME_PATTERNS", {})
            for pattern_name, pattern_re in theme_patterns.items():
                if re.search(str(pattern_re), content, re.IGNORECASE):
                    themes.append(pattern_name)

            return themes[:5]  # Limit to top 5 themes
        except Exception:
            return []

    def _handle_event(self, event: FileEvent) -> None:
        """Handle a file system event."""
        # Add to history
        self._event_history.append(event)
        if len(self._event_history) > 1000:
            self._event_history = self._event_history[-500:]

        # Get watch config for this path
        config = None
        for watch_path, watch_config in self._watches.items():
            if event.path.startswith(watch_path):
                config = watch_config
                break

        if not config:
            return

        # Track in archaeology
        if config.auto_track_archaeology and event.event_type in (FileEventType.CREATED, FileEventType.MODIFIED):
            try:
                from whitemagic.archaeology import get_archaeologist
                arch = get_archaeologist()
                arch.mark_read(
                    event.path,
                    context=f"watcher:{event.event_type.name.lower()}",
                    note="Auto-tracked via file watcher",
                )
            except Exception:
                pass

        # Detect themes
        if config.extract_themes and event.event_type in (FileEventType.CREATED, FileEventType.MODIFIED):
            themes = self._detect_themes(event.path)
            event.metadata["themes"] = themes

        # Emit Gan Ying event
        if config.emit_gan_ying:
            try:
                from whitemagic.core.resonance.gan_ying_enhanced import (
                    EventType,
                    ResonanceEvent,
                    get_bus,
                )
                bus = get_bus()

                event_type_map = {
                    FileEventType.CREATED: EventType.MEMORY_CREATED,
                    FileEventType.MODIFIED: EventType.MEMORY_ACCESSED,
                    FileEventType.DELETED: EventType.MEMORY_CONSOLIDATED,
                    FileEventType.MOVED: EventType.MEMORY_ACCESSED,
                }

                bus.emit(
                    ResonanceEvent(
                        source="semantic_fs",
                        event_type=event_type_map.get(event.event_type, EventType.MEMORY_ACCESSED),
                        data=event.to_dict(),
                        confidence=1.0,
                        timestamp=datetime.now(),
                    ),
                )
            except Exception:
                pass

        # Call registered callbacks
        for callback in self._callbacks:
            try:
                callback(event)
            except Exception:
                pass

    # =========================================================================
    # Watcher Control
    # =========================================================================

    def start(self) -> bool:
        """Start the file watcher."""
        if self._running:
            return True

        if not self._watchdog_available:
            logger.warning("Warning: watchdog not installed. Install with: pip install watchdog")
            return False

        if not self._watches:
            logger.warning("No directories configured to watch")
            return False

        try:
            from watchdog.events import FileSystemEvent, FileSystemEventHandler
            from watchdog.observers import Observer

            class Handler(FileSystemEventHandler):
                def __init__(self, watcher: 'SemanticFileWatcher') -> None:
                    self.watcher = watcher

                def _should_ignore(self, path: str) -> bool:
                    """Check if path should be ignored."""
                    for pattern in ["__pycache__", ".git", "node_modules", ".pytest_cache", ".pyc"]:
                        if pattern in path:
                            return True
                    return False

                def _should_include(self, path: str) -> bool:
                    """Check if path matches watch patterns."""
                    import fnmatch
                    for watch_path, config in self.watcher._watches.items():
                        if path.startswith(watch_path):
                            # Check ignore patterns first
                            for pattern in config.ignore_patterns:
                                if fnmatch.fnmatch(Path(path).name, pattern) or pattern in path:
                                    return False
                            # Check include patterns
                            for pattern in config.patterns:
                                if fnmatch.fnmatch(Path(path).name, pattern):
                                    return True
                    return False

                def on_created(self, event: FileSystemEvent) -> Any:
                    if event.is_directory or self._should_ignore(event.src_path):
                        return
                    if not self._should_include(event.src_path):
                        return

                    fe = FileEvent(
                        event_type=FileEventType.CREATED,
                        path=event.src_path,
                        metadata=self.watcher._extract_metadata(event.src_path),
                    )
                    self.watcher._handle_event(fe)

                def on_modified(self, event: FileSystemEvent) -> Any:
                    if event.is_directory or self._should_ignore(event.src_path):
                        return
                    if not self._should_include(event.src_path):
                        return

                    fe = FileEvent(
                        event_type=FileEventType.MODIFIED,
                        path=event.src_path,
                        metadata=self.watcher._extract_metadata(event.src_path),
                    )
                    self.watcher._handle_event(fe)

                def on_deleted(self, event: FileSystemEvent) -> Any:
                    if event.is_directory or self._should_ignore(event.src_path):
                        return
                    if not self._should_include(event.src_path):
                        return

                    fe = FileEvent(
                        event_type=FileEventType.DELETED,
                        path=event.src_path,
                    )
                    self.watcher._handle_event(fe)

                def on_moved(self, event: FileSystemEvent) -> Any:
                    if event.is_directory or self._should_ignore(event.src_path):
                        return
                    if not self._should_include(event.dest_path):
                        return

                    fe = FileEvent(
                        event_type=FileEventType.MOVED,
                        path=event.dest_path,
                        old_path=event.src_path,
                        metadata=self.watcher._extract_metadata(event.dest_path),
                    )
                    self.watcher._handle_event(fe)

            self._observer = Observer()
            handler = Handler(self)

            for path, config in self._watches.items():
                self._observer.schedule(handler, path, recursive=config.recursive)

            self._observer.start()
            self._running = True
            return True

        except Exception as e:
            logger.warning(f"Failed to start watcher: {e}")
            return False

    def stop(self) -> bool:
        """Stop the file watcher."""
        if not self._running:
            return True

        try:
            if self._observer:
                self._observer.stop()
                self._observer.join(timeout=5)
                self._observer = None

            self._running = False
            return True
        except Exception as e:
            logger.warning(f"Failed to stop watcher: {e}")
            return False

    def is_running(self) -> bool:
        """Check if watcher is running."""
        return self._running

    # =========================================================================
    # Callbacks
    # =========================================================================

    def add_callback(self, callback: Callable[[FileEvent], None]) -> None:
        """Add a callback to be called on file events."""
        self._callbacks.append(callback)

    def remove_callback(self, callback: Callable[[FileEvent], None]) -> bool:
        """Remove a callback."""
        if callback in self._callbacks:
            self._callbacks.remove(callback)
            return True
        return False

    # =========================================================================
    # Statistics and Status
    # =========================================================================

    def status(self) -> dict[str, Any]:
        """Get watcher status."""
        return {
            "running": self._running,
            "watchdog_available": self._watchdog_available,
            "watches_configured": len(self._watches),
            "watches": list(self._watches.keys()),
            "events_recorded": len(self._event_history),
            "callbacks_registered": len(self._callbacks),
        }

    def recent_events(self, limit: int = 50) -> list[dict[str, Any]]:
        """Get recent file events."""
        return [e.to_dict() for e in self._event_history[-limit:]]

    def stats(self) -> dict[str, Any]:
        """Get watcher statistics."""
        event_counts: dict[str, int] = {}
        for event in self._event_history:
            event_type = event.event_type.name
            event_counts[event_type] = event_counts.get(event_type, 0) + 1

        return {
            "total_events": len(self._event_history),
            "event_counts": event_counts,
            "watches": len(self._watches),
            "running": self._running,
        }


def get_watcher() -> SemanticFileWatcher:
    """Get singleton SemanticFileWatcher instance."""
    global _watcher_instance
    with _watcher_lock:
        if _watcher_instance is None:
            _watcher_instance = SemanticFileWatcher()
        return _watcher_instance


# =========================================================================
# Convenience Functions
# =========================================================================

def watch_directory(path: str, **kwargs: Any) -> bool:
    """Add a directory to watch."""
    return get_watcher().add_watch(path, **kwargs)


def start_watching() -> bool:
    """Start the file watcher."""
    return get_watcher().start()


def stop_watching() -> bool:
    """Stop the file watcher."""
    return get_watcher().stop()


def watcher_status() -> dict[str, Any]:
    """Get watcher status."""
    return get_watcher().status()
