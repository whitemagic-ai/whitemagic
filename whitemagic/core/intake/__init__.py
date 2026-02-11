"""Holographic Intake System v2.0 - Auto-ingestion for new files.

Features:
- Background daemon for continuous monitoring
- Integration with SemanticFileWatcher
- Automatic 4D coordinate encoding
- Deduplication via content hashing
- Queue-based processing
"""

import hashlib
import json
import threading
import time
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import DATA_DIR


@dataclass
class IntakeStats:
    """Statistics for intake operations."""

    files_scanned: int = 0
    files_ingested: int = 0
    files_skipped: int = 0
    last_scan: str | None = None
    errors: list[str] = field(default_factory=list)

class HolographicIntake:
    """Auto-ingestion for new files into holographic hypercube."""

    SUPPORTED = {".md", ".txt", ".json", ".yaml", ".yml", ".py", ".ts", ".js"}

    def __init__(self) -> None:
        state_dir = DATA_DIR / "intake"
        self.config_path = state_dir / "intake_config.json"
        self.queue_path = state_dir / "intake_queue.json"
        self.hashes_path = state_dir / "intake_hashes.json"
        state_dir.mkdir(parents=True, exist_ok=True)

        self._watch_dirs: list[str] = []
        self._known_hashes: set[str] = set()
        self._queue: list[dict[str, Any]] = []
        self._running = False
        self._thread: threading.Thread | None = None
        self._callbacks: list[Callable[[dict[str, Any]], None]] = []
        self.stats = IntakeStats()

        self._load()

    def _load(self) -> None:
        """Load configuration and known hashes."""
        if self.config_path.exists():
            try:
                data = json.loads(self.config_path.read_text())
                self._watch_dirs = data.get("watch_dirs", [])
            except (json.JSONDecodeError, IOError) as e:
                self.stats.errors.append(f"Config load error: {e}")
                pass

        if self.hashes_path.exists():
            try:
                self._known_hashes = set(json.loads(self.hashes_path.read_text()))
            except (json.JSONDecodeError, IOError) as e:
                self.stats.errors.append(f"Hashes load error: {e}")
                pass

        if self.queue_path.exists():
            try:
                self._queue = json.loads(self.queue_path.read_text())
            except (json.JSONDecodeError, IOError) as e:
                self.stats.errors.append(f"Queue load error: {e}")
                pass

    def _save(self) -> None:
        """Save configuration and state."""
        self.config_path.write_text(json.dumps({
            "watch_dirs": self._watch_dirs,
            "updated": datetime.now().isoformat(),
        }, indent=2))

        self.hashes_path.write_text(json.dumps(list(self._known_hashes)))
        self.queue_path.write_text(json.dumps(self._queue, indent=2))

    def add_watch(self, path: str) -> bool:
        """Add a directory to watch list."""
        p = Path(path).resolve()
        if p.is_dir() and str(p) not in self._watch_dirs:
            self._watch_dirs.append(str(p))
            self._save()
            return True
        return False

    def remove_watch(self, path: str) -> bool:
        """Remove a directory from watch list."""
        p = str(Path(path).resolve())
        if p in self._watch_dirs:
            self._watch_dirs.remove(p)
            self._save()
            return True
        return False

    def list_watches(self) -> list[str]:
        """Get current watch directories."""
        return self._watch_dirs.copy()

    def _hash_file(self, path: Path) -> str:
        """Generate content hash for deduplication."""
        try:
            return hashlib.sha256(path.read_bytes()).hexdigest()[:16]
        except (IOError, PermissionError):
            return hashlib.sha256(str(path).encode()).hexdigest()[:16]

    def scan(self, path: str | None = None) -> list[dict[str, Any]]:
        """Scan for new files not in memory."""
        new_files = []
        paths_to_scan = [path] if path else self._watch_dirs

        for scan_path in paths_to_scan:
            p = Path(scan_path)
            if not p.exists():
                continue

            for f in p.rglob("*"):
                if f.is_file() and f.suffix.lower() in self.SUPPORTED:
                    self.stats.files_scanned += 1
                    h = self._hash_file(f)
                    if h not in self._known_hashes:
                        new_files.append({
                            "path": str(f),
                            "hash": h,
                            "size": f.stat().st_size,
                            "name": f.name,
                            "discovered": datetime.now().isoformat(),
                        })

        self.stats.last_scan = datetime.now().isoformat()
        return new_files

    def queue_file(self, path: str) -> bool:
        """Add a file to the ingestion queue."""
        p = Path(path)
        if not p.exists():
            return False

        h = self._hash_file(p)
        if h in self._known_hashes:
            return False

        entry = {
            "path": str(p),
            "hash": h,
            "queued": datetime.now().isoformat(),
            "status": "pending",
        }
        self._queue.append(entry)
        self._save()
        return True

    def ingest(self, path: str) -> str | None:
        """Ingest a single file to holographic memory."""
        try:
            from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
            from whitemagic.core.memory.sqlite_backend import SQLiteBackend
            from whitemagic.core.memory.unified_types import Memory, MemoryType

            f = Path(path)
            if not f.exists():
                self.stats.errors.append(f"File not found: {path}")
                return None

            content = f.read_text()[:10000]
            h = self._hash_file(f)

            # Already ingested?
            if h in self._known_hashes:
                self.stats.files_skipped += 1
                return None

            # Encode to 4D coordinates
            encoder = CoordinateEncoder()
            memory_dict = {
                "content": content,
                "title": f.stem,
                "tags": ["auto-ingested"],
                "memory_type": "long_term",
            }
            coords = encoder.encode(memory_dict)

            # Generate unique ID
            mem_id = hashlib.sha256(f"{path}:{h}".encode()).hexdigest()[:16]

            # Create memory object
            memory = Memory(
                id=mem_id,
                content=content,
                memory_type=MemoryType.LONG_TERM,
                title=f.stem,
                created_at=datetime.now(),
                accessed_at=datetime.now(),
                access_count=1,
                tags={"auto-ingested", "intake"},
                associations={},
                emotional_valence=0.0,
                importance=coords.w,
                metadata={"source": str(path), "hash": h},
            )

            # Store in SQLite
            from whitemagic.config.paths import DB_PATH
            db = SQLiteBackend(DB_PATH)
            db.store(memory)
            db.store_coords(mem_id, coords.x, coords.y, coords.z, coords.w)

            # Mark as known
            self._known_hashes.add(h)
            self._save()

            self.stats.files_ingested += 1

            # Notify callbacks
            for cb in self._callbacks:
                try:
                    cb({"event": "ingested", "path": path, "id": mem_id})
                except Exception as e:
                    self.stats.errors.append(f"Callback error: {e}")
                    pass

            return mem_id

        except Exception as e:
            self.stats.errors.append(f"Ingest error {path}: {str(e)}")
            return None

    def process_queue(self) -> dict[str, int]:
        """Process all queued files."""
        results = {"ingested": 0, "failed": 0, "skipped": 0}

        for entry in self._queue:
            if entry.get("status") == "pending":
                mem_id = self.ingest(entry["path"])
                if mem_id:
                    entry["status"] = "ingested"
                    entry["memory_id"] = mem_id
                    results["ingested"] += 1
                else:
                    entry["status"] = "failed"
                    results["failed"] += 1

        self._save()
        return results

    def register_callback(self, callback: Callable[[dict[str, Any]], None]) -> None:
        """Register a callback for intake events."""
        self._callbacks.append(callback)

    def start_daemon(self, interval: int = 60) -> bool:
        """Start background monitoring daemon."""
        if self._running:
            return False

        self._running = True

        def daemon_loop() -> None:
            while self._running:
                try:
                    # Scan all watch directories
                    new_files = self.scan()
                    for f in new_files:
                        self.queue_file(f["path"])

                    # Process queue
                    if self._queue:
                        self.process_queue()

                except Exception as e:
                    self.stats.errors.append(f"Daemon error: {str(e)}")

                time.sleep(interval)

        self._thread = threading.Thread(target=daemon_loop, daemon=True)
        self._thread.start()
        return True

    def stop_daemon(self) -> None:
        """Stop the background daemon."""
        self._running = False
        if self._thread:
            self._thread.join(timeout=5)
        self._thread = None

    def get_status(self) -> dict[str, Any]:
        """Get daemon status."""
        return {
            "running": self._running,
            "watch_dirs": len(self._watch_dirs),
            "queue_size": len([q for q in self._queue if q.get("status") == "pending"]),
            "known_hashes": len(self._known_hashes),
            "stats": {
                "scanned": self.stats.files_scanned,
                "ingested": self.stats.files_ingested,
                "skipped": self.stats.files_skipped,
                "errors": len(self.stats.errors),
                "last_scan": self.stats.last_scan,
            },
        }

# Global instance
_instance: HolographicIntake | None = None

def get_intake() -> HolographicIntake:
    """Get global intake instance."""
    global _instance
    if _instance is None:
        _instance = HolographicIntake()
    return _instance

def start_intake_daemon(interval: int = 60) -> HolographicIntake:
    """Start the holographic intake daemon with defaults."""
    intake = get_intake()

    # Add default watch directories if empty
    if not intake._watch_dirs:
        from whitemagic.config.paths import DATA_DIR

        # Safe default: an internal "inbox" directory under WM_STATE_ROOT.
        inbox = DATA_DIR / "intake" / "inbox"
        inbox.mkdir(parents=True, exist_ok=True)
        intake.add_watch(str(inbox))

    intake.start_daemon(interval)
    return intake
