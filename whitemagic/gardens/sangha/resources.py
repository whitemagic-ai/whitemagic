"""Sangha Resources - Distributed resource locking for multi-agent coordination.

Prevents race conditions when multiple agents attempt to modify the same files
or execute conflicting tasks.
"""

from __future__ import annotations

import json
from collections.abc import Generator
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any

from whitemagic.utils.core import parse_datetime

from .workspace import get_sangha_base_dir

try:
    from whitemagic.utils.fileio import atomic_write as _atomic_write
    from whitemagic.utils.fileio import file_lock as _file_lock
    FILEIO_AVAILABLE = True
except ImportError:
    FILEIO_AVAILABLE = False

    @contextmanager
    def _file_lock(filepath: str | Path, timeout: float = 5.0) -> Generator[None, None, None]:
        _ = timeout
        yield

    def _atomic_write(filepath: str | Path, content: str) -> None:
        Path(filepath).write_text(content, encoding="utf-8")

file_lock = _file_lock
atomic_write = _atomic_write


@dataclass
class ResourceLock:
    """A lock on a specific resource (file, task, or abstract ID)."""

    resource_id: str
    locked_by: str
    acquired_at: datetime
    expires_at: datetime
    reason: str


class ResourceManager:
    """Manages distributed locks for Sangha agents."""

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir if base_dir is not None else get_sangha_base_dir()
        self.lock_dir = self.base_dir / "memory" / "collective" / "locks"
        self.lock_dir.mkdir(parents=True, exist_ok=True)
        self.registry_file = self.lock_dir / "registry.json"

        if not self.registry_file.exists():
            self._save_registry({})

    def _load_registry(self) -> dict[str, dict[str, Any]]:
        """Load lock registry safely."""
        if not self.registry_file.exists():
            return {}
        try:
            with file_lock(self.registry_file):
                loaded = json.loads(self.registry_file.read_text())
                if isinstance(loaded, dict):
                    return {
                        str(k): v for k, v in loaded.items() if isinstance(v, dict)
                    }
                return {}
        except (json.JSONDecodeError, FileNotFoundError):
            return {}

    def _save_registry(self, registry: dict[str, dict[str, Any]]) -> None:
        """Save lock registry safely."""
        with file_lock(self.registry_file):
            atomic_write(self.registry_file, json.dumps(registry, indent=2))

    def acquire_lock(self,
                     resource_id: str,
                     agent_id: str,
                     reason: str,
                     ttl_seconds: int = 300) -> bool:
        """Attempt to acquire a lock on a resource.
        Supports automatic renewal if the agent already holds the lock.
        """
        resource_id = str(resource_id)

        with file_lock(self.registry_file):
            if self.registry_file.exists():
                registry = json.loads(self.registry_file.read_text())
            else:
                registry = {}

            now = datetime.now()
            self._clean_expired_locks(registry, now)

            if resource_id in registry:
                lock_data = registry[resource_id]
                expires_at = parse_datetime(lock_data["expires_at"])

                # If locked by someone else and not expired
                if lock_data["locked_by"] != agent_id and now < expires_at:
                    return False

                # If owned by us or expired, we continue to create/renew

            # Create or Renew lock
            expires_at = now + timedelta(seconds=ttl_seconds)
            registry[resource_id] = {
                "resource_id": resource_id,
                "locked_by": agent_id,
                "acquired_at": now.isoformat(),
                "expires_at": expires_at.isoformat(),
                "reason": reason,
            }

            atomic_write(self.registry_file, json.dumps(registry, indent=2))
            return True

    def heartbeat(self, resource_id: str, agent_id: str, ttl_seconds: int = 300) -> bool:
        """Extend lock lifetime (heartbeat) if owned by agent."""
        resource_id = str(resource_id)
        with file_lock(self.registry_file):
            if not self.registry_file.exists():
                return False

            registry = json.loads(self.registry_file.read_text())
            if resource_id in registry and registry[resource_id]["locked_by"] == agent_id:
                now = datetime.now()
                expires_at = now + timedelta(seconds=ttl_seconds)
                registry[resource_id]["expires_at"] = expires_at.isoformat()
                atomic_write(self.registry_file, json.dumps(registry, indent=2))
                return True
            return False

    def release_lock(self, resource_id: str, agent_id: str) -> bool:
        """Release a lock if owned by agent."""
        resource_id = str(resource_id)

        with file_lock(self.registry_file):
            if self.registry_file.exists():
                registry = json.loads(self.registry_file.read_text())
            else:
                return True

            if resource_id in registry:
                if registry[resource_id]["locked_by"] == agent_id:
                    del registry[resource_id]
                    atomic_write(self.registry_file, json.dumps(registry, indent=2))
                    return True
                else:
                    return False
            return True

    def get_lock_status(self, resource_id: str) -> ResourceLock | None:
        """Get status of a lock."""
        resource_id = str(resource_id)
        registry = self._load_registry()

        if resource_id in registry:
            data = registry[resource_id]
            return ResourceLock(
                resource_id=data["resource_id"],
                locked_by=data["locked_by"],
                acquired_at=parse_datetime(data["acquired_at"]),
                expires_at=parse_datetime(data["expires_at"]),
                reason=data["reason"],
            )
        return None

    def list_locks(self) -> list[ResourceLock]:
        """List all active locks."""
        registry = self._load_registry()
        locks = []
        now = datetime.now()

        # Clean expired locks before listing
        self._clean_expired_locks(registry, now)

        for data in registry.values():
            expires_at = parse_datetime(data["expires_at"])
            if now < expires_at:
                locks.append(ResourceLock(
                    resource_id=data["resource_id"],
                    locked_by=data["locked_by"],
                    acquired_at=parse_datetime(data["acquired_at"]),
                    expires_at=expires_at,
                    reason=data["reason"],
                ))
        return locks

    def _clean_expired_locks(self, registry: dict[str, dict[str, Any]], now: datetime) -> None:
        """Remove expired locks from the registry."""
        expired = []
        for resource_id, lock_data in registry.items():
            expires_at = parse_datetime(lock_data["expires_at"])
            if now >= expires_at:
                expired.append(resource_id)
        for resource_id in expired:
            del registry[resource_id]
        if expired:
            atomic_write(self.registry_file, json.dumps(registry, indent=2))


# Global instance
_resources: ResourceManager | None = None

def get_resources() -> ResourceManager:
    global _resources
    if _resources is None:
        _resources = ResourceManager()
    return _resources
