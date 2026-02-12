"""Multi-Agent Coordinator - Prevent Conflicts Between AI Instances.

When multiple AI instances work with WhiteMagic simultaneously,
they need coordination to avoid conflicts and race conditions.

Inspired by distributed systems locking and consensus protocols.
"""

from __future__ import annotations

from typing import Any
import threading
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone


@dataclass
class ResourceLock:
    """A lock on a shared resource."""

    resource: str
    agent_id: str
    acquired_at: datetime
    expires_at: datetime
    operation: str  # What the agent is doing


class MultiAgentCoordinator:
    """Coordinates multiple AI agents to prevent conflicts.

    Like traffic lights at an intersection - ensures only one
    agent modifies a resource at a time.
    """

    def __init__(self, lock_timeout: int = 300) -> None:  # 5 minutes default
        self.active_agents: set[str] = set()
        self.locks: dict[str, ResourceLock] = {}
        self.lock_timeout = lock_timeout
        self._lock = threading.Lock()

    def register_agent(self, agent_id: str) -> bool:
        """Register a new agent."""
        with self._lock:
            self.active_agents.add(agent_id)
            return True

    def unregister_agent(self, agent_id: str) -> bool:
        """Unregister an agent and release all its locks."""
        with self._lock:
            # Release all locks held by this agent
            locks_to_release = [
                resource for resource, lock in self.locks.items()
                if lock.agent_id == agent_id
            ]

            for resource in locks_to_release:
                del self.locks[resource]

            self.active_agents.discard(agent_id)
            return True

    def request_lock(
        self,
        agent_id: str,
        resource: str,
        operation: str,
        timeout: int | None = None,
    ) -> tuple[bool, str | None]:
        """Request exclusive lock on a resource.

        Args:
            agent_id: Unique identifier for the agent
            resource: Resource to lock (e.g., "memory:12345", "file:core.py")
            operation: What the agent wants to do
            timeout: Lock timeout in seconds (uses default if None)

        Returns:
            (success, reason) tuple

        """
        with self._lock:
            # Clean up expired locks first
            self._cleanup_expired_locks()

            # Check if resource is already locked
            if resource in self.locks:
                existing_lock = self.locks[resource]

                # Allow same agent to re-acquire (reentrant lock)
                if existing_lock.agent_id == agent_id:
                    # Extend expiration
                    existing_lock.expires_at = datetime.now(timezone.utc) + timedelta(
                        seconds=timeout or self.lock_timeout,
                    )
                    return True, None

                # Resource locked by another agent
                return False, f"Resource locked by agent {existing_lock.agent_id} for operation: {existing_lock.operation}"

            # Grant lock
            lock = ResourceLock(
                resource=resource,
                agent_id=agent_id,
                acquired_at=datetime.now(timezone.utc),
                expires_at=datetime.now(timezone.utc) + timedelta(seconds=timeout or self.lock_timeout),
                operation=operation,
            )

            self.locks[resource] = lock
            return True, None

    def release_lock(self, agent_id: str, resource: str) -> bool:
        """Release a lock on a resource."""
        with self._lock:
            if resource not in self.locks:
                return False

            lock = self.locks[resource]

            # Only the owning agent can release
            if lock.agent_id != agent_id:
                return False

            del self.locks[resource]
            return True

    def check_conflicts(self, agent_id: str, resources: list[str]) -> list[str]:
        """Check which resources would conflict.

        Returns list of conflicting resources.
        """
        with self._lock:
            self._cleanup_expired_locks()

            conflicts = []
            for resource in resources:
                if resource in self.locks:
                    lock = self.locks[resource]
                    if lock.agent_id != agent_id:
                        conflicts.append(resource)

            return conflicts

    def get_agent_locks(self, agent_id: str) -> list[ResourceLock]:
        """Get all locks held by an agent."""
        with self._lock:
            return [
                lock for lock in self.locks.values()
                if lock.agent_id == agent_id
            ]

    def _cleanup_expired_locks(self) -> Any:
        """Remove expired locks (called with lock held)."""
        now = datetime.now(timezone.utc)
        expired = [
            resource for resource, lock in self.locks.items()
            if lock.expires_at < now
        ]

        for resource in expired:
            del self.locks[resource]

    def get_status(self) -> dict:
        """Get coordinator status."""
        with self._lock:
            self._cleanup_expired_locks()

            return {
                "active_agents": len(self.active_agents),
                "active_locks": len(self.locks),
                "locks_by_agent": {
                    agent_id: len([
                        lock for lock in self.locks.values()
                        if lock.agent_id == agent_id
                    ])
                    for agent_id in self.active_agents
                },
            }


# Singleton instance
_coordinator = None
_coordinator_lock = threading.Lock()


def get_coordinator() -> MultiAgentCoordinator:
    """Get or create the global coordinator instance."""
    global _coordinator

    if _coordinator is None:
        with _coordinator_lock:
            if _coordinator is None:
                _coordinator = MultiAgentCoordinator()

    return _coordinator
