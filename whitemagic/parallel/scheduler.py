from __future__ import annotations

import asyncio
import logging
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from typing import Any
from uuid import uuid4

logger = logging.getLogger(__name__)


class TaskPriority(Enum):
    """Task priority levels."""

    CRITICAL = 0  # Must execute first
    HIGH = 1  # Important tasks
    NORMAL = 2  # Default priority
    LOW = 3  # Background tasks
    BULK = 4  # Batch operations


class TaskStatus(Enum):
    """Task execution status."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class Task:
    """Represents a schedulable task."""

    id: str = field(default_factory=lambda: str(uuid4()))
    func: Callable | None = None
    args: tuple = field(default_factory=tuple)
    kwargs: dict = field(default_factory=dict)

    priority: TaskPriority = TaskPriority.NORMAL
    dependencies: set[str] = field(default_factory=set)

    status: TaskStatus = TaskStatus.PENDING
    result: Any = None
    error: str | None = None

    created_at: float = 0.0
    started_at: float | None = None
    completed_at: float | None = None

    def __hash__(self) -> int:
        return hash(self.id)

    @property
    def is_ready(self) -> bool:
        """Check if task is ready to execute (no pending dependencies)."""
        return self.status == TaskStatus.PENDING and not self.dependencies


@dataclass
class SchedulerStats:
    """Scheduler performance statistics."""

    total_tasks: int = 0
    completed_tasks: int = 0
    failed_tasks: int = 0
    cancelled_tasks: int = 0
    running_tasks: int = 0
    pending_tasks: int = 0

    avg_execution_time: float = 0.0
    total_execution_time: float = 0.0

    @property
    def success_rate(self) -> float:
        """Calculate success rate."""
        if self.total_tasks == 0:
            return 0.0
        return self.completed_tasks / self.total_tasks


class ParallelScheduler:
    """Priority-based parallel task scheduler.

    Manages task execution with priorities, dependencies, and resource limits.
    Optimizes for maximum throughput while respecting constraints.
    """

    def __init__(self, max_concurrent: int = 64):
        """Initialize scheduler.

        Args:
            max_concurrent: Maximum concurrent tasks

        """
        self.max_concurrent = max_concurrent

        # Task storage
        self._tasks: dict[str, Task] = {}
        self._ready_queue: asyncio.PriorityQueue = asyncio.PriorityQueue()
        self._running: set[str] = set()

        # Statistics
        self.stats = SchedulerStats()

        # Control
        self._running_scheduler = False
        self._semaphore: asyncio.Semaphore | None = None

    def add_task(
        self,
        func: Callable[..., Any],
        *args: Any,
        priority: TaskPriority = TaskPriority.NORMAL,
        dependencies: list[str] | None = None,
        **kwargs: Any,
    ) -> str:
        """Add task to scheduler.

        Args:
            func: Function to execute
            *args: Positional arguments
            priority: Task priority
            dependencies: List of task IDs this task depends on
            **kwargs: Keyword arguments

        Returns:
            Task ID

        """
        task = Task(
            func=func,
            args=args,
            kwargs=kwargs,
            priority=priority,
            dependencies=set(dependencies) if dependencies else set(),
        )

        self._tasks[task.id] = task
        self.stats.total_tasks += 1
        self.stats.pending_tasks += 1

        # Add to queue if ready
        if task.is_ready:
            self._ready_queue.put_nowait((priority.value, task.id))

        return task.id

    async def _execute_task(self, task: Task) -> None:
        """Execute a single task."""
        import time

        task.status = TaskStatus.RUNNING
        task.started_at = time.time()
        self.stats.running_tasks += 1
        self.stats.pending_tasks -= 1

        try:
            # Execute function
            if asyncio.iscoroutinefunction(task.func):
                task.result = await task.func(*task.args, **task.kwargs)
            else:
                task.result = task.func(*task.args, **task.kwargs)  # type: ignore[misc]

            task.status = TaskStatus.COMPLETED
            self.stats.completed_tasks += 1

        except Exception as e:
            task.status = TaskStatus.FAILED
            task.error = str(e)
            self.stats.failed_tasks += 1

        finally:
            task.completed_at = time.time()
            execution_time = task.completed_at - task.started_at

            # Update stats
            self.stats.total_execution_time += execution_time
            self.stats.running_tasks -= 1

            # Check dependent tasks
            await self._check_dependencies(task.id)

    async def _check_dependencies(self, completed_task_id: str) -> None:
        """Check if any tasks are now ready after task completion."""
        for task in self._tasks.values():
            if completed_task_id in task.dependencies:
                task.dependencies.remove(completed_task_id)

                if task.is_ready:
                    await self._ready_queue.put((task.priority.value, task.id))

    async def _worker(self) -> None:
        """Worker coroutine that processes tasks from queue."""
        while self._running_scheduler:
            try:
                # Wait for task with timeout
                priority, task_id = await asyncio.wait_for(self._ready_queue.get(), timeout=0.1)

                task = self._tasks.get(task_id)
                if not task or task.status != TaskStatus.PENDING:
                    continue

                # Acquire semaphore
                assert self._semaphore is not None
                async with self._semaphore:
                    self._running.add(task_id)
                    await self._execute_task(task)
                    self._running.discard(task_id)

            except asyncio.TimeoutError:
                # No tasks available, check if done
                if not self._running and self._ready_queue.empty():
                    break

            except Exception as e:
                logger.info(f"Worker error: {e}")

    async def run(self, num_workers: int | None = None) -> SchedulerStats:
        """Run scheduler until all tasks complete.

        Args:
            num_workers: Number of worker coroutines (default: max_concurrent)

        Returns:
            Scheduler statistics

        """
        if not self._tasks:
            return self.stats

        self._running_scheduler = True
        self._semaphore = asyncio.Semaphore(self.max_concurrent)

        # Create workers
        num_workers = num_workers or self.max_concurrent
        workers = [asyncio.create_task(self._worker()) for _ in range(num_workers)]

        # Wait for all workers
        await asyncio.gather(*workers, return_exceptions=True)

        self._running_scheduler = False

        # Update final stats
        if self.stats.completed_tasks > 0:
            self.stats.avg_execution_time = (
                self.stats.total_execution_time / self.stats.completed_tasks
            )

        return self.stats

    def get_task(self, task_id: str) -> Task | None:
        """Get task by ID."""
        return self._tasks.get(task_id)

    def get_results(self) -> dict[str, Any]:
        """Get results of all completed tasks."""
        return {
            task_id: task.result
            for task_id, task in self._tasks.items()
            if task.status == TaskStatus.COMPLETED
        }

    def cancel_task(self, task_id: str) -> bool:
        """Cancel a pending task.

        Args:
            task_id: Task ID to cancel

        Returns:
            True if cancelled, False if already running/completed

        """
        task = self._tasks.get(task_id)
        if not task or task.status != TaskStatus.PENDING:
            return False

        task.status = TaskStatus.CANCELLED
        self.stats.cancelled_tasks += 1
        self.stats.pending_tasks -= 1

        return True

    def clear(self) -> None:
        """Clear all tasks and reset scheduler."""
        self._tasks.clear()
        self._running.clear()
        self._ready_queue = asyncio.PriorityQueue()
        self.stats = SchedulerStats()
