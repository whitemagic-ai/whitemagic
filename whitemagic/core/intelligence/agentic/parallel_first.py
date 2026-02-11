"""Parallel-First Mode - Default to parallel, sequential requires justification
BRAIN UPGRADE #5.

Old: Sequential by default, parallel optional
New: PARALLEL by default, sequential only when necessary
"""
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import datetime
from typing import Any


@dataclass
class TaskResult:
    """Result of a parallel task."""

    task_id: str
    success: bool
    result: Any
    duration_ms: float

class ParallelFirst:
    """Parallel execution by default.
    Sequential only when dependencies require it.
    """

    def __init__(self, max_workers: int | None = None):
        from whitemagic.config.concurrency import IO_WORKERS
        self.max_workers = max_workers or IO_WORKERS
        self.mode = "PARALLEL"  # Always start parallel!
        self.tasks_run = 0
        self.parallel_runs = 0
        self.sequential_runs = 0

    def execute(self, tasks: list[dict]) -> list[TaskResult]:
        """Execute tasks - PARALLEL by default!

        Each task should have:
        - id: str
        - func: callable
        - args: tuple (optional)
        - depends_on: list of task ids (optional)
        """
        if not tasks:
            return []

        # Check for dependencies
        has_deps = any(t.get("depends_on") for t in tasks)

        if has_deps:
            # Must run sequential due to dependencies
            self.sequential_runs += 1
            return self._run_sequential(tasks)
        else:
            # PARALLEL! (Default!)
            self.parallel_runs += 1
            return self._run_parallel(tasks)

    def _run_parallel(self, tasks: list[dict]) -> list[TaskResult]:
        """Run all tasks in parallel."""
        results = []

        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            futures = {}
            for task in tasks:
                future = executor.submit(
                    task["func"],
                    *task.get("args", ()),
                )
                futures[future] = task["id"]

            for future in as_completed(futures):
                task_id = futures[future]
                start = datetime.now()
                try:
                    result = future.result()
                    results.append(TaskResult(
                        task_id=task_id,
                        success=True,
                        result=result,
                        duration_ms=(datetime.now() - start).total_seconds() * 1000,
                    ))
                except Exception as e:
                    results.append(TaskResult(
                        task_id=task_id,
                        success=False,
                        result=str(e),
                        duration_ms=(datetime.now() - start).total_seconds() * 1000,
                    ))

        self.tasks_run += len(tasks)
        return results

    def _run_sequential(self, tasks: list[dict]) -> list[TaskResult]:
        """Run tasks sequentially (only when dependencies require it)."""
        results = []
        completed = set()

        for task in tasks:
            # Wait for dependencies
            deps = task.get("depends_on", [])
            if not all(d in completed for d in deps):
                # Dependency not met - skip for now
                # (In full implementation, would reorder)
                continue

            start = datetime.now()
            try:
                result = task["func"](*task.get("args", ()))
                results.append(TaskResult(
                    task_id=task["id"],
                    success=True,
                    result=result,
                    duration_ms=(datetime.now() - start).total_seconds() * 1000,
                ))
                completed.add(task["id"])
            except Exception as e:
                results.append(TaskResult(
                    task_id=task["id"],
                    success=False,
                    result=str(e),
                    duration_ms=(datetime.now() - start).total_seconds() * 1000,
                ))

        self.tasks_run += len(tasks)
        return results

    def get_stats(self) -> dict:
        """Get execution statistics."""
        total = self.parallel_runs + self.sequential_runs
        parallel_pct = (self.parallel_runs / total * 100) if total > 0 else 100

        return {
            "mode": self.mode,
            "tasks_run": self.tasks_run,
            "parallel_runs": self.parallel_runs,
            "sequential_runs": self.sequential_runs,
            "parallel_percentage": f"{parallel_pct:.1f}%",
        }


# Singleton
_parallel = None

def get_parallel_first() -> ParallelFirst:
    """Get the global Parallel-First executor."""
    global _parallel
    if _parallel is None:
        _parallel = ParallelFirst()
    return _parallel

# Convenience function
def parallel_do(tasks: list[dict]) -> list[TaskResult]:
    """Quick parallel execution."""
    return get_parallel_first().execute(tasks)
