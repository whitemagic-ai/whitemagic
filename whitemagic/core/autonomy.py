"""Bounded Autonomy System - Stop Conditions & Plateau Detection.

Prevents runaway autonomous execution with:
- Session timeout detection
- Progress plateau detection (no meaningful work)
- Iteration limits with completion criteria
- Resource usage monitoring
"""

import logging
import os
import time
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

import psutil

logger = logging.getLogger(__name__)


@dataclass
class StopConditions:
    """Configuration for autonomous execution boundaries."""

    # Time-based limits
    max_duration_seconds: int = 3600  # 1 hour default
    max_iterations: int = 100

    # Plateau detection
    plateau_window_seconds: int = 300  # 5 minutes
    plateau_threshold: int = 3  # No meaningful progress in N checks

    # Resource limits
    max_memory_mb: int = 4096  # 4GB per agent
    max_cpu_percent: float = 80.0

    # Completion criteria (optional callable)
    completion_check: Callable[[], bool] | None = None

    # File change monitoring
    watch_directories: list[str] = field(default_factory=list)
    min_file_changes: int = 1  # Minimum changes to consider progress


@dataclass
class ExecutionState:
    """Tracks execution state for stop condition evaluation."""

    start_time: datetime = field(default_factory=datetime.now)
    iteration_count: int = 0
    last_progress_time: datetime = field(default_factory=datetime.now)
    file_change_count: int = 0
    plateau_strikes: int = 0

    # Snapshot of watched files
    file_mtimes: dict[str, float] = field(default_factory=dict)

    def reset_progress(self) -> None:
        """Reset progress indicators."""
        self.last_progress_time = datetime.now()
        self.plateau_strikes = 0


class BoundedExecutor:
    """Executes autonomous tasks with stop conditions and plateau detection.

    Example:
        executor = BoundedExecutor(
            max_duration_seconds=1800,  # 30 minutes
            plateau_window_seconds=180,  # 3 minutes
            watch_directories=["whitemagic/", "tests/"]
        )

        result = executor.run(my_autonomous_function)
        if result.stopped_early:
            logger.info(f"Stopped: {result.stop_reason}")

    """

    def __init__(
        self,
        max_duration_seconds: int = 3600,
        max_iterations: int = 100,
        plateau_window_seconds: int = 300,
        plateau_threshold: int = 3,
        max_memory_mb: int = 4096,
        max_cpu_percent: float = 80.0,
        watch_directories: list[str] | None = None,
        completion_check: Callable[[], bool] | None = None,
    ):
        self.conditions = StopConditions(
            max_duration_seconds=max_duration_seconds,
            max_iterations=max_iterations,
            plateau_window_seconds=plateau_window_seconds,
            plateau_threshold=plateau_threshold,
            max_memory_mb=max_memory_mb,
            max_cpu_percent=max_cpu_percent,
            watch_directories=watch_directories or [],
            completion_check=completion_check,
        )
        self.state = ExecutionState()
        self._init_file_tracking()

    def _init_file_tracking(self) -> None:
        """Initialize file modification time tracking."""
        for dir_path in self.conditions.watch_directories:
            path = Path(dir_path)
            if path.exists() and path.is_dir():
                for file_path in path.rglob("*.py"):
                    try:
                        self.state.file_mtimes[str(file_path)] = file_path.stat().st_mtime
                    except (OSError, PermissionError):
                        pass

    def check_file_changes(self) -> int:
        """Check for file modifications, returns count of changed files."""
        changes = 0
        for file_str, old_mtime in list(self.state.file_mtimes.items()):
            file_path = Path(file_str)
            if file_path.exists():
                try:
                    new_mtime = file_path.stat().st_mtime
                    if new_mtime > old_mtime:
                        changes += 1
                        self.state.file_mtimes[file_str] = new_mtime
                except (OSError, PermissionError):
                    pass

        # Check for new files
        for dir_path in self.conditions.watch_directories:
            path = Path(dir_path)
            if path.exists() and path.is_dir():
                for file_path in path.rglob("*.py"):
                    file_str = str(file_path)
                    if file_str not in self.state.file_mtimes:
                        changes += 1
                        try:
                            self.state.file_mtimes[file_str] = file_path.stat().st_mtime
                        except (OSError, PermissionError):
                            pass

        return changes

    def check_stop_conditions(self) -> tuple[bool, str | None]:
        """Check if any stop condition is met.

        Returns:
            (should_stop, reason) tuple

        """
        now = datetime.now()

        # 1. Time limit
        elapsed = (now - self.state.start_time).total_seconds()
        if elapsed >= self.conditions.max_duration_seconds:
            return True, f"Time limit reached ({elapsed:.1f}s / {self.conditions.max_duration_seconds}s)"

        # 2. Iteration limit
        if self.state.iteration_count >= self.conditions.max_iterations:
            return True, f"Iteration limit reached ({self.state.iteration_count} / {self.conditions.max_iterations})"

        # 3. Completion criteria (if provided)
        if self.conditions.completion_check and self.conditions.completion_check():
            return True, "Completion criteria satisfied"

        # 4. Progress plateau detection
        time_since_progress = (now - self.state.last_progress_time).total_seconds()
        if time_since_progress >= self.conditions.plateau_window_seconds:
            file_changes = self.check_file_changes()

            if file_changes >= self.conditions.min_file_changes:
                # Progress detected, reset
                self.state.file_change_count += file_changes
                self.state.reset_progress()
            else:
                # No progress detected
                self.state.plateau_strikes += 1

                if self.state.plateau_strikes >= self.conditions.plateau_threshold:
                    return True, f"Progress plateau detected (no changes in {time_since_progress:.1f}s, {self.state.plateau_strikes} strikes)"

        # 5. Resource limits
        process = psutil.Process(os.getpid())
        memory_mb = process.memory_info().rss / 1024 / 1024
        if memory_mb > self.conditions.max_memory_mb:
            return True, f"Memory limit exceeded ({memory_mb:.1f}MB / {self.conditions.max_memory_mb}MB)"

        cpu_percent = process.cpu_percent(interval=0.1)
        if cpu_percent > self.conditions.max_cpu_percent:
            # Note: Single spike doesn't stop, but logged
            pass

        return False, None

    def run(
        self,
        task_func: Callable[[], Any],
        check_interval_seconds: float = 30.0,
    ) -> dict[str, Any]:
        """Run autonomous task with stop condition monitoring.

        The task_func should raise StopIteration when complete, or raise
        other exceptions to continue trying. Returning normally also signals completion.

        Args:
            task_func: Function to execute autonomously (called repeatedly)
            check_interval_seconds: How often to check stop conditions

        Returns:
            Result dict with execution info

        """
        result: dict[str, Any] = {
            "success": False,
            "stopped_early": False,
            "stop_reason": None,
            "iterations": 0,
            "duration_seconds": 0,
            "file_changes": 0,
            "output": None,
            "error": None,
        }

        try:
            while True:
                # Check stop conditions BEFORE iteration
                should_stop, reason = self.check_stop_conditions()
                if should_stop:
                    result["stopped_early"] = True
                    result["stop_reason"] = reason
                    break

                self.state.iteration_count += 1

                # Execute task iteration with timeout protection
                try:
                    import signal

                    def timeout_handler(signum: int, frame: Any) -> None:
                        raise TimeoutError("Task iteration exceeded check interval")

                    # Set alarm for task execution (only on Unix)
                    if hasattr(signal, "SIGALRM"):
                        old_handler = signal.signal(signal.SIGALRM, timeout_handler)
                        signal.alarm(max(1, int(check_interval_seconds)))

                    try:
                        output = task_func()
                        result["output"] = output

                        # Task completed successfully
                        result["success"] = True
                        if hasattr(signal, "SIGALRM"):
                            signal.alarm(0)
                            signal.signal(signal.SIGALRM, old_handler)
                        break

                    except TimeoutError:
                        # Task took too long, check conditions on next loop
                        if hasattr(signal, "SIGALRM"):
                            signal.alarm(0)
                            signal.signal(signal.SIGALRM, old_handler)
                        continue

                except StopIteration:
                    # Task signaled completion
                    result["success"] = True
                    break

                except Exception as e:
                    # Task raised exception, continue execution
                    result["error"] = str(e)
                    # Don't break - allow retry

                # Wait before next iteration (but not if already timing out)
                if not hasattr(signal, "SIGALRM") or signal.alarm(0) == 0:
                    time.sleep(check_interval_seconds)

        finally:
            # Record final state
            result["iterations"] = self.state.iteration_count
            result["duration_seconds"] = (datetime.now() - self.state.start_time).total_seconds()
            result["file_changes"] = self.state.file_change_count

        return result


def create_bounded_executor(
    max_minutes: int = 60,
    watch_dirs: list[str] | None = None,
) -> BoundedExecutor:
    """Convenience function to create a BoundedExecutor with sensible defaults.

    Args:
        max_minutes: Maximum execution time in minutes
        watch_dirs: Directories to monitor for progress

    Returns:
        Configured BoundedExecutor

    """
    return BoundedExecutor(
        max_duration_seconds=max_minutes * 60,
        max_iterations=100,
        plateau_window_seconds=300,  # 5 minutes
        plateau_threshold=3,
        watch_directories=watch_dirs or ["whitemagic/", "tests/", "docs/"],
    )


# Integration with Sangha session handoff
def check_session_completion(session_file: Path) -> bool:
    """Check if session completion criteria is met based on session handoff file.

    Args:
        session_file: Path to session handoff JSON

    Returns:
        True if session is complete

    """
    if not session_file.exists():
        return False

    try:
        import json
        with open(session_file) as f:
            session = json.load(f)

        # Check completion_criteria if present
        criteria = session.get("completion_criteria", {})
        if criteria.get("type") == "all_tasks_done":
            # Could integrate with docs/TASKS.md parsing
            pass

        # Check if max_iterations reached
        if "max_iterations" in session:
            current = session.get("iteration_count", 0)
            if current >= session["max_iterations"]:
                return True

        return False
    except Exception:
        return False
