"""Worker Daemon — Background Task Execution Agent
=================================================
Inspired by Tools(copy)/scripts/distributed/worker.py.

A persistent background process that polls for pending tasks from the
WM task queue and executes them. Designed to run on secondary machines
(e.g. Inspiron) or as a local background service.

Integrates with:
  - Task distribution (whitemagic/tools/handlers/task_dist.py)
  - Agent registry (registers itself as a worker agent)
  - Gan Ying bus (emits task lifecycle events)
  - Redis bridge (for cross-machine coordination)

Usage:
    # As a module
    from whitemagic.agents.worker_daemon import WorkerDaemon
    daemon = WorkerDaemon()
    daemon.run()  # blocking

    # As a script
    python -m whitemagic.agents.worker_daemon [--poll-interval 5]
"""

import json
import logging
import os
import shlex
import subprocess
import time
from datetime import datetime
from pathlib import Path
from typing import Any, cast

logger = logging.getLogger(__name__)


def _tasks_dir() -> Path:
    """Return the tasks directory under WM_STATE_ROOT."""
    try:
        from whitemagic.config.paths import WM_ROOT
        d = WM_ROOT / "tasks"
    except Exception:
        d = Path.home() / ".whitemagic" / "tasks"
    d.mkdir(parents=True, exist_ok=True)
    return cast(Path, d)


def _results_dir() -> Path:
    d = _tasks_dir() / "results"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _emit(event_type: str, data: dict) -> None:
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type, data, source="worker_daemon")
    except Exception:
        pass


class WorkerDaemon:
    """Background task worker that polls for pending tasks and executes them.

    Supports two execution modes:
      - **tool**: Execute a Whitemagic MCP tool call
      - **shell**: Execute a shell command (with safety checks)
    """

    def __init__(
        self,
        poll_interval: float = 5.0,
        worker_name: str | None = None,
        max_concurrent: int = 3,
    ):
        self.poll_interval = poll_interval
        self.worker_name = worker_name or f"worker-{os.uname().nodename}"
        self.max_concurrent = max_concurrent
        self._running = False
        self._tasks_completed = 0
        self._tasks_failed = 0
        self._started_at: str | None = None

    # ------------------------------------------------------------------
    # Main loop
    # ------------------------------------------------------------------

    def run(self) -> None:
        """Main blocking worker loop."""
        self._running = True
        self._started_at = datetime.now().isoformat()

        logger.info(f"Worker daemon '{self.worker_name}' started")
        logger.info(f"Tasks dir: {_tasks_dir()}")
        logger.info(f"Poll interval: {self.poll_interval}s")

        # Register as an agent
        self._register()

        try:
            while self._running:
                try:
                    pending = self._get_pending_tasks()
                    if pending:
                        logger.info(f"Found {len(pending)} pending task(s)")
                        for task in pending[:self.max_concurrent]:
                            self._execute_task(task)
                    time.sleep(self.poll_interval)

                    # Heartbeat
                    self._heartbeat()

                except KeyboardInterrupt:
                    logger.info("Worker stopped by user")
                    break
                except Exception as e:
                    logger.error(f"Worker loop error: {e}")
                    time.sleep(self.poll_interval * 2)
        finally:
            self._running = False
            self._deregister()
            logger.info("Worker daemon stopped")

    def stop(self) -> None:
        """Signal the worker to stop."""
        self._running = False

    # ------------------------------------------------------------------
    # Task management
    # ------------------------------------------------------------------

    def _get_pending_tasks(self) -> list[dict[str, Any]]:
        """Scan tasks directory for pending tasks."""
        tasks = []
        tdir = _tasks_dir()
        for f in sorted(tdir.glob("task-*.json")):
            try:
                task = json.loads(f.read_text(encoding="utf-8"))
                if task.get("status") == "pending":
                    # Check if assigned to us or unassigned
                    target = task.get("target_worker")
                    if target is None or target == self.worker_name or target == "any":
                        tasks.append(task)
            except (json.JSONDecodeError, OSError):
                continue
        return tasks

    def _execute_task(self, task: dict[str, Any]) -> dict[str, Any]:
        """Execute a single task."""
        task_id = task.get("id", "unknown")
        mode = task.get("mode", "tool")
        logger.info(f"Executing task {task_id} (mode={mode})")

        # Mark as running
        task["status"] = "running"
        task["worker"] = self.worker_name
        task["started_at"] = datetime.now().isoformat()
        self._save_task(task)

        _emit("TASK_STARTED", {"task_id": task_id, "worker": self.worker_name, "mode": mode})

        result: dict[str, Any]
        try:
            if mode == "tool":
                result = self._execute_tool_task(task)
            elif mode == "shell":
                result = self._execute_shell_task(task)
            else:
                result = {"success": False, "error": f"Unknown mode: {mode}"}
        except Exception as e:
            result = {"success": False, "error": str(e)}

        # Update task status
        task["status"] = "completed" if result.get("success") else "failed"
        task["result"] = result
        task["completed_at"] = datetime.now().isoformat()
        self._save_task(task)

        # Save result separately
        result_path = _results_dir() / f"{task_id}.json"
        try:
            result_path.write_text(json.dumps({
                "task_id": task_id,
                "worker": self.worker_name,
                "result": result,
                "completed_at": task["completed_at"],
            }, indent=2, default=str), encoding="utf-8")
        except Exception:
            pass

        if result.get("success"):
            self._tasks_completed += 1
            _emit("TASK_COMPLETED", {"task_id": task_id, "worker": self.worker_name})
        else:
            self._tasks_failed += 1
            _emit("TASK_FAILED", {"task_id": task_id, "worker": self.worker_name,
                                   "error": result.get("error", "")[:200]})

        return result

    def _execute_tool_task(self, task: dict[str, Any]) -> dict[str, Any]:
        """Execute a Whitemagic tool call."""
        tool_name = task.get("tool")
        if not tool_name:
            return {"success": False, "error": "No tool specified"}

        args = task.get("args", {})
        try:
            from whitemagic.tools.unified_api import call_tool
            result = call_tool(tool_name, **args)
            return {
                "success": result.get("status") != "error",
                "details": result,
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    def _execute_shell_task(self, task: dict[str, Any]) -> dict[str, Any]:
        """Execute a shell command with safety checks."""
        command = task.get("command")
        if not command:
            return {"success": False, "error": "No command specified"}

        if isinstance(command, str):
            try:
                command_args = shlex.split(command)
            except ValueError as e:
                return {"success": False, "error": f"Invalid command syntax: {e}"}
        elif isinstance(command, list) and all(isinstance(p, str) for p in command):
            command_args = command
        else:
            return {"success": False, "error": "Command must be a string or list of strings"}

        if not command_args:
            return {"success": False, "error": "No command specified"}

        # Safety: reject dangerous patterns and shell metacharacters
        dangerous = ["rm -rf /", "dd if=", "mkfs", ":(){:|:&};:", "chmod -R 777"]
        rendered_command = " ".join(command_args)
        if any(p in rendered_command for p in dangerous):
            return {"success": False, "error": "Command rejected for safety"}
        if any(t in command_args for t in {"|", "||", "&&", ";", ">", ">>", "<", "`"}):
            return {"success": False, "error": "Command rejected: shell metacharacters are not allowed"}

        cwd = task.get("cwd", str(Path.home()))
        timeout = task.get("timeout", 300)

        try:
            result = subprocess.run(
                command_args,
                shell=False,
                capture_output=True,
                text=True,
                cwd=cwd, timeout=timeout,
            )
            return {
                "success": result.returncode == 0,
                "returncode": result.returncode,
                "stdout": result.stdout[-2000:] if result.stdout else "",
                "stderr": result.stderr[-1000:] if result.stderr else "",
            }
        except subprocess.TimeoutExpired:
            return {"success": False, "error": f"Timeout after {timeout}s"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def _save_task(self, task: dict[str, Any]) -> None:
        """Save task state to disk."""
        task_id = task.get("id", "unknown")
        path = _tasks_dir() / f"{task_id}.json"
        try:
            path.write_text(json.dumps(task, indent=2, default=str), encoding="utf-8")
        except Exception as e:
            logger.error(f"Failed to save task {task_id}: {e}")

    # ------------------------------------------------------------------
    # Agent registry integration
    # ------------------------------------------------------------------

    def _register(self) -> None:
        """Register this worker in the agent registry."""
        try:
            from whitemagic.tools.handlers.agent_registry import handle_agent_register
            handle_agent_register(
                name=self.worker_name,
                agent_id=self.worker_name,
                capabilities=["task_execution", "shell_execution", "tool_execution"],
                metadata={"type": "worker_daemon", "host": os.uname().nodename},
            )
        except Exception as e:
            logger.debug(f"Agent registration failed: {e}")

    def _heartbeat(self) -> None:
        """Send heartbeat to agent registry."""
        try:
            from whitemagic.tools.handlers.agent_registry import handle_agent_heartbeat
            handle_agent_heartbeat(
                agent_id=self.worker_name,
                workload=self._tasks_completed + self._tasks_failed,
                current_task=None,
            )
        except Exception:
            pass

    def _deregister(self) -> None:
        """Deregister from agent registry."""
        try:
            from whitemagic.tools.handlers.agent_registry import handle_agent_deregister
            handle_agent_deregister(agent_id=self.worker_name)
        except Exception:
            pass

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {
            "worker_name": self.worker_name,
            "running": self._running,
            "started_at": self._started_at,
            "tasks_completed": self._tasks_completed,
            "tasks_failed": self._tasks_failed,
            "poll_interval": self.poll_interval,
            "max_concurrent": self.max_concurrent,
        }


# ---------------------------------------------------------------------------
# CLI entrypoint
# ---------------------------------------------------------------------------

def main() -> None:
    import argparse
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")

    parser = argparse.ArgumentParser(description="WhiteMagic Worker Daemon")
    parser.add_argument("--poll-interval", type=float, default=5.0)
    parser.add_argument("--name", type=str, default=None)
    parser.add_argument("--max-concurrent", type=int, default=3)
    args = parser.parse_args()

    daemon = WorkerDaemon(
        poll_interval=args.poll_interval,
        worker_name=args.name,
        max_concurrent=args.max_concurrent,
    )
    daemon.run()


if __name__ == "__main__":
    main()
