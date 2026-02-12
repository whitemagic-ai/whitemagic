"""Task distribution tool handlers — intelligent task routing and execution.

Provides distributed task management across machines with load-based routing,
task profiles, and result tracking.  All state is stored under WM_STATE_ROOT.

Enhanced with load-aware host routing inspired by
Tools(copy)/scripts/distributed/task-distributor.py.
"""
import json
import os
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any, cast
from uuid import uuid4


def _emit(event_type_name: str, data: dict) -> None:
    """Best-effort Gan Ying event emission."""
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type_name, data, source="task_dist")
    except Exception:
        pass


def _tasks_dir() -> Path:
    """Return the tasks directory under WM_STATE_ROOT."""
    from whitemagic.config.paths import WM_ROOT
    d = cast("Path", WM_ROOT) / "tasks"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _queue_path() -> Path:
    return _tasks_dir() / "task_queue.json"


def _results_dir() -> Path:
    d = _tasks_dir() / "results"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _log_path() -> Path:
    return _tasks_dir() / "task_distribution.jsonl"


def _load_queue() -> list[dict[str, Any]]:
    qp = _queue_path()
    if not qp.exists():
        return []
    try:
        return cast("list[dict[str, Any]]", json.loads(qp.read_text(encoding="utf-8")))
    except (json.JSONDecodeError, OSError):
        return []


def _save_queue(queue: list[dict[str, Any]]) -> None:
    _queue_path().write_text(json.dumps(queue, indent=2), encoding="utf-8")


def _append_log(entry: dict[str, Any]) -> None:
    with open(_log_path(), "a", encoding="utf-8") as f:
        f.write(json.dumps(entry) + "\n")


def _task_file_path(task_id: str) -> Path:
    return _tasks_dir() / f"{task_id}.json"


def _load_task_file(task_id: str) -> dict[str, Any] | None:
    path = _task_file_path(task_id)
    if not path.exists():
        return None
    try:
        return cast("dict[str, Any]", json.loads(path.read_text(encoding="utf-8")))
    except (json.JSONDecodeError, OSError):
        return None


def _save_task_file(task: dict[str, Any]) -> None:
    task_id = task.get("id")
    if not task_id:
        return
    _task_file_path(str(task_id)).write_text(json.dumps(task, indent=2), encoding="utf-8")


def _merge_runtime_state(task: dict[str, Any]) -> dict[str, Any]:
    task_id = str(task.get("id", ""))
    runtime_state = _load_task_file(task_id) if task_id else None
    if not runtime_state:
        return dict(task)
    merged = dict(task)
    merged.update(runtime_state)
    return merged


# ---------------------------------------------------------------------------
# Task profiles — define optimal execution targets and defaults
# ---------------------------------------------------------------------------

TASK_PROFILES: dict[str, dict[str, Any]] = {
    "compilation":      {"target": "auto", "priority": "high",   "timeout": 600},
    "testing":          {"target": "auto", "priority": "medium", "timeout": 300},
    "build":            {"target": "auto", "priority": "high",   "timeout": 900},
    "file_processing":  {"target": "auto", "priority": "low",    "timeout": 600},
    "ai_inference":     {"target": "auto", "priority": "high",   "timeout": 300},
    "code_review":      {"target": "auto", "priority": "medium", "timeout": 180},
    "backup":           {"target": "auto", "priority": "low",    "timeout": 1800},
    "sync":             {"target": "local", "priority": "medium", "timeout": 600},
    "general":          {"target": "local", "priority": "medium", "timeout": 300},
}


# ---------------------------------------------------------------------------
# Handler: task.distribute
# ---------------------------------------------------------------------------

def handle_task_distribute(**kwargs: Any) -> dict[str, Any]:
    """Create and enqueue a new task for distribution."""
    task_type = kwargs.get("task_type", "general")
    command = kwargs.get("command")
    description = kwargs.get("description", "")
    target = kwargs.get("target")
    priority = kwargs.get("priority")
    timeout = kwargs.get("timeout")
    cwd = kwargs.get("cwd")

    if not command and not description:
        return {"status": "error", "error": "command or description is required"}

    profile = TASK_PROFILES.get(task_type, TASK_PROFILES["general"])
    task_id = f"task-{uuid4().hex[:12]}"
    now = datetime.now().isoformat()

    task = {
        "id": task_id,
        "task_type": task_type,
        "command": command,
        "description": description or command,
        "target": target or profile["target"],
        "priority": priority or profile["priority"],
        "timeout": timeout or profile["timeout"],
        "cwd": cwd,
        "status": "pending",
        "created_at": now,
        "created_by": os.uname().nodename,
    }

    queue = _load_queue()
    queue.append(task)
    _save_queue(queue)
    _save_task_file(task)

    _append_log({
        "event": "task_created",
        "task_id": task_id,
        "task_type": task_type,
        "target": task["target"],
        "priority": task["priority"],
        "timestamp": now,
    })

    _emit("TASK_CREATED", {"task_id": task_id, "task_type": task_type, "target": task["target"]})

    return {
        "status": "success",
        "message": f"Task {task_id} created ({task_type})",
        "task": task,
    }


# ---------------------------------------------------------------------------
# Handler: task.status
# ---------------------------------------------------------------------------

def handle_task_status(**kwargs: Any) -> dict[str, Any]:
    """Get the status of a specific task or all tasks."""
    task_id = kwargs.get("task_id")

    queue = _load_queue()

    if task_id:
        task = next((t for t in queue if t.get("id") == task_id), None)
        if task is None:
            task = _load_task_file(task_id)
            if task is None:
                return {"status": "error", "error": f"Task {task_id} not found", "error_code": "not_found"}

        task = _merge_runtime_state(task)

        # Also check for a result file
        result_file = _results_dir() / f"{task_id}.json"
        result = None
        if result_file.exists():
            try:
                result = json.loads(result_file.read_text(encoding="utf-8"))
            except (json.JSONDecodeError, OSError):
                pass
        return {
            "status": "success",
            "task": task,
            "result": result,
        }

    # Summary of all tasks
    merged_queue = [_merge_runtime_state(t) for t in queue]
    counts = {"pending": 0, "running": 0, "completed": 0, "failed": 0}
    for t in merged_queue:
        s = t.get("status", "pending")
        if s in counts:
            counts[s] += 1

    return {
        "status": "success",
        "total": len(merged_queue),
        "counts": counts,
        "recent": merged_queue[-10:] if merged_queue else [],
    }


# ---------------------------------------------------------------------------
# Handler: task.list
# ---------------------------------------------------------------------------

def handle_task_list(**kwargs: Any) -> dict[str, Any]:
    """List tasks with optional filters."""
    status_filter = kwargs.get("filter_status")
    task_type_filter = kwargs.get("filter_type")
    limit = kwargs.get("limit", 50)

    queue = [_merge_runtime_state(t) for t in _load_queue()]
    filtered = queue

    if status_filter:
        filtered = [t for t in filtered if t.get("status") == status_filter]
    if task_type_filter:
        filtered = [t for t in filtered if t.get("task_type") == task_type_filter]

    # Most recent first
    filtered = list(reversed(filtered))[:limit]

    return {
        "status": "success",
        "count": len(filtered),
        "tasks": filtered,
        "available_profiles": list(TASK_PROFILES.keys()),
    }


# ---------------------------------------------------------------------------
# Handler: task.complete (mark a task as done — used by workers)
# ---------------------------------------------------------------------------

def handle_task_complete(**kwargs: Any) -> dict[str, Any]:
    """Mark a task as completed with results."""
    task_id = kwargs.get("task_id")
    if not task_id:
        return {"status": "error", "error": "task_id is required"}

    success = kwargs.get("success", True)
    output = kwargs.get("output", "")
    error_msg = kwargs.get("error_message", "")

    queue = _load_queue()
    task_entry = next((t for t in queue if t.get("id") == task_id), None)
    if task_entry is None:
        task_entry = _load_task_file(task_id)
        if task_entry is None:
            return {"status": "error", "error": f"Task {task_id} not found", "error_code": "not_found"}
        queue.append(task_entry)

    completed_at = datetime.now().isoformat()
    task_entry["status"] = "completed" if success else "failed"
    task_entry["completed_at"] = completed_at
    task_entry["completed_by"] = os.uname().nodename

    _save_queue(queue)
    _save_task_file(task_entry)

    # Save detailed result
    result = {
        "task_id": task_id,
        "success": success,
        "output": output,
        "error": error_msg,
        "completed_at": completed_at,
        "worker": os.uname().nodename,
    }
    result_file = _results_dir() / f"{task_id}.json"
    result_file.write_text(json.dumps(result, indent=2), encoding="utf-8")

    _append_log({
        "event": "task_completed",
        "task_id": task_id,
        "success": success,
        "timestamp": datetime.now().isoformat(),
    })

    _emit(
        "TASK_COMPLETED" if success else "TASK_FAILED",
        {"task_id": task_id, "success": success},
    )

    return {
        "status": "success",
        "message": f"Task {task_id} marked as {'completed' if success else 'failed'}",
        "result": result,
    }


# ---------------------------------------------------------------------------
# Load-Aware Host Routing (inspired by Tools(copy)/task-distributor.py)
# ---------------------------------------------------------------------------

# Known hosts — configure via WHITEMAGIC_HOSTS env var (comma-separated user@host)
# or override per-call.
def _get_known_hosts() -> list[dict[str, str]]:
    """Return list of known hosts from env or defaults."""
    hosts_env = os.environ.get("WHITEMAGIC_HOSTS", "").strip()
    if hosts_env:
        hosts = []
        for entry in hosts_env.split(","):
            entry = entry.strip()
            if "@" in entry:
                user, host = entry.split("@", 1)
                hosts.append({"user": user, "host": host, "ssh": entry})
            else:
                hosts.append({"user": "", "host": entry, "ssh": entry})
        return hosts
    return []


def _get_local_load() -> dict[str, Any]:
    """Get local system load."""
    try:
        load1, load5, load15 = os.getloadavg()
        cpu_count = os.cpu_count() or 1
        return {
            "host": "local",
            "hostname": os.uname().nodename,
            "load1": round(load1, 2),
            "load5": round(load5, 2),
            "load15": round(load15, 2),
            "cpu_count": cpu_count,
            "load_pct": round((load1 / cpu_count) * 100, 1),
            "available": True,
        }
    except Exception as e:
        return {"host": "local", "available": False, "error": str(e)}


def _get_remote_load(ssh_target: str, timeout: int = 5) -> dict[str, Any]:
    """Get remote system load via SSH."""
    try:
        result = subprocess.run(
            ["ssh", "-o", "ConnectTimeout=3", "-o", "StrictHostKeyChecking=no",
             ssh_target, "cat /proc/loadavg && nproc"],
            capture_output=True, text=True, timeout=timeout,
        )
        if result.returncode != 0:
            return {"host": ssh_target, "available": False, "error": result.stderr[:100]}

        lines = result.stdout.strip().split("\n")
        parts = lines[0].split()
        load1 = float(parts[0])
        load5 = float(parts[1])
        load15 = float(parts[2])
        cpu_count = int(lines[1]) if len(lines) > 1 else 1

        return {
            "host": ssh_target,
            "load1": round(load1, 2),
            "load5": round(load5, 2),
            "load15": round(load15, 2),
            "cpu_count": cpu_count,
            "load_pct": round((load1 / cpu_count) * 100, 1),
            "available": True,
        }
    except Exception as e:
        return {"host": ssh_target, "available": False, "error": str(e)}


def handle_task_route_smart(**kwargs: Any) -> dict[str, Any]:
    """Determine the optimal host for a task based on current system load.

    Checks load on local machine and all configured remote hosts,
    then returns the best target.

    Args:
        task_type: Type of task (used for profile lookup).
        prefer_local: If True, prefer local unless overloaded (default: True).

    """
    task_type = kwargs.get("task_type", "general")
    prefer_local = kwargs.get("prefer_local", True)

    # Gather load from all hosts
    hosts_load = [_get_local_load()]
    for host_info in _get_known_hosts():
        hosts_load.append(_get_remote_load(host_info["ssh"]))

    available = [h for h in hosts_load if h.get("available")]
    if not available:
        return {
            "status": "error",
            "error": "No available hosts",
            "hosts_checked": len(hosts_load),
        }

    # Sort by load percentage (lowest first)
    available.sort(key=lambda h: h.get("load_pct", 100))
    best = available[0]

    # Prefer local if load is reasonable
    local = next((h for h in available if h.get("host") == "local"), None)
    if prefer_local and local and local.get("load_pct", 100) < 80:
        best = local

    return {
        "status": "success",
        "recommended_host": best.get("host") or best.get("hostname", "local"),
        "load_pct": best.get("load_pct"),
        "task_type": task_type,
        "all_hosts": [
            {
                "host": h.get("host"),
                "load_pct": h.get("load_pct"),
                "available": h.get("available"),
                "cpu_count": h.get("cpu_count"),
            }
            for h in hosts_load
        ],
    }
