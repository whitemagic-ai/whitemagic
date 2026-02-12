"""Self-Prompting System

AI can queue work for itself and process asynchronously.  Integrates with
the Temporal Scheduler (SLOW lane), Diary for audit, and TokenEconomy for
budget tracking.

Architecture
------------
WorkItems live in a priority queue persisted as JSON.  ``process_queue``
pops items in priority order, executes registered handlers, and logs
results to the Diary.  ``ask_human`` records a pending question that the
CLI or MCP layer can surface.

Usage::

    from whitemagic.autonomous.self_prompting import queue_work, process_queue

    queue_work("consolidate stale memories", priority=3)
    results = process_queue(limit=5)
"""

from __future__ import annotations

import json
import logging
import os
import time
import uuid
from dataclasses import asdict, dataclass, field
from datetime import datetime
from enum import IntEnum
from pathlib import Path
from typing import Any, Callable

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

class Priority(IntEnum):
    """Work-item priority (lower number = higher priority)."""
    CRITICAL = 1
    HIGH = 3
    NORMAL = 5
    LOW = 7
    BACKGROUND = 9


@dataclass
class WorkItem:
    """A self-queued task."""

    id: str = field(default_factory=lambda: uuid.uuid4().hex[:12])
    task: str = ""
    priority: int = Priority.NORMAL
    handler: str = ""
    params: dict[str, Any] = field(default_factory=dict)
    created_at: str = field(default_factory=lambda: datetime.now().isoformat())
    status: str = "pending"  # pending | running | done | failed | deferred
    result: str = ""
    attempts: int = 0
    max_attempts: int = 3

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "WorkItem":
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class HumanQuestion:
    """A question awaiting human input."""

    id: str = field(default_factory=lambda: uuid.uuid4().hex[:12])
    question: str = ""
    context: str = ""
    asked_at: str = field(default_factory=lambda: datetime.now().isoformat())
    answered: bool = False
    answer: str = ""


# ---------------------------------------------------------------------------
# Persistence helpers
# ---------------------------------------------------------------------------

def _state_dir() -> Path:
    root = Path(os.environ.get("WM_STATE_ROOT", os.path.expanduser("~/.whitemagic")))
    d = root / "autonomous"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _load_queue() -> list[WorkItem]:
    path = _state_dir() / "work_queue.json"
    if not path.exists():
        return []
    try:
        data = json.loads(path.read_text())
        return [WorkItem.from_dict(d) for d in data]
    except Exception as exc:
        logger.warning("Failed to load work queue: %s", exc)
        return []


def _save_queue(items: list[WorkItem]) -> None:
    path = _state_dir() / "work_queue.json"
    path.write_text(json.dumps([i.to_dict() for i in items], indent=2))


def _load_questions() -> list[HumanQuestion]:
    path = _state_dir() / "human_questions.json"
    if not path.exists():
        return []
    try:
        data = json.loads(path.read_text())
        return [HumanQuestion(**{k: v for k, v in d.items() if k in HumanQuestion.__dataclass_fields__}) for d in data]
    except Exception:
        return []


def _save_questions(qs: list[HumanQuestion]) -> None:
    path = _state_dir() / "human_questions.json"
    path.write_text(json.dumps([asdict(q) for q in qs], indent=2))


# ---------------------------------------------------------------------------
# Handler registry
# ---------------------------------------------------------------------------

_handlers: dict[str, Callable[..., Any]] = {}


def register_handler(name: str, fn: Callable[..., Any]) -> None:
    """Register a named handler for work items."""
    _handlers[name] = fn


def _default_handler(item: WorkItem) -> str:
    """Fallback handler that just logs the task."""
    logger.info("Self-prompt executing (no handler): %s", item.task)
    return f"executed: {item.task}"


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def queue_work(
    task: str,
    priority: int = Priority.NORMAL,
    handler: str = "",
    params: dict[str, Any] | None = None,
) -> str:
    """Queue work for later processing.

    Returns the work-item ID.
    """
    item = WorkItem(
        task=task,
        priority=int(priority),
        handler=handler,
        params=params or {},
    )
    items = _load_queue()
    items.append(item)
    _save_queue(items)
    logger.info("Queued work [%s] pri=%d: %s", item.id, item.priority, task)
    return item.id


def process_queue(limit: int = 10) -> dict[str, Any]:
    """Process queued work items in priority order.

    Returns a summary dict with counts of processed / failed / remaining.
    """
    items = _load_queue()
    pending = [i for i in items if i.status == "pending"]
    pending.sort(key=lambda i: i.priority)

    stats: dict[str, Any] = {"processed": 0, "failed": 0, "skipped": 0, "remaining": 0, "results": []}

    for item in pending[:limit]:
        item.attempts += 1
        item.status = "running"
        handler_fn = _handlers.get(item.handler, _default_handler)
        start = time.monotonic()
        try:
            result = handler_fn(item)
            item.status = "done"
            item.result = str(result) if result else "ok"
            stats["processed"] += 1
            stats["results"].append({"id": item.id, "task": item.task, "result": item.result})
        except Exception as exc:
            if item.attempts >= item.max_attempts:
                item.status = "failed"
                stats["failed"] += 1
            else:
                item.status = "pending"  # retry later
                stats["skipped"] += 1
            item.result = f"error: {exc}"
            logger.warning("Work item %s failed (attempt %d): %s", item.id, item.attempts, exc)
        elapsed = time.monotonic() - start

        # Log to diary if available
        try:
            from whitemagic.autonomous.diary import get_diary
            diary = get_diary()
            diary.log_hourly(
                f"self_prompt:{item.handler or 'default'}",
                f"task={item.task} status={item.status} elapsed={round(elapsed, 3)}s",
            )
        except Exception:
            pass

    stats["remaining"] = sum(1 for i in items if i.status == "pending")
    _save_queue(items)
    return stats


def ask_human(question: str, context: str = "") -> str:
    """Record a question for the human operator.

    The question is persisted.  The CLI ``wm questions`` command (or MCP
    ``ask_human`` tool) can list and answer them.  Returns the question ID.
    """
    q = HumanQuestion(question=question, context=context)
    qs = _load_questions()
    qs.append(q)
    _save_questions(qs)
    logger.info("Question queued [%s]: %s", q.id, question)
    return q.id


def answer_question(question_id: str, answer: str) -> bool:
    """Provide an answer to a pending question. Returns True if found."""
    qs = _load_questions()
    for q in qs:
        if q.id == question_id and not q.answered:
            q.answered = True
            q.answer = answer
            _save_questions(qs)
            logger.info("Question %s answered: %s", question_id, answer[:80])
            return True
    return False


def get_pending_questions() -> list[dict[str, Any]]:
    """Return all unanswered questions."""
    qs = _load_questions()
    return [asdict(q) for q in qs if not q.answered]


def get_queue_status() -> dict[str, Any]:
    """Return a summary of the work queue."""
    items = _load_queue()
    by_status: dict[str, int] = {}
    for item in items:
        by_status[item.status] = by_status.get(item.status, 0) + 1
    return {
        "total": len(items),
        "by_status": by_status,
        "pending_questions": len([q for q in _load_questions() if not q.answered]),
    }


def clear_completed() -> int:
    """Remove done/failed items from the queue. Returns count removed."""
    items = _load_queue()
    before = len(items)
    items = [i for i in items if i.status not in ("done", "failed")]
    _save_queue(items)
    removed = before - len(items)
    logger.info("Cleared %d completed work items", removed)
    return removed
