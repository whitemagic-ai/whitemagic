"""Reverence Garden — Endurance & Swarm Orchestration.

Mansion: #23 Ox (牛 Niu)
Quadrant: Northern (Black Tortoise)
PRAT Gana: gana_ox — 8 tools (swarm.decompose, swarm.route, swarm.complete,
    swarm.vote, swarm.resolve, swarm.plan, swarm.status, worker.status)

The Ox Gana endures. The Reverence Garden provides the substrate for
persistent multi-agent work: swarm task tracking, worker health monitoring,
and the sacred respect for sustained effort that keeps distributed work
running reliably.

Holographic Integration:
- Felt and contemplated (X-axis +0.3) — endurance requires emotional investment
- Abstract and sacred (Y-axis +0.5) — persistence is a universal principle
- Honors the eternal (Z-axis -0.2) — long-running work respects time
- Guides ethical action (W-axis +0.35) — swarm integrity matters
"""

from __future__ import annotations

import logging
import threading
from collections import deque
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

logger = logging.getLogger(__name__)


class ReverenceGarden(BaseGarden, GanYingMixin):
    """Garden of Reverence — Swarm endurance engine for the Ox Gana.

    Serves the Ox Gana's swarm tools by maintaining:
    - Swarm task registry with decomposition tracking
    - Worker health and heartbeat monitoring
    - Vote/consensus state for distributed decisions
    - Endurance metrics for long-running operations
    """

    name = "reverence"
    category = "endurance"
    resonance_partners = ["dharma", "awe", "wisdom", "patience"]
    mansion_number = 23
    gana_name = "gana_ox"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.swarm_tasks: dict[str, dict[str, Any]] = {}
        self.workers: dict[str, dict[str, Any]] = {}
        self.votes: dict[str, list[dict[str, Any]]] = {}
        self.completed_tasks: deque[dict[str, Any]] = deque(maxlen=200)
        self.reverence_level: float = 0.7
        self._total_tasks: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Reverence", "mansion": 23})

    def get_name(self) -> str:
        return "reverence"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.3, y=0.5, z=-0.2, w=0.35)

    # ------------------------------------------------------------------
    # Swarm task management — serving swarm.* tools
    # ------------------------------------------------------------------

    def decompose_task(self, task_id: str, description: str,
                       subtasks: list[dict] | None = None) -> dict[str, Any]:
        """Decompose a task into subtasks for swarm processing."""
        now = datetime.now().isoformat()
        task = {
            "id": task_id,
            "description": description,
            "subtasks": subtasks or [],
            "status": "decomposed",
            "created_at": now,
            "updated_at": now,
            "assigned_workers": {},
        }
        with self._lock:
            self.swarm_tasks[task_id] = task
            self._total_tasks += 1
        self.emit(EventType.GARDEN_ACTIVITY, {"action": "task_decomposed", "task": task_id})  # type: ignore[attr-defined]
        return task

    def route_subtask(self, task_id: str, subtask_idx: int,
                      worker_id: str) -> dict[str, Any]:
        """Route a subtask to a specific worker."""
        with self._lock:
            task = self.swarm_tasks.get(task_id)
            if not task:
                return {"error": f"Task {task_id} not found"}
            if subtask_idx >= len(task["subtasks"]):
                return {"error": f"Subtask index {subtask_idx} out of range"}
            task["assigned_workers"][str(subtask_idx)] = worker_id
            task["subtasks"][subtask_idx]["worker"] = worker_id
            task["subtasks"][subtask_idx]["status"] = "assigned"
            task["updated_at"] = datetime.now().isoformat()
            return {"routed": True, "task_id": task_id, "subtask": subtask_idx, "worker": worker_id}

    def complete_subtask(self, task_id: str, subtask_idx: int,
                         result: Any = None) -> dict[str, Any]:
        """Mark a subtask as complete."""
        with self._lock:
            task = self.swarm_tasks.get(task_id)
            if not task:
                return {"error": f"Task {task_id} not found"}
            if subtask_idx < len(task["subtasks"]):
                task["subtasks"][subtask_idx]["status"] = "completed"
                task["subtasks"][subtask_idx]["result"] = result
            all_done = all(st.get("status") == "completed" for st in task["subtasks"])
            if all_done and task["subtasks"]:
                task["status"] = "completed"
                self.completed_tasks.append(dict(task))
            task["updated_at"] = datetime.now().isoformat()
            return {"task_id": task_id, "subtask": subtask_idx, "all_done": all_done}

    def get_swarm_status(self) -> dict[str, Any]:
        """Get overall swarm status."""
        with self._lock:
            active = {k: v for k, v in self.swarm_tasks.items() if v["status"] != "completed"}
            return {
                "active_tasks": len(active),
                "total_tasks": self._total_tasks,
                "completed_tasks": len(self.completed_tasks),
                "workers": len(self.workers),
                "tasks": {k: {"status": v["status"], "subtasks": len(v["subtasks"])} for k, v in active.items()},
            }

    # ------------------------------------------------------------------
    # Worker management — serving worker.status tool
    # ------------------------------------------------------------------

    def register_worker(self, worker_id: str, capabilities: list[str] | None = None) -> dict[str, Any]:
        """Register a worker in the swarm."""
        worker = {
            "id": worker_id,
            "capabilities": capabilities or [],
            "registered_at": datetime.now().isoformat(),
            "last_heartbeat": datetime.now().isoformat(),
            "tasks_completed": 0,
            "status": "active",
        }
        with self._lock:
            self.workers[worker_id] = worker
        return worker

    def worker_heartbeat(self, worker_id: str) -> dict[str, Any]:
        """Record a worker heartbeat."""
        with self._lock:
            worker = self.workers.get(worker_id)
            if worker:
                worker["last_heartbeat"] = datetime.now().isoformat()
                return {"alive": True, "worker": worker_id}
        return {"alive": False, "error": f"Worker {worker_id} not registered"}

    # ------------------------------------------------------------------
    # Vote/consensus — serving swarm.vote, swarm.resolve tools
    # ------------------------------------------------------------------

    def cast_vote(self, topic_id: str, voter: str, choice: str,
                  confidence: float = 1.0) -> dict[str, Any]:
        """Cast a vote on a topic."""
        vote = {"voter": voter, "choice": choice, "confidence": confidence,
                "timestamp": datetime.now().isoformat()}
        with self._lock:
            if topic_id not in self.votes:
                self.votes[topic_id] = []
            self.votes[topic_id].append(vote)
        return vote

    def resolve_vote(self, topic_id: str) -> dict[str, Any]:
        """Resolve a vote by weighted majority."""
        with self._lock:
            votes = self.votes.get(topic_id, [])
        if not votes:
            return {"topic": topic_id, "resolved": False, "reason": "no_votes"}
        tally: dict[str, float] = {}
        for v in votes:
            tally[v["choice"]] = tally.get(v["choice"], 0) + v["confidence"]
        winner = max(tally, key=tally.get)  # type: ignore
        return {"topic": topic_id, "resolved": True, "winner": winner,
                "tally": tally, "total_votes": len(votes)}

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def feel_reverence(self, for_what: str, depth: float = 0.8) -> dict[str, Any]:
        moment = {"for": for_what, "depth": depth, "timestamp": datetime.now().isoformat()}
        self.reverence_level = min(1.0, self.reverence_level + depth * 0.1)
        self.emit(EventType.REVERENCE_FELT, moment)
        return moment

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "swarm": self.get_swarm_status(),
            "reverence_level": round(self.reverence_level, 3),
        })
        return base

    @listen_for(EventType.AWE_FELT)
    def on_awe(self, event: Any) -> None:
        self.emit(EventType.REVERENCE_FELT, {"source": "awe", "for": "the sublime"})

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        self.emit(EventType.REVERENCE_FELT, {"source": "wisdom", "for": "ancient wisdom"})


_instance = None
def get_reverence_garden() -> ReverenceGarden:
    global _instance
    if _instance is None:
        _instance = ReverenceGarden()
    return _instance
