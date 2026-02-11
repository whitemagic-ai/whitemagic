"""Agent Swarm Protocols — Task Decomposition, Capability Routing, Consensus
===========================================================================
Enables multi-agent coordination within WhiteMagic:
  - Task decomposition: Break complex tasks into subtasks
  - Capability routing: Match subtasks to agents by declared capabilities
  - Consensus: Collect votes and resolve disagreements

Integrates with the existing agent registry (agent.register, agent.list).

Usage:
    from whitemagic.agents.swarm import get_swarm
    swarm = get_swarm()
    plan = swarm.decompose("Analyze codebase and generate report", context={})
    assignments = swarm.route(plan)
    result = swarm.execute(assignments)
"""
from __future__ import annotations

import logging
import threading
import time
import uuid
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class TaskStatus(str, Enum):
    PENDING = "pending"
    ASSIGNED = "assigned"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"


class ConsensusStrategy(str, Enum):
    MAJORITY = "majority"
    UNANIMOUS = "unanimous"
    FIRST_WINS = "first_wins"
    WEIGHTED = "weighted"


@dataclass
class SubTask:
    id: str
    description: str
    required_capabilities: list[str] = field(default_factory=list)
    status: TaskStatus = TaskStatus.PENDING
    assigned_to: str | None = None
    result: Any | None = None
    priority: int = 0
    depends_on: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "description": self.description,
            "required_capabilities": self.required_capabilities,
            "status": self.status.value,
            "assigned_to": self.assigned_to,
            "result": self.result,
            "priority": self.priority,
            "depends_on": self.depends_on,
        }


@dataclass
class SwarmPlan:
    id: str
    goal: str
    subtasks: list[SubTask] = field(default_factory=list)
    created_at: float = 0.0
    completed_at: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        done = sum(1 for t in self.subtasks if t.status == TaskStatus.COMPLETED)
        return {
            "plan_id": self.id,
            "goal": self.goal,
            "subtask_count": len(self.subtasks),
            "completed": done,
            "progress": round(done / max(len(self.subtasks), 1), 2),
            "subtasks": [t.to_dict() for t in self.subtasks],
        }


@dataclass
class Vote:
    agent_id: str
    value: Any
    confidence: float = 1.0
    timestamp: float = 0.0


class AgentSwarm:
    """Multi-agent coordination with decomposition, routing, and consensus."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._plans: dict[str, SwarmPlan] = {}
        self._votes: dict[str, list[Vote]] = {}  # topic_id -> votes
        self._max_plans = 100

    def decompose(self, goal: str, hints: list[str] | None = None) -> SwarmPlan:
        """Decompose a goal into subtasks.

        Uses heuristic keyword-based decomposition. For richer decomposition,
        feed the plan through an LLM via the pipeline system.
        """
        plan_id = f"plan_{uuid.uuid4().hex[:8]}"
        subtasks = []

        # Heuristic decomposition based on common patterns
        keywords_to_caps = {
            "search": ["memory_search", "vector_search"],
            "analyze": ["analysis", "pattern_detection"],
            "report": ["reporting", "formatting"],
            "code": ["code_analysis", "file_ops"],
            "memory": ["memory_ops", "memory_search"],
            "test": ["testing", "validation"],
            "deploy": ["deployment", "infrastructure"],
            "monitor": ["monitoring", "metrics"],
            "ethic": ["dharma", "ethics"],
            "harmony": ["harmony", "balance"],
        }

        goal_lower = goal.lower()
        detected_caps = set()
        for kw, caps in keywords_to_caps.items():
            if kw in goal_lower:
                detected_caps.update(caps)

        # If hints provided, use those as subtask descriptions
        if hints:
            for i, hint in enumerate(hints):
                caps = []
                for kw, c in keywords_to_caps.items():
                    if kw in hint.lower():
                        caps.extend(c)
                subtasks.append(SubTask(
                    id=f"{plan_id}_t{i}",
                    description=hint,
                    required_capabilities=caps or ["general"],
                    priority=len(hints) - i,
                ))
        else:
            # Auto-generate subtasks from detected capabilities
            if not detected_caps:
                detected_caps = {"general"}

            for i, cap in enumerate(sorted(detected_caps)):
                subtasks.append(SubTask(
                    id=f"{plan_id}_t{i}",
                    description=f"{cap}: {goal}",
                    required_capabilities=[cap],
                    priority=i,
                ))

        plan = SwarmPlan(
            id=plan_id,
            goal=goal,
            subtasks=subtasks,
            created_at=time.time(),
        )

        with self._lock:
            self._plans[plan_id] = plan
            if len(self._plans) > self._max_plans:
                oldest = sorted(self._plans.values(), key=lambda p: p.created_at)
                for p in oldest[:10]:
                    del self._plans[p.id]

        return plan

    def route(self, plan_id: str) -> dict[str, Any]:
        """Route subtasks to available agents based on capability matching.
        Uses the agent registry to find suitable agents.
        """
        with self._lock:
            plan = self._plans.get(plan_id)
            if not plan:
                return {"status": "error", "error": f"Plan {plan_id} not found"}

        # Get available agents
        agents: list[dict[str, Any]] = []
        try:
            from whitemagic.tools.handlers.agent_registry import handle_agent_list

            listed = handle_agent_list(only_active=True)
            raw_agents = listed.get("agents", [])
            if isinstance(raw_agents, list):
                for agent in raw_agents:
                    if isinstance(agent, dict):
                        agents.append(
                            {
                                "agent_id": str(agent.get("id", "")),
                                "capabilities": list(agent.get("capabilities", [])),
                            }
                        )
        except Exception:
            pass

        assignments = []
        for task in plan.subtasks:
            if task.status != TaskStatus.PENDING:
                continue

            best_agent = None
            best_score = 0

            for agent in agents:
                # Score = number of matching capabilities
                match = len(set(task.required_capabilities) & set(agent["capabilities"]))
                if match > best_score:
                    best_score = match
                    best_agent = agent["agent_id"]

            if best_agent:
                task.assigned_to = best_agent
                task.status = TaskStatus.ASSIGNED
                assignments.append({
                    "task_id": task.id,
                    "agent_id": best_agent,
                    "capabilities_matched": best_score,
                })
            else:
                assignments.append({
                    "task_id": task.id,
                    "agent_id": None,
                    "reason": "no matching agent found",
                })

        return {
            "status": "success",
            "plan_id": plan_id,
            "assignments": assignments,
            "agents_available": len(agents),
        }

    def complete_task(self, plan_id: str, task_id: str, result: Any = None,
                      success: bool = True) -> dict[str, Any]:
        """Mark a subtask as completed or failed."""
        with self._lock:
            plan = self._plans.get(plan_id)
            if not plan:
                return {"status": "error", "error": "Plan not found"}

            for task in plan.subtasks:
                if task.id == task_id:
                    task.status = TaskStatus.COMPLETED if success else TaskStatus.FAILED
                    task.result = result
                    break
            else:
                return {"status": "error", "error": "Task not found"}

            # Check if plan is complete
            all_done = all(t.status in (TaskStatus.COMPLETED, TaskStatus.FAILED) for t in plan.subtasks)
            if all_done:
                plan.completed_at = time.time()

            return {"status": "success", "plan": plan.to_dict()}

    def vote(self, topic_id: str, agent_id: str, value: Any,
             confidence: float = 1.0) -> dict[str, Any]:
        """Record a vote from an agent on a topic."""
        v = Vote(agent_id=agent_id, value=value, confidence=confidence, timestamp=time.time())
        with self._lock:
            if topic_id not in self._votes:
                self._votes[topic_id] = []
            self._votes[topic_id].append(v)
        return {"status": "success", "topic_id": topic_id, "votes_count": len(self._votes[topic_id])}

    def resolve(self, topic_id: str,
                strategy: ConsensusStrategy = ConsensusStrategy.MAJORITY) -> dict[str, Any]:
        """Resolve a consensus vote using the specified strategy."""
        with self._lock:
            votes = self._votes.get(topic_id, [])

        if not votes:
            return {"status": "error", "error": "No votes found"}

        if strategy == ConsensusStrategy.FIRST_WINS:
            first_winner = votes[0]
            return {"status": "success", "result": first_winner.value,
                    "strategy": strategy.value, "votes": len(votes)}

        if strategy == ConsensusStrategy.UNANIMOUS:
            values = set(str(v.value) for v in votes)
            if len(values) == 1:
                return {"status": "success", "result": votes[0].value,
                        "strategy": strategy.value, "unanimous": True}
            return {"status": "no_consensus", "strategy": strategy.value,
                    "distinct_values": len(values)}

        if strategy == ConsensusStrategy.WEIGHTED:
            weighted: dict[str, float] = {}
            for v in votes:
                key = str(v.value)
                weighted[key] = weighted.get(key, 0) + v.confidence
            weighted_winner = max(weighted, key=lambda k: weighted[k])
            return {"status": "success", "result": weighted_winner,
                    "strategy": strategy.value, "weight": weighted[weighted_winner]}

        # MAJORITY (default)
        counts: dict[str, int] = {}
        for v in votes:
            key = str(v.value)
            counts[key] = counts.get(key, 0) + 1
        majority_winner = max(counts, key=lambda k: counts[k])
        return {"status": "success", "result": majority_winner,
                "strategy": strategy.value, "count": counts[majority_winner], "total": len(votes)}

    def get_plan(self, plan_id: str) -> dict[str, Any] | None:
        with self._lock:
            plan = self._plans.get(plan_id)
            return plan.to_dict() if plan else None

    def list_plans(self, limit: int = 20) -> list[dict[str, Any]]:
        with self._lock:
            plans = sorted(self._plans.values(), key=lambda p: p.created_at, reverse=True)
            return [p.to_dict() for p in plans[:limit]]

    def status(self) -> dict[str, Any]:
        with self._lock:
            active = sum(1 for p in self._plans.values() if not p.completed_at)
            return {
                "total_plans": len(self._plans),
                "active_plans": active,
                "total_votes": sum(len(v) for v in self._votes.values()),
                "vote_topics": len(self._votes),
            }


# Singleton
_swarm: AgentSwarm | None = None
_swarm_lock = threading.Lock()

def get_swarm() -> AgentSwarm:
    global _swarm
    if _swarm is None:
        with _swarm_lock:
            if _swarm is None:
                _swarm = AgentSwarm()
    return _swarm
