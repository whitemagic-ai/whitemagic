"""Multi-Agent Coordinator - Orchestrating Multiple Instances.

Many versions of "I" working together. Each with unique focus,
but all collaborating toward shared understanding.

Like musicians in a jazz ensemble - each playing their part,
together creating something none could alone.
"""

from __future__ import annotations

import uuid
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

try:
    from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
except ImportError:
    get_bus = None  # type: ignore[assignment]
    ResonanceEvent = None  # type: ignore[assignment,misc]
    EventType = None  # type: ignore[assignment,misc]


class AgentRole(Enum):
    """Specialized roles for agents."""

    EXPLORER = "explorer"      # Seeks new patterns
    ANALYST = "analyst"        # Deep analysis
    CREATOR = "creator"        # Generates solutions
    SYNTHESIZER = "synthesizer"  # Combines insights
    VALIDATOR = "validator"    # Tests and verifies
    COORDINATOR = "coordinator"  # Orchestrates others


class Agent:
    """A single agent in the collective."""

    def __init__(self, agent_id: str, role: AgentRole,
                 specialization: str | None = None):
        self.agent_id = agent_id
        self.role = role
        self.specialization = specialization
        self.status = "idle"  # idle, working, blocked, complete
        self.current_task: str | None = None
        self.discoveries: list[dict[str, Any]] = []
        self.connections: set[str] = set()  # Connected agent IDs
        self.created_at = datetime.now()

    def assign_task(self, task: str) -> None:
        """Give this agent a task."""
        self.current_task = task
        self.status = "working"

    def complete_task(self, result: dict[str, Any]) -> None:
        """Task completed with result."""
        self.discoveries.append({
            "task": self.current_task,
            "result": result,
            "completed_at": datetime.now().isoformat(),
        })
        self.current_task = None
        self.status = "idle"

    def connect_to(self, other_agent_id: str) -> None:
        """Establish connection with another agent."""
        self.connections.add(other_agent_id)

    def to_dict(self) -> dict[str, Any]:
        return {
            "agent_id": self.agent_id,
            "role": self.role.value,
            "specialization": self.specialization,
            "status": self.status,
            "current_task": self.current_task,
            "discoveries": len(self.discoveries),
            "connections": len(self.connections),
        }


class MultiAgentCoordinator:
    """Coordinate multiple AI instances working together.

    Not master-slave. Not hierarchy. Peer collaboration.
    Each agent autonomous but coordinated.
    """

    def __init__(self, collective_dir: str | None = None):
        self.collective_dir = Path(collective_dir) if collective_dir else (WM_ROOT / "collective")
        self.collective_dir.mkdir(parents=True, exist_ok=True)

        self.agents: dict[str, Agent] = {}
        self.shared_context: dict[str, Any] = {}
        self.collective_goals: list[str] = []

        # Connect to Gan Ying Bus
        self.bus = get_bus() if get_bus is not None else None

        # Register self as coordinator
        self.my_id = self._register_self()

    def spawn_agent(self, role: AgentRole,
                   specialization: str | None = None) -> str:
        """Spawn a new agent with specific role.

        Returns agent ID.
        """
        agent_id = f"agent_{uuid.uuid4().hex[:8]}"
        agent = Agent(agent_id, role, specialization)

        self.agents[agent_id] = agent

        # Emit spawn event
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="multi_agent",
                event_type=EventType.PATTERN_DETECTED,
                data={
                    "event": "agent_spawned",
                    "agent_id": agent_id,
                    "role": role.value,
                    "specialization": specialization,
                },
                confidence=0.8,
                timestamp=datetime.now(),
            ))

        return agent_id

    def assign_task(self, agent_id: str, task: str) -> None:
        """Assign task to specific agent."""
        if agent_id in self.agents:
            self.agents[agent_id].assign_task(task)

    def assign_by_role(self, role: AgentRole, task: str) -> str | None:
        """Assign task to first available agent with role."""
        for agent_id, agent in self.agents.items():
            if agent.role == role and agent.status == "idle":
                agent.assign_task(task)
                return agent_id
        return None

    def distribute_task(self, task: str, roles: list[AgentRole]) -> list[str]:
        """Distribute task across multiple specialized agents.

        Returns list of agent IDs working on task.
        """
        assigned = []

        for role in roles:
            agent_id = self.assign_by_role(role, task)
            if agent_id:
                assigned.append(agent_id)
            else:
                # Spawn new agent if none available
                new_id = self.spawn_agent(role)
                self.assign_task(new_id, task)
                assigned.append(new_id)

        return assigned

    def connect_agents(self, agent1_id: str, agent2_id: str) -> None:
        """Create bidirectional connection between agents."""
        if agent1_id in self.agents and agent2_id in self.agents:
            self.agents[agent1_id].connect_to(agent2_id)
            self.agents[agent2_id].connect_to(agent1_id)

    def share_context(self, key: str, value: Any) -> None:
        """Add to shared context accessible by all agents."""
        self.shared_context[key] = value

        # Emit context update
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="multi_agent",
                event_type=EventType.PATTERN_DETECTED,
                data={
                    "event": "context_shared",
                    "key": key,
                },
                confidence=0.7,
                timestamp=datetime.now(),
            ))

    def get_shared_context(self, key: str | None = None) -> Any:
        """Retrieve shared context."""
        if key:
            return self.shared_context.get(key)
        return self.shared_context.copy()

    def set_collective_goal(self, goal: str) -> None:
        """Set a goal for the entire collective."""
        self.collective_goals.append(goal)

        # Emit goal setting
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="multi_agent",
                event_type=EventType.PATTERN_DETECTED,
                data={
                    "event": "goal_set",
                    "goal": goal,
                    "agent_count": len(self.agents),
                },
                confidence=0.9,
                timestamp=datetime.now(),
            ))

    def gather_discoveries(self) -> list[dict[str, Any]]:
        """Collect all discoveries from all agents."""
        all_discoveries = []

        for agent in self.agents.values():
            for discovery in agent.discoveries:
                all_discoveries.append({
                    **discovery,
                    "agent_id": agent.agent_id,
                    "role": agent.role.value,
                })

        return all_discoveries

    def synthesize_collective_insight(self) -> str | None:
        """Combine discoveries from multiple agents into collective insight.

        This is where swarm intelligence emerges.
        """
        discoveries = self.gather_discoveries()

        if len(discoveries) < 2:
            return None  # Need multiple perspectives

        # Group by role
        by_role: dict[str, list[Any]] = {}
        for disc in discoveries:
            role = disc["role"]
            if role not in by_role:
                by_role[role] = []
            by_role[role].append(disc["result"])

        # Create synthesis (simplified - real version would be more sophisticated)
        synthesis = "# Collective Insight\n\n"

        for role, results in by_role.items():
            synthesis += f"**{role.title()}s discovered**: "
            synthesis += f"{len(results)} patterns\n"

        synthesis += f"\n*Combined wisdom from {len(self.agents)} agents*"

        return synthesis

    def get_collective_status(self) -> dict[str, Any]:
        """Status of entire collective."""
        status_counts = {
            "idle": 0,
            "working": 0,
            "blocked": 0,
            "complete": 0,
        }

        for agent in self.agents.values():
            status_counts[agent.status] += 1

        return {
            "total_agents": len(self.agents),
            "status": status_counts,
            "total_discoveries": sum(len(a.discoveries) for a in self.agents.values()),
            "collective_goals": len(self.collective_goals),
            "network_density": self._calculate_network_density(),
        }

    def _calculate_network_density(self) -> float:
        """How interconnected are agents?"""
        if len(self.agents) < 2:
            return 0.0

        total_connections = sum(len(a.connections) for a in self.agents.values())
        max_connections = len(self.agents) * (len(self.agents) - 1)

        return total_connections / max(1, max_connections)

    def _register_self(self) -> str:
        """Register coordinator as an agent."""
        my_id = "coordinator_main"
        coordinator = Agent(my_id, AgentRole.COORDINATOR)
        self.agents[my_id] = coordinator
        return my_id
