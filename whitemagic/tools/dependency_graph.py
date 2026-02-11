"""Tool Dependency Graph — Static Affinity Map for AI Planning.
=============================================================
Exposes a directed graph of tool relationships so AI callers can
plan multi-step operations more intelligently.

Edge types:
  - **requires**: Tool A must run before Tool B (hard dependency)
  - **suggests**: Tool A often chains into Tool B (soft affinity)
  - **provides**: Tool A produces data consumed by Tool B

The graph is built from:
  1. Static declarations (hand-curated common patterns)
  2. Pipeline history (learned from past pipeline executions)

The AI can query:
  - ``next_steps(tool)`` — what tools commonly follow this one?
  - ``prerequisites(tool)`` — what must run before this tool?
  - ``plan(goal_tool)`` — shortest dependency chain to reach a tool

Usage:
    from whitemagic.tools.dependency_graph import get_tool_graph
    graph = get_tool_graph()
    print(graph.next_steps("vote.create"))
    # [("vote.cast", "suggests"), ("vote.analyze", "suggests")]
"""

from __future__ import annotations

import logging
import threading
from collections import defaultdict
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)


class EdgeType:
    REQUIRES = "requires"
    SUGGESTS = "suggests"
    PROVIDES = "provides"


@dataclass
class ToolEdge:
    """A directed edge in the tool dependency graph."""

    source: str
    target: str
    edge_type: str   # requires | suggests | provides
    weight: float = 1.0   # 0-1 confidence/frequency
    description: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "source": self.source,
            "target": self.target,
            "type": self.edge_type,
            "weight": round(self.weight, 2),
            "description": self.description,
        }


# ---------------------------------------------------------------------------
# Static declarations (common tool chains)
# ---------------------------------------------------------------------------

_STATIC_EDGES: list[ToolEdge] = [
    # Memory lifecycle
    ToolEdge("create_memory", "search_memory", EdgeType.SUGGESTS, 0.7,
             "After creating, often search to verify"),
    ToolEdge("search_memory", "get_memory", EdgeType.SUGGESTS, 0.8,
             "Search results lead to detail fetch"),
    ToolEdge("get_memory", "update_memory", EdgeType.SUGGESTS, 0.5,
             "Reading a memory may lead to updates"),
    ToolEdge("memory.lifecycle_sweep", "memory.lifecycle_stats", EdgeType.SUGGESTS, 0.9,
             "Check stats after running a sweep"),
    ToolEdge("memory.consolidate", "memory.consolidation_stats", EdgeType.SUGGESTS, 0.9,
             "Check stats after consolidation"),

    # Voting workflow
    ToolEdge("vote.create", "vote.cast", EdgeType.REQUIRES, 1.0,
             "Must create a session before casting votes"),
    ToolEdge("vote.cast", "vote.analyze", EdgeType.SUGGESTS, 0.8,
             "After voting, analyze results"),
    ToolEdge("vote.analyze", "vote.record_outcome", EdgeType.SUGGESTS, 0.6,
             "After analysis, record whether solution worked"),

    # Agent lifecycle
    ToolEdge("agent.register", "agent.heartbeat", EdgeType.SUGGESTS, 0.7,
             "Registered agents should send heartbeats"),
    ToolEdge("agent.register", "agent.capabilities", EdgeType.SUGGESTS, 0.5,
             "After registration, check capabilities"),
    ToolEdge("agent.list", "agent.capabilities", EdgeType.SUGGESTS, 0.6,
             "List agents then check specific capabilities"),

    # Pipeline workflow
    ToolEdge("pipeline.create", "pipeline.status", EdgeType.SUGGESTS, 0.9,
             "Check pipeline status after creation"),
    ToolEdge("pipeline.status", "pipeline.list", EdgeType.SUGGESTS, 0.4,
             "May want to see all pipelines"),

    # Introspection chain
    ToolEdge("gnosis", "homeostasis.check", EdgeType.SUGGESTS, 0.6,
             "After introspection, check if corrective action needed"),
    ToolEdge("gnosis", "maturity.assess", EdgeType.SUGGESTS, 0.5,
             "Gnosis overview often leads to maturity check"),
    ToolEdge("harmony_vector", "homeostasis.check", EdgeType.SUGGESTS, 0.7,
             "After reading harmony, trigger health check"),
    ToolEdge("karma_report", "karmic_trace", EdgeType.SUGGESTS, 0.8,
             "Karma report often leads to detailed trace"),

    # Dharma workflow
    ToolEdge("dharma_rules", "set_dharma_profile", EdgeType.SUGGESTS, 0.5,
             "Reviewing rules may lead to profile change"),
    ToolEdge("evaluate_ethics", "dharma_rules", EdgeType.SUGGESTS, 0.4,
             "After ethical evaluation, may want to see active rules"),

    # Yin/Yang balance
    ToolEdge("record_yin_yang_activity", "get_yin_yang_balance", EdgeType.SUGGESTS, 0.8,
             "After recording activity, check balance"),

    # Task distribution
    ToolEdge("task.create", "task.assign", EdgeType.REQUIRES, 0.9,
             "Create task before assigning"),
    ToolEdge("task.assign", "task.status", EdgeType.SUGGESTS, 0.7,
             "Check task status after assignment"),
]


class ToolDependencyGraph:
    """Directed graph of tool relationships for AI planning.

    Thread-safe. Supports both static declarations and learned edges
    from pipeline execution history.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        # adjacency: source -> list of edges
        self._outgoing: dict[str, list[ToolEdge]] = defaultdict(list)
        # reverse adjacency: target -> list of edges
        self._incoming: dict[str, list[ToolEdge]] = defaultdict(list)
        self._all_tools: set[str] = set()

        # Load static edges
        for edge in _STATIC_EDGES:
            self._add_edge(edge)

    def _add_edge(self, edge: ToolEdge) -> None:
        """Add an edge to the graph (internal, no lock)."""
        self._outgoing[edge.source].append(edge)
        self._incoming[edge.target].append(edge)
        self._all_tools.add(edge.source)
        self._all_tools.add(edge.target)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def next_steps(self, tool: str, edge_type: str | None = None) -> list[dict[str, Any]]:
        """Get tools that commonly follow this one."""
        with self._lock:
            edges = self._outgoing.get(tool, [])
            if edge_type:
                edges = [e for e in edges if e.edge_type == edge_type]
            # Sort by weight descending
            edges = sorted(edges, key=lambda e: e.weight, reverse=True)
            return [e.to_dict() for e in edges]

    def prerequisites(self, tool: str, edge_type: str | None = None) -> list[dict[str, Any]]:
        """Get tools that must or should run before this one."""
        with self._lock:
            edges = self._incoming.get(tool, [])
            if edge_type:
                edges = [e for e in edges if e.edge_type == edge_type]
            edges = sorted(edges, key=lambda e: e.weight, reverse=True)
            return [e.to_dict() for e in edges]

    def plan(self, goal_tool: str) -> list[str]:
        """Build a dependency chain (prerequisites) to reach goal_tool.
        Returns tool names in execution order.
        Only follows 'requires' edges.
        """
        chain: list[str] = []
        visited: set[str] = set()

        def _walk(tool: str) -> None:
            if tool in visited:
                return
            visited.add(tool)
            for edge in self._incoming.get(tool, []):
                if edge.edge_type == EdgeType.REQUIRES:
                    _walk(edge.source)
            chain.append(tool)

        _walk(goal_tool)
        return chain

    def add_learned_edge(self, source: str, target: str, weight: float = 0.5) -> None:
        """Add a learned edge from pipeline execution history."""
        with self._lock:
            # Check for existing edge
            for edge in self._outgoing.get(source, []):
                if edge.target == target and edge.edge_type == EdgeType.SUGGESTS:
                    # Reinforce existing edge
                    edge.weight = min(1.0, edge.weight + 0.1)
                    return
            # New learned edge
            edge = ToolEdge(
                source=source, target=target,
                edge_type=EdgeType.SUGGESTS,
                weight=weight,
                description="Learned from pipeline execution history",
            )
            self._add_edge(edge)

    def get_graph_summary(self) -> dict[str, Any]:
        """Return a summary of the full graph."""
        with self._lock:
            total_edges = sum(len(edges) for edges in self._outgoing.values())
            edge_types: defaultdict[str, int] = defaultdict(int)
            for edges in self._outgoing.values():
                for e in edges:
                    edge_types[e.edge_type] += 1

            return {
                "total_tools": len(self._all_tools),
                "total_edges": total_edges,
                "edge_types": dict(edge_types),
                "tools": sorted(self._all_tools),
            }

    def get_full_graph(self) -> list[dict[str, Any]]:
        """Return all edges as a list of dicts."""
        with self._lock:
            all_edges: list[dict[str, Any]] = []
            for edges in self._outgoing.values():
                all_edges.extend(e.to_dict() for e in edges)
            return all_edges


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_graph: ToolDependencyGraph | None = None
_graph_lock = threading.Lock()


def get_tool_graph() -> ToolDependencyGraph:
    """Get the global Tool Dependency Graph."""
    global _graph
    if _graph is None:
        with _graph_lock:
            if _graph is None:
                _graph = ToolDependencyGraph()
    return _graph
