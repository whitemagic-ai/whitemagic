"""Dependency Graph - DAG builder for parallel batch execution"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class ActivityType(Enum):
    """Activity types for Yin-Yang balance (imported from patterns)"""

    CREATE = "yang"
    WRITE = "yang"
    IMPLEMENT = "yang"
    EXECUTE = "yang"
    DEPLOY = "yang"
    CODE = "yang"
    BUILD = "yang"
    UPDATE = "yang"
    READ = "yin"
    SEARCH = "yin"
    ANALYZE = "yin"
    REFLECT = "yin"
    CONSOLIDATE = "yin"
    REVIEW = "yin"
    LEARN = "yin"
    LIST = "yin"


@dataclass
class ToolCall:
    """Single tool invocation in a cascade"""

    tool_name: str
    params: dict[str, Any] = field(default_factory=dict)
    activity_type: ActivityType = ActivityType.READ
    depends_on: list[str] = field(default_factory=list)
    optional: bool = False
    call_id: str | None = None

    def __post_init__(self) -> Any:
        """Generate unique ID if not provided"""
        if not self.call_id:
            self.call_id = f"{self.tool_name}_{id(self)}"


@dataclass
class Batch:
    """Group of tool calls that can execute in parallel"""

    calls: list[ToolCall] = field(default_factory=list)
    batch_id: int = 0
    depends_on_batches: list[int] = field(default_factory=list)

    @property
    def yang_count(self) -> int:
        """Count of Yang activities in this batch"""
        return sum(1 for call in self.calls if call.activity_type.value == "yang")

    @property
    def yin_count(self) -> int:
        """Count of Yin activities in this batch"""
        return sum(1 for call in self.calls if call.activity_type.value == "yin")

    @property
    def is_balanced(self) -> bool:
        """Check if batch has reasonable Yin-Yang balance"""
        total = len(self.calls)
        if total == 0:
            return True
        yang_ratio = self.yang_count / total
        # Consider balanced if between 30-70%
        return 0.3 <= yang_ratio <= 0.7


class DependencyGraph:
    """Directed acyclic graph of tool calls with batch organization

    Analyzes dependencies between tool calls and organizes them into
    parallel batches for efficient execution.
    """

    def __init__(self) -> None:
        self.calls: list[ToolCall] = []
        self.batches: list[Batch] = []
        self._call_map: dict[str, ToolCall] = {}

    def add_call(self, call: ToolCall) -> None:
        """Add a tool call to the graph

        Args:
            call: Tool call to add

        """
        if not call.call_id:
            call.call_id = f"{call.tool_name}_{len(self.calls)}"

        self.calls.append(call)
        self._call_map[call.call_id] = call

    def build_batches(self) -> None:
        """Organize calls into parallel batches based on dependencies

        Uses topological sorting to find independent calls that can
        execute in parallel.
        """
        if not self.calls:
            return

        # Track which calls have been scheduled
        scheduled: set[str] = set()
        batch_id = 0

        while len(scheduled) < len(self.calls):
            # Find calls with all dependencies satisfied
            ready_calls = []
            for call in self.calls:
                if call.call_id in scheduled:
                    continue

                # Check if all dependencies are scheduled
                deps_satisfied = all(
                    dep_name in scheduled or dep_name not in self._call_map
                    for dep_name in call.depends_on
                )

                if deps_satisfied:
                    ready_calls.append(call)

            if not ready_calls:
                # Circular dependency or error - add remaining calls
                for call in self.calls:
                    if call.call_id not in scheduled:
                        ready_calls.append(call)
                break

            # Create batch from ready calls
            batch = Batch(calls=ready_calls, batch_id=batch_id)
            self.batches.append(batch)

            # Mark as scheduled
            for call in ready_calls:
                if call.call_id is not None:
                    scheduled.add(call.call_id)

            batch_id += 1

    def get_batch(self, batch_id: int) -> Batch | None:
        """Get batch by ID

        Args:
            batch_id: Batch identifier

        Returns:
            Batch or None if not found

        """
        for batch in self.batches:
            if batch.batch_id == batch_id:
                return batch
        return None

    def get_call(self, call_id: str) -> ToolCall | None:
        """Get call by ID

        Args:
            call_id: Call identifier

        Returns:
            ToolCall or None if not found

        """
        return self._call_map.get(call_id)

    def get_statistics(self) -> dict[str, Any]:
        """Calculate graph statistics

        Returns:
            Dictionary with statistics

        """
        if not self.calls:
            return {
                "total_calls": 0,
                "num_batches": 0,
                "avg_batch_size": 0.0,
                "max_batch_size": 0,
                "yang_count": 0,
                "yin_count": 0,
                "parallel_efficiency": 0.0,
            }

        yang_count = sum(1 for call in self.calls if call.activity_type.value == "yang")
        yin_count = sum(1 for call in self.calls if call.activity_type.value == "yin")
        batch_sizes = [len(batch.calls) for batch in self.batches]

        return {
            "total_calls": len(self.calls),
            "num_batches": len(self.batches),
            "avg_batch_size": sum(batch_sizes) / len(batch_sizes) if batch_sizes else 0.0,
            "max_batch_size": max(batch_sizes) if batch_sizes else 0,
            "yang_count": yang_count,
            "yin_count": yin_count,
            "yang_ratio": yang_count / len(self.calls) if self.calls else 0.0,
            "parallel_efficiency": self._calculate_efficiency(),
        }

    def _calculate_efficiency(self) -> float:
        """Calculate how well calls are parallelized

        Returns:
            Efficiency score (0.0 to 1.0)

        """
        if not self.calls or not self.batches:
            return 0.0

        total_calls = len(self.calls)
        num_batches = len(self.batches)

        # Perfect parallelization = all in 1 batch
        # Worst parallelization = 1 call per batch
        # Efficiency = how close to perfect
        if total_calls == num_batches:
            return 0.0

        return 1.0 - (num_batches - 1) / (total_calls - 1)
