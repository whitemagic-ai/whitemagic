"""Cascade Engine - Core orchestration layer for tool chains"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from .executor import ParallelBatchExecutor
from .graph import ActivityType as GraphActivityType
from .graph import DependencyGraph, ToolCall
from .patterns import CascadePattern, PatternLibrary
from .synthesizer import ResultSynthesizer

logger = logging.getLogger(__name__)


@dataclass
class CascadeResult:
    """Result of a cascade execution"""

    success: bool
    results: dict[str, Any]
    metrics: dict[str, Any]
    synthesis: dict[str, Any] | None = None
    error: str | None = None
    started_at: datetime | None = None
    completed_at: datetime | None = None

    @property
    def duration(self) -> float:
        """Duration in seconds"""
        if self.started_at and self.completed_at:
            return (self.completed_at - self.started_at).total_seconds()
        return 0.0


class CascadeEngine:
    """Meta-orchestration engine for intelligent tool chains

    The engine decomposes user intent into a dependency graph of tool calls,
    executes them in parallel batches while maintaining Yin-Yang balance,
    and synthesizes results using multi-spectral analysis.

    Example:
        engine = CascadeEngine()
        result = await engine.execute("analyze_codebase", {"module": "auth"})
        logger.info(f"Balance score: {result.metrics['balance_score']}")

    """

    def __init__(
        self,
        pattern_library: PatternLibrary | None = None,
        max_cascade_depth: int = 5,
        max_parallel_calls: int = 20,
        enable_yin_yang: bool = True,
        enable_wu_xing: bool = False,
        enable_scratchpad_multiplexing: bool = False,
    ):
        """Initialize cascade engine

        Args:
            pattern_library: Library of cascade patterns (uses default if None)
            max_cascade_depth: Maximum cascade recursion depth (safety limit)
            max_parallel_calls: Maximum parallel tool calls per batch
            enable_yin_yang: Enable Yin-Yang balance pacing
            enable_wu_xing: Enable Wu Xing phase sequencing (Phase 3)
            enable_scratchpad_multiplexing: Enable parallel reasoning paths (Phase 4)

        """
        self.patterns = pattern_library or PatternLibrary()
        self.max_cascade_depth = max_cascade_depth
        self.max_parallel_calls = max_parallel_calls
        self.enable_yin_yang = enable_yin_yang
        self.enable_wu_xing = enable_wu_xing
        self.enable_scratchpad_multiplexing = enable_scratchpad_multiplexing

        # Initialize components
        self.executor = ParallelBatchExecutor(
            enable_yin_yang=enable_yin_yang,
            enable_wu_xing=enable_wu_xing,
            max_parallel_calls=max_parallel_calls,
        )
        self.synthesizer = ResultSynthesizer(
            enable_scratchpad_multiplexing=enable_scratchpad_multiplexing,
        )

    async def execute(
        self,
        pattern_name: str,
        context: dict[str, Any],
        depth: int = 0,
    ) -> CascadeResult:
        """Execute a cascade pattern

        Args:
            pattern_name: Name of pattern to execute (e.g., "analyze_codebase")
            context: Context dictionary with inputs for pattern
            depth: Current recursion depth (internal use)

        Returns:
            CascadeResult with execution details

        Raises:
            ValueError: If pattern not found or depth exceeded

        """
        started_at = datetime.now()

        # Safety: Check cascade depth
        if depth >= self.max_cascade_depth:
            return CascadeResult(
                success=False,
                results={},
                metrics={"depth": depth},
                error=f"Maximum cascade depth {self.max_cascade_depth} exceeded",
                started_at=started_at,
                completed_at=datetime.now(),
            )

        try:
            # Step 1: Get pattern and decompose
            pattern = self.patterns.get_pattern(pattern_name)
            if not pattern:
                raise ValueError(f"Pattern '{pattern_name}' not found")

            # Step 2: Build dependency graph
            graph = self._build_graph(pattern, context)

            # Step 3: Execute batches in parallel
            batch_results = await self.executor.execute_graph(graph)

            # Step 4: Synthesize results
            synthesis = await self.synthesizer.synthesize(batch_results, pattern)

            # Step 5: Calculate metrics
            metrics = self._calculate_metrics(graph, batch_results)

            completed_at = datetime.now()

            return CascadeResult(
                success=True,
                results=batch_results,
                metrics=metrics,
                synthesis=synthesis,
                started_at=started_at,
                completed_at=completed_at,
            )

        except Exception as e:
            return CascadeResult(
                success=False,
                results={},
                metrics={},
                error=str(e),
                started_at=started_at,
                completed_at=datetime.now(),
            )

    def _build_graph(
        self,
        pattern: CascadePattern,
        context: dict[str, Any],
    ) -> DependencyGraph:
        """Build dependency graph from pattern and context

        Args:
            pattern: Cascade pattern to execute
            context: Execution context

        Returns:
            Dependency graph with batches

        """
        graph = DependencyGraph()

        # Convert pattern steps to tool calls with dependencies
        for step in pattern.steps:
            if isinstance(step.activity_type, GraphActivityType):
                graph_activity = step.activity_type
            else:
                graph_activity = GraphActivityType.__members__.get(
                    step.activity_type.name,
                    GraphActivityType.READ,
                )
            tool_call = ToolCall(
                tool_name=step.tool_name,
                params=self._resolve_params(step.params, context),
                activity_type=graph_activity,
                depends_on=step.depends_on,
            )
            graph.add_call(tool_call)

        # Organize into parallel batches
        graph.build_batches()

        return graph

    def _resolve_params(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """Resolve parameter references from context

        Args:
            params: Parameter template (may contain references like "$query")
            context: Context with actual values

        Returns:
            Resolved parameters

        """
        resolved = {}
        for key, value in params.items():
            if isinstance(value, str) and value.startswith("$"):
                # Reference to context variable
                context_key = value[1:]
                resolved[key] = context.get(context_key, value)
            else:
                resolved[key] = value
        return resolved

    def _calculate_metrics(
        self,
        graph: DependencyGraph,
        results: dict[str, Any],
    ) -> dict[str, Any]:
        """Calculate cascade execution metrics

        Args:
            graph: Dependency graph that was executed
            results: Execution results

        Returns:
            Metrics dictionary

        """
        metrics = {
            "total_calls": len(graph.calls),
            "num_batches": len(graph.batches),
            "parallel_efficiency": self._calc_parallel_efficiency(graph),
        }

        # Add Yin-Yang metrics if enabled
        if self.enable_yin_yang and self.executor.tracker:
            balance = self.executor.tracker.calculate_balance()
            metrics.update({
                "balance_score": balance.balance_score,
                "burnout_risk": balance.burnout_risk,
                "yang_ratio": balance.yang_ratio,
            })

        return metrics

    def _calc_parallel_efficiency(self, graph: DependencyGraph) -> float:
        """Calculate how efficiently calls were parallelized

        Args:
            graph: Executed dependency graph

        Returns:
            Efficiency ratio (1.0 = all parallel, 0.0 = all sequential)

        """
        if not graph.calls:
            return 0.0

        total_calls = len(graph.calls)
        sequential_cost = total_calls  # If all sequential

        # Actual cost = number of batches (each batch executes in parallel)
        actual_cost = len(graph.batches)

        if sequential_cost == 0:
            return 0.0

        # Efficiency = how much parallelism we achieved
        efficiency = 1.0 - (actual_cost / sequential_cost)
        return max(0.0, min(1.0, efficiency))
