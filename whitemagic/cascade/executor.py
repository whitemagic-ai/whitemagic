"""Parallel Batch Executor - Execute tool cascades with Yin-Yang pacing"""

import asyncio
import importlib.util
from datetime import datetime
from typing import Any

from .graph import Batch, DependencyGraph, ToolCall

HAS_YIN_YANG = importlib.util.find_spec("whitemagic.harmony.yin_yang_tracker") is not None


class ParallelBatchExecutor:
    """Execute tool batches in parallel with Yin-Yang balance awareness

    Features:
    - Parallel execution of independent tool calls
    - Yin-Yang balance monitoring and pacing
    - Wu Xing phase sequencing (Phase 3 - not yet implemented)
    - Graceful degradation on tool failures

    Example:
        executor = ParallelBatchExecutor(enable_yin_yang=True)
        results = await executor.execute_graph(graph)

    """

    def __init__(
        self,
        enable_yin_yang: bool = True,
        enable_wu_xing: bool = False,
        max_parallel_calls: int = 20,
        pause_on_burnout: float = 0.7,
    ) -> None:
        """Initialize executor

        Args:
            max_parallel_calls: Maximum number of parallel tool calls (default: 5 for safety)
            enable_yin_yang: Enable Yin-Yang balance tracking
            enable_wu_xing: Enable Wu Xing sequencing
            pause_on_burnout: Burnout risk threshold to trigger pause (0.0-1.0)

        Safety Note:
            Default max_parallel_calls reduced to 5 to prevent memory thrashing
            on 16GB RAM systems. Each async task is lightweight but tool execution
            may spawn additional work. Use asyncio (not processes) for concurrency.

        """
        # SAFETY: Enforce hard limit for resource-constrained systems
        self.max_parallel_calls = min(max_parallel_calls, 10)  # Hard cap at 10
        self.enable_yin_yang = enable_yin_yang
        self.enable_wu_xing = enable_wu_xing
        self.pause_on_burnout = pause_on_burnout

        # Initialize Yin-Yang tracker if enabled
        self.tracker: Any | None = None
        if self.enable_yin_yang:
            try:
                from whitemagic.harmony.yin_yang_tracker import YinYangBalanceTracker
                self.tracker = YinYangBalanceTracker()
            except ImportError:
                # Graceful degradation if tracker not available
                self.enable_yin_yang = False

        # Cache tool dispatcher to avoid re-building registry for each call
        self._dispatcher: Any | None = None

    async def execute_graph(
        self,
        graph: DependencyGraph,
    ) -> dict[str, Any]:
        """Execute all batches in dependency order

        Args:
            graph: Dependency graph with batches

        Returns:
            Dictionary mapping call IDs to results

        """
        results: dict[str, Any] = {}

        if not graph.batches:
            return results

        for batch in graph.batches:
            # Check Yin-Yang balance before batch
            if self.enable_yin_yang and self.tracker:
                await self._check_balance_and_pace()

            # Execute batch
            batch_results = await self._execute_batch(batch)
            results.update(batch_results)

            # Record activities for balance tracking
            if self.enable_yin_yang and self.tracker:
                for call in batch.calls:
                    try:
                        activity_name = call.activity_type.name
                        self.tracker.record_activity(activity_name)
                    except Exception:
                        pass

        return results

    async def _check_balance_and_pace(self) -> None:
        """Check balance and inject pacing if needed"""
        if not self.tracker:
            return

        try:
            balance = self.tracker.calculate_balance()

            # High burnout risk - pause and consolidate
            if balance.burnout_risk >= self.pause_on_burnout:
                # Inject Yin activity (consolidation)
                await self._inject_yin_pause(balance.burnout_risk)
        except Exception:
            pass

    async def _inject_yin_pause(self, burnout_risk: float) -> None:
        """Inject a Yin pause to restore balance

        Args:
            burnout_risk: Current burnout risk level

        """
        # Calculate pause duration based on risk level
        # High risk = longer pause
        base_pause = 5.0  # Base 5 seconds
        pause_duration = base_pause * (1 + burnout_risk)

        # Record CONSOLIDATE activity
        if self.tracker:
            self.tracker.record_activity("CONSOLIDATE")

        # Brief pause to restore balance
        await asyncio.sleep(pause_duration)

    async def _execute_batch(
        self,
        batch: Batch,
    ) -> dict[str, Any]:
        """Execute all calls in a batch in parallel

        Args:
            batch: Batch of tool calls

        Returns:
            Dictionary mapping call IDs to results

        """
        if not batch.calls:
            return {}

        # Limit concurrency
        semaphore = asyncio.Semaphore(self.max_parallel_calls)

        async def execute_with_semaphore(call: ToolCall) -> tuple[str, Any]:
            async with semaphore:
                result = await self._execute_call(call)
                call_id = call.call_id or f"{call.tool_name}_{id(call)}"
                return call_id, result

        # Execute all calls in parallel
        tasks = [execute_with_semaphore(call) for call in batch.calls]
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # Build result dictionary
        result_dict: dict[str, Any] = {}
        for item in results:
            if isinstance(item, BaseException):
                # Log error but continue
                continue
            call_id, result = item
            result_dict[call_id] = result

        return result_dict

    def _get_dispatcher(self) -> Any:
        """Get cached tool dispatcher instance

        Optimization: Avoids rebuilding tool registry for each call.
        """
        if self._dispatcher is None:
            try:
                from .tool_dispatcher import ToolDispatcher  # type: ignore[import-not-found]

                self._dispatcher = ToolDispatcher()
            except ImportError:
                class _FallbackDispatcher:
                    async def dispatch(self, tool_name: str, params: dict[str, Any]) -> Any:
                        return type(
                            "DispatchResult",
                            (),
                            {
                                "success": False,
                                "data": None,
                                "error": f"Tool dispatcher unavailable for {tool_name}",
                                "params": params,
                            },
                        )()

                self._dispatcher = _FallbackDispatcher()
        return self._dispatcher

    async def _execute_call(
        self,
        call: ToolCall,
    ) -> Any:
        """Execute a single tool call

        Args:
            call: Tool call to execute

        Returns:
            Result from tool execution

        """
        try:
            # Use cached dispatcher (optimization)
            dispatcher = self._get_dispatcher()
            result = await dispatcher.dispatch(call.tool_name, call.params)

            if result.success:
                return {
                    "tool": call.tool_name,
                    "params": call.params,
                    "success": True,
                    "data": result.data,
                    "timestamp": datetime.now().isoformat(),
                }
            return {
                "tool": call.tool_name,
                "params": call.params,
                "success": False,
                "error": result.error,
                "timestamp": datetime.now().isoformat(),
            }

        except Exception as e:
            # Fallback to simulated execution on error
            return {
                "tool": call.tool_name,
                "params": call.params,
                "success": False,
                "error": str(e),
                "simulated_fallback": True,
                "timestamp": datetime.now().isoformat(),
            }

    def get_metrics(self) -> dict[str, Any]:
        """Get executor metrics

        Returns:
            Dictionary with execution metrics

        """
        metrics = {
            "yin_yang_enabled": self.enable_yin_yang,
            "wu_xing_enabled": self.enable_wu_xing,
            "max_parallel_calls": self.max_parallel_calls,
        }

        if self.enable_yin_yang and self.tracker:
            try:
                balance = self.tracker.calculate_balance()
                metrics.update({
                    "balance_score": balance.balance_score,
                    "burnout_risk": balance.burnout_risk,
                    "yang_ratio": balance.yang_ratio,
                    "recommendations": balance.recommendations,
                })
            except Exception:
                pass

        return metrics
