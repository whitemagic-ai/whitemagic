"""Gana Chain - Resonant Sequence Executor.

Executes sequences of Ganas where each invocation:
- Receives output from predecessor (resonance)
- Seeds context for successor (preparation)
- Adapts to current Harmony Vector state
- Logs Karma traces
"""

from typing import Any

from .base import BaseGana, GanaCall, GanaResult, LunarMansion, ResonanceHints
from .karma import get_karma_logger
from .lunar import get_current_lunar_phase


class GanaChain:
    """Executes resonant sequences of Gana calls.

    Each Gana in the sequence receives context from the previous one
    and prepares hints for the next. Creates a flowing, adaptive chain.
    """

    def __init__(self, ganas: list[BaseGana], harmony_monitor: Any | None = None) -> None:
        """Initialize chain with Ganas.

        Args:
            ganas: List of Gana instances
            harmony_monitor: Optional HarmonyMonitor for system state

        """
        self.ganas = {g.mansion: g for g in ganas}
        self.harmony_monitor = harmony_monitor
        self.logger = get_karma_logger()

        # If no harmony monitor, create fallback
        if not self.harmony_monitor:
            from ..embodiment import HarmonyMonitor
            self.harmony_monitor = HarmonyMonitor()

        # Ensure Mypy knows it's not None
        assert self.harmony_monitor is not None

    async def execute_chain(
        self,
        mansion_sequence: list[LunarMansion],
        initial_task: str,
        initial_state: dict[str, Any] | None = None,
    ) -> list[GanaResult]:
        """Execute Ganas in sequence with full resonance.

        Args:
            mansion_sequence: Ordered list of LunarMansions to invoke
            initial_task: Starting task description
            initial_state: Optional initial state vector

        Returns:
            List of GanaResult objects, one per Gana in sequence

        Example:
            results = await chain.execute_chain(
                [LunarMansion.HORN, LunarMansion.HEART, LunarMansion.NET],
                "Analyze project status"
            )

        """
        results: list[GanaResult] = []
        predecessor_output: Any | None = None
        current_task = initial_task
        state_vector = initial_state or {}

        # Get current system harmony
        monitor = self.harmony_monitor
        if monitor is None:
            from ..embodiment import HarmonyMonitor
            monitor = HarmonyMonitor()
            self.harmony_monitor = monitor

        harmony = monitor.get_current(force_update=True)
        lunar_phase = get_current_lunar_phase()

        for i, mansion in enumerate(mansion_sequence):
            # Get Gana for this mansion
            gana = self.ganas.get(mansion)
            if not gana:
                raise ValueError(f"No Gana registered for {mansion.name}")

            # Build resonance hints
            hints = ResonanceHints(
                predecessor_output=predecessor_output,
                successor_context=mansion_sequence[i+1].name if i < len(mansion_sequence)-1 else None,
                lunar_phase=lunar_phase,
                harmony_score=harmony["harmony_score"],
                guna_tag=harmony["guna_tag"],
            )

            # Update state vector
            state_vector.update({
                "chain_position": i,
                "total_ganas": len(mansion_sequence),
                "harmony": harmony["harmony_score"],
                "guna": harmony["guna_tag"],
            })

            # Create call
            call = GanaCall(
                task=current_task,
                state_vector=state_vector,
                resonance_hints=hints,
            )

            # Invoke with resonance
            result = await gana.invoke(call)
            results.append(result)

            # Log Karma Trace
            # We log synchronously for now to ensure trace is written before proceeding,
            # though async logging is available in logger.
            self.logger.log(result)

            # Update for next iteration
            predecessor_output = result.output
            current_task = result.successor_hint or current_task

        return results

    async def execute_circle(
        self,
        start_mansion: LunarMansion,
        num_mansions: int,
        initial_task: str,
    ) -> list[GanaResult]:
        """Execute a circular sequence starting from any mansion.

        Args:
            start_mansion: Starting lunar mansion
            num_mansions: Number of mansions to traverse (up to 28 for full circle)
            initial_task: Starting task

        Returns:
            List of results from circular traversal

        Example:
            # Execute quadrant (7 mansions) starting from Horn
            results = await chain.execute_circle(
                LunarMansion.HORN,
                7,
                "Bootstrap project"
            )

        """
        sequence = []
        current = start_mansion

        for _ in range(min(num_mansions, 28)):
            sequence.append(current)
            current = current.successor()

        return await self.execute_chain(sequence, initial_task)

    def get_stats(self) -> dict[str, Any]:
        """Get performance statistics for all Ganas."""
        return {
            "total_ganas": len(self.ganas),
            "gana_stats": {
                mansion.name: gana.stats
                for mansion, gana in self.ganas.items()
            },
        }
