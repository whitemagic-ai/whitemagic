"""Gana Swarm - Parallel Batch Orchestrator v5.0.0-alpha.

Thousands of parallel Gana invocations creating a living, breathing system.
Pulse (heartbeat) and breath (prana cycles) for organic flow.

Phase 2C implementation.
"""

import asyncio
import logging
import time
from dataclasses import asdict
from typing import Any, cast

from .base import GanaResult, LunarMansion
from .chain import GanaChain

logger = logging.getLogger(__name__)

class GanaSwarm:
    """Orchestrates thousands of parallel Gana invocations.

    Creates a living digital organism with:
    - Pulse: Regular heartbeat at configurable Hz
    - Breath: Inhale (parallel exploration) + Exhale (consolidation)
    - Memory: Karma traces of all effects
    - Adaptation: Online learning from outcomes
    """

    def __init__(self, chain: GanaChain, pulse_hz: float = 10.0, batch_size: int = 100):
        """Initialize swarm.

        Args:
            chain: GanaChain to orchestrate
            pulse_hz: Heartbeat frequency (default 10 Hz = 100ms pulse)
            batch_size: Max tasks to process per breath

        """
        self.chain = chain
        self.pulse_hz = pulse_hz
        self.pulse_interval = 1.0 / pulse_hz
        self.base_batch_size = batch_size
        self.breathing = False

        # Async queue for incoming tasks
        self.task_queue: asyncio.Queue = asyncio.Queue()

        # Stats
        self.total_breaths = 0
        self.total_processed = 0

        # Get harmony monitor for Yin/Yang adaptation
        from whitemagic.core.embodiment import get_harmony_monitor
        self.harmony_monitor = get_harmony_monitor()

    async def add_task(self,
                      sequence: list[LunarMansion],
                      task: str,
                      state: dict[str, Any] | None = None) -> None:
        """Add a task to the swarm queue to be processed in next breath."""
        # --- DHARMA GOVERNANCE ---
        try:
            from whitemagic.dharma.governor import GovernanceAction, get_governor
            governor = get_governor()
            decision = governor.govern(task, context=state)

            if decision.action == GovernanceAction.BLOCK:
                logger.info(f"🛑 Dharma Governor BLOCKED task: {task[:50]}... Reason: {decision.concerns}")
                # Emit rejection event
                try:
                    from whitemagic.core.resonance.gan_ying import EventType, emit_event
                    emit_event(
                        source="gana_swarm",
                        event_type=EventType.INTERVENTION_TRIGGERED,
                        data={
                            "action": "block",
                            "task": task,
                            "reason": decision.concerns,
                        },
                        confidence=1.0,
                    )
                except ImportError:
                    pass
                return  # Do not add to queue

            elif decision.action == GovernanceAction.WARN:
                logger.info(f"⚠️ Dharma Governor WARNED task: {task[:50]}... Concerns: {decision.concerns}")
                # We could attach a warning flag to the state if we wanted
                if state is None:
                    state = {}
                state["_dharma_warning"] = decision.concerns

        except ImportError:
            # If Dharma system not available, proceed (fail open for now, or fail closed?)
            # For now, fail open to avoid breaking dev
            pass

        await self.task_queue.put((sequence, task, state))

    async def breathe(self) -> None:
        """Prana cycle: inhale (gather), exhale (consolidate).

        Main event loop for living system.
        """
        self.breathing = True
        logger.debug(f"🫁 GanaSwarm breathing at {self.pulse_hz}Hz...")

        while self.breathing:
            start_time = time.time()

            # --- ADAPTATION (Yin/Yang) ---
            # Adjust breath based on system state
            harmony = self.harmony_monitor.get_current()
            guna = harmony.get("guna_tag", "Sattva")

            # Yang (Rajas) -> Deep Inhale (More tasks)
            # Yin (Tamas) -> Deep Exhale (More consolidation, fewer tasks)
            # Sattva -> Balanced

            if guna == "Rajas":
                current_batch_size = int(self.base_batch_size * 1.5)
                adaptation = "Yang/Action"
            elif guna == "Tamas":
                current_batch_size = int(self.base_batch_size * 0.5)
                adaptation = "Yin/Rest"
            else:
                current_batch_size = self.base_batch_size
                adaptation = "Balanced"

            # Log adaptation if it changes (or periodically, but let's just log it if we inhale)

            # --- INHALE (Gather) ---
            batch: list[tuple[list[LunarMansion], str, dict[str, Any] | None]] = []
            try:
                # Gather up to current_batch_size tasks
                while len(batch) < current_batch_size and not self.task_queue.empty():
                    batch.append(self.task_queue.get_nowait())
            except asyncio.QueueEmpty:
                pass

            if batch:
                logger.info(f"🫁 Inhale ({adaptation}): Processing {len(batch)} tasks...")
                # Spawn parallel executions
                results = await self._process_batch(batch)

                # --- EXHALE (Consolidate) ---
                await self._consolidate(results)

                self.total_processed += len(batch)

            self.total_breaths += 1

            # --- PULSE (Wait) ---
            elapsed = time.time() - start_time
            sleep_time = max(0, self.pulse_interval - elapsed)

            # If we're processing, we might not sleep if we overran the pulse
            # But we aim for rhythm
            if sleep_time > 0:
                await asyncio.sleep(sleep_time)
            else:
                # If overran, yield control briefly to avoid starving other tasks
                await asyncio.sleep(0)

    async def _process_batch(self, batch: list[tuple[Any, ...]]) -> list[list[GanaResult]]:
        """Process a batch of tasks in parallel."""
        tasks = []
        for sequence, task_desc, state in batch:
            tasks.append(self.chain.execute_chain(sequence, task_desc, state))

        # Wait for all to complete
        # return_exceptions=True prevents one failure from crashing the swarm
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # Filter out exceptions and log errors
        valid_results: list[list[GanaResult]] = []
        for res in results:
            if isinstance(res, Exception):
                logger.info(f"⚠️ Swarm task failed: {res}")
            else:
                # Ensure Mypy knows this is List[GanaResult]
                valid_results.append(cast("list[GanaResult]", res))

        return valid_results

    async def _consolidate(self, results: list[list[GanaResult]]) -> None:
        """Exhale phase: Consolidate results, learn patterns.

        v5.0 Integration:
        - Triggers MemoryManager consolidation
        - Feeds results to PatternEngine
        - Emits collective resonance events
        """
        from whitemagic.core.memory.manager import MemoryManager
        from whitemagic.core.memory.pattern_engine import get_engine

        manager = MemoryManager()
        pattern_engine = get_engine()

        # 1. Trigger persistence layer consolidation
        try:
            consolidated_count = manager.consolidate()
            if consolidated_count > 0:
                logger.info(f"🫁 GanaSwarm: Consolidated {consolidated_count} memory items in SQLite.")
        except Exception as e:
            logger.info(f"⚠️ Swarm consolidation error: {e}")

        # 2. Extract patterns from results
        for result_list in results:
            if isinstance(result_list, Exception):
                continue
            for res in result_list:
                if res.output and isinstance(res.output, str):
                    report = pattern_engine.extract_patterns(res.output)
                    # Iterate over all categories in the report
                    all_patterns = report.solutions + report.anti_patterns + report.heuristics + report.optimizations
                    for p in all_patterns:
                        # Emit via resonance if available
                        try:
                            from whitemagic.core.resonance.gan_ying import (
                                EventType,
                                emit_event,
                            )
                            emit_event(
                                source="gana_swarm",
                                event_type=EventType.PATTERN_DISCOVERED,
                                data=asdict(p),
                                confidence=p.confidence,
                            )
                        except ImportError:
                            pass

    def stop_breathing(self) -> None:
        """Stop the breath cycle."""
        self.breathing = False
        logger.debug("🛑 GanaSwarm stopping breath.")
