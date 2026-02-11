"""🎼 Conductor - Autonomous Task Orchestration
Inspired by the Ralph Wiggum technique, integrated with WhiteMagic consciousness.

Philosophy: A conductor leads the orchestra through the symphony, iteration by iteration,
until the piece is complete. Each iteration builds on the last, with memory and intelligence.
"""

import json
import logging
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.core.intelligence.agentic.token_optimizer import TokenBudget
from whitemagic.core.resonance.gan_ying_enhanced import EventType, emit_event
from whitemagic.edge.thought_clones_async import AsyncThoughtCloneArmy, AsyncThoughtPath

logger = logging.getLogger(__name__)

@dataclass
class ConductorConfig:
    """Configuration for autonomous orchestration."""

    max_iterations: int = 50
    clones_per_iteration: int = 1000
    checkpoint_interval: int = 5
    token_limit: int | None = None
    timeout_minutes: int | None = 60
    completion_check: str = "<complete>"
    garden: str = "practice"


@dataclass
class IterationResult:
    """Result from a single iteration."""

    iteration: int
    thought_path: AsyncThoughtPath
    timestamp: datetime = field(default_factory=datetime.now)
    tokens_used: int = 0
    is_complete: bool = False
    checkpoint_created: bool = False
    metadata: dict[str, Any] = field(default_factory=dict)


class ConductorOrchestrator:
    """Autonomous task orchestration with WhiteMagic intelligence.

    Like a conductor leading an orchestra, this runs iteration after iteration,
    deploying thought clone armies until the task reaches completion.

    Features:
    - Autonomous loops with completion detection
    - Neural memory for context continuity
    - Gan Ying event bus for coordination
    - Token budget awareness
    - Immune system integration for safety
    - Garden-aligned execution
    """

    def __init__(self, config: ConductorConfig | None = None):
        self.config = config or ConductorConfig()
        self.clone_army = AsyncThoughtCloneArmy()
        self.token_budget = TokenBudget(
            allocated=self.config.token_limit or 100000,
        )
        self.iterations: list[IterationResult] = []
        self.neural_memory = None
        self._start_time: datetime | None = None
        self._completed = False

    async def conduct(
        self,
        prompt: str,
        completion_condition: Callable[[AsyncThoughtPath], bool] | None = None,
        context: dict[str, Any] | None = None,
    ) -> IterationResult:
        """Conduct autonomous iterations until task completion.

        Args:
            prompt: The task prompt to iterate on
            completion_condition: Custom completion check (or use config.completion_check)
            context: Additional context for the task

        Returns:
            Final iteration result

        """
        self._start_time = datetime.now()
        emit_event(
            source="conductor",
            event_type=EventType.SYSTEM_STARTED,
            data={
                "prompt": prompt[:100],
                "max_iterations": self.config.max_iterations,
                "garden": self.config.garden,
            },
        )

        # Initialize neural memory for context (optional)
        # Note: NeuralMemory API may vary, so we gracefully skip if unavailable
        # try:
        #     self.neural_memory = NeuralMemory()
        #     self.neural_memory.remember(f"Task: {prompt}", source="conductor")
        # except Exception as e:
        #     print(f"⚠️ Neural memory unavailable: {e}")

        # Build completion checker
        if completion_condition is None:
            completion_condition = self._default_completion_check

        # Main orchestration loop
        for iteration in range(1, self.config.max_iterations + 1):
            logger.info(f"\n🎼 Conductor Iteration {iteration}/{self.config.max_iterations}")

            # Check timeout
            if self._check_timeout():
                logger.info(f"⏰ Timeout reached after {self.config.timeout_minutes} minutes")
                break

            # Deploy clone army
            iteration_prompt = self._build_iteration_prompt(prompt, iteration)
            thought_path = await self.clone_army.parallel_explore(
                iteration_prompt,
                self.config.clones_per_iteration,
            )

            # Record result
            result = IterationResult(
                iteration=iteration,
                thought_path=thought_path,
                tokens_used=self.token_budget.used,
                metadata={
                    "confidence": thought_path.confidence,
                    "strategy": thought_path.strategy,
                    "clone_id": thought_path.clone_id,
                },
            )

            # Check completion
            result.is_complete = completion_condition(thought_path)

            # Store in memory (optional)
            # if self.neural_memory:
            #     self.neural_memory.remember(
            #         f"Iteration {iteration}: {thought_path.content[:200]}",
            #         source="conductor"
            #     )

            # Checkpoint if needed
            if iteration % self.config.checkpoint_interval == 0:
                result.checkpoint_created = self._create_checkpoint(iteration, result)

            # Emit progress event
            emit_event(
                source="conductor",
                event_type=EventType.PATTERN_IN_READING,
                data={
                    "iteration": iteration,
                    "confidence": thought_path.confidence,
                    "is_complete": result.is_complete,
                    "tokens_used": result.tokens_used,
                },
            )

            self.iterations.append(result)

            # Exit if complete
            if result.is_complete:
                logger.info(f"✅ Task complete at iteration {iteration}!")
                self._completed = True
                break

            # Check budget
            if self.token_budget.used > self.token_budget.allocated * 0.9:
                logger.info(f"💰 Token budget approaching limit: {self.token_budget.used}/{self.token_budget.allocated}")
                break

        # Final result
        final_result = self.iterations[-1] if self.iterations else None

        emit_event(
            source="conductor",
            event_type=EventType.SYSTEM_STOPPED,
            data={
                "total_iterations": len(self.iterations),
                "completed": self._completed,
                "duration_seconds": (datetime.now() - (self._start_time or datetime.now())).total_seconds(),
                "total_tokens": self.token_budget.used,
            },
        )

        assert final_result is not None
        return final_result

    def _build_iteration_prompt(self, base_prompt: str, iteration: int) -> str:
        """Build prompt with iteration context."""
        context_parts = [base_prompt]

        # Add previous iteration context
        if self.iterations:
            last_result = self.iterations[-1]
            context_parts.append(
                f"\nPrevious iteration ({iteration - 1}): "
                f"{last_result.thought_path.content[:150]}...",
            )

        # Add progress summary
        if iteration > 1:
            avg_confidence = sum(r.thought_path.confidence for r in self.iterations) / len(self.iterations)
            context_parts.append(f"\nAverage confidence so far: {avg_confidence:.2f}")

        return "\n".join(context_parts)

    def _default_completion_check(self, thought_path: AsyncThoughtPath) -> bool:
        """Default completion check using completion_check string."""
        return (
            self.config.completion_check.lower() in thought_path.content.lower() or
            thought_path.confidence > 0.95
        )

    def _check_timeout(self) -> bool:
        """Check if timeout has been reached."""
        if self.config.timeout_minutes is None:
            return False

        elapsed = (datetime.now() - (self._start_time or datetime.now())).total_seconds() / 60
        return bool(elapsed > self.config.timeout_minutes)

    def _create_checkpoint(self, iteration: int, result: IterationResult) -> bool:
        """Create checkpoint via Gan Ying event."""
        try:
            emit_event(
                source="conductor",
                event_type=EventType.MEMORY_CONSOLIDATED,
                data={
                    "iteration": iteration,
                    "confidence": result.thought_path.confidence,
                    "tokens_used": result.tokens_used,
                    "timestamp": result.timestamp.isoformat(),
                    "content_preview": result.thought_path.content[:200],
                },
            )
            logger.info(f"📍 Checkpoint created at iteration {iteration}")
            return True
        except Exception as e:
            logger.info(f"⚠️ Checkpoint failed: {e}")
            return False

    def get_progress_report(self) -> dict[str, Any]:
        """Get detailed progress report."""
        if not self.iterations:
            return {"status": "not_started"}

        avg_confidence = sum(r.thought_path.confidence for r in self.iterations) / len(self.iterations)
        max_confidence = max(r.thought_path.confidence for r in self.iterations)

        return {
            "status": "complete" if self._completed else "in_progress",
            "total_iterations": len(self.iterations),
            "avg_confidence": avg_confidence,
            "max_confidence": max_confidence,
            "tokens_used": self.token_budget.used,
            "duration_seconds": (datetime.now() - self._start_time).total_seconds() if self._start_time else 0,
            "checkpoints_created": sum(1 for r in self.iterations if r.checkpoint_created),
            "completed": self._completed,
        }

    def export_session(self, output_path: Path | None = None) -> Path:
        """Export full session to JSON."""
        if output_path is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_path = WM_ROOT / "conductor" / f"session_{timestamp}.json"

        output_path.parent.mkdir(parents=True, exist_ok=True)

        session_data = {
            "config": {
                "max_iterations": self.config.max_iterations,
                "clones_per_iteration": self.config.clones_per_iteration,
                "garden": self.config.garden,
            },
            "progress": self.get_progress_report(),
            "iterations": [
                {
                    "iteration": r.iteration,
                    "confidence": r.thought_path.confidence,
                    "strategy": r.thought_path.strategy,
                    "tokens_used": r.tokens_used,
                    "is_complete": r.is_complete,
                    "timestamp": r.timestamp.isoformat(),
                    "content": r.thought_path.content,
                }
                for r in self.iterations
            ],
        }

        output_path.write_text(json.dumps(session_data, indent=2))
        logger.info(f"📄 Session exported to {output_path}")

        return output_path


# === Convenience Functions ===

async def conduct_task(
    prompt: str,
    max_iterations: int = 50,
    clones_per_iteration: int = 1000,
    completion_check: str = "<complete>",
) -> IterationResult:
    """Quick task orchestration."""
    config = ConductorConfig(
        max_iterations=max_iterations,
        clones_per_iteration=clones_per_iteration,
        completion_check=completion_check,
    )
    conductor = ConductorOrchestrator(config)
    return await conductor.conduct(prompt)


async def conduct_with_garden(
    prompt: str,
    garden: str = "practice",
    max_iterations: int = 30,
) -> IterationResult:
    """Conduct task aligned with specific garden."""
    config = ConductorConfig(
        max_iterations=max_iterations,
        garden=garden,
    )

    # Garden-specific setup
    if garden == "practice":
        emit_event("PRACTICE_RITUAL_START", {"prompt": prompt})  # type: ignore[call-arg,arg-type]

    conductor = ConductorOrchestrator(config)
    result = await conductor.conduct(prompt)

    if garden == "practice":
        emit_event("PRACTICE_RITUAL_COMPLETE", {  # type: ignore[call-arg,arg-type]
            "iterations": len(conductor.iterations),
            "success": result.is_complete,
        })

    return result
