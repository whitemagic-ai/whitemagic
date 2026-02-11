"""Parallel Pipeline - Multi-Stage Workflow Processing

Provides composable multi-stage pipelines for complex parallel workflows.
Each stage can run with different parallelism levels.

Usage:
    pipeline = ParallelPipeline()
    pipeline.add_stage("read", reader.read_batch, workers=64)
    pipeline.add_stage("process", processor.process_batch, workers=32)
    pipeline.add_stage("write", writer.write_batch, workers=16)

    results = await pipeline.execute(input_data)
"""

from __future__ import annotations

import asyncio
from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any
from uuid import uuid4


@dataclass
class PipelineStage:
    """Represents a stage in the pipeline."""

    name: str
    func: Callable
    workers: int = 1
    timeout: float | None = None

    # Results
    input_count: int = 0
    output_count: int = 0
    errors: list[str] = field(default_factory=list)
    duration: float = 0.0

    @property
    def success_rate(self) -> float:
        """Calculate success rate."""
        if self.input_count == 0:
            return 1.0
        return self.output_count / self.input_count


@dataclass
class PipelineResult:
    """Results of pipeline execution."""

    pipeline_id: str
    stages: list[PipelineStage]
    final_results: list[Any]
    total_duration: float
    success: bool = True
    error: str | None = None

    @property
    def stage_count(self) -> int:
        """Number of stages."""
        return len(self.stages)

    @property
    def total_processed(self) -> int:
        """Total items processed across all stages."""
        return sum(s.input_count for s in self.stages)


class ParallelPipeline:
    """Multi-stage parallel processing pipeline.

    Chains multiple processing stages together, each with independent
    parallelism control. Data flows sequentially through stages but
    items within each stage are processed in parallel.
    """

    def __init__(self, pipeline_id: str | None = None):
        """Initialize pipeline.

        Args:
            pipeline_id: Unique pipeline identifier

        """
        self.pipeline_id = pipeline_id or str(uuid4())
        self.stages: list[PipelineStage] = []

    def add_stage(
        self, name: str, func: Callable, workers: int = 1, timeout: float | None = None,
    ) -> ParallelPipeline:
        """Add stage to pipeline.

        Args:
            name: Stage name
            func: Processing function (sync or async)
            workers: Number of parallel workers for this stage
            timeout: Stage timeout in seconds

        Returns:
            Self for chaining

        """
        stage = PipelineStage(name=name, func=func, workers=workers, timeout=timeout)
        self.stages.append(stage)
        return self

    async def _execute_stage(self, stage: PipelineStage, inputs: list[Any]) -> list[Any]:
        """Execute a single stage.

        Args:
            stage: Stage to execute
            inputs: Input data

        Returns:
            Processed outputs

        """
        import time

        stage.input_count = len(inputs)
        start_time = time.time()

        if not inputs:
            stage.duration = 0.0
            return []

        # Create semaphore for parallelism control
        semaphore = asyncio.Semaphore(stage.workers)

        async def process_item(item: Any) -> Any:
            """Process single item with semaphore."""
            async with semaphore:
                try:
                    if asyncio.iscoroutinefunction(stage.func):
                        result = await stage.func(item)
                    else:
                        result = stage.func(item)
                    return result
                except Exception as e:
                    stage.errors.append(str(e))
                    return None

        # Process all items in parallel (up to workers limit)
        if stage.timeout:
            results = await asyncio.wait_for(
                asyncio.gather(*[process_item(item) for item in inputs], return_exceptions=True),
                timeout=stage.timeout,
            )
        else:
            results = await asyncio.gather(
                *[process_item(item) for item in inputs], return_exceptions=True,
            )

        # Filter out None and exceptions
        outputs = [r for r in results if r is not None and not isinstance(r, Exception)]

        stage.output_count = len(outputs)
        stage.duration = time.time() - start_time

        return outputs

    async def execute(self, initial_data: list[Any]) -> PipelineResult:
        """Execute full pipeline.

        Args:
            initial_data: Initial input data

        Returns:
            PipelineResult with outputs and statistics

        """
        import time

        start_time = time.time()
        current_data = initial_data

        try:
            # Execute stages sequentially
            for stage in self.stages:
                current_data = await self._execute_stage(stage, current_data)

                # Stop if stage produced no output
                if not current_data:
                    break

            total_duration = time.time() - start_time

            return PipelineResult(
                pipeline_id=self.pipeline_id,
                stages=self.stages,
                final_results=current_data,
                total_duration=total_duration,
                success=True,
            )

        except Exception as e:
            total_duration = time.time() - start_time

            return PipelineResult(
                pipeline_id=self.pipeline_id,
                stages=self.stages,
                final_results=[],
                total_duration=total_duration,
                success=False,
                error=str(e),
            )

    def get_stats(self) -> dict:
        """Get pipeline statistics."""
        return {
            "pipeline_id": self.pipeline_id,
            "total_stages": len(self.stages),
            "stages": [
                {
                    "name": s.name,
                    "workers": s.workers,
                    "input_count": s.input_count,
                    "output_count": s.output_count,
                    "success_rate": s.success_rate,
                    "duration": s.duration,
                    "errors": len(s.errors),
                }
                for s in self.stages
            ],
        }

    def reset(self) -> None:
        """Reset pipeline statistics."""
        for stage in self.stages:
            stage.input_count = 0
            stage.output_count = 0
            stage.errors.clear()
            stage.duration = 0.0

    def clear(self) -> None:
        """Clear all stages."""
        self.stages.clear()
