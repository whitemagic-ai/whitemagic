"""Progress Assessor - Determines if objectives are complete.

Placeholder implementation for minimum viable system.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime

from .continuous_executor import ExecutionResult, Objective


@dataclass
class Assessment:
    """Assessment of objective completion."""

    complete: bool
    retry: bool = False
    confidence: float = 0.0
    reason: str = ""
    summary: str = ""


class ProgressAssessor:
    """Assesses whether objectives are complete."""

    def __init__(self) -> None:
        self.consecutive_failures = 0
        self.max_consecutive_failures = 5  # Circuit breaker threshold
        self.success_streak = 0
        self.last_success_at: datetime | None = None
        self.plateau_detected = False
        self.plateau_threshold = 10  # Iterations without progress
        self.iterations_since_success = 0

    async def assess_objective(self, obj: Objective, result: ExecutionResult) -> Assessment:
        """Assess if objective is complete."""
        # Check for errors first
        if result.errors:
            self.consecutive_failures += 1
            self.iterations_since_success += 1
            self.success_streak = 0

            # Check for circuit breaker trip
            if self.consecutive_failures >= self.max_consecutive_failures:
                return Assessment(
                    complete=False,
                    retry=False,
                    confidence=0.0,
                    reason=f"Circuit breaker tripped: {self.consecutive_failures} consecutive failures",
                    summary=f"Stopped due to repeated failures: {obj.description}",
                )

            # Check for plateau (no progress for too long)
            if self.iterations_since_success >= self.plateau_threshold and not self.plateau_detected:
                self.plateau_detected = True
                return Assessment(
                    complete=False,
                    retry=False,
                    confidence=0.0,
                    reason=f"Plateau detected: No progress after {self.iterations_since_success} iterations",
                    summary=f"Stopped due to plateau: {obj.description}",
                )

            return Assessment(
                complete=False,
                retry=len(result.errors) < 3,
                confidence=0.0,
                reason=f"Errors: {result.errors[:3]}",
            )

        # Check success criteria
        criteria_met = await self.check_criteria(obj.success_criteria, result)

        if not criteria_met:
            self.consecutive_failures += 1
            self.iterations_since_success += 1
            self.success_streak = 0

            # Check for circuit breaker trip
            if self.consecutive_failures >= self.max_consecutive_failures:
                return Assessment(
                    complete=False,
                    retry=False,
                    confidence=0.0,
                    reason=f"Circuit breaker tripped: {self.consecutive_failures} consecutive failures",
                    summary=f"Stopped due to repeated failures: {obj.description}",
                )

            # Check for plateau
            if self.iterations_since_success >= self.plateau_threshold and not self.plateau_detected:
                self.plateau_detected = True
                return Assessment(
                    complete=False,
                    retry=False,
                    confidence=0.0,
                    reason=f"Plateau detected: No progress after {self.iterations_since_success} iterations",
                    summary=f"Stopped due to plateau: {obj.description}",
                )

            return Assessment(
                complete=False,
                retry=True,
                confidence=0.3,
                reason="Success criteria not met",
            )

        # All checks passed - reset counters
        self.consecutive_failures = 0
        self.success_streak += 1
        self.iterations_since_success = 0
        self.plateau_detected = False
        self.last_success_at = datetime.now()

        return Assessment(
            complete=True,
            confidence=0.9,
            summary=f"Completed: {obj.description}",
        )

    async def check_criteria(self, criteria: list[str], result: ExecutionResult) -> bool:
        """Check if success criteria are met."""
        from pathlib import Path
        if not criteria:
            return bool(result.success)  # No criteria = rely on execution result

        # Check each criterion
        for criterion in criteria:
            # String matching in output
            if criterion.lower() in result.output.lower():
                continue

            # File existence check
            elif criterion.startswith("file_exists:"):
                path = criterion.split(":", 1)[1]
                if not Path(path).exists():
                    return False

            # Test passing check
            elif criterion == "tests_pass":
                if not ("PASSED" in result.output or "OK" in result.output):
                    return False

            else:
                # If any criterion isn't met by default string matching
                return False

        return True
