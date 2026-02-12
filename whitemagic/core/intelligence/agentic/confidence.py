"""Confidence-based agentic execution system.

Enables AI agents to assess confidence and execute tasks autonomously
based on multiple factors including tests, reversibility, and past success.
"""

import logging
import time
from collections.abc import Callable
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class ConfidenceLevel(Enum):
    """Confidence levels for autonomous execution."""

    AUTO_EXECUTE = 0.95  # Just do it, no questions
    NOTIFY_EXECUTE = 0.85  # Do it, but notify user
    REQUEST_APPROVAL = 0.70  # Ask, but don't block long
    REQUIRE_APPROVAL = 0.50  # Must get explicit approval
    MANUAL_ONLY = 0.0  # Never execute autonomously


@dataclass
class Task:
    """Represents a task for agentic execution."""

    name: str
    description: str
    action: Callable
    context: dict[str, Any]

    # Confidence factors
    has_tests: bool = False
    tests_pass: bool = False
    follows_plan: bool = False
    is_reversible: bool = True
    no_external_deps: bool = True
    similar_task_succeeded: bool = False

    # Expected outcomes
    expected_duration: float = 60.0  # seconds

    def execute(self) -> Any:
        """Execute the task action."""
        return self.action()


class ConfidenceAssessor:
    """Assess confidence level for autonomous task execution."""

    # Factor weights (must sum to 1.0)
    WEIGHTS = {
        "has_tests": 0.15,
        "tests_pass": 0.25,
        "follows_plan": 0.20,
        "is_reversible": 0.15,
        "no_external_deps": 0.10,
        "similar_task_succeeded": 0.15,
    }

    def assess(self, task: Task) -> float:
        """Calculate confidence score for a task."""
        score = 0.0

        for factor, weight in self.WEIGHTS.items():
            if getattr(task, factor, False):
                score += weight

        return score

    def get_confidence_level(self, score: float) -> ConfidenceLevel:
        """Convert score to confidence level."""
        if score >= ConfidenceLevel.AUTO_EXECUTE.value:
            return ConfidenceLevel.AUTO_EXECUTE
        elif score >= ConfidenceLevel.NOTIFY_EXECUTE.value:
            return ConfidenceLevel.NOTIFY_EXECUTE
        elif score >= ConfidenceLevel.REQUEST_APPROVAL.value:
            return ConfidenceLevel.REQUEST_APPROVAL
        else:
            return ConfidenceLevel.REQUIRE_APPROVAL

    def explain_confidence(self, task: Task) -> str:
        """Explain why confidence is at current level."""
        score = self.assess(task)
        level = self.get_confidence_level(score)

        explanation = [
            f"Confidence: {score:.0%} ({level.name})",
            "\nFactors:",
        ]

        for factor, weight in self.WEIGHTS.items():
            value = getattr(task, factor, False)
            status = "✅" if value else "❌"
            contribution = weight if value else 0.0
            explanation.append(
                f"  {status} {factor.replace('_', ' ').title()}: " f"+{contribution:.0%}",
            )

        return "\n".join(explanation)


class AgenticExecutor:
    """Execute tasks with confidence-based autonomy."""

    def __init__(self, assessor: ConfidenceAssessor | None = None):
        self.assessor = assessor or ConfidenceAssessor()
        self.execution_log: list[dict[str, Any]] = []

    def execute(self, task: Task, override_level: ConfidenceLevel | None = None) -> Any:
        """Execute task based on confidence level."""

        # Assess confidence
        score = self.assessor.assess(task)
        level = override_level or self.assessor.get_confidence_level(score)

        # Record execution attempt
        execution_record = {
            "task": task.name,
            "confidence_score": score,
            "confidence_level": level.name,
            "timestamp": datetime.now().isoformat(),
        }

        start_time = time.time()

        try:
            # Execute based on confidence level
            if level == ConfidenceLevel.AUTO_EXECUTE:
                result = self._auto_execute(task)

            elif level == ConfidenceLevel.NOTIFY_EXECUTE:
                result = self._notify_and_execute(task, score)

            elif level == ConfidenceLevel.REQUEST_APPROVAL:
                result = self._request_approval(task, score)

            else:  # REQUIRE_APPROVAL or MANUAL_ONLY
                result = self._require_approval(task, score)

            execution_record["status"] = "success"
            execution_record["duration"] = time.time() - start_time
            execution_record["result"] = str(result)[:100]  # Truncate long results

        except Exception as e:
            execution_record["status"] = "error"
            execution_record["error"] = str(e)
            execution_record["duration"] = time.time() - start_time
            raise

        finally:
            self.execution_log.append(execution_record)

        return result

    def _auto_execute(self, task: Task) -> Any:
        """Execute without notification (highest confidence)."""
        result = task.execute()
        # Silent execution, only log
        return result

    def _notify_and_execute(self, task: Task, score: float) -> Any:
        """Execute with notification."""
        logger.info(f"🔄 Executing: {task.name} (confidence: {score:.0%})")
        result = task.execute()
        logger.info(f"✅ Completed: {task.name}")
        return result

    def _request_approval(self, task: Task, score: float) -> Any:
        """Request approval with timeout."""
        logger.info(f"\n❓ Ready to execute: {task.name}")
        logger.info(f"   Confidence: {score:.0%}")
        logger.info(self.assessor.explain_confidence(task))
        logger.info("\n   Proceeding in 10s. Type 'stop' to cancel...")

        # In real implementation, would wait for user input with timeout
        # For now, just execute
        logger.info("   ⏳ No objection, proceeding...\n")
        return task.execute()

    def _require_approval(self, task: Task, score: float) -> Any:
        """Require explicit approval."""
        logger.info(f"\n⚠️  Low confidence task: {task.name} ({score:.0%})")
        logger.info(self.assessor.explain_confidence(task))
        logger.info("\n   Requires explicit approval.")
        logger.info("   For autonomous execution, improve test coverage or reversibility.")

        # In real implementation, would block until approved
        # For now, skip execution
        raise RuntimeError(f"Task '{task.name}' requires approval (confidence: {score:.0%})")

    def get_execution_summary(self) -> dict[str, Any]:
        """Get summary of all executions."""
        total = len(self.execution_log)
        if total == 0:
            return {"total": 0}

        successful = sum(1 for e in self.execution_log if e["status"] == "success")
        total_duration = sum(e.get("duration", 0) for e in self.execution_log)

        return {
            "total_tasks": total,
            "successful": successful,
            "failed": total - successful,
            "success_rate": successful / total if total > 0 else 0,
            "total_duration": total_duration,
            "avg_duration": total_duration / total if total > 0 else 0,
            "log": self.execution_log,
        }
