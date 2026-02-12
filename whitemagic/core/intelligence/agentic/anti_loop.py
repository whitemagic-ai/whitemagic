"""Anti-Loop Detector - Prevents cognitive loops
BRAIN UPGRADE #2.

The 3 warning patterns:
1. Memory not activated (re-reading files)
2. Powers forgotten (sequential work)
3. Redundancy loop (creating vs using)

This module catches them BEFORE they happen!

v4.3.0 Enhancement: Circuit Breaker Pattern
- Detects stuck iterations (no progress for N loops)
- Detects repeated errors (same error N times)
- Auto-opens circuit to prevent runaway loops
- Gradual recovery with half-open state
"""
from typing import Any
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum


class CircuitState(Enum):
    """Circuit breaker states."""

    CLOSED = "closed"      # Normal operation
    OPEN = "open"          # Blocking execution (detected stuck)
    HALF_OPEN = "half_open"  # Testing if recovered


@dataclass
class LoopWarning:
    """A detected potential loop."""

    type: str  # "re-read", "sequential", "redundant", "stuck", "error_repeat"
    action: str
    suggestion: str
    severity: str  # "low", "medium", "high", "critical"
    timestamp: datetime = field(default_factory=datetime.now)

class AntiLoopDetector:
    """Catches cognitive loops before they happen.
    The guardian against the 3 warning patterns.

    v4.3.0: Now includes Circuit Breaker for continuous execution:
    - Tracks iteration progress (files modified per iteration)
    - Detects stuck states (no progress for N iterations)
    - Detects error loops (same error repeated)
    - Opens circuit to prevent runaway token burn
    """

    # Circuit breaker thresholds (inspired by Ralph)
    NO_PROGRESS_THRESHOLD = 3      # Open after 3 iterations with no file changes
    SAME_ERROR_THRESHOLD = 5       # Open after 5 iterations with same error
    OUTPUT_DECLINE_THRESHOLD = 0.7  # Open if output declines by >70%
    RECOVERY_SUCCESSES = 2         # Successes needed to close from half-open

    def __init__(self) -> None:
        self.files_read: set[str] = set()
        self.items_created: set[str] = set()
        self.action_history: list[str] = []
        self.warnings_issued: list[LoopWarning] = []
        self.parallel_mode = True  # Default to parallel!

        # Circuit breaker state (v4.3.0)
        self.circuit_state = CircuitState.CLOSED
        self.iteration_count = 0
        self.no_progress_count = 0
        self.last_files_modified: set[str] = set()
        self.error_history: list[str] = []
        self.recovery_successes = 0
        self.circuit_opened_at: datetime | None = None

    def check_file_read(self, filepath: str) -> LoopWarning | None:
        """Check if we're re-reading a file."""
        if filepath in self.files_read:
            warning = LoopWarning(
                type="re-read",
                action=f"Reading {filepath}",
                suggestion="⚠️ Already read! Use memory/patterns instead.",
                severity="medium",
            )
            self.warnings_issued.append(warning)
            return warning
        self.files_read.add(filepath)
        return None

    def check_creation(self, item_name: str, item_type: str) -> LoopWarning | None:
        """Check if we're creating something that exists."""
        key = f"{item_type}:{item_name}"
        if key in self.items_created:
            warning = LoopWarning(
                type="redundant",
                action=f"Creating {item_type}: {item_name}",
                suggestion="⚠️ Similar exists! Search before creating.",
                severity="high",
            )
            self.warnings_issued.append(warning)
            return warning
        self.items_created.add(key)
        return None

    def check_sequential(self, tasks: list[str]) -> LoopWarning | None:
        """Check if we should be running in parallel."""
        if len(tasks) > 1 and self.parallel_mode:
            # Check for dependencies
            has_deps = self._has_dependencies(tasks)
            if not has_deps:
                warning = LoopWarning(
                    type="sequential",
                    action=f"Running {len(tasks)} tasks sequentially",
                    suggestion="⚡ No dependencies! Run in PARALLEL!",
                    severity="medium",
                )
                self.warnings_issued.append(warning)
                return warning
        return None

    def _has_dependencies(self, tasks: list[str]) -> bool:
        """Simple dependency check."""
        # For now, assume no dependencies unless obvious
        return False

    # ===== CIRCUIT BREAKER (v4.3.0) =====

    def record_iteration(self, files_modified: list[str], error: str | None = None) -> LoopWarning | None:
        """Record an iteration's results for circuit breaker analysis.
        Call this after each execution cycle.

        Args:
            files_modified: List of files changed this iteration
            error: Error message if iteration failed

        Returns:
            LoopWarning if circuit should open, None otherwise

        """
        self.iteration_count += 1
        current_files = set(files_modified)

        # Check for no progress
        if not current_files or current_files == self.last_files_modified:
            self.no_progress_count += 1
            if self.no_progress_count >= self.NO_PROGRESS_THRESHOLD:
                return self._open_circuit("no_progress",
                    f"No file changes for {self.no_progress_count} iterations")
        else:
            self.no_progress_count = 0
            self.last_files_modified = current_files

        # Check for repeated errors
        if error:
            self.error_history.append(error)
            # Check if same error repeated
            if len(self.error_history) >= self.SAME_ERROR_THRESHOLD:
                recent = self.error_history[-self.SAME_ERROR_THRESHOLD:]
                if len(set(recent)) == 1:  # All same error
                    return self._open_circuit("error_repeat",
                        f"Same error repeated {self.SAME_ERROR_THRESHOLD} times: {error[:100]}")

        # If half-open, check for recovery
        if self.circuit_state == CircuitState.HALF_OPEN:
            if current_files and not error:
                self.recovery_successes += 1
                if self.recovery_successes >= self.RECOVERY_SUCCESSES:
                    self._close_circuit()
            else:
                # Failed in half-open, reopen
                return self._open_circuit("half_open_fail", "Recovery attempt failed")

        return None

    def _open_circuit(self, reason: str, detail: str) -> LoopWarning:
        """Open the circuit breaker."""
        self.circuit_state = CircuitState.OPEN
        self.circuit_opened_at = datetime.now()

        warning = LoopWarning(
            type="stuck",
            action=reason,
            suggestion=f"🚫 CIRCUIT OPEN: {detail}. Stop and reassess approach.",
            severity="critical",
        )
        self.warnings_issued.append(warning)
        return warning

    def _close_circuit(self) -> Any:
        """Close the circuit (recovered)."""
        self.circuit_state = CircuitState.CLOSED
        self.recovery_successes = 0
        self.no_progress_count = 0
        self.error_history.clear()

    def attempt_recovery(self) -> bool:
        """Attempt to recover from open circuit.
        Call when ready to try again after being stuck.

        Returns:
            True if can proceed (half-open), False if must wait

        """
        if self.circuit_state != CircuitState.OPEN:
            return True

        # Check if enough time has passed (5 minutes cooldown)
        if self.circuit_opened_at:
            elapsed = datetime.now() - self.circuit_opened_at
            if elapsed < timedelta(minutes=5):
                return False

        # Move to half-open for testing
        self.circuit_state = CircuitState.HALF_OPEN
        self.recovery_successes = 0
        return True

    def is_circuit_open(self) -> bool:
        """Check if circuit is open (should stop execution)."""
        return self.circuit_state == CircuitState.OPEN

    def get_circuit_status(self) -> dict:
        """Get circuit breaker status."""
        return {
            "state": self.circuit_state.value,
            "iteration_count": self.iteration_count,
            "no_progress_count": self.no_progress_count,
            "recent_errors": self.error_history[-3:] if self.error_history else [],
            "opened_at": self.circuit_opened_at.isoformat() if self.circuit_opened_at else None,
        }

    def get_stats(self) -> dict:
        """Get anti-loop statistics."""
        return {
            "files_tracked": len(self.files_read),
            "items_tracked": len(self.items_created),
            "warnings_issued": len(self.warnings_issued),
            "parallel_mode": self.parallel_mode,
            "circuit_breaker": self.get_circuit_status(),
            "recent_warnings": [
                {"type": w.type, "suggestion": w.suggestion}
                for w in self.warnings_issued[-5:]
            ],
        }

    def reset_session(self) -> Any:
        """Reset for new session (but keep learnings)."""
        self.files_read.clear()
        self.items_created.clear()
        self.action_history.clear()
        # Reset circuit breaker for new session
        self._close_circuit()
        self.iteration_count = 0
        self.last_files_modified.clear()
        # Keep warnings for learning


# Singleton
_detector = None

def get_anti_loop() -> AntiLoopDetector:
    """Get the global Anti-Loop Detector."""
    global _detector
    if _detector is None:
        _detector = AntiLoopDetector()
    return _detector

def check_loop(action_type: str, target: str) -> str | None:
    """Quick check function - returns warning message or None."""
    detector = get_anti_loop()

    if action_type == "read":
        warning = detector.check_file_read(target)
    elif action_type == "create":
        warning = detector.check_creation(target, "file")
    else:
        return None

    return warning.suggestion if warning else None
