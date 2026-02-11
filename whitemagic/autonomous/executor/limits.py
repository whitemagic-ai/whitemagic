"""Resource Limits - Track and enforce execution limits.

Tracks:
- Iterations
- Time
- Tokens
- File operations
- API calls
"""

from datetime import datetime
from typing import Any


class ResourceLimits:
    """Track and enforce resource limits."""

    def __init__(self, config: Any) -> None:
        # Support both dict and object config
        if isinstance(config, dict):
            self.max_iterations = config.get("max_iterations", 10)
            self.max_time_seconds = config.get("max_time_seconds", 3600)
            self.max_tokens = config.get("max_tokens", 100000)
            self.max_file_ops = config.get("max_file_ops", 50)
            self.max_api_calls = config.get("max_api_calls", 100)
        else:
            self.max_iterations = config.max_iterations
            self.max_time_seconds = config.max_time_seconds
            self.max_tokens = config.max_tokens
            self.max_file_ops = config.max_file_ops
            self.max_api_calls = config.max_api_calls

        self.start_time = datetime.now()
        self.iterations = 0
        self.tokens_used = 0
        self.file_ops = 0
        self.api_calls = 0

        # Circuit Breaker for consecutive failures
        self.consecutive_failures = 0
        self.max_consecutive_failures = 3

    def should_stop(self) -> tuple[bool, str | None]:
        """Check if any limit is hit."""
        if self.iterations >= self.max_iterations:
            return True, f"Max iterations ({self.max_iterations})"

        elapsed = (datetime.now() - self.start_time).total_seconds()
        if elapsed >= self.max_time_seconds:
            return True, f"Max time ({self.max_time_seconds}s)"

        if self.tokens_used >= self.max_tokens:
            return True, f"Max tokens ({self.max_tokens})"

        if self.file_ops >= self.max_file_ops:
            return True, f"Max file operations ({self.max_file_ops})"

        if self.api_calls >= self.max_api_calls:
            return True, f"Max API calls ({self.max_api_calls})"

        # Check circuit breaker for consecutive failures
        if self.consecutive_failures >= self.max_consecutive_failures:
            return True, f"Max consecutive failures ({self.max_consecutive_failures})"

        return False, None

    def track_operation(self, op_type: str, cost: int = 1) -> None:
        """Track resource usage."""
        if op_type == "iteration":
            self.iterations += cost
        elif op_type == "tokens":
            self.tokens_used += cost
        elif op_type == "file":
            self.file_ops += cost
        elif op_type == "api":
            self.api_calls += cost

    def track_failure(self) -> None:
        """Track a failed operation for circuit breaker."""
        self.consecutive_failures += 1

    def track_success(self) -> None:
        """Reset failure count on success."""
        self.consecutive_failures = 0

    def get_status(self) -> dict[str, Any]:
        """Get current resource status."""
        elapsed = (datetime.now() - self.start_time).total_seconds()

        return {
            "iterations": f"{self.iterations}/{self.max_iterations}",
            "time": f"{elapsed:.0f}s/{self.max_time_seconds}s",
            "tokens": f"{self.tokens_used}/{self.max_tokens}",
            "file_ops": f"{self.file_ops}/{self.max_file_ops}",
            "api_calls": f"{self.api_calls}/{self.max_api_calls}",
            "consecutive_failures": f"{self.consecutive_failures}/{self.max_consecutive_failures}",
        }
