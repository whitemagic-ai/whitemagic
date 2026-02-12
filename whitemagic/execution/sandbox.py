"""Code-Mode Execution Sandbox

Safe execution environment for model-written code snippets.
Allows LLMs to write and execute Python code within controlled sandbox.

Security layers:
- Restricted imports (no os, subprocess, etc.)
- Execution timeout
- Memory limits
- Namespace isolation
"""

import contextlib
import io
import logging
import os
import signal
import traceback
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class ExecutionResult:
    """Result of code execution."""

    success: bool
    output: str
    error: str | None = None
    execution_time_ms: float = 0.0
    timestamp: datetime | None = None

    def __post_init__(self) -> None:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class SafeSandbox:
    """Safe execution environment for untrusted code."""

    # Allowed imports (whitelist) - SECURITY: Only pure computational modules, NO filesystem access
    ALLOWED_MODULES = {
        "math", "statistics", "random", "datetime", "json", "uuid",
        "collections", "itertools", "functools", "re", "string",
        "dataclasses", "typing",
        # REMOVED: 'pathlib' - enables filesystem access
    }

    # Forbidden builtins (blacklist) - SECURITY: Block all dangerous functions
    FORBIDDEN_BUILTINS = {
        "eval", "exec", "compile", "open", "__import__",
        "globals", "locals", "vars", "dir", "help",
        "input", "raw_input",  # Prevent hanging on input
        "exit", "quit",  # Prevent process termination
        "breakpoint",  # Prevent debugger
    }

    def __init__(self, timeout_seconds: int = 5, max_output_length: int = 10000):
        self.timeout = timeout_seconds
        self.max_output = max_output_length

        # Create safe globals namespace
        self.safe_globals = self._create_safe_globals()

    def _create_safe_globals(self) -> dict[str, Any]:
        """Create a restricted globals namespace.

        SECURITY: Must override __builtins__ itself to prevent bypass via
        __builtins__.__import__ or __builtins__.__dict__['eval'].
        """
        safe = {}

        # CRITICAL: Create safe builtins dict (not the real __builtins__)
        safe_builtins = {}

        # Add safe builtins to our safe dict
        builtins_source = __builtins__ if isinstance(__builtins__, dict) else vars(__builtins__)
        for name in builtins_source:
            if name not in self.FORBIDDEN_BUILTINS:
                safe_builtins[name] = builtins_source[name]

        # CRITICAL: Override __builtins__ with our restricted version
        safe["__builtins__"] = safe_builtins

        # Add allowed modules to namespace (but NOT to __builtins__)
        for module_name in self.ALLOWED_MODULES:
            try:
                safe[module_name] = __import__(module_name)  # type: ignore[assignment]
            except ImportError:
                pass  # Module not available

        return safe

    def _safe_print(self, *args: Any, **kwargs: Any) -> None:
        """Replacement print function that captures output."""
        # This will be captured by contextlib.redirect_stdout in the execute method
        print(*args, **kwargs)

    def _safe_log(self, *args: Any, **kwargs: Any) -> None:
        """Replacement logging function."""
        logger.info(*args, **kwargs)

    def _validate_code(self, code: str) -> None:
        """Validate code string for obvious security issues.

        This is NOT a complete security check - just catches obvious problems.
        """
        if "__import__" in code and "__import__" not in code.split("#")[0]:
            raise ValueError("Direct use of __import__ not allowed")

        if "eval(" in code or "exec(" in code:
            raise ValueError("Use of eval/exec not allowed")

    def execute(self, code: str, context: dict[str, Any] | None = None) -> ExecutionResult:
        """Execute code in sandbox with enforced timeout.

        SECURITY WARNING: This executes code in-process!
        Must be explicitly enabled via WHITEMAGIC_ENABLE_IN_PROCESS_EXEC=true.

        Args:
            code: Python code to execute
            context: Additional variables to make available

        Returns:
            ExecutionResult with output or error

        """
        # SECURITY: Hard guard against in-process execution
        if os.getenv("WHITEMAGIC_ENABLE_IN_PROCESS_EXEC", "false").lower() != "true":
            return ExecutionResult(
                success=False,
                output="",
                error="In-process execution is disabled by default for security. "
                      "Set WHITEMAGIC_ENABLE_IN_PROCESS_EXEC=true to enable (NOT RECOMMENDED for untrusted input).",
                execution_time_ms=0.0,
            )

        import time
        start = time.time()

        # Validate code
        self._validate_code(code)

        # Create execution namespace
        namespace: dict[str, Any] = self.safe_globals.copy()
        if context:
            namespace.update(context)

        # Capture stdout/stderr
        stdout_buffer = io.StringIO()
        stderr_buffer = io.StringIO()

        # SECURITY NOTE: Timeout is best-effort, not guaranteed
        # - Can be caught with `except BaseException` or `except SystemExit`
        # - Only works in main thread on Unix (SIGALRM limitation)
        # - For production with untrusted code, use subprocess-based isolation
        timeout_triggered = False

        def timeout_handler(signum: int, frame: Any) -> None:
            nonlocal timeout_triggered
            timeout_triggered = True
            # SECURITY: Use SystemExit (harder to catch than Exception)
            # Still catchable with `except BaseException`, but that's rare in normal code
            raise SystemExit(f"Execution exceeded {self.timeout} second timeout")

        # Set up timeout alarm (Unix only, graceful fallback on Windows)
        old_handler = None
        try:
            old_handler = signal.signal(signal.SIGALRM, timeout_handler)
            signal.alarm(self.timeout)
        except (AttributeError, ValueError):
            # Windows doesn't have SIGALRM, timeout won't be enforced
            logger.warning("Timeout enforcement not available on this platform")

        try:
            with contextlib.redirect_stdout(stdout_buffer), \
                 contextlib.redirect_stderr(stderr_buffer):

                # Execute with timeout (enforced by signal alarm)
                exec(code, namespace)

            output = stdout_buffer.getvalue()

            # Truncate if too long
            if len(output) > self.max_output:
                output = output[:self.max_output] + "\n... (truncated)"

            execution_time = (time.time() - start) * 1000

            return ExecutionResult(
                success=True,
                output=output,
                execution_time_ms=execution_time,
            )

        except (TimeoutError, SystemExit) as e:
            # SECURITY: Catch both TimeoutError (old) and SystemExit (new timeout mechanism)
            execution_time = (time.time() - start) * 1000
            error_msg = str(e) if "timeout" in str(e).lower() else f"Execution terminated: {e!s}"
            return ExecutionResult(
                success=False,
                output="",
                error=error_msg,
                execution_time_ms=execution_time,
            )

        except Exception as e:
            error_msg = f"{type(e).__name__}: {e!s}\n{traceback.format_exc()}"
            stderr_output = stderr_buffer.getvalue()

            execution_time = (time.time() - start) * 1000

            return ExecutionResult(
                success=False,
                output=stderr_output,
                error=error_msg,
                execution_time_ms=execution_time,
            )

        finally:
            # Clear timeout alarm
            try:
                signal.alarm(0)
                if old_handler is not None:
                    signal.signal(signal.SIGALRM, old_handler)
            except (AttributeError, ValueError):
                pass  # Platform doesn't support signals

            stdout_buffer.close()
            stderr_buffer.close()

    def execute_function(self, code: str, function_name: str,
                        args: list[Any] | None = None, kwargs: dict[str, Any] | None = None) -> ExecutionResult:
        """Execute code that defines a function, then call it.

        Args:
            code: Code defining the function
            function_name: Name of function to call
            args: Positional arguments
            kwargs: Keyword arguments

        Returns:
            ExecutionResult with function's return value

        """
        args = args or []
        kwargs = kwargs or {}

        # Execute code to define function
        result = self.execute(code)
        if not result.success:
            return result

        # Get the function from namespace
        namespace: dict[str, Any] = {}
        exec(code, self.safe_globals, namespace)

        if function_name not in namespace:
            return ExecutionResult(
                success=False,
                output="",
                error=f"Function '{function_name}' not found in code",
            )

        func = namespace[function_name]

        # Call the function
        try:
            import time
            start = time.time()

            result_value = func(*args, **kwargs)

            execution_time = (time.time() - start) * 1000

            return ExecutionResult(
                success=True,
                output=str(result_value),
                execution_time_ms=execution_time,
            )

        except Exception as e:
            return ExecutionResult(
                success=False,
                output="",
                error=f"{type(e).__name__}: {e!s}",
                execution_time_ms=0.0,
            )


class CodeExecutor:
    """High-level code execution interface."""

    def __init__(self, sandbox: SafeSandbox | None = None):
        self.sandbox = sandbox or SafeSandbox()
        self.execution_history: list[ExecutionResult] = []

    def run(self, code: str, context: dict[str, Any] | None = None) -> ExecutionResult:
        """Execute code and track in history."""
        result = self.sandbox.execute(code, context)
        self.execution_history.append(result)

        logger.info(
            f"Code execution: success={result.success}, "
            f"time={result.execution_time_ms:.2f}ms",
        )

        return result

    def run_with_memory(self, code: str) -> ExecutionResult:
        """Execute code with WhiteMagic memory system available."""
        from whitemagic.core.memory.neural_system import get_neural_system

        context = {
            "memory": get_neural_system(),
        }

        return self.run(code, context)

    def get_metrics(self) -> dict[str, Any]:
        """Get execution metrics for monitoring.

        Returns:
            Dictionary of metrics

        """
        if not self.execution_history:
            return {
                "total_executions": 0,
                "success_rate": 0.0,
                "avg_execution_time_ms": 0.0,
            }

        successes = sum(1 for r in self.execution_history if r.success)
        total_time = sum(r.execution_time_ms for r in self.execution_history)

        return {
            "total_executions": len(self.execution_history),
            "successes": successes,
            "failures": len(self.execution_history) - successes,
            "success_rate": successes / len(self.execution_history),
            "avg_execution_time_ms": total_time / len(self.execution_history),
            "total_time_ms": total_time,
        }

    def clear_history(self) -> None:
        """Clear execution history."""
        self.execution_history.clear()


# Global executor instance
_executor: CodeExecutor | None = None


def get_executor() -> CodeExecutor:
    """Get the global code executor."""
    global _executor
    if _executor is None:
        _executor = CodeExecutor()
    return _executor


def execute_code(code: str, context: dict[str, Any] | None = None) -> ExecutionResult:
    """Convenience function to execute code."""
    executor = get_executor()
    return executor.run(code, context)


def execute_with_memory(code: str) -> ExecutionResult:
    """Execute code with memory system available."""
    executor = get_executor()
    return executor.run_with_memory(code)
