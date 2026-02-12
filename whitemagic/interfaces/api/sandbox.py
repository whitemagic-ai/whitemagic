"""Sandbox wrapper for exec API."""

import os
import shlex
import subprocess

from whitemagic.execution.safe_executor import CommandNotAllowedError, SafeExecutor

SANDBOX_ENABLED = os.getenv("WHITEMAGIC_EXEC_REQUIRE_SANDBOX", "false").lower() == "true"
SANDBOX_CMD = os.getenv("WHITEMAGIC_EXEC_SANDBOX_CMD", "firejail --quiet --")

def is_sandbox_available() -> bool:
    """Check if sandbox command is available."""
    try:
        result = subprocess.run(
            ["which", SANDBOX_CMD.split()[0]],
            capture_output=True,
            timeout=1,
        )
        return result.returncode == 0
    except Exception:
        return False

def wrap_command_with_sandbox(cmd_array: list[str]) -> list[str]:
    """Wrap command array with sandbox if enabled."""
    if not SANDBOX_ENABLED:
        return cmd_array

    if not is_sandbox_available():
        raise RuntimeError(f"Sandbox required but {SANDBOX_CMD.split()[0]} not available")

    # Prepend sandbox parts safely
    return shlex.split(SANDBOX_CMD) + cmd_array

def execute_sandboxed(
    command: str,
    cwd: str | None = None,
    timeout: int = 30,
) -> tuple[int, str, str]:
    """Execute command with optional sandboxing using SafeExecutor."""

    # Parse command safely
    cmd_array = shlex.split(command)

    executor = SafeExecutor()

    # SECURITY: Validate original command before sandbox wrapping
    try:
        executor._validate_command(cmd_array)
    except CommandNotAllowedError as e:
        return -1, "", f"Command not allowed: {e}"

    # Wrap with sandbox if enabled
    if SANDBOX_ENABLED:
        cmd_array = wrap_command_with_sandbox(cmd_array)

    try:
        result = executor.execute(
            cmd_array,
            cwd=cwd,
            timeout=timeout,
            capture_output=True,
            text=True,
        )
        return result.returncode, result.stdout, result.stderr
    except CommandNotAllowedError as e:
        return -1, "", f"Command not allowed: {e}"
    except subprocess.TimeoutExpired:
        return -1, "", f"Command timed out after {timeout}s"
    except Exception as e:
        return -1, "", str(e)

__all__ = [
    "SANDBOX_ENABLED",
    "is_sandbox_available",
    "wrap_command_with_sandbox",
    "execute_sandboxed",
]
