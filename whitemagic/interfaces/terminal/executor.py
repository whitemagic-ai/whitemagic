"""Core execution engine."""

import logging
import os
import shlex
import subprocess
import time
from dataclasses import dataclass

from whitemagic.execution.safe_executor import CommandNotAllowedError, SafeExecutor


@dataclass
class ExecutionResult:
    """Result of command execution."""

    exit_code: int
    stdout: str
    stderr: str
    duration_ms: float
    command: str


class Executor:
    """Execute commands safely."""

    def __init__(self, timeout: int = 30):
        self.timeout = timeout
        self.logger = logging.getLogger(__name__)
        self.sandbox_cmd = os.getenv("WHITEMAGIC_EXEC_SANDBOX_CMD")

    def _filter_env_vars(self, env: dict[str, str]) -> dict[str, str]:
        """Filter environment variables to prevent injection attacks.

        SECURITY FIX:
        - Removed PATH, HOME, SHELL from allowlist (can be exploited)
        - Added LC_* wildcards for locale support
        - Added DANGEROUS blocklist
        - Use server's PATH, not caller-supplied

        For untrusted execution contexts, caller-supplied env should be
        ignored entirely - use only the server's process environment.
        """
        # DANGEROUS: Never allow these to be overridden
        DANGEROUS_ENV_VARS = {
            "LD_PRELOAD",      # Library injection
            "LD_LIBRARY_PATH",  # Library path manipulation
            "DYLD_INSERT_LIBRARIES",  # macOS library injection
            "PYTHONPATH",      # Python import hijacking
            "NODE_PATH",       # Node.js import hijacking
            "GIT_SSH_COMMAND", # Git SSH command injection
            "GIT_EXEC_PATH",   # Git executable path manipulation
            "EDITOR",          # Editor command injection
            "VISUAL",          # Visual editor command injection
            "PAGER",           # Pager command injection
        }

        # SAFE: Read-only environment info (no execution risk)
        SAFE_ENV_VARS = {
            "LANG",
            "LC_ALL",
            "LC_CTYPE",
            "LC_COLLATE",
            "LC_MESSAGES",
            "LC_MONETARY",
            "LC_NUMERIC",
            "LC_TIME",
            "USER",
            "USERNAME",
            "LOGNAME",
            "TZ",              # Timezone
            "TERM",            # Terminal type (for display)
        }

        # Filter out dangerous vars and only keep safe ones
        filtered = {}
        for k, v in env.items():
            if k in DANGEROUS_ENV_VARS:
                self.logger.warning(f"Blocked dangerous env var: {k}")
                continue
            if k in SAFE_ENV_VARS or k.startswith("LC_"):
                filtered[k] = v

        return filtered

    def execute(
        self,
        cmd: str,
        args: list[str] | None = None,
        cwd: str | None = None,
        timeout_ms: int | None = None,
        env: dict[str, str] | None = None,
        stdin: str | None = None,
    ) -> ExecutionResult:
        """Execute command safely using SafeExecutor.

        Args:
            cmd: Command to execute
            args: Command arguments
            cwd: Working directory
            timeout_ms: Timeout in milliseconds (overrides default)
            env: Environment variables to merge with process env
            stdin: Input to pipe to command

        """
        start = time.time()

        # Combine cmd and args
        original_cmd_list = [cmd] + (args or [])

        safe_exec = SafeExecutor()

        # SECURITY FIX: Validate the ORIGINAL command before wrapping
        # This prevents bypassing the allowlist by just allowing 'firejail'
        try:
            safe_exec._validate_command(original_cmd_list)
        except CommandNotAllowedError as e:
             return ExecutionResult(
                exit_code=-1,
                stdout="",
                stderr=f"Security Error: {str(e)}",
                duration_ms=(time.time() - start) * 1000,
                command=" ".join(original_cmd_list),
            )

        # Handle sandbox wrapping if configured
        full_cmd_list = original_cmd_list
        if self.sandbox_cmd:
            # Note: The sandbox wrapper itself (e.g. firejail) is considered trusted
            # once the inner command is validated.
            full_cmd_list = shlex.split(self.sandbox_cmd) + full_cmd_list

        # Convert timeout_ms to seconds, or use default
        timeout_sec = (timeout_ms / 1000.0) if timeout_ms is not None else self.timeout

        # Build environment with SECURITY filtering
        exec_env = None
        if env:
            # SECURITY: Filter environment variables to prevent command execution
            filtered_env = self._filter_env_vars(env)
            if len(filtered_env) < len(env):
                blocked = set(env.keys()) - set(filtered_env.keys())
                self.logger.warning(f"Blocked dangerous env vars: {blocked}")

            exec_env = filtered_env

        try:
            # Re-construct environment for subprocess
            final_env = os.environ.copy()
            if exec_env:
                final_env.update(exec_env)

            # Execute with shell=False (CRITICAL)
            result = subprocess.run(
                full_cmd_list,
                cwd=cwd,
                env=final_env,
                input=stdin,
                capture_output=True,
                text=True,
                timeout=timeout_sec,
                shell=False,
            )

            duration = (time.time() - start) * 1000

            return ExecutionResult(
                exit_code=result.returncode,
                stdout=result.stdout,
                stderr=result.stderr,
                duration_ms=round(duration, 2),
                command=" ".join(full_cmd_list),
            )
        except CommandNotAllowedError as e:
             return ExecutionResult(
                exit_code=-1,
                stdout="",
                stderr=f"Security Error: {str(e)}",
                duration_ms=(time.time() - start) * 1000,
                command=" ".join(full_cmd_list),
            )
        except subprocess.TimeoutExpired:
            return ExecutionResult(
                exit_code=-1,
                stdout="",
                stderr=f"Timeout after {timeout_sec}s",
                duration_ms=(time.time() - start) * 1000,
                command=" ".join(full_cmd_list),
            )
        except Exception as e:
            return ExecutionResult(
                exit_code=-1,
                stdout="",
                stderr=str(e),
                duration_ms=(time.time() - start) * 1000,
                command=" ".join(full_cmd_list),
            )
