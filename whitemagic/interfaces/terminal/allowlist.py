"""Command allowlist system with profiles."""

import logging
from enum import Enum

logger = logging.getLogger(__name__)


class Profile(str, Enum):
    """Execution profiles."""

    DEV = "dev"  # Development (relaxed)
    CI = "ci"  # CI/CD
    AGENT = "agent"  # AI agent
    PROD = "prod"  # Production (strict)


class Allowlist:
    """Command allowlist with profiles."""

    # Always blocked
    BLOCKED = {
        "rm",
        "rmdir",
        "dd",
        "mkfs",
        "chmod",
        "chown",
        "sudo",
        "su",
        "shutdown",
        "reboot",
        "halt",
        "kill",
        "killall",
        "pkill",
    }

    # Read-only safe commands - SECURITY: Commands must be safe even with arbitrary arguments
    READ_ONLY = {
        # Basic commands (truly read-only)
        "ls",
        "cat",
        "head",
        "tail",
        "less",
        "more",
        # NOTE: fd and rg removed - they support --exec and --pre flags for command execution
        # Users should use grep/ls instead for read-only operations
        "grep",    # Safe for pattern matching (no exec flags)
        "ps",
        "top",
        "df",
        "du",
        "wc",
        "stat",
        "which",
        "type",
        "pwd",
        # REMOVED DANGEROUS COMMANDS:
        # - find: Can execute arbitrary code via -exec flag
        # - awk: Can execute shell commands via system()
        # - sed: Can execute shell commands via e flag
        # - echo/printf: Can be used for injection if not careful
        # - env: Can leak sensitive environment variables
        # Git read operations - RESTRICTED to safe subcommands only
        "git log",
        "git show",
        "git diff",
        "git status",
        "git branch",
        # NOTE: Removed wildcard patterns to prevent git alias attacks
        # Git aliases can execute arbitrary commands, so we only allow exact matches
    }

    # Write operations (need approval)
    WRITE_OPS = {
        "git add",
        "git commit",
        "git push",
        "git pull",
        "git clone",
        "cp",
        "mv",
        "mkdir",
        "touch",
        "npm install",
        "pip install",
        "cargo build",
        "make",
    }

    def __init__(self, profile: Profile = Profile.AGENT, extra_allowed: set[str] | None = None):
        self.profile = profile
        self.extra_allowed = set(extra_allowed or [])

    def is_allowed(self, cmd: str, args: list[str] | None = None) -> bool:
        """Check if command is allowed with argument-level validation.

        SECURITY: Validates both command and arguments to prevent injection attacks.
        Even "safe" commands can be dangerous with certain arguments:
        - git can execute arbitrary code via aliases, hooks, or -c commands
        - Many commands support environment variable injection

        Args:
            cmd: Command name (e.g., "git")
            args: Command arguments (e.g., ["log", "-5"])

        Returns:
            True if command is allowed, False otherwise

        """
        args = args or []

        # SECURITY: Block dangerous argument patterns
        dangerous_patterns = [
            "-exec",     # find -exec
            "--exec",
            "system(",   # awk system() calls
            "`",         # Command substitution
            "$(",        # Command substitution
            "|",         # Pipe to other commands
            ";",         # Command chaining
            "&",         # Background execution
            ">",         # File redirection (write)
            ">>",        # File append (write)
            # SECURITY FIX: Block git command execution vectors
            "-c ",       # git -c core.pager=... or -c alias.x=!cmd
            "--config",  # git --config (same as -c)
            "--paginate", # git --paginate (can execute pager)
            "!",         # git alias command execution marker
        ]

        full_args = " ".join(args)
        for pattern in dangerous_patterns:
            if pattern in full_args:
                logger.warning(f"Blocked command with dangerous pattern '{pattern}': {cmd} {full_args}")
                return False

        # Build variants for matching
        base_cmd = cmd
        cmd_with_verb = f"{cmd} {args[0]}" if args else cmd
        full_cmd = cmd + (" " + " ".join(args) if args else "")

        # Always block dangerous commands (check base command)
        if any(base_cmd.startswith(blocked) for blocked in self.BLOCKED):
            return False

        # Helper to check if command matches pattern
        def matches_pattern(pattern: str, test_cmd: str) -> bool:
            """Check if command matches pattern with wildcards."""
            if "*" not in pattern:
                return pattern == test_cmd
            # Simple wildcard matching
            parts = pattern.split("*")
            pos = 0
            for part in parts:
                if not part:
                    continue
                idx = test_cmd.find(part, pos)
                if idx == -1:
                    return False
                pos = idx + len(part)
            return True

        # Check against allowlist
        def is_in_set(cmd_set: set[str]) -> bool:
            """Check if command matches any entry in set."""
            for allowed in cmd_set.union(self.extra_allowed):
                # Exact match on full command
                if allowed == full_cmd:
                    return True
                # Base command match
                if allowed == base_cmd:
                    return True
                # Command + verb match
                if allowed == cmd_with_verb:
                    return True
                # Pattern match
                if matches_pattern(allowed, full_cmd):
                    return True
            return False

        # Profile-specific logic
        if self.profile == Profile.PROD:
            # Prod: only explicit READ_ONLY commands
            return is_in_set(self.READ_ONLY)

        if self.profile == Profile.AGENT:
            # Agent: READ_ONLY + WRITE_OPS
            return is_in_set(self.READ_ONLY) or is_in_set(self.WRITE_OPS)

        # Dev and CI allow most things (not blocked)
        return True

    def requires_approval(self, cmd: str, args: list[str] | None = None) -> bool:
        """Check if command requires approval.

        Args:
            cmd: Command name
            args: Command arguments

        Returns:
            True if approval required, False otherwise

        """
        full_cmd = cmd
        if args:
            full_cmd = cmd + " " + " ".join(args)

        return full_cmd in self.WRITE_OPS or cmd in self.WRITE_OPS
