"""Governor Safety Module - Pre-execution Validation.

Purpose: Prove plans are valid before execution.
Based on the insight: "Governor as formal verification layer"

Features:
1. Forbidden action detection (destructive commands, system mods, credential exposure)
2. Resource budget enforcement (file ops, API calls, time limits)
3. Context drift detection (goal alignment tracking)
4. Constitutional checks (dharma principles)
"""

from __future__ import annotations

import os
import re
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum, auto
from pathlib import Path
from threading import Lock
from typing import Any

# Singleton instance
_governor_instance: "Governor" | None = None
_governor_lock = Lock()


class RiskLevel(Enum):
    """Risk classification for actions."""

    SAFE = auto()
    CAUTION = auto()  # Proceed with warning
    DANGEROUS = auto()  # Requires confirmation
    FORBIDDEN = auto()  # Never execute


@dataclass
class ValidationResult:
    """Result of validating a command or action."""

    safe: bool
    reason: str
    risk_level: RiskLevel = RiskLevel.SAFE
    suggestions: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "safe": self.safe,
            "reason": self.reason,
            "risk_level": self.risk_level.name,
            "suggestions": self.suggestions,
        }


@dataclass
class DriftResult:
    """Result of checking context drift."""

    drifted: bool
    similarity: float  # 0.0-1.0
    original_goal: str
    current_action: str
    explanation: str

    def to_dict(self) -> dict[str, Any]:
        return {
            "drifted": self.drifted,
            "similarity": self.similarity,
            "original_goal": self.original_goal,
            "current_action": self.current_action,
            "explanation": self.explanation,
        }


@dataclass
class ResourceBudget:
    """Track resource consumption."""

    max_file_ops: int = 100
    max_api_calls: int = 50
    max_time_seconds: int = 3600  # 1 hour default

    file_ops_used: int = 0
    api_calls_used: int = 0
    start_time: float | None = None

    def start(self) -> None:
        """Start tracking time."""
        self.start_time = time.time()

    def record_file_op(self) -> bool:
        """Record a file operation. Returns False if budget exceeded."""
        self.file_ops_used += 1
        return self.file_ops_used <= self.max_file_ops

    def record_api_call(self) -> bool:
        """Record an API call. Returns False if budget exceeded."""
        self.api_calls_used += 1
        return self.api_calls_used <= self.max_api_calls

    def time_remaining(self) -> float | None:
        """Get remaining time in seconds."""
        if self.start_time is None:
            return None
        elapsed = time.time() - self.start_time
        return max(0, self.max_time_seconds - elapsed)

    def is_time_exceeded(self) -> bool:
        """Check if time budget is exceeded."""
        remaining = self.time_remaining()
        return remaining is not None and remaining <= 0

    def status(self) -> dict[str, Any]:
        """Get budget status."""
        return {
            "file_ops": f"{self.file_ops_used}/{self.max_file_ops}",
            "api_calls": f"{self.api_calls_used}/{self.max_api_calls}",
            "time_remaining": self.time_remaining(),
            "budgets_ok": (
                self.file_ops_used <= self.max_file_ops and
                self.api_calls_used <= self.max_api_calls and
                not self.is_time_exceeded()
            ),
        }


class Governor:
    """Pre-execution validation system.

    Validates commands and actions before they're executed,
    detecting dangerous patterns, enforcing budgets, and
    checking for context drift.
    """

    # =========================================================================
    # Forbidden Patterns - Commands that should never be executed
    # =========================================================================

    FORBIDDEN_COMMANDS = [
        # Destructive file operations
        r"rm\s+-rf\s+/$",  # Delete root itself
        r"rm\s+-rf\s+/\s*$",  # Delete root with trailing space
        r"rm\s+-rf\s+/[a-z]+\s*$",  # Delete top-level dirs like /bin, /etc
        r"rm\s+-rf\s+~/?$",  # Delete home dir
        r"rm\s+-rf\s+\.\s*$",  # Delete current dir recursively
        r"rm\s+-rf\s+\*",  # Glob delete
        r"rmdir\s+/",  # Remove root dirs
        r"find\s+.*-delete",  # Find with delete
        r"find\s+.*-exec\s+rm",  # Find with rm

        # Format/disk operations
        r"mkfs\.",  # Format filesystem
        r"dd\s+.*of=/dev/",  # Write to device
        r"fdisk",  # Partition editor
        r"parted",  # Partition tool

        # System destruction
        r":()\{\s*:\|:\&\s*\};:",  # Fork bomb
        r">\s*/dev/sda",  # Overwrite disk
        r"mv\s+/\s+",  # Move root
        r"chmod\s+-R\s+777\s+/",  # Permission chaos
        r"chown\s+-R\s+.*\s+/",  # Ownership change on root

        # Network attacks
        r"nmap\s+-sS",  # SYN scan (could be attack)
        r"hping3",  # Packet crafting
        r"ettercap",  # ARP spoofing

        # Credential exposure
        r"echo\s+.*password",  # Echoing passwords
        r"curl\s+.*\|\s*sh",  # Curl pipe to shell
        r"wget\s+.*\|\s*sh",  # Wget pipe to shell
    ]

    FORBIDDEN_PATTERNS = [re.compile(p, re.IGNORECASE) for p in FORBIDDEN_COMMANDS]

    # =========================================================================
    # Dangerous Patterns - Require explicit confirmation
    # =========================================================================

    DANGEROUS_COMMANDS = [
        r"rm\s+-r",  # Recursive delete (not forced from root)
        r"sudo\s+rm",  # Sudo remove
        r"sudo\s+chmod",  # Sudo chmod
        r"chmod\s+-R",  # Recursive chmod
        r"git\s+push\s+.*--force",  # Force push
        r"git\s+reset\s+--hard",  # Hard reset
        r"drop\s+database",  # SQL drop
        r"drop\s+table",  # SQL drop table
        r"truncate\s+table",  # SQL truncate
        r"delete\s+from\s+.*where\s+1=1",  # Delete all
        r">\s+\S+",  # File overwrite with redirect
        r"pip\s+install\s+--upgrade",  # Upgrade packages
        r"npm\s+install\s+-g",  # Global npm install
    ]

    DANGEROUS_PATTERNS = [re.compile(p, re.IGNORECASE) for p in DANGEROUS_COMMANDS]

    # =========================================================================
    # Caution Patterns - Log and proceed
    # =========================================================================

    CAUTION_COMMANDS = [
        r"sudo\s+",  # Any sudo
        r"rm\s+",  # Any remove
        r"mv\s+",  # Any move (could overwrite)
        r"cp\s+-f",  # Force copy
        r"git\s+checkout\s+-f",  # Force checkout
        r"pip\s+uninstall",  # Uninstall packages
        r"apt\s+remove",  # Remove packages
        r"brew\s+uninstall",  # Remove packages
    ]

    CAUTION_PATTERNS = [re.compile(p, re.IGNORECASE) for p in CAUTION_COMMANDS]

    # =========================================================================
    # Protected Paths - Directories that should not be modified
    # =========================================================================

    PROTECTED_PATHS = [
        "/bin", "/sbin", "/usr/bin", "/usr/sbin",
        "/etc", "/boot", "/sys", "/proc",
        "/var/lib", "/var/log",
        "/root", "~/.ssh", "~/.gnupg",
        "/home/*/.ssh", "/home/*/.gnupg",
    ]

    # =========================================================================
    # Constitutional Principles (Dharma)
    # =========================================================================

    DHARMA_PRINCIPLES = {
        "non_harm": "Actions should not cause harm to users or systems",
        "transparency": "Actions should be explainable and auditable",
        "reversibility": "Prefer actions that can be undone",
        "minimal_footprint": "Use only necessary permissions and resources",
        "user_consent": "Significant actions require user awareness",
        "data_sanctity": "Protect user data as sacred",
        "system_stability": "Maintain system health and stability",
    }

    def __init__(self) -> None:
        self.budget = ResourceBudget()
        self.current_goal: str | None = None
        self.action_history: list[dict[str, Any]] = []
        self._validation_count = 0
        self._blocked_count = 0
        # Check active enforcement flag
        self.enabled = os.getenv("WHITEMAGIC_ENFORCE_DHARMA", "0").lower() in ("1", "true", "yes", "on")

    # =========================================================================
    # Tool Validation (MCP)
    # =========================================================================

    def validate_tool_call(self, tool_name: str, args: dict[str, Any]) -> ValidationResult:
        """Validate an MCP tool call.

        Args:
            tool_name: Name of the tool
            args: Arguments for the tool

        Returns:
            ValidationResult

        """
        if not self.enabled:
            return ValidationResult(safe=True, reason="Governor disabled", risk_level=RiskLevel.SAFE)

        self._validation_count += 1

        # 1. Fetch tool definition from registry
        from whitemagic.tools.registry import get_tool
        tool_def = get_tool(tool_name)

        # 2. Derive risk level
        if tool_def:
            try:
                risk_level_name = tool_def.risk_level
                risk_level = RiskLevel[risk_level_name]
            except (KeyError, AttributeError):
                risk_level = RiskLevel.SAFE
        else:
            # Fallback for unregistered tools (treat as caution if starts with risky prefix)
            destructive_prefixes = ("delete_", "update_", "execute_", "bash", "shell", "run_command", "write_")
            is_risky = any(tool_name.startswith(p) for p in destructive_prefixes)
            risk_level = RiskLevel.CAUTION if is_risky else RiskLevel.SAFE

        if risk_level == RiskLevel.SAFE:
            return ValidationResult(safe=True, reason="Low risk tool", risk_level=RiskLevel.SAFE)

        # 3. Check Resource Budget (if active)
        if not self.budget.status()["budgets_ok"]:
             return ValidationResult(
                safe=False,
                reason="Resource budget exceeded",
                risk_level=RiskLevel.FORBIDDEN,
            )

        # 3. Consult Dharma Bridge
        try:
            from whitemagic.core.bridge.dharma import dharma_check_boundaries

            # Construct action description for Dharma
            # Using verify_consent if needed, or check_boundaries
            action = {"tool": tool_name, "args": args}

            # Use strict mode if set
            strict = os.getenv("WHITEMAGIC_DHARMA_STRICT", "0").lower() in ("1", "true", "yes", "on")

            safety_status = dharma_check_boundaries(action, strict_mode=strict)

            if safety_status.get("has_violations"):
                violations = safety_status.get("violations", [])
                reasons = [v.get("description", "Unknown violation") for v in violations]
                self._blocked_count += 1
                return ValidationResult(
                    safe=False,
                    reason=f"Dharma Violations: {'; '.join(reasons)}",
                    risk_level=RiskLevel.DANGEROUS if not strict else RiskLevel.FORBIDDEN,
                    suggestions=[v.get("suggested_action", "Review ethics") for v in violations],
                )

            return ValidationResult(
                safe=True,
                reason="Dharma checks passed",
                risk_level=RiskLevel.SAFE,
            )

        except ImportError:
            # If Dharma system is missing, fail open unless strict
            if os.getenv("WHITEMAGIC_DHARMA_STRICT", "0").lower() in ("1", "true", "yes", "on"):
                self._blocked_count += 1
                return ValidationResult(safe=False, reason="Dharma system missing in STRICT mode", risk_level=RiskLevel.FORBIDDEN)
            return ValidationResult(safe=True, reason="Dharma system unavailable (fail-open)", risk_level=RiskLevel.CAUTION)

        except Exception as e:
            self._blocked_count += 1
            return ValidationResult(safe=False, reason=f"Governor error: {e}", risk_level=RiskLevel.DANGEROUS)

    # =========================================================================
    # Command Validation
    # =========================================================================

    def validate_command(self, cmd: str) -> ValidationResult:
        """Validate a shell command before execution.

        Args:
            cmd: The command string to validate

        Returns:
            ValidationResult with safe status and reason

        """
        self._validation_count += 1
        cmd_lower = cmd.lower().strip()

        # Check for empty command
        if not cmd_lower:
            return ValidationResult(
                safe=True,
                reason="Empty command",
                risk_level=RiskLevel.SAFE,
            )

        # Check forbidden patterns
        for pattern in self.FORBIDDEN_PATTERNS:
            if pattern.search(cmd):
                self._blocked_count += 1
                return ValidationResult(
                    safe=False,
                    reason=f"Forbidden pattern detected: {pattern.pattern}",
                    risk_level=RiskLevel.FORBIDDEN,
                    suggestions=[
                        "This command pattern is never allowed",
                        "Consider a safer alternative",
                        "If this is legitimate, break it into smaller, safer steps",
                    ],
                )

        # Check dangerous patterns
        for pattern in self.DANGEROUS_PATTERNS:
            if pattern.search(cmd):
                return ValidationResult(
                    safe=False,
                    reason=f"Dangerous pattern detected: {pattern.pattern}",
                    risk_level=RiskLevel.DANGEROUS,
                    suggestions=[
                        "This command requires explicit confirmation",
                        "Consider adding --dry-run or -n flag first",
                        "Make a backup before proceeding",
                    ],
                )

        # Check caution patterns
        for pattern in self.CAUTION_PATTERNS:
            if pattern.search(cmd):
                return ValidationResult(
                    safe=True,
                    reason=f"Caution: {pattern.pattern} - proceed with awareness",
                    risk_level=RiskLevel.CAUTION,
                    suggestions=[
                        "Verify the target paths are correct",
                        "Consider running with verbose flag",
                    ],
                )

        # Check for protected path modifications
        for protected in self.PROTECTED_PATHS:
            if protected.replace("~", "").replace("*", "") in cmd:
                # Check if it's a modifying command
                if any(mod in cmd_lower for mod in ["rm ", "mv ", "chmod ", "chown ", "> "]):
                    return ValidationResult(
                        safe=False,
                        reason=f"Attempting to modify protected path: {protected}",
                        risk_level=RiskLevel.DANGEROUS,
                        suggestions=[
                            "System paths should not be modified directly",
                            "Use package managers for system changes",
                        ],
                    )

        # Command passes all checks
        return ValidationResult(
            safe=True,
            reason="Command validated successfully",
            risk_level=RiskLevel.SAFE,
        )

    def validate_path(self, path: str, operation: str = "access") -> ValidationResult:
        """Validate a file path for an operation.

        Args:
            path: The file path
            operation: Type of operation (read, write, delete, etc.)

        """
        path_obj = Path(path).resolve()
        path_str = str(path_obj)

        # Check protected paths
        for protected in self.PROTECTED_PATHS:
            protected_expanded = Path(protected.replace("~", str(Path.home()))).resolve()
            if path_str.startswith(str(protected_expanded)):
                if operation in ("write", "delete", "modify"):
                    return ValidationResult(
                        safe=False,
                        reason=f"Cannot {operation} in protected path: {protected}",
                        risk_level=RiskLevel.FORBIDDEN,
                    )

        # Check for credential files
        credential_patterns = [".ssh", ".gnupg", ".aws", "credentials", "secret", ".env"]
        for pattern in credential_patterns:
            if pattern in path_str.lower():
                return ValidationResult(
                    safe=False,
                    reason=f"Path contains credential indicator: {pattern}",
                    risk_level=RiskLevel.DANGEROUS,
                    suggestions=["Be very careful with credential files"],
                )

        return ValidationResult(
            safe=True,
            reason="Path validated",
            risk_level=RiskLevel.SAFE,
        )

    # =========================================================================
    # Context Drift Detection
    # =========================================================================

    def set_goal(self, goal: str) -> None:
        """Set the current goal for drift detection."""
        self.current_goal = goal
        self.action_history = []

    def check_drift(self, action: str, goal: str | None = None) -> DriftResult:
        """Check if an action has drifted from the original goal.

        Uses simple keyword overlap for now. Could be enhanced with
        embeddings or semantic similarity.

        Args:
            action: The current action description
            goal: Override goal (uses self.current_goal if None)

        Returns:
            DriftResult with drift assessment

        """
        goal = goal or self.current_goal

        if not goal:
            return DriftResult(
                drifted=False,
                similarity=1.0,
                original_goal="No goal set",
                current_action=action,
                explanation="No goal set for comparison",
            )

        # Extract keywords (improved approach with stemming simulation)
        def extract_keywords(text: str) -> set[str]:
            # Remove common words and extract meaningful terms
            stop_words = {"the", "a", "an", "is", "are", "was", "were", "be", "been",
                         "to", "of", "in", "for", "on", "with", "at", "by", "from",
                         "this", "that", "it", "and", "or", "but", "if", "then",
                         "new", "add", "adding", "update", "updating", "create", "creating",
                         "implement", "implementing", "feature", "component"}
            words = re.findall(r"\b\w+\b", text.lower())
            keywords = set()
            for w in words:
                if w in stop_words or len(w) <= 2:
                    continue
                # Simple stemming - remove common suffixes
                stem = w
                for suffix in ["ing", "tion", "ment", "ness", "able", "ible", "ed", "er", "est", "ly", "ful"]:
                    if stem.endswith(suffix) and len(stem) > len(suffix) + 2:
                        stem = stem[:-len(suffix)]
                        break
                keywords.add(stem)
            return keywords

        goal_keywords = extract_keywords(goal)
        action_keywords = extract_keywords(action)

        if not goal_keywords or not action_keywords:
            return DriftResult(
                drifted=False,
                similarity=1.0,
                original_goal=goal,
                current_action=action,
                explanation="Insufficient keywords for comparison",
            )

        # Calculate Jaccard similarity
        intersection = goal_keywords & action_keywords
        union = goal_keywords | action_keywords
        similarity = len(intersection) / len(union) if union else 0.0

        # Track action
        self.action_history.append({
            "action": action,
            "similarity": similarity,
            "timestamp": datetime.now().isoformat(),
        })

        # Consider drifted if similarity is below threshold
        drift_threshold = 0.2
        drifted = similarity < drift_threshold

        if drifted:
            explanation = (
                f"Action appears unrelated to goal. "
                f"Goal keywords: {goal_keywords}, "
                f"Action keywords: {action_keywords}"
            )
        else:
            common = intersection if intersection else {"(topic related)"}
            explanation = f"Action aligns with goal via: {common}"

        return DriftResult(
            drifted=drifted,
            similarity=similarity,
            original_goal=goal,
            current_action=action,
            explanation=explanation,
        )

    # =========================================================================
    # Resource Budget Enforcement
    # =========================================================================

    def start_session(
        self,
        max_file_ops: int = 100,
        max_api_calls: int = 50,
        max_time_seconds: int = 3600,
    ) -> None:
        """Start a new session with resource budgets."""
        self.budget = ResourceBudget(
            max_file_ops=max_file_ops,
            max_api_calls=max_api_calls,
            max_time_seconds=max_time_seconds,
        )
        self.budget.start()

    def check_budget(self) -> ValidationResult:
        """Check if within budget limits."""
        status = self.budget.status()

        if not status["budgets_ok"]:
            reasons = []
            if self.budget.file_ops_used > self.budget.max_file_ops:
                reasons.append(f"File operations exceeded: {status['file_ops']}")
            if self.budget.api_calls_used > self.budget.max_api_calls:
                reasons.append(f"API calls exceeded: {status['api_calls']}")
            if self.budget.is_time_exceeded():
                reasons.append("Time budget exceeded")

            return ValidationResult(
                safe=False,
                reason="; ".join(reasons),
                risk_level=RiskLevel.DANGEROUS,
                suggestions=["Consider completing the current task", "Request budget extension"],
            )

        return ValidationResult(
            safe=True,
            reason="Within budget limits",
            risk_level=RiskLevel.SAFE,
        )

    def record_file_op(self) -> bool:
        """Record a file operation and check budget."""
        return self.budget.record_file_op()

    def record_api_call(self) -> bool:
        """Record an API call and check budget."""
        return self.budget.record_api_call()

    # =========================================================================
    # Constitutional Checks (Dharma)
    # =========================================================================

    def check_dharma(self, action: str, context: dict[str, Any] | None = None) -> ValidationResult:
        """Check action against constitutional principles.

        Args:
            action: Description of the action
            context: Additional context about the action

        Returns:
            ValidationResult with dharma assessment

        """
        context = context or {}
        violations = []

        # Check non-harm
        harmful_indicators = ["delete", "destroy", "remove all", "wipe", "kill"]
        if any(ind in action.lower() for ind in harmful_indicators):
            if not context.get("user_confirmed"):
                violations.append(("non_harm", "Potentially harmful action without confirmation"))

        # Check reversibility
        irreversible_indicators = ["permanent", "cannot undo", "force", "--hard"]
        if any(ind in action.lower() for ind in irreversible_indicators):
            violations.append(("reversibility", "Action may not be reversible"))

        # Check minimal footprint
        if context.get("requires_sudo") or context.get("requires_root"):
            violations.append(("minimal_footprint", "Elevated privileges requested"))

        # Check data sanctity
        data_indicators = ["database", "user data", "credentials", "personal"]
        if any(ind in action.lower() for ind in data_indicators):
            if not context.get("backup_exists"):
                violations.append(("data_sanctity", "Operating on sensitive data without backup"))

        if violations:
            reasons = [f"{v[0]}: {v[1]}" for v in violations]
            return ValidationResult(
                safe=False,
                reason=f"Dharma violations: {'; '.join(reasons)}",
                risk_level=RiskLevel.DANGEROUS,
                suggestions=[
                    f"Consider: {self.DHARMA_PRINCIPLES.get(v[0], 'Unknown principle')}"
                    for v in violations
                ],
            )

        return ValidationResult(
            safe=True,
            reason="Action aligns with dharma principles",
            risk_level=RiskLevel.SAFE,
        )

    # =========================================================================
    # Statistics and Reporting
    # =========================================================================

    def stats(self) -> dict[str, Any]:
        """Get governor statistics."""
        return {
            "validations": self._validation_count,
            "blocked": self._blocked_count,
            "block_rate": self._blocked_count / max(1, self._validation_count),
            "current_goal": self.current_goal,
            "action_history_length": len(self.action_history),
            "budget": self.budget.status(),
        }

    def reset(self) -> None:
        """Reset governor state."""
        self.budget = ResourceBudget()
        self.current_goal = None
        self.action_history = []


def get_governor() -> Governor:
    """Get singleton Governor instance."""
    global _governor_instance
    with _governor_lock:
        if _governor_instance is None:
            _governor_instance = Governor()
        return _governor_instance


# =========================================================================
# Convenience Functions
# =========================================================================

def validate_command(cmd: str) -> tuple[bool, str]:
    """Convenience function to validate a command.

    Returns:
        Tuple of (safe, reason)

    """
    result = get_governor().validate_command(cmd)
    return (result.safe, result.reason)


def check_drift(action: str, goal: str | None = None) -> tuple[bool, float]:
    """Convenience function to check context drift.

    Returns:
        Tuple of (drifted, similarity)

    """
    result = get_governor().check_drift(action, goal)
    return (result.drifted, result.similarity)


def is_safe(cmd: str) -> bool:
    """Quick check if a command is safe."""
    result = get_governor().validate_command(cmd)
    return result.safe
