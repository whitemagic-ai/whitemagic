"""Tool Security Gating - Per-tool authorization and path restriction.

Provides:
- Per-tool safety classification (safe, restricted, dangerous)
- Path validation and allowlisting
- Deployment-mode aware restrictions
- Error sanitization
"""

from __future__ import annotations

import logging
import os
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.config.validator import get_validator

logger = logging.getLogger(__name__)


class ToolRisk(str, Enum):
    """Tool risk classification for security gating."""

    SAFE = "safe"           # Read-only, no external access
    MODERATE = "moderate"   # Write to allowed paths only
    RESTRICTED = "restricted"  # Requires explicit permission
    DANGEROUS = "dangerous"    # Disabled in cloud mode


# Tools classified by risk level
TOOL_RISK_CLASSIFICATION: dict[str, ToolRisk] = {
    # SAFE - Read-only memory/session operations
    "search_memories": ToolRisk.SAFE,
    "list_memories": ToolRisk.SAFE,
    "read_memory": ToolRisk.SAFE,
    "get_session_context": ToolRisk.SAFE,
    "session_bootstrap": ToolRisk.SAFE,
    "get_yin_yang_balance": ToolRisk.SAFE,
    "garden_status": ToolRisk.SAFE,
    "list_gardens": ToolRisk.SAFE,
    "prat_status": ToolRisk.SAFE,
    "prat_list_morphologies": ToolRisk.SAFE,
    "check_system_health": ToolRisk.SAFE,
    "get_metrics_summary": ToolRisk.SAFE,
    "capabilities": ToolRisk.SAFE,
    "manifest": ToolRisk.SAFE,
    "state.paths": ToolRisk.SAFE,
    "state.summary": ToolRisk.SAFE,
    "repo.summary": ToolRisk.SAFE,
    "ship.check": ToolRisk.SAFE,

    # MODERATE - Write to WhiteMagic data only
    "create_memory": ToolRisk.MODERATE,
    "update_memory": ToolRisk.MODERATE,
    "delete_memory": ToolRisk.MODERATE,
    "create_session": ToolRisk.MODERATE,
    "consolidate": ToolRisk.MODERATE,
    "track_metric": ToolRisk.MODERATE,
    "garden_activate": ToolRisk.MODERATE,

    # RESTRICTED - Requires explicit cloud mode permission
    "fast_read_memory": ToolRisk.RESTRICTED,  # Can read arbitrary files
    "batch_read_memories": ToolRisk.RESTRICTED,
    "windsurf_list_conversations": ToolRisk.RESTRICTED,
    "windsurf_read_conversation": ToolRisk.RESTRICTED,
    "windsurf_export_conversation": ToolRisk.RESTRICTED,
    "windsurf_search_conversations": ToolRisk.RESTRICTED,
    "windsurf_stats": ToolRisk.RESTRICTED,
    "archaeology_find_unread": ToolRisk.RESTRICTED,
    "rust_audit": ToolRisk.RESTRICTED,

    # DANGEROUS - Disabled in cloud mode by default
    "execute_command": ToolRisk.DANGEROUS,
    "browser_automation": ToolRisk.DANGEROUS,
    "browser_navigate": ToolRisk.DANGEROUS,
    "browser_click": ToolRisk.DANGEROUS,
    "browser_type": ToolRisk.DANGEROUS,
    "browser_extract_dom": ToolRisk.DANGEROUS,
    "browser_screenshot": ToolRisk.DANGEROUS,
    "browser_get_interactables": ToolRisk.DANGEROUS,
    "edge_infer": ToolRisk.DANGEROUS,
}


@dataclass
class PathValidator:
    """Validates and restricts file system paths."""

    # Allowed base paths (resolved at init from env/config)
    allowed_bases: set[Path] = field(default_factory=set)

    # Explicitly blocked paths (sensitive system files)
    blocked_paths: set[str] = field(default_factory=lambda: {
        "/etc/passwd",
        "/etc/shadow",
        "/etc/sudoers",
        "~/.ssh",
        "~/.gnupg",
        "~/.aws",
        "~/.config/gcloud",
        "/proc",
        "/sys",
    })

    def __post_init__(self) -> None:
        """Initialize allowed paths from environment/config."""
        # Default: WM_STATE_ROOT (canonical Whitemagic state root)
        config_root = WM_ROOT.resolve()
        self.allowed_bases = {
            config_root,
            config_root / "memory",
            config_root / "data",
            config_root / "cache",
        }

        # Local mode convenience: allow current working directory *reads*.
        # NOTE: in cloud mode, ToolGate.__post_init__ may remove this unless
        # explicitly enabled.
        self.allowed_bases.add(Path.cwd().resolve())

        # Add from environment if set
        extra_paths = os.getenv("WHITEMAGIC_ALLOWED_PATHS", "")
        if extra_paths:
            for p in extra_paths.split(":"):
                if p.strip():
                    self.allowed_bases.add(Path(p.strip()).resolve())

    def is_path_allowed(self, path: str) -> tuple[bool, str]:
        """Check if a path is allowed for access.

        Returns:
            (allowed: bool, reason: str)

        """
        try:
            resolved = Path(path).expanduser().resolve()
        except Exception as e:
            return False, f"Invalid path: {e}"

        # Check blocked paths
        path_str = str(resolved)
        for blocked in self.blocked_paths:
            # Expand ~ once if present, then resolve
            blocked_resolved = str(Path(blocked).expanduser().resolve())
            if path_str.startswith(blocked_resolved):
                return False, f"Access to {blocked} is blocked"

        # Check if within allowed bases
        for base in self.allowed_bases:
            try:
                resolved.relative_to(base)
                return True, "Path within allowed directory"
            except ValueError:
                continue

        return False, f"Path {path} is outside allowed directories"

    def sanitize_path(self, path: str) -> Path | None:
        """Sanitize and validate a path, returning None if invalid.
        """
        allowed, reason = self.is_path_allowed(path)
        if not allowed:
            logger.warning(f"Path blocked: {path} - {reason}")
            return None
        return Path(path).expanduser().resolve()


@dataclass
class ToolGate:
    """Security gate for tool execution.

    Enforces:
    - Per-tool risk classification
    - Deployment-mode restrictions
    - Path validation for file operations
    - Error sanitization
    """

    deployment_mode: str = "local"
    path_validator: PathValidator = field(default_factory=PathValidator)

    # Cloud mode settings
    allow_restricted_in_cloud: bool = False
    allow_dangerous_in_cloud: bool = False

    def __post_init__(self) -> None:
        """Initialize from config."""
        validator = get_validator()
        self.deployment_mode = validator.validate_deployment_mode()

        # Check environment overrides
        self.allow_restricted_in_cloud = os.getenv(
            "WHITEMAGIC_ALLOW_RESTRICTED_TOOLS", "false",
        ).lower() == "true"
        self.allow_dangerous_in_cloud = os.getenv(
            "WHITEMAGIC_ALLOW_DANGEROUS_TOOLS", "false",
        ).lower() == "true"

        # Cloud mode: do not allow arbitrary cwd access unless explicitly allowed
        if self.deployment_mode == "cloud":
            if os.getenv("WHITEMAGIC_ALLOW_CWD_PATH", "false").lower() != "true":
                self.path_validator.allowed_bases.discard(Path.cwd().resolve())

    def check_tool_allowed(self, tool_name: str) -> tuple[bool, str]:
        """Check if a tool is allowed to execute.

        Returns:
            (allowed: bool, reason: str)

        """
        risk = TOOL_RISK_CLASSIFICATION.get(tool_name, ToolRisk.RESTRICTED)

        # Safe tools always allowed
        if risk == ToolRisk.SAFE:
            return True, "Tool is safe"

        # Moderate tools allowed in both modes
        if risk == ToolRisk.MODERATE:
            return True, "Tool is moderate risk"

        # Local mode: allow everything
        if self.deployment_mode == "local":
            return True, "Local mode - all tools allowed"

        # Cloud mode restrictions
        if risk == ToolRisk.RESTRICTED:
            if self.allow_restricted_in_cloud:
                return True, "Restricted tool explicitly allowed"

            # Report to immune system
            self._report_blocked_tool(tool_name, risk, "restricted_in_cloud")
            return False, f"Tool '{tool_name}' is restricted in cloud mode"

        if risk == ToolRisk.DANGEROUS:
            if self.allow_dangerous_in_cloud:
                logger.warning(f"Dangerous tool {tool_name} allowed via override")
                return True, "Dangerous tool explicitly allowed (WARNING)"

            # Report to immune system
            self._report_blocked_tool(tool_name, risk, "dangerous_in_cloud")
            return False, f"Tool '{tool_name}' is disabled in cloud mode"

        return False, f"Unknown risk level for tool '{tool_name}'"

    def _report_blocked_tool(self, tool_name: str, risk: ToolRisk, reason: str) -> None:
        """Report blocked tool to immune system for pattern learning."""
        try:
            from whitemagic.immune.security_integration import report_threat
            report_threat(
                threat_type="tool_blocked",
                tool=tool_name,
                params={"risk": risk.value},
                reason=reason,
                severity="high" if risk == ToolRisk.DANGEROUS else "medium",
            )
        except Exception as e:
            logger.debug(f"Failed to report to immune system: {e}")

    def validate_tool_params(
        self, tool_name: str, params: dict[str, Any],
    ) -> tuple[bool, str, dict[str, Any]]:
        """Validate and sanitize tool parameters.

        Returns:
            (valid: bool, reason: str, sanitized_params: dict)

        """
        sanitized = params.copy()

        # Path-based tools need path validation
        path_params = ["path", "file_path", "filename", "directory", "base_path"]

        for param in path_params:
            if param in sanitized:
                path_value = sanitized[param]
                if isinstance(path_value, str):
                    allowed, reason = self.path_validator.is_path_allowed(path_value)
                    if not allowed:
                        # Report to immune system
                        self._report_path_violation(tool_name, param, path_value, reason)
                        return False, f"Invalid {param}: {reason}", {}

        # URL validation for browser tools
        if tool_name in {
            "browser_automation",
            "browser_open",
            "browser_navigate",
        }:
            url = sanitized.get("url", "")
            if url and not self._is_url_allowed(url):
                # Report to immune system
                self._report_url_violation(tool_name, url)
                return False, f"URL not allowed: {url}", {}

        return True, "Parameters valid", sanitized

    def _report_path_violation(self, tool_name: str, param: str, path: str, reason: str) -> None:
        """Report path violation to immune system."""
        try:
            from whitemagic.immune.security_integration import report_threat
            report_threat(
                threat_type="path_violation",
                tool=tool_name,
                params={param: path},
                reason=reason,
                severity="high",
            )
        except Exception as e:
            logger.debug(f"Failed to report path violation: {e}")

    def _report_url_violation(self, tool_name: str, url: str) -> None:
        """Report URL violation to immune system."""
        try:
            from whitemagic.immune.security_integration import report_threat
            report_threat(
                threat_type="url_blocked",
                tool=tool_name,
                params={"url": url},
                reason="Internal network or file:// URL blocked",
                severity="high",
            )
        except Exception as e:
            logger.debug(f"Failed to report URL violation: {e}")

    def _is_url_allowed(self, url: str) -> bool:
        """Check if a URL is allowed for browser automation."""
        # Block internal network access
        blocked_prefixes = [
            "http://localhost",
            "https://localhost",
            "http://127.0.0.1",
            "https://127.0.0.1",
            "http://[::1]",
            "https://[::1]",
            "http://0.0.0.0",
            "https://0.0.0.0",
            "http://10.",
            "https://10.",
            "http://172.16.",
            "https://172.16.",
            "http://172.17.",
            "https://172.17.",
            "http://172.18.",
            "https://172.18.",
            "http://172.19.",
            "https://172.19.",
            "http://172.20.",
            "https://172.20.",
            "http://172.21.",
            "https://172.21.",
            "http://172.22.",
            "https://172.22.",
            "http://172.23.",
            "https://172.23.",
            "http://172.24.",
            "https://172.24.",
            "http://172.25.",
            "https://172.25.",
            "http://172.26.",
            "https://172.26.",
            "http://172.27.",
            "https://172.27.",
            "http://172.28.",
            "https://172.28.",
            "http://172.29.",
            "https://172.29.",
            "http://172.30.",
            "https://172.30.",
            "http://172.31.",
            "https://172.31.",
            "http://192.168.",
            "https://192.168.",
            "file://",
        ]

        url_lower = url.lower()
        for prefix in blocked_prefixes:
            if url_lower.startswith(prefix):
                # Allow localhost in local mode
                if self.deployment_mode == "local" and "localhost" in prefix:
                    return True
                return False

        return True

    def sanitize_error(self, error: Exception) -> str:
        """Sanitize error message for external response.

        Removes:
        - File paths
        - Stack traces
        - Internal details
        """
        if self.deployment_mode == "local":
            # Local mode: full error details
            return str(error)

        # Cloud mode: sanitized error
        error_str = str(error)

        # Remove file paths
        import re
        error_str = re.sub(r"/[^\s:]+\.py", "[file]", error_str)
        error_str = re.sub(r"/home/[^\s:]+", "[path]", error_str)
        error_str = re.sub(r"line \d+", "line [N]", error_str)

        return error_str


# Singleton instance
_tool_gate: ToolGate | None = None


def get_tool_gate() -> ToolGate:
    """Get the singleton ToolGate instance."""
    global _tool_gate
    if _tool_gate is None:
        _tool_gate = ToolGate()
    return _tool_gate


def check_tool_execution(
    tool_name: str, params: dict[str, Any],
) -> tuple[bool, str, dict[str, Any]]:
    """Check if a tool can be executed with given parameters.

    Returns:
        (allowed: bool, reason: str, sanitized_params: dict)

    """
    gate = get_tool_gate()

    # Check tool allowed
    allowed, reason = gate.check_tool_allowed(tool_name)
    if not allowed:
        return False, reason, {}

    # Validate parameters
    return gate.validate_tool_params(tool_name, params)


__all__ = [
    "TOOL_RISK_CLASSIFICATION",
    "PathValidator",
    "ToolGate",
    "ToolRisk",
    "check_tool_execution",
    "get_tool_gate",
]
