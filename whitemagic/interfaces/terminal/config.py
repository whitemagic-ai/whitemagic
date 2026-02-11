"""Configuration for terminal tool."""

import os
from pathlib import Path
from typing import cast

from pydantic import BaseModel, Field

from whitemagic.config.paths import WM_ROOT


class TerminalConfig(BaseModel):
    """Terminal tool configuration."""

    # Execution settings
    default_profile: str = Field("agent", description="Default execution profile")
    default_timeout: int = Field(30, description="Default timeout in seconds")
    auto_approve: bool = Field(False, description="Auto-approve write operations")

    # Audit settings
    audit_enabled: bool = Field(True, description="Enable audit logging")
    audit_dir: Path | None = Field(None, description="Audit log directory")

    # Allowlist customization
    custom_blocked: list[str] = Field(
        default_factory=list, description="Additional blocked commands",
    )
    custom_allowed: list[str] = Field(
        default_factory=list, description="Additional allowed commands",
    )

    # Safety settings
    require_approval_for_write: bool = Field(True, description="Require approval for write ops")
    allow_interactive: bool = Field(False, description="Allow interactive commands")

    @classmethod
    def from_env(cls) -> "TerminalConfig":
        """Load configuration from environment."""
        timeout_str = os.getenv("WM_EXEC_TIMEOUT", "30")
        try:
            timeout = int(timeout_str)
        except ValueError:
            from logging import getLogger
            getLogger(__name__).warning(f"Invalid WM_EXEC_TIMEOUT: {timeout_str}. Using default 30.")
            timeout = 30

        return cls(
            default_profile=os.getenv("WM_EXEC_PROFILE", "agent"),
            default_timeout=timeout,
            auto_approve=os.getenv("WM_AUTO_APPROVE", "false").lower() == "true",
            audit_enabled=os.getenv("WM_AUDIT_ENABLED", "true").lower() == "true",
            audit_dir=None,
            require_approval_for_write=True,
            allow_interactive=False,
        )

    def save(self, path: Path | None = None) -> None:
        """Save configuration to file."""
        if path is None:
            path = WM_ROOT / "terminal_config.json"

        path.parent.mkdir(parents=True, exist_ok=True)

        with open(path, "w") as f:
            f.write(self.model_dump_json(indent=2))

    @classmethod
    def load(cls, path: Path | None = None) -> "TerminalConfig":
        """Load configuration from file."""
        if path is None:
            path = WM_ROOT / "terminal_config.json"

        if not path.exists():
            return cls.from_env()

        with open(path) as f:
            return cast("TerminalConfig", cls.model_validate_json(f.read()))
