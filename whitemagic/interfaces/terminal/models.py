"""Data models for terminal execution."""

from enum import Enum

from pydantic import BaseModel, Field


class ExecutionMode(str, Enum):
    """Execution mode."""

    READ = "read"  # Read-only (safe)
    WRITE = "write"  # Write operations (requires approval)
    INTERACTIVE = "interactive"  # Interactive commands


class ExecutionRequest(BaseModel):
    """Request to execute command.

    SECURITY WARNING: env parameter should be validated before use.
    Dangerous env vars like GIT_PAGER, PAGER, LESSOPEN can execute arbitrary commands.
    """

    cmd: str = Field(..., description="Command to execute")
    args: list[str] = Field(default_factory=list, description="Command arguments")
    cwd: str | None = Field(None, description="Working directory")
    env: dict[str, str] | None = Field(None, description="Environment variables (MUST be validated!)")
    stdin: str | None = Field(None, description="Standard input")
    timeout_ms: int = Field(30000, description="Timeout in milliseconds")
    mode: ExecutionMode = Field(ExecutionMode.READ, description="Execution mode")
    correlation_id: str | None = Field(None, description="Correlation ID")


class ExecutionResponse(BaseModel):
    """Response from command execution."""

    exit_code: int
    stdout: str
    stderr: str
    duration_ms: float
    run_id: str | None = None
    command: str
    mode: str
    approved: bool = True


class ApprovalRequest(BaseModel):
    """Request for approval."""

    command: str
    mode: ExecutionMode
    cwd: str | None = None
    preview: str | None = None  # Patch preview for write ops


class ApprovalResponse(BaseModel):
    """Approval response."""

    approved: bool
    reason: str | None = None
