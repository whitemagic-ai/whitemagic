"""Enhanced exec endpoint with audit logging and allowlist checks."""

import logging
import os
from typing import cast

from fastapi import APIRouter, HTTPException, Request

from whitemagic.security.tool_gating import get_tool_gate

from ...terminal import ExecutionMode, Profile, TerminalMCPTools
from ...terminal.allowlist import Allowlist
from ...terminal.approver import Approver
from ...terminal.models import ExecutionRequest, ExecutionResponse
from ..dependencies import CurrentUser
from ..exec_service import (
    check_user_allowlist,
    get_user_allowlist_patterns,
    get_user_key,
    log_terminal_execution,
)

logger = logging.getLogger(__name__)

# Commands that read file paths and need path validation
PATH_READING_COMMANDS = {"cat", "head", "tail", "less", "more", "stat", "grep", "git"}

FILE_EXTENSIONS = (
    ".py", ".txt", ".md", ".json", ".yaml", ".yml", ".toml", ".ini", ".cfg",
    ".csv", ".log", ".js", ".ts", ".rs", ".go", ".html", ".css",
)


def _looks_like_path(arg: str) -> bool:
    if arg.startswith(("./", "../", "~")):
        return True
    if "/" in arg:
        return True
    if arg.lower().endswith(FILE_EXTENSIONS):
        return True
    return False


def _validate_path_args(cmd: str, args: list[str]) -> tuple[bool, str]:
    """Validate file path arguments for read commands.

    Returns:
        (valid: bool, reason: str)

    """
    if cmd not in PATH_READING_COMMANDS:
        return True, "Command does not read paths"

    gate = get_tool_gate()

    # Special handling for grep: first non-flag arg is the pattern
    if cmd == "grep":
        non_flag_seen = False
        for arg in args:
            if arg.startswith("-"):
                continue
            if not non_flag_seen:
                non_flag_seen = True
                continue
            if _looks_like_path(arg):
                allowed, reason = gate.path_validator.is_path_allowed(arg)
                if not allowed:
                    return False, f"Path access denied for '{arg}': {reason}"
        return True, "All paths valid"

    # Special handling for git show/diff: validate only path-like args
    if cmd == "git":
        if not args:
            return True, "No git args"
        subcmd = args[0]
        if subcmd not in {"show", "diff"}:
            return True, "Git subcommand does not read paths"
        for arg in args[1:]:
            if arg.startswith("-"):
                continue
            if _looks_like_path(arg):
                allowed, reason = gate.path_validator.is_path_allowed(arg)
                if not allowed:
                    return False, f"Path access denied for '{arg}': {reason}"
        return True, "All paths valid"

    # Default: validate all non-flag args as paths
    for arg in args:
        if arg.startswith("-"):
            continue
        allowed, reason = gate.path_validator.is_path_allowed(arg)
        if not allowed:
            return False, f"Path access denied for '{arg}': {reason}"

    return True, "All paths valid"

router = APIRouter(prefix="/exec", tags=["Terminal"])

@router.post("/read", response_model=ExecutionResponse)
async def execute_read(request: ExecutionRequest, user: CurrentUser) -> ExecutionResponse:
    """Execute read-only command with audit logging."""
    if request.mode != ExecutionMode.READ:
        raise HTTPException(400, "Only READ mode allowed on this endpoint")

    user_id = getattr(user, "id", 0)
    user_key = get_user_key(user)
    require_allowlist = os.getenv("WHITEMAGIC_REQUIRE_ALLOWLIST", "").lower() in {"1", "true", "yes"}

    # Security: Validate file path arguments before execution
    path_valid, path_reason = _validate_path_args(request.cmd, request.args or [])
    if not path_valid:
        await log_terminal_execution(
            user_id=user_id,
            command=request.cmd,
            args=request.args,
            mode="read",
            cwd=request.cwd,
            exit_code=-1,
            duration_ms=0,
            approved=False,
            approved_by="path_validation_rejection",
        )
        raise HTTPException(403, path_reason)

    extra_patterns = await get_user_allowlist_patterns(user_id, user_key, "read")
    allowlist = Allowlist(Profile.PROD, extra_allowed=set(extra_patterns))

    # Check user allowlist
    allowed = await check_user_allowlist(
        user_id=user_id,
        user_key=user_key,
        command=request.cmd,
        args=request.args,
        mode="read",
        profile=Profile.PROD,
        require_explicit=require_allowlist,
    )
    if not allowed:
        # Log rejection
        await log_terminal_execution(
            user_id=user_id,
            command=request.cmd,
            args=request.args,
            mode="read",
            cwd=request.cwd,
            exit_code=-1,
            duration_ms=0,
            approved=False,
            approved_by="allowlist_rejection",
        )
        raise HTTPException(403, f"Command '{request.cmd}' not in user allowlist")

    tools = TerminalMCPTools(profile=Profile.PROD, allowlist=allowlist)
    result = tools.exec_read(
        cmd=request.cmd,
        args=request.args,
        cwd=request.cwd,
        env=request.env,
        stdin=request.stdin,
        correlation_id=request.correlation_id,
    )

    if "error" in result:
        raise HTTPException(403, result["error"])

    # Log successful execution
    await log_terminal_execution(
        user_id=user_id,
        command=request.cmd,
        args=request.args,
        mode="read",
        cwd=request.cwd,
        exit_code=result["exit_code"],
        duration_ms=result["duration_ms"],
        approved=True,
        approved_by="allowlist",
    )

    return ExecutionResponse(
        exit_code=result["exit_code"],
        stdout=result["stdout"],
        stderr=result["stderr"],
        duration_ms=result["duration_ms"],
        run_id=result["run_id"],
        command=result["command"],
        mode="read",
    )

@router.post("/", response_model=ExecutionResponse)
async def execute_command_full(
    payload: ExecutionRequest,
    http_request: Request,
    user: CurrentUser,
) -> ExecutionResponse:
    """Execute command with audit logging and allowlist checks."""
    user_id = getattr(user, "id", 0)
    user_key = get_user_key(user)
    require_allowlist = os.getenv("WHITEMAGIC_REQUIRE_ALLOWLIST", "").lower() in {"1", "true", "yes"}
    require_sandbox = os.getenv("WHITEMAGIC_EXEC_REQUIRE_SANDBOX", "").lower() in {"1", "true", "yes"}

    # For read-only, delegate
    if payload.mode == ExecutionMode.READ:
        read_response = await execute_read(payload, user)
        return cast("ExecutionResponse", read_response)

    # Write mode requires confirmation header AND allowlist
    if payload.mode == ExecutionMode.WRITE:
        confirmed = http_request.headers.get("X-Confirm-Write-Operation", "").lower() == "confirmed"
        if not confirmed:
            raise HTTPException(403, "Write operations require header X-Confirm-Write-Operation: confirmed")

        if require_sandbox and not os.getenv("WHITEMAGIC_EXEC_SANDBOX_CMD"):
            raise HTTPException(403, "Write operations require WHITEMAGIC_EXEC_SANDBOX_CMD")

        # Security: In local mode, require a local token to prevent any process from running commands
        deployment_mode = os.getenv("DEPLOYMENT_MODE", "local")
        if deployment_mode == "local":
             local_token = http_request.headers.get("X-Local-Token", "")
             expected_token = os.getenv("WHITEMAGIC_LOCAL_TOKEN", "")
             if not expected_token:
                 # If no token configured, fail safe (or warn massively, but fail safe is better)
                 # For now, let's assume we set a default in venv if not set, or fail.
                 # Let's log warning and FAIL.
                 logger.error("WHITEMAGIC_LOCAL_TOKEN not set in local mode. rejecting Exec write.")
                 raise HTTPException(403, "Local Write Access Requires WHITEMAGIC_LOCAL_TOKEN env var to be set and provided in X-Local-Token header.")

             if local_token != expected_token:
                 raise HTTPException(403, "Invalid Local Token")

        # Check user allowlist for write
        allowed = await check_user_allowlist(
            user_id=user_id,
            user_key=user_key,
            command=payload.cmd,
            args=payload.args,
            mode="write",
            profile=Profile.AGENT,
            require_explicit=require_allowlist,
        )
        if not allowed:
            # Log rejection
            await log_terminal_execution(
                user_id=user_id,
                command=payload.cmd,
                args=payload.args,
                mode="write",
                cwd=payload.cwd,
                exit_code=-1,
                duration_ms=0,
                approved=False,
                approved_by="allowlist_rejection",
            )
            raise HTTPException(403, f"Write command '{payload.cmd}' not in user allowlist")

        extra_patterns = await get_user_allowlist_patterns(user_id, user_key, "write")
        allowlist = Allowlist(Profile.AGENT, extra_allowed=set(extra_patterns))
        # Execute with restricted profile + no auto-approve
        tools = TerminalMCPTools(
            profile=Profile.PROD,
            approver=Approver(auto_approve=False),
            allowlist=allowlist,
        )
        result = await tools.execute_command(
            cmd=payload.cmd,
            args=payload.args,
            mode=payload.mode,
            cwd=payload.cwd,
            timeout_ms=payload.timeout_ms,
            env=payload.env,
            stdin=payload.stdin,
            correlation_id=payload.correlation_id,
        )

        # Log execution
        await log_terminal_execution(
            user_id=user_id,
            command=payload.cmd,
            args=payload.args,
            mode="write",
            cwd=payload.cwd,
            exit_code=result.get("exit_code", -1),
            duration_ms=result.get("duration_ms", 0),
            approved=result.get("success", False),
            approved_by="header+allowlist",
        )

        if not result.get("success"):
            error_msg = result.get("error", "Command execution failed")
            raise HTTPException(403 if "not allowed" in error_msg.lower() else 500, error_msg)

        return ExecutionResponse(
            exit_code=result["exit_code"],
            stdout=result["stdout"],
            stderr=result["stderr"],
            duration_ms=result["duration_ms"],
            run_id=result["run_id"],
            command=result["command"],
            mode="write",
            approved=True,
        )

    raise HTTPException(400, f"Unsupported execution mode: {payload.mode}")
