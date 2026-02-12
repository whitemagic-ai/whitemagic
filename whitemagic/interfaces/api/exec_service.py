"""Service layer for exec endpoint with audit logging."""

import json
import os
import uuid
from pathlib import Path
from typing import Any, cast

from whitemagic.config.paths import WM_ROOT

try:
    from sqlalchemy import select

    from .db_engine import get_async_session
    from .models_exec import TerminalAuditLog, UserAllowlist
    HAS_DB = True
except ImportError:
    HAS_DB = False
    get_async_session = cast("Any", None)
    TerminalAuditLog = cast("Any", None)  # type: ignore[misc]
    UserAllowlist = cast("Any", None)  # type: ignore[misc]
    select = cast("Any", None)

from ..terminal.allowlist import Allowlist, Profile


async def log_terminal_execution(
    user_id: int,
    command: str,
    args: list,
    mode: str,
    cwd: str | None,
    exit_code: int | None,
    duration_ms: float,
    approved: bool,
    approved_by: str = "header",
) -> str:
    """Log terminal execution to audit database."""
    run_id = str(uuid.uuid4())

    if not HAS_DB:
        return run_id

    try:
        async with get_async_session() as session:
            log = TerminalAuditLog(
                user_id=user_id,
                run_id=run_id,
                command=command,
                args=json.dumps(args) if args else None,
                mode=mode,
                cwd=cwd,
                exit_code=exit_code,
                duration_ms=duration_ms,
                approved=1 if approved else 0,
                approved_by=approved_by,
            )
            session.add(log)
            await session.commit()
    except Exception as e:
        import logging
        logging.error(f"Failed to log terminal execution: {e}", exc_info=True)

    return run_id

def get_user_key(user: Any) -> str:
    """Resolve a stable user key for allowlist lookups."""
    for attr in ("id", "username", "email"):
        value = getattr(user, attr, None)
        if value:
            return str(value)
    return "anonymous"


def _allowlist_dir() -> Path:
    directory = Path(os.getenv("WHITEMAGIC_ALLOWLIST_DIR", "")).expanduser()
    if not directory.as_posix().strip():
        directory = WM_ROOT / "allowlists"
    directory.mkdir(parents=True, exist_ok=True)
    return directory


async def _read_allowlist_file(user_key: str) -> dict:
    # SECURITY FIX: Sanitize user_key to prevent path traversal
    import re
    safe_key = re.sub(r"[^a-zA-Z0-9_-]", "_", user_key)

    path = _allowlist_dir() / f"{safe_key}.json"
    if not path.exists():
        return {}
    try:
        raw = path.read_text(encoding="utf-8")
        data = json.loads(raw)
        return data if isinstance(data, dict) else {}
    except Exception:
        return {}


async def get_user_allowlist_patterns(
    user_id: int | None,
    user_key: str,
    mode: str,
) -> list[str]:
    """Load user allowlist patterns from DB and local file."""
    patterns: list[str] = []

    if HAS_DB and user_id:
        try:
            allowlist_model = cast("Any", UserAllowlist)
            select_fn = cast("Any", select)
            async with get_async_session() as session:
                stmt = (
                    select_fn(allowlist_model)
                    .where(allowlist_model.user_id == user_id)
                    .where(allowlist_model.mode == mode)
                    .where(allowlist_model.enabled == 1)
                )
                result = await session.execute(stmt)
                allowlist = result.scalars().all()
                patterns.extend([str(entry.command_pattern) for entry in allowlist])
        except Exception:
            pass

    file_data = await _read_allowlist_file(user_key)
    patterns.extend(file_data.get(mode, []))
    return patterns


async def check_user_allowlist(
    user_id: int | None,
    user_key: str,
    command: str,
    args: list[str] | None,
    mode: str,
    profile: Profile,
    require_explicit: bool = False,
) -> bool:
    """Check if user is allowed to run command."""
    patterns = await get_user_allowlist_patterns(user_id, user_key, mode)
    if require_explicit and not patterns:
        return False
    allowlist = Allowlist(profile, extra_allowed=set(patterns))
    return cast("bool", allowlist.is_allowed(command, args))

async def get_audit_logs(user_id: int, limit: int = 50) -> list[Any]:
    """Get recent audit logs for user."""
    if not HAS_DB:
        return []

    try:
        audit_model = cast("Any", TerminalAuditLog)
        select_fn = cast("Any", select)
        async with get_async_session() as session:
            stmt = (
                select_fn(audit_model)
                .where(audit_model.user_id == user_id)
                .order_by(audit_model.created_at.desc())
                .limit(limit)
            )
            result = await session.execute(stmt)
            return cast("list[Any]", result.scalars().all())
    except Exception:
        return []
