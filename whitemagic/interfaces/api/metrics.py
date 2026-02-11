"""Metrics and observability endpoints."""

from datetime import datetime, timedelta, timezone
from typing import Any, cast

try:
    from sqlalchemy import func, select

    from .db_engine import get_async_session
    from .models import APIKey, Quota, UsageRecord, User
    from .models_exec import TerminalAuditLog
    HAS_DB = True
except ImportError:
    HAS_DB = False
    get_async_session = cast("Any", None)  # type: ignore[misc]
    UsageRecord = cast("Any", None)  # type: ignore[misc]
    User = cast("Any", None)  # type: ignore[misc]
    APIKey = cast("Any", None)  # type: ignore[misc]
    Quota = cast("Any", None)  # type: ignore[misc]
    TerminalAuditLog = cast("Any", None)  # type: ignore[misc]

async def get_system_metrics() -> dict[str, Any]:
    """Get system-wide metrics."""
    if not HAS_DB:
        return {"error": "Database not available"}

    try:
        user_model = cast("Any", User)
        api_key_model = cast("Any", APIKey)
        usage_model = cast("Any", UsageRecord)
        terminal_audit_model = cast("Any", TerminalAuditLog)
        async with get_async_session() as session:
            # Total users
            user_count = await session.execute(select(func.count(user_model.id)))
            total_users = user_count.scalar() or 0

            # Active API keys
            key_count = await session.execute(
                select(func.count(api_key_model.id)).where(api_key_model.revoked.is_(False)),
            )
            active_keys = key_count.scalar() or 0

            # Requests in last 24h
            since = datetime.now(timezone.utc) - timedelta(days=1)
            req_count = await session.execute(
                select(func.count(usage_model.id)).where(usage_model.created_at >= since),
            )
            requests_24h = req_count.scalar() or 0

            # Terminal executions in last 24h
            if HAS_DB:
                try:
                    exec_count = await session.execute(
                        select(func.count(terminal_audit_model.id))
                        .where(terminal_audit_model.created_at >= since),
                    )
                    exec_24h = exec_count.scalar() or 0
                except Exception:
                    exec_24h = 0
            else:
                exec_24h = 0

            return {
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "users": {
                    "total": total_users,
                    "active_keys": active_keys,
                },
                "requests": {
                    "last_24h": requests_24h,
                },
                "terminal": {
                    "executions_24h": exec_24h,
                },
            }
    except Exception as e:
        return {"error": str(e)}

async def get_user_metrics(user_id: int) -> dict[str, Any]:
    """Get metrics for specific user."""
    if not HAS_DB:
        return {"error": "Database not available"}

    try:
        quota_model = cast("Any", Quota)
        usage_model = cast("Any", UsageRecord)
        async with get_async_session() as session:
            # Get user quota
            quota_stmt = select(quota_model).where(quota_model.user_id == user_id)
            quota_result = await session.execute(quota_stmt)
            quota = cast("Any", quota_result.scalar_one_or_none())

            # Requests in last hour
            since_hour = datetime.now(timezone.utc) - timedelta(hours=1)
            req_count = await session.execute(
                select(func.count(usage_model.id))
                .where(usage_model.user_id == user_id)
                .where(usage_model.created_at >= since_hour),
            )
            requests_1h = req_count.scalar() or 0

            # Average response time
            avg_time = await session.execute(
                select(func.avg(usage_model.response_time_ms))
                .where(usage_model.user_id == user_id)
                .where(usage_model.created_at >= since_hour),
            )
            avg_response_ms = avg_time.scalar() or 0

            return {
                "user_id": user_id,
                "quota": {
                    "plan": quota.plan if quota else "free",
                    "requests_used": quota.requests_used if quota else 0,
                    "requests_limit": quota.requests_limit if quota else 1000,
                    "requests_remaining": (quota.requests_limit - quota.requests_used) if quota else 1000,
                },
                "usage": {
                    "requests_last_hour": requests_1h,
                    "avg_response_ms": round(avg_response_ms, 2),
                },
            }
    except Exception as e:
        return {"error": str(e)}
