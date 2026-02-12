"""Rate limiting per endpoint."""

import time
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from typing import Any, cast

try:
    from sqlalchemy import select, update

    from .db_engine import get_async_session
    from .models import Quota
    HAS_DB = True
except ImportError:
    HAS_DB = False
    get_async_session = cast("Any", None)
    Quota = cast("Any", None)  # type: ignore[misc]

# In-memory rate limit tracker (fallback)
_request_counts: dict[tuple[int, str], list] = defaultdict(list)

async def check_rate_limit(user_id: int, endpoint: str) -> tuple[bool, str]:
    """Check if user has exceeded rate limit for endpoint.

    Returns:
        (allowed, reason) - (True, "") if allowed, (False, reason) if blocked

    """
    if not HAS_DB:
        # Fallback: Simple in-memory tracking (100 req/min per endpoint)
        now = time.time()
        key = (user_id, endpoint)
        _request_counts[key] = [t for t in _request_counts[key] if now - t < 60]

        if len(_request_counts[key]) >= 100:
            return (False, "Rate limit exceeded: 100 requests per minute")

        _request_counts[key].append(now)
        return (True, "")

    try:
        async with get_async_session() as session:
            # Get user quota
            stmt = select(Quota).where(Quota.user_id == user_id)
            result = await session.execute(stmt)
            quota = result.scalar_one_or_none()

            if not quota:
                return (False, "No quota found for user")

            # Check if quota exceeded
            if quota.requests_used >= quota.requests_limit:
                reset_in = quota.reset_at - datetime.now(timezone.utc)
                hours = int(reset_in.total_seconds() / 3600)
                return (False, f"Quota exceeded. Resets in {hours} hours.")

            # Increment usage
            quota.requests_used += 1
            await session.commit()

            return (True, "")
    except Exception as e:
        # Don't block on DB errors
        return (True, f"Rate limit check failed: {e}")

async def reset_quota(user_id: int) -> bool:
    """Reset user quota (admin function)."""
    if not HAS_DB:
        return False

    try:
        async with get_async_session() as session:
            stmt = (
                update(Quota)
                .where(Quota.user_id == user_id)
                .values(
                    requests_used=0,
                    tokens_used=0,
                    reset_at=datetime.now(timezone.utc) + timedelta(days=30),
                )
            )
            await session.execute(stmt)
            await session.commit()
            return True
    except Exception:
        return False
