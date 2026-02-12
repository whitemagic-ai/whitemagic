"""Rate limiting and quota management for API."""

import asyncio
from datetime import datetime, timedelta, timezone
from typing import Any, cast

from sqlalchemy import select
from sqlalchemy.exc import IntegrityError
from sqlalchemy.ext.asyncio import AsyncSession

# Plan limits configuration
PLAN_LIMITS = {
    "free": {
        "rpm": 10,  # Requests per minute
        "daily": 100,  # Daily requests
        "monthly": 1000,  # Monthly requests
        "memories": 100,  # Maximum memories
        "storage_mb": 50,  # Storage in MB
    },
    "starter": {
        "rpm": 60,
        "daily": 1000,
        "monthly": 10000,
        "memories": 1000,
        "storage_mb": 500,
    },
    "pro": {
        "rpm": 300,
        "daily": 10000,
        "monthly": 100000,
        "memories": 10000,
        "storage_mb": 5000,
    },
    "enterprise": {
        "rpm": 1000,
        "daily": 50000,
        "monthly": 500000,
        "memories": 100000,
        "storage_mb": 50000,
    },
}


class RateLimitExceeded(Exception):
    """Rate limit exceeded exception."""

    def __init__(self, message: str, retry_after: int | None = None):
        self.message = message
        self.retry_after = retry_after
        super().__init__(message)


class RateLimiter:
    """Rate limiter with sliding window and burst capability."""

    def __init__(self, max_requests: int = 100, window_seconds: int = 60, burst_limit: int | None = None):
        self.max_requests = max_requests
        self.window_seconds = window_seconds
        self.burst_limit = burst_limit or int(max_requests * 1.5)  # Default 50% burst
        self.requests: dict[str, list] = {}
        self._lock = asyncio.Lock()

    async def check_rate_limit(self, user_id: str) -> bool:
        """Check if user is within rate limit with burst allowance."""
        async with self._lock:
            now = datetime.now()

            if user_id not in self.requests:
                self.requests[user_id] = []

            # Clean old requests
            cutoff = now - timedelta(seconds=self.window_seconds)
            self.requests[user_id] = [
                req_time for req_time in self.requests[user_id]
                if req_time > cutoff
            ]

            # Dynamic limit: allow burst if window is relatively empty
            # If recently used < 50% of window, allow up to burst_limit
            # Otherwise stay at max_requests
            current_usage = len(self.requests[user_id])
            effective_limit = self.max_requests

            # If usage in the last 10% of the window is low, allow burst
            burst_cutoff = now - timedelta(seconds=self.window_seconds * 0.1)
            recent_usage = len([t for t in self.requests[user_id] if t > burst_cutoff])

            if recent_usage < (self.max_requests * 0.1):
                effective_limit = self.burst_limit

            if current_usage >= effective_limit:
                return False

            # Record request
            self.requests[user_id].append(now)
            return True

    async def get_remaining(self, user_id: str) -> int:
        """Get remaining requests in current window."""
        async with self._lock:
            if user_id not in self.requests:
                return self.max_requests

            now = datetime.now()
            cutoff = now - timedelta(seconds=self.window_seconds)
            active_requests = [
                req_time for req_time in self.requests[user_id]
                if req_time > cutoff
            ]

            return max(0, self.max_requests - len(active_requests))


async def check_quota_limits(session: AsyncSession, user_id: int, plan_tier: str) -> dict:
    """Check user's quota against plan limits.

    Args:
        session: Database session
        user_id: User ID
        plan_tier: User's plan tier

    Returns:
        Dict with quota status and limits

    Raises:
        RateLimitExceeded: If quota exceeded

    """
    from .database import Quota
    quota_model = cast("Any", Quota)

    # Get user's quota
    result = await session.execute(
        select(quota_model).where(quota_model.user_id == user_id).with_for_update(),
    )
    quota = cast("Any", result.scalar_one_or_none())

    if not quota:
        # Create default quota
        quota = quota_model(user_id=user_id)
        session.add(quota)
        try:
            await session.flush()
        except IntegrityError:
            await session.rollback()
            result = await session.execute(
                select(quota_model).where(quota_model.user_id == user_id).with_for_update(),
            )
            quota = cast("Any", result.scalar_one())
        else:
            await session.commit()
            await session.refresh(quota)

    # Get plan limits
    limits = PLAN_LIMITS.get(plan_tier, PLAN_LIMITS["free"])

    # Check memory limit
    if quota.memories_count >= limits["memories"]:
        raise RateLimitExceeded(
            f"Memory limit reached ({limits['memories']} memories for {plan_tier} plan)",
        )

    # Check storage limit
    storage_limit_bytes = limits["storage_mb"] * 1024 * 1024
    if quota.storage_bytes >= storage_limit_bytes:
        raise RateLimitExceeded(
            f"Storage limit reached ({limits['storage_mb']}MB for {plan_tier} plan)",
        )

    # Check daily requests
    if quota.daily_requests >= limits["daily"]:
        raise RateLimitExceeded(
            f"Daily request limit reached ({limits['daily']} for {plan_tier} plan)",
            retry_after=86400,  # 24 hours
        )

    # Check monthly requests
    if quota.monthly_requests >= limits["monthly"]:
        raise RateLimitExceeded(
            f"Monthly request limit reached ({limits['monthly']} for {plan_tier} plan)",
            retry_after=86400 * 30,  # 30 days
        )

    return {
        "memories": {"used": quota.memories_count, "limit": limits["memories"]},
        "storage_mb": {"used": quota.storage_bytes / (1024 * 1024), "limit": limits["storage_mb"]},
        "daily": {"used": quota.daily_requests, "limit": limits["daily"]},
        "monthly": {"used": quota.monthly_requests, "limit": limits["monthly"]},
    }


async def update_quota_in_db(
    session: AsyncSession,
    user_id: int,
    memories_delta: int = 0,
    storage_delta: int = 0,
    request_count: int = 1,
) -> None:
    """Update user's quota in database.

    Args:
        session: Database session
        user_id: User ID
        memories_delta: Change in memory count (can be negative)
        storage_delta: Change in storage bytes (can be negative)
        request_count: Number of requests to add

    """
    from .database import Quota
    quota_model = cast("Any", Quota)

    # Get or create quota
    result = await session.execute(
        select(quota_model).where(quota_model.user_id == user_id).with_for_update(),
    )
    quota = cast("Any", result.scalar_one_or_none())

    if not quota:
        quota = quota_model(user_id=user_id)
        session.add(quota)
        try:
            await session.flush()
        except IntegrityError:
            await session.rollback()
            result = await session.execute(
                select(quota_model).where(quota_model.user_id == user_id).with_for_update(),
            )
            quota = cast("Any", result.scalar_one())

    # Update counters
    quota.memories_count = max(0, quota.memories_count + memories_delta)
    quota.storage_bytes = max(0, quota.storage_bytes + storage_delta)
    quota.daily_requests += request_count
    quota.monthly_requests += request_count
    quota.last_reset = datetime.now(timezone.utc)

    await session.commit()


def get_rate_limiter(plan_tier: str = "free") -> RateLimiter:
    """Get rate limiter configured for plan tier.

    Args:
        plan_tier: User's plan tier

    Returns:
        Configured RateLimiter instance

    """
    limits = PLAN_LIMITS.get(plan_tier, PLAN_LIMITS["free"])
    return RateLimiter(
        max_requests=limits["rpm"],
        window_seconds=60,
    )
