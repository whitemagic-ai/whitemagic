"""WhiteMagic API - Dashboard Routes.

User dashboard endpoints for account management.
"""

from datetime import datetime
from typing import Any, cast

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from sqlalchemy import select

from ..auth_unified import create_api_key, revoke_api_key, rotate_api_key
from ..database import APIKey, Quota
from ..dependencies import CurrentUser, DBSession
from ..rate_limit import PLAN_LIMITS

router = APIRouter(prefix="/dashboard", tags=["Dashboard"])


# Request/Response Models


class CreateAPIKeyRequest(BaseModel):
    """Request to create a new API key."""

    name: str
    expires_days: int | None = None


class CreateAPIKeyResponse(BaseModel):
    """Response with new API key."""

    success: bool
    api_key: str  # Only shown once!
    key_id: str
    key_prefix: str
    name: str
    message: str


class APIKeyInfo(BaseModel):
    """API key information (without secret)."""

    id: str
    key_prefix: str
    name: str
    is_active: bool
    created_at: datetime
    last_used_at: datetime | None
    expires_at: datetime | None


class ListAPIKeysResponse(BaseModel):
    """Response with list of API keys."""

    success: bool
    keys: list[APIKeyInfo]
    total: int


class RevokeAPIKeyResponse(BaseModel):
    """Response after revoking API key."""

    success: bool
    message: str


class RotateAPIKeyResponse(BaseModel):
    """Response after rotating API key."""

    success: bool
    new_api_key: str  # Only shown once!
    key_id: str
    key_prefix: str
    message: str


class AccountInfo(BaseModel):
    """User account information."""

    email: str
    plan_tier: str
    whop_user_id: str | None
    has_subscription: bool
    created_at: datetime
    limits: dict


class UsageStatistics(BaseModel):
    """Usage statistics for current user."""

    requests_today: int
    requests_this_month: int
    memories_count: int
    storage_bytes: int
    storage_mb: float
    limits: dict
    usage_percent: dict


class AccountResponse(BaseModel):
    """Response with account info."""

    success: bool
    account: AccountInfo
    usage: UsageStatistics


# Endpoints


@router.post("/api-keys", response_model=CreateAPIKeyResponse)
async def create_new_api_key(
    request: CreateAPIKeyRequest,
    user: CurrentUser,
    session: DBSession,
) -> CreateAPIKeyResponse:
    """Create a new API key.

    The raw API key is only shown once and cannot be retrieved later.
    """
    user_obj = cast("Any", user)
    # Create the API key
    raw_key, api_key = await create_api_key(
        session,
        user_obj.id,
        name=request.name,
        expires_in_days=request.expires_days,
    )

    return CreateAPIKeyResponse(
        success=True,
        api_key=raw_key,  # Full key with prefix
        key_id=str(api_key.id),
        key_prefix=api_key.key_prefix,
        name=api_key.name,
        message="API key created successfully. Save it now - it won't be shown again!",
    )


@router.get("/api-keys", response_model=ListAPIKeysResponse)
async def list_api_keys(
    user: CurrentUser,
    session: DBSession,
) -> ListAPIKeysResponse:
    """List all API keys for current user.

    Returns metadata only (not the actual keys).
    """
    user_obj = cast("Any", user)
    api_key_model = cast("Any", APIKey)
    result = await session.execute(
        select(api_key_model)
        .where(api_key_model.user_id == user_obj.id)
        .order_by(api_key_model.created_at.desc()),
    )
    api_keys = result.scalars().all()

    keys_info = [
        APIKeyInfo(
            id=str(key.id),
            key_prefix=key.key_prefix,
            name=key.name,
            is_active=key.is_active,
            created_at=key.created_at,
            last_used_at=key.last_used_at,
            expires_at=key.expires_at,
        )
        for key in api_keys
    ]

    return ListAPIKeysResponse(
        success=True,
        keys=keys_info,
        total=len(keys_info),
    )


@router.delete("/api-keys/{key_id}", response_model=RevokeAPIKeyResponse)
async def revoke_key(
    key_id: str,
    user: CurrentUser,
    session: DBSession,
) -> RevokeAPIKeyResponse:
    """Revoke (deactivate) an API key.

    The key can no longer be used for authentication.
    """
    user_obj = cast("Any", user)
    api_key_model = cast("Any", APIKey)
    # Find the key
    result = await session.execute(
        select(api_key_model).where(
            api_key_model.id == key_id,
            api_key_model.user_id == user_obj.id,
        ),
    )
    api_key = result.scalar_one_or_none()

    if not api_key:
        raise HTTPException(404, "API key not found")

    # Revoke it
    await revoke_api_key(session, api_key.id)

    return RevokeAPIKeyResponse(
        success=True,
        message=f"API key '{api_key.name}' has been revoked",
    )


@router.post("/api-keys/{key_id}/rotate", response_model=RotateAPIKeyResponse)
async def rotate_key(
    key_id: str,
    user: CurrentUser,
    session: DBSession,
) -> RotateAPIKeyResponse:
    """Rotate an API key.

    Creates a new key and revokes the old one.
    The new raw key is only shown once.
    """
    user_obj = cast("Any", user)
    api_key_model = cast("Any", APIKey)
    # Find the key
    result = await session.execute(
        select(api_key_model).where(
            api_key_model.id == key_id,
            api_key_model.user_id == user_obj.id,
        ),
    )
    old_key = result.scalar_one_or_none()

    if not old_key:
        raise HTTPException(404, "API key not found")

    # Rotate it
    raw_key, new_key = await rotate_api_key(session, old_key.id)

    return RotateAPIKeyResponse(
        success=True,
        new_api_key=raw_key,
        key_id=str(new_key.id),
        key_prefix=new_key.key_prefix,
        message=f"API key rotated. Old key '{old_key.name}' revoked, new key created.",
    )


@router.get("/account", response_model=AccountResponse)
async def get_account_info(
    user: CurrentUser,
    session: DBSession,
) -> AccountResponse:
    """Get account information and usage statistics.

    Returns user details, plan limits, and current usage.
    """
    user_obj = cast("Any", user)
    quota_model = cast("Any", Quota)
    # Get or create quota
    result = await session.execute(select(quota_model).where(quota_model.user_id == user_obj.id))
    quota = cast("Any", result.scalar_one_or_none())

    if not quota:
        # Create initial quota
        quota = quota_model(user_id=user_obj.id)
        session.add(quota)
        await session.commit()
        await session.refresh(quota)

    # Get plan limits
    limits = PLAN_LIMITS.get(user_obj.plan_tier, PLAN_LIMITS["free"])

    # Calculate usage percentages
    usage_percent = {
        "requests_today": (
            (quota.requests_today / limits["daily"] * 100) if limits["daily"] > 0 else 0
        ),
        "requests_month": (
            (quota.requests_this_month / limits["monthly"] * 100) if limits["monthly"] > 0 else 0
        ),
        "memories": (
            (quota.memories_count / limits["memories"] * 100) if limits["memories"] > 0 else 0
        ),
        "storage": (
            (quota.storage_bytes / (limits["storage_mb"] * 1024 * 1024) * 100)
            if limits["storage_mb"] > 0
            else 0
        ),
    }

    account_info = AccountInfo(
        email=user_obj.email,
        plan_tier=user_obj.plan_tier,
        whop_user_id=user_obj.whop_user_id,
        has_subscription=user_obj.whop_membership_id is not None,
        created_at=user_obj.created_at,
        limits=limits,
    )

    usage_stats = UsageStatistics(
        requests_today=quota.requests_today,
        requests_this_month=quota.requests_this_month,
        memories_count=quota.memories_count,
        storage_bytes=quota.storage_bytes,
        storage_mb=round(quota.storage_bytes / (1024 * 1024), 2),
        limits=limits,
        usage_percent=usage_percent,
    )

    return AccountResponse(
        success=True,
        account=account_info,
        usage=usage_stats,
    )
