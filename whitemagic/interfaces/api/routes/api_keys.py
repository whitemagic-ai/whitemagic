"""WhiteMagic API - API Key Management Routes.

Public endpoint for users to retrieve/generate their API key.
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, EmailStr
from sqlalchemy import select

from ..auth_unified import create_api_key
from ..database import User
from ..dependencies import DBSession

router = APIRouter(prefix="/api-keys", tags=["API Keys"])


class RetrieveKeyRequest(BaseModel):
    """Request to retrieve API key by email."""

    email: EmailStr


class RetrieveKeyResponse(BaseModel):
    """Response containing API key."""

    success: bool
    api_key: str
    message: str
    user_email: str
    plan_tier: str


@router.post("/retrieve", response_model=RetrieveKeyResponse)
async def retrieve_api_key(
    request: RetrieveKeyRequest,
    session: DBSession,
) -> RetrieveKeyResponse:
    """🚨 SECURITY WARNING: This endpoint is DISABLED for security reasons.

    VULNERABILITY: No authentication allows anyone with an email to generate API keys.
    IMPACT: Account takeover + remote code execution (when combined with exec API).

    PROPER FLOW NEEDED:
    1. User subscribes on Whop
    2. Webhook creates user account + sends signed token to user's email
    3. User clicks email link with single-use, time-limited token
    4. This endpoint validates token signature + expiry before generating key

    ROADMAP v2.1.7 (Security Enhancement):
    - Implement signed token generation in Whop webhook
    - Send token via email (requires email service)
    - Validate token signature + single-use + expiry here
    - Rate limit to 1 key generation per hour per user

    Current: Use Whop dashboard or manual admin provisioning.
    """
    raise HTTPException(
        503,
        detail="API key retrieval temporarily disabled for security hardening. "
        "Please contact support for manual key provisioning. "
        "ETA for secure self-service: v2.1.7 (December 2025)",
    )
    # Look up user by email
    result = await session.execute(select(User).where(User.email == request.email))
    user = result.scalar_one_or_none()

    if not user:
        raise HTTPException(
            404,
            detail="No account found with this email. Please subscribe via Whop first: https://whop.com/whitemagic",
        )

    # Generate a new API key
    raw_key, api_key = await create_api_key(
        session,
        user_id=user.id,
        name=f"Dashboard Key - {request.email}",
    )

    return RetrieveKeyResponse(
        success=True,
        api_key=raw_key,
        message="API key generated successfully! Save this key - it won't be shown again.",
        user_email=user.email,
        plan_tier=user.plan_tier,
    )
