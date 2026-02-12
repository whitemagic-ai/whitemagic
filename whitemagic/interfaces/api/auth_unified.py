"""Unified authentication - auto-selects real DB, JWT, or dev mode."""

import hashlib
import os
import secrets
from datetime import datetime, timezone
from typing import Any, cast

from whitemagic.core.exceptions import AuthenticationError, InvalidAPIKeyError
from whitemagic.logging_config import get_logger

try:
    from .auth_jwt import verify_token as _verify_token  # type: ignore[import-not-found]
except ImportError:  # pragma: no cover - optional JWT module
    _verify_token = None


def verify_token(token: str) -> dict[str, Any]:
    """Validate JWT token via optional auth_jwt module."""
    if _verify_token is None:
        raise AuthenticationError("JWT auth module unavailable")
    return cast("dict[str, Any]", _verify_token(token))

logger = get_logger(__name__)

# Try to import real auth dependencies
try:
    from sqlalchemy import select

    from .models import APIKey, User
    HAS_DB = True
except ImportError:
    HAS_DB = False
    select = cast("Any", None)
    APIKey = cast("Any", None)  # type: ignore[misc]
    User = cast("Any", None)  # type: ignore[misc]

# Dev mode flags
DEV_MODE = os.getenv("WHITEMAGIC_DEV_MODE", "0") == "1"
INSECURE_AUTH = os.getenv("WHITEMAGIC_INSECURE_DEV_AUTH", "0") == "1"

async def validate_auth(session: Any, token_or_key: str, update_last_used: bool = True) -> tuple[Any, Any] | None:
    """Validate either a JWT token or a traditional API key.

    Priority:
    1. JWT Token (if starts with 'eyJ')
    2. API Key (database check)
    3. Dev Mode bypass (if enabled)
    """
    if not token_or_key:
        raise AuthenticationError("No credentials provided")

    # 1. JWT Token validation
    if token_or_key.startswith("eyJ"):
        try:
            payload = verify_token(token_or_key)
            # If database is available, fetch the user
            if HAS_DB and session and "user_id" in payload:
                user_model = cast("Any", User)
                select_fn = cast("Any", select)
                stmt = (
                    select_fn(user_model)
                    .where(user_model.id == payload["user_id"])
                    .where(user_model.is_active.is_(True))
                )
                result = await session.execute(stmt)
                user = result.scalar_one_or_none()
                if user:
                    return (user, None) # No APIKey object for pure JWT sessions
            # Fallback to stub user if payload is valid but no DB user found
            return ({"username": payload.get("sub", "jwt_user"), "id": payload.get("user_id")}, None)
        except Exception as e:
            raise AuthenticationError(str(e))

    # 2. Traditional API Key (database)
    if HAS_DB and session:
        key_hash = hashlib.sha256(token_or_key.encode()).hexdigest()
        api_key_model = cast("Any", APIKey)
        user_model = cast("Any", User)
        select_fn = cast("Any", select)

        stmt = (
            select_fn(api_key_model, user_model)
            .join(user_model, api_key_model.user_id == user_model.id)
            .where(api_key_model.key_hash == key_hash)
            .where(api_key_model.revoked.is_(False))
            .where(user_model.is_active.is_(True))
        )

        result = await session.execute(stmt)
        row = result.one_or_none()

        if row:
            api_key_obj, user = row
            if update_last_used:
                api_key_obj.last_used = datetime.now(timezone.utc)
                await session.commit()
            return (user, api_key_obj)

    # 3. Dev mode bypass (explicit opt-in)
    if DEV_MODE and INSECURE_AUTH and len(token_or_key) > 10:
        logger.warning("⚠️  INSECURE DEV AUTH BYPASS USED")
        return ({"username": "dev_user", "id": 1}, {"id": 1, "key_hash": "dev_stub"})

    raise InvalidAPIKeyError()

# --- API Key Management (Consolidated) ---

def generate_api_key() -> str:
    """Generate a high-entropy API key."""
    return f"wm-{secrets.token_urlsafe(32)}"

def hash_api_key(api_key: str) -> str:
    """Hash an API key for storage."""
    return hashlib.sha256(api_key.encode()).hexdigest()

async def create_api_key(session: Any, user_id: int, name: str = "New Key", expires_in_days: int | None = None) -> tuple[str, Any]:
    """Create and store a new API key."""
    raw_key = generate_api_key()
    key_hash = hash_api_key(raw_key)

    if HAS_DB and session:
        # DB implementation
        api_key_model = cast("Any", APIKey)
        new_key = api_key_model(
            user_id=user_id,
            key_hash=key_hash,
            name=name,
            revoked=False,
        )
        session.add(new_key)
        await session.commit()
        await session.refresh(new_key)

        # Add key_prefix attribute if model supports it (backward compat)
        if not hasattr(new_key, "key_prefix"):
            setattr(new_key, "key_prefix", raw_key[:8] + "...")

        return raw_key, new_key

    # Non-DB fallback (stub)
    from .database import APIKey as StubKey
    stub = StubKey(key=key_hash, user_id=user_id)
    return raw_key, stub

async def revoke_api_key(session: Any, key_id: Any) -> bool:
    """Revoke an API key."""
    if HAS_DB and session:
        api_key_model = cast("Any", APIKey)
        select_fn = cast("Any", select)
        stmt = select_fn(api_key_model).where(api_key_model.id == key_id)
        result = await session.execute(stmt)
        key = result.scalar_one_or_none()
        if key:
            key.revoked = True
            key.revoked_at = datetime.now(timezone.utc)
            await session.commit()
            return True
        return False
    return True

async def rotate_api_key(session: Any, key_id: Any) -> tuple[str, Any]:
    """Rotate an API key: revoke old and create new."""
    if HAS_DB and session:
        api_key_model = cast("Any", APIKey)
        select_fn = cast("Any", select)
        stmt = select_fn(api_key_model).where(api_key_model.id == key_id)
        result = await session.execute(stmt)
        old_key = result.scalar_one_or_none()
        if not old_key:
            raise AuthenticationError("Original key not found")

        # Revoke old
        old_key.revoked = True
        old_key.revoked_at = datetime.now(timezone.utc)

        # Create new
        raw_key, new_key = await create_api_key(
            session,
            user_id=old_key.user_id,
            name=f"Rotated: {old_key.name}",
        )
        return raw_key, new_key

    # Stub fallback
    return generate_api_key(), None

# Re-export for compatibility
validate_api_key = validate_auth
__all__ = [
    "validate_auth",
    "validate_api_key",
    "create_api_key",
    "revoke_api_key",
    "rotate_api_key",
    "generate_api_key",
    "hash_api_key",
    "AuthenticationError",
    "DEV_MODE",
]
