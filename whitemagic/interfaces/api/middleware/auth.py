"""API authentication middleware.

Supports:
- API key authentication
- JWT tokens
- Optional public endpoints
"""

import hashlib
import os
import secrets

from fastapi import HTTPException, Security, status
from fastapi.security import APIKeyHeader, HTTPBearer

# API Key security
api_key_header = APIKeyHeader(name="X-API-Key", auto_error=False)
bearer_scheme = HTTPBearer(auto_error=False)

# Load valid API keys from environment (filter out empty strings)
VALID_API_KEYS = {k for k in os.getenv("WHITEMAGIC_API_KEYS", "").split(",") if k}

# Public endpoints that don't require authentication
PUBLIC_ENDPOINTS = {
    "/",
    "/health",
    "/docs",
    "/openapi.json",
    "/performance/status",
}


def hash_api_key(api_key: str) -> str:
    """Hash API key for secure comparison."""
    return hashlib.sha256(api_key.encode()).hexdigest()


async def validate_api_key(api_key: str | None = Security(api_key_header)) -> str:
    """Validate API key from header.

    Raises HTTPException if invalid.
    """
    if api_key is None:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="API key required. Include X-API-Key header.",
        )

    # Check if API key is valid
    if api_key not in VALID_API_KEYS and hash_api_key(api_key) not in VALID_API_KEYS:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Invalid API key",
        )

    return api_key


async def optional_api_key(
    api_key: str | None = Security(api_key_header),
) -> str | None:
    """Optional API key validation.

    Returns API key if provided and valid, None otherwise.
    """
    if api_key and (api_key in VALID_API_KEYS or hash_api_key(api_key) in VALID_API_KEYS):
        return api_key
    return None


def generate_api_key() -> str:
    """Generate a new API key."""
    return secrets.token_urlsafe(32)


# Rate limit overrides for authenticated users
AUTHENTICATED_RATE_LIMITS = {
    "/performance/audit": "50/minute",      # 5x increase
    "/performance/consolidate": "20/minute", # 4x increase
    "/api/memories": "200/minute",           # 4x increase
}
