"""Authentication module for WhiteMagic

Provides:
- API key generation and validation
"""

from .api_keys import (
    APIKeyMetadata,
    APIKeySystem,
    api_key_system,
)

__all__ = [
    "APIKeyMetadata",
    "APIKeySystem",
    "api_key_system",
]
