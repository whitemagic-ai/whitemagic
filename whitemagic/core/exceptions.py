"""WhiteMagic custom exceptions.
"""

from typing import Any


class WhiteMagicError(Exception):
    """Base exception for all WhiteMagic errors."""

    pass


class MemoryNotFoundError(WhiteMagicError):
    """Raised when a requested memory does not exist."""

    def __init__(self, filename: str):
        self.filename = filename
        super().__init__(f"Memory '{filename}' not found")


class MemoryAlreadyExistsError(WhiteMagicError):
    """Raised when attempting to create a memory that already exists."""

    def __init__(self, filename: str):
        self.filename = filename
        super().__init__(f"Memory '{filename}' already exists")


class InvalidMemoryTypeError(WhiteMagicError):
    """Raised when an invalid memory type is specified."""

    def __init__(self, memory_type: str, valid_types: set):
        self.memory_type = memory_type
        self.valid_types = valid_types
        super().__init__(
            f"Invalid memory type '{memory_type}'. Must be one of: {', '.join(valid_types)}",
        )


class InvalidSortOptionError(WhiteMagicError):
    """Raised when an invalid sort option is specified."""

    def __init__(self, sort_by: str, valid_options: set):
        self.sort_by = sort_by
        self.valid_options = valid_options
        super().__init__(
            f"Invalid sort option '{sort_by}'. Must be one of: {', '.join(valid_options)}",
        )


class InvalidTierError(WhiteMagicError):
    """Raised when an invalid tier is specified."""

    def __init__(self, tier: int, valid_tiers: set):
        self.tier = tier
        self.valid_tiers = valid_tiers
        super().__init__(f"Invalid tier {tier}. Must be one of: {', '.join(map(str, valid_tiers))}")


class MemoryAlreadyArchivedError(WhiteMagicError):
    """Raised when attempting to archive a memory that's already archived."""

    def __init__(self, filename: str):
        self.filename = filename
        super().__init__(f"Memory '{filename}' is already archived")


class MemoryNotArchivedError(WhiteMagicError):
    """Raised when attempting to restore a memory that's not archived."""

    def __init__(self, filename: str):
        self.filename = filename
        super().__init__(f"Memory '{filename}' is not archived")


class FileOperationError(WhiteMagicError):
    """Raised when a file operation fails."""

    def __init__(self, operation: str, path: str, reason: str):
        self.operation = operation
        self.path = path
        self.reason = reason
        super().__init__(f"Failed to {operation} '{path}': {reason}")


class MetadataCorruptedError(WhiteMagicError):
    """Raised when metadata file is corrupted or invalid."""

    def __init__(self, path: str, reason: str):
        self.path = path
        self.reason = reason
        super().__init__(f"Metadata file '{path}' is corrupted: {reason}")


class ValidationError(WhiteMagicError):
    """Raised when input validation fails."""

    def __init__(self, field: str, value: Any, reason: str):
        self.field = field
        self.value = value
        self.reason = reason
        super().__init__(f"Validation failed for '{field}' = '{value}': {reason}")


# API-specific exceptions


class APIError(WhiteMagicError):
    """Base exception for API-related errors."""

    def __init__(self, message: str, status_code: int = 500):
        self.status_code = status_code
        super().__init__(message)


class AuthenticationError(APIError):
    """Raised when authentication fails."""

    def __init__(self, message: str = "Authentication failed"):
        super().__init__(message, status_code=401)


class AuthorizationError(APIError):
    """Raised when authorization fails (valid auth but insufficient permissions)."""

    def __init__(self, message: str = "Insufficient permissions"):
        super().__init__(message, status_code=403)


class RateLimitExceededError(APIError):
    """Raised when rate limit is exceeded."""

    def __init__(self, limit: int, window: str = "minute"):
        super().__init__(f"Rate limit exceeded: {limit} requests per {window}", status_code=429)


class QuotaExceededError(APIError):
    """Raised when storage or usage quota is exceeded."""

    def __init__(self, quota_type: str, limit: int):
        super().__init__(f"{quota_type} quota exceeded: {limit} limit reached", status_code=402)


class InvalidAPIKeyError(AuthenticationError):
    """Raised when API key is invalid or revoked."""

    def __init__(self) -> None:
        super().__init__("Invalid or revoked API key")


class APIKeyExpiredError(AuthenticationError):
    """Raised when API key has expired."""

    def __init__(self) -> None:
        super().__init__("API key has expired")
