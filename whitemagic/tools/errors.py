"""Stable error codes for the Whitemagic tool contract.

Keep these codes stable across versions; add new codes instead of renaming.
"""

from __future__ import annotations


class ErrorCode:
    TOOL_NOT_FOUND = "tool_not_found"
    INVALID_PARAMS = "invalid_params"
    POLICY_BLOCKED = "policy_blocked"
    MISSING_DEPENDENCY = "missing_dependency"
    NOT_IMPLEMENTED = "not_implemented"
    NOT_FOUND = "not_found"
    TIMEOUT = "timeout"
    UNAUTHORIZED = "unauthorized"
    CONFLICT = "conflict"
    RATE_LIMIT = "rate_limit"
    DOWNSTREAM_ERROR = "downstream_error"
    INTERNAL_ERROR = "internal_error"
    # Dispatch pipeline gates
    INPUT_INVALID = "input_invalid"
    INPUT_REJECTED = "input_rejected"
    PERMISSION_DENIED = "permission_denied"
    RATE_LIMITED = "rate_limited"
    CIRCUIT_BREAKER_OPEN = "circuit_breaker_open"
    MATURITY_GATE = "maturity_gate"

