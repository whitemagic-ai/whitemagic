"""Minimal JSON-schema-ish validation for tool params.

We intentionally keep this lightweight to avoid introducing `jsonschema` as a
hard dependency. The goal is *predictable failures* for AI callers, not full
Draft-2020-12 compliance.
"""

from __future__ import annotations

from typing import Any


def _type_ok(expected: str, value: Any) -> bool:
    if expected == "string":
        return isinstance(value, str)
    if expected == "integer":
        return isinstance(value, int) and not isinstance(value, bool)
    if expected == "number":
        return isinstance(value, (int, float)) and not isinstance(value, bool)
    if expected == "boolean":
        return isinstance(value, bool)
    if expected == "array":
        return isinstance(value, list)
    if expected == "object":
        return isinstance(value, dict)
    return True  # unknown types: don't block


def validate_params(schema: dict[str, Any], params: dict[str, Any]) -> tuple[bool, str, dict[str, Any]]:
    """Validate tool params against a simple schema.

    Returns:
        (valid, reason, sanitized_params)

    """
    if not isinstance(params, dict):
        return False, "params must be an object", {}

    props: dict[str, Any] = schema.get("properties", {}) or {}
    required: list[str] = list(schema.get("required", []) or [])

    sanitized = {k: v for k, v in params.items() if v is not None}

    missing = [k for k in required if k not in sanitized]
    if missing:
        return False, f"missing required params: {', '.join(missing)}", {}

    # Type/enum checks for provided keys that exist in schema.
    for key, value in sanitized.items():
        if key not in props:
            # Additional properties allowed by default.
            continue
        spec = props.get(key) or {}
        expected_type = spec.get("type")
        if isinstance(expected_type, str) and not _type_ok(expected_type, value):
            return False, f"invalid type for '{key}': expected {expected_type}", {}
        enum = spec.get("enum")
        if isinstance(enum, list) and enum and value not in enum:
            return False, f"invalid value for '{key}': expected one of {enum}", {}

    return True, "ok", sanitized

