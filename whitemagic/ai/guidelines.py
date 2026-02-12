"""AI Guidelines compatibility shim"""

from typing import Any


def load_guidelines() -> dict[str, list[str]]:
    """Load AI guidelines"""
    return {
        "principles": [
            "do no harm",
            "respect privacy",
            "be helpful",
            "maintain transparency",
        ],
        "constraints": [
            "no harmful actions",
            "respect user consent",
            "protect sensitive data",
            "stay within capabilities",
        ],
    }

def validate_action(action: dict) -> bool:
    """Validate action against guidelines"""
    if not isinstance(action, dict):
        return False

    # Check for harmful actions
    if action.get("type") == "harmful_action":
        return False

    # Check for required fields
    if "content" not in action:
        return False

    # Default to allowed
    return True

def check_constraints(request: dict[str, Any]) -> dict[str, Any]:
    """Check request constraints"""
    # Basic constraint checking
    max_tokens = 4000
    max_temperature = 2.0

    allowed = True
    reason = ""

    if request.get("tokens", 0) > max_tokens:
        allowed = False
        reason = f"Token limit exceeded: {request.get('tokens')} > {max_tokens}"

    if request.get("temperature", 0) > max_temperature:
        allowed = False
        reason = f"Temperature too high: {request.get('temperature')} > {max_temperature}"

    return {
        "allowed": allowed,
        "reason": reason,
    }

# Re-export
__all__ = ["check_constraints", "load_guidelines", "validate_action"]
