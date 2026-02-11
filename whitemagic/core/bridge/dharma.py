
from typing import Any


def dharma_evaluate_ethics(
    action: dict[str, Any],
    context: dict[str, Any] | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Evaluate an action against ethical principles."""
    from whitemagic.dharma import get_dharma_system

    dharma = get_dharma_system()
    score, concerns = dharma.evaluate_action(action)

    return {
        "ethical_score": score,
        "concerns": concerns,
        "action": action,
        "context": context or {},
        "timestamp": str(dharma.guidance_history[-1]["timestamp"]) if dharma.guidance_history else None,
    }


def dharma_check_boundaries(action: dict[str, Any], strict_mode: bool = False, **kwargs: Any) -> dict[str, Any]:
    """Check if an action violates any ethical boundaries."""
    from whitemagic.dharma import get_dharma_system

    dharma = get_dharma_system()
    violations = dharma.check_boundaries(action)

    return {
        "violations": [
            {
                "boundary_type": v.boundary_type,
                "severity": v.severity,
                "description": v.description,
                "suggested_action": v.suggested_action,
                "timestamp": str(v.timestamp),
            }
            for v in violations
        ],
        "has_violations": len(violations) > 0,
        "strict_mode": strict_mode,
        "action": action,
    }


def dharma_verify_consent(action: dict[str, Any], consent_type: str = "explicit", **kwargs: Any) -> dict[str, Any]:
    """Verify if proper consent has been obtained for an action."""
    # Check if action involves consent-requiring operations
    consent_keywords = ["personal", "private", "user", "data", "modify", "delete"]
    action_str = str(action).lower()
    requires_consent = any(keyword in action_str for keyword in consent_keywords)

    # For now, assume consent is granted if user_requested is in context
    consent_granted = action.get("user_requested", False) or action.get("consent_granted", False)

    return {
        "requires_consent": requires_consent,
        "consent_granted": consent_granted,
        "consent_type": consent_type,
        "action": action,
        "recommendation": "Proceed" if consent_granted or not requires_consent else "Request explicit consent",
    }


def dharma_get_guidance(situation: str, principles: list[str] | None = None, **kwargs: Any) -> dict[str, Any]:
    """Get ethical guidance for a situation."""
    from whitemagic.dharma import get_dharma_system

    dharma = get_dharma_system()
    guidance = dharma.get_guidance(situation)

    return {
        "situation": guidance["situation"],
        "relevant_principles": guidance["relevant_principles"],
        "advice": guidance["advice"],
        "confidence": guidance["confidence"],
        "timestamp": str(guidance["timestamp"]),
    }


def dharma_get_ethical_score(time_window_hours: int = 24, **kwargs: Any) -> dict[str, Any]:
    """Get overall ethical score based on recent system actions."""
    from whitemagic.dharma import get_dharma_system

    dharma = get_dharma_system()

    # Clear old violations first
    dharma.clear_violations(older_than_hours=time_window_hours)

    score = dharma.get_ethical_score()

    return {
        "ethical_score": score,
        "time_window_hours": time_window_hours,
        "recent_violations": len(dharma.violations),
        "status": "excellent" if score > 0.9 else "good" if score > 0.7 else "concerning" if score > 0.5 else "violation",
    }


def dharma_list_principles(level: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """List all ethical principles with their descriptions and weights."""
    from whitemagic.dharma import get_dharma_system

    dharma = get_dharma_system()

    principles = []
    for key, principle in dharma.principles.items():
        if level is None or principle.level.value == level:
            principles.append({
                "key": key,
                "name": principle.name,
                "level": principle.level.value,
                "description": principle.description,
                "weight": principle.weight,
            })

    return {
        "principles": principles,
        "total_count": len(principles),
        "filter_level": level,
    }
