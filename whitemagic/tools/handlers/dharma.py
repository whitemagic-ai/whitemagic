"""Dharma bridge tool handlers."""
from typing import Any, cast


def handle_evaluate_ethics(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.dharma import dharma_evaluate_ethics
    return cast("dict[str, Any]", dharma_evaluate_ethics(action={"tool": "evaluate_ethics", "args": kwargs}))


def handle_check_boundaries(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.dharma import dharma_check_boundaries
    return cast("dict[str, Any]", dharma_check_boundaries(action={"tool": "check_boundaries", "args": kwargs}))


def handle_verify_consent(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.dharma import dharma_verify_consent
    return cast("dict[str, Any]", dharma_verify_consent(action={"tool": "verify_consent", "args": kwargs}))


def handle_get_ethical_score(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.dharma import dharma_get_ethical_score
    return cast("dict[str, Any]", dharma_get_ethical_score(**kwargs))


def handle_get_dharma_guidance(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.dharma import dharma_get_guidance
    return cast("dict[str, Any]", dharma_get_guidance(**kwargs))


def handle_karma_report(**kwargs: Any) -> dict[str, Any]:
    """Return the Karma Ledger report (declared vs actual side-effects)."""
    from whitemagic.dharma.karma_ledger import get_karma_ledger
    limit = int(kwargs.get("limit", 100))
    return {"status": "success", "karma": get_karma_ledger().report(limit=limit)}


def handle_karmic_trace(**kwargs: Any) -> dict[str, Any]:
    """Return recent Karmic Trace entries from the Dharma Rules Engine."""
    from whitemagic.dharma.rules import get_rules_engine
    limit = int(kwargs.get("limit", 50))
    engine = get_rules_engine()
    return {"status": "success", "trace": engine.get_karmic_trace(limit=limit)}


def handle_dharma_rules(**kwargs: Any) -> dict[str, Any]:
    """List active Dharma rules and the current profile."""
    from whitemagic.dharma.rules import get_rules_engine
    engine = get_rules_engine()
    profile = kwargs.get("profile", None)
    return {
        "status": "success",
        "active_profile": engine.get_profile(),
        "rules": engine.get_rules(profile=profile),
    }


def handle_karma_verify_chain(**kwargs: Any) -> dict[str, Any]:
    """Verify the Merkle hash chain integrity of the Karma Ledger."""
    from whitemagic.dharma.karma_ledger import get_karma_ledger
    ledger = get_karma_ledger()
    return {"status": "success", **ledger.verify_chain()}


def handle_set_dharma_profile(**kwargs: Any) -> dict[str, Any]:
    """Switch the active Dharma profile (default, creative, secure)."""
    from whitemagic.dharma.rules import get_rules_engine
    profile = kwargs.get("profile", "default")
    engine = get_rules_engine()
    engine.set_profile(profile)
    return {
        "status": "success",
        "message": f"Dharma profile set to: {profile}",
        "active_profile": engine.get_profile(),
    }
