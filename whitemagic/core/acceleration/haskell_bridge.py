"""Haskell Accelerator Bridge — BoundaryDetection + MaturityGate + RuleComposer.
==============================================================================
Extends the existing Haskell bridge (haskell/haskell_bridge.py) with Python
wrappers for the three new Haskell modules added in the polyglot expansion.

Each module provides formally verified, exhaustive pattern matching that
complements the Python implementations with type-safety guarantees.

Falls back to pure Python when the Haskell shared library is not available.

Usage:
    from whitemagic.core.acceleration.haskell_bridge import (
        haskell_check_boundaries, haskell_maturity_assess,
        haskell_evaluate_rules, haskell_bridge_status
    )
"""
from __future__ import annotations

import json
import logging
import threading
from typing import Any

logger = logging.getLogger(__name__)

_bridge = None
_bridge_lock = threading.Lock()
_HAS_HASKELL = False


def _get_bridge() -> Any:
    """Lazy-load the Haskell divination bridge singleton."""
    global _bridge, _HAS_HASKELL
    if _bridge is not None:
        return _bridge
    with _bridge_lock:
        if _bridge is not None:
            return _bridge
        try:
            from haskell.haskell_bridge import HaskellDivination
            _bridge = HaskellDivination()
            _HAS_HASKELL = True
            logger.info("Haskell bridge loaded for boundary/maturity/rules")
            return _bridge
        except Exception as e:
            logger.debug("Haskell bridge unavailable: %s", e)
            return None


# ---------------------------------------------------------------------------
# BoundaryDetection.hs — Injection/boundary violation detection
# ---------------------------------------------------------------------------

def haskell_check_boundaries(
    tool_name: str,
    description: str = "",
    args_str: str = "",
) -> list[dict[str, Any]] | None:
    """Check for boundary violations using Haskell's exhaustive pattern matching.

    Returns:
        List of violation dicts {name, severity, message, pattern}, or None.

    """
    bridge = _get_bridge()
    if bridge is None:
        return None

    try:
        lib = bridge._lib
        if not hasattr(lib, "c_check_boundaries"):
            return None

        result_ptr = lib.c_check_boundaries(
            tool_name.encode("utf-8"),
            description.encode("utf-8"),
            args_str.encode("utf-8"),
        )
        if result_ptr:
            result = json.loads(result_ptr.decode("utf-8"))
            return result if isinstance(result, list) else [result]
    except Exception as e:
        logger.debug("Haskell check_boundaries failed: %s", e)

    return None


def haskell_classify_content(
    tool_name: str,
    description: str = "",
) -> dict[str, Any] | None:
    """Classify tool content into safety categories using Haskell.

    Returns:
        Dict with {category, risk_level, keywords}, or None.

    """
    bridge = _get_bridge()
    if bridge is None:
        return None

    try:
        lib = bridge._lib
        if not hasattr(lib, "c_classify_content"):
            return None

        result_ptr = lib.c_classify_content(
            tool_name.encode("utf-8"),
            description.encode("utf-8"),
        )
        if result_ptr:
            parsed = json.loads(result_ptr.decode("utf-8"))
            if isinstance(parsed, dict):
                return parsed
    except Exception as e:
        logger.debug("Haskell classify_content failed: %s", e)

    return None


def haskell_detect_injection(input_text: str) -> list[dict[str, Any]] | None:
    """Detect injection patterns (path traversal, shell, prompt, SQL) using Haskell.

    Returns:
        List of violation dicts, or None.

    """
    bridge = _get_bridge()
    if bridge is None:
        return None

    try:
        lib = bridge._lib
        if not hasattr(lib, "c_detect_injection"):
            return None

        result_ptr = lib.c_detect_injection(input_text.encode("utf-8"))
        if result_ptr:
            parsed = json.loads(result_ptr.decode("utf-8"))
            if isinstance(parsed, list):
                return [item for item in parsed if isinstance(item, dict)]
    except Exception as e:
        logger.debug("Haskell detect_injection failed: %s", e)

    return None


# ---------------------------------------------------------------------------
# MaturityGate.hs — Algebraic state machine for staged capability unlocking
# ---------------------------------------------------------------------------

def haskell_maturity_assess(
    stage: int,
    tools_executed: int,
    session_count: int,
    dharma_score: float,
    harmony_score: float,
    consolidations: int = 0,
    agents_registered: int = 0,
    error_rate: float = 0.0,
) -> dict[str, Any] | None:
    """Assess maturity readiness using Haskell's algebraic state machine.

    Args:
        stage: Current maturity stage (0=Seed, 1=Bicameral, ..., 5=Logos).
        tools_executed: Total tools executed.
        session_count: Number of sessions.
        dharma_score: Ethical compliance (0-1).
        harmony_score: System health (0-1).
        consolidations: Memory consolidations performed.
        agents_registered: Number of agents.
        error_rate: Recent error frequency (0-1).

    Returns:
        Dict with {stage, can_advance, unmet_requirements, allowed_categories}, or None.

    """
    bridge = _get_bridge()
    if bridge is None:
        return None

    try:
        lib = bridge._lib
        if not hasattr(lib, "c_maturity_assess"):
            return None

        request = json.dumps({
            "stage": stage,
            "tools_executed": tools_executed,
            "session_count": session_count,
            "dharma_score": dharma_score,
            "harmony_score": harmony_score,
            "consolidations": consolidations,
            "agents_registered": agents_registered,
            "error_rate": error_rate,
        })
        result_ptr = lib.c_maturity_assess(request.encode("utf-8"))
        if result_ptr:
            parsed = json.loads(result_ptr.decode("utf-8"))
            if isinstance(parsed, dict):
                return parsed
    except Exception as e:
        logger.debug("Haskell maturity_assess failed: %s", e)

    return None


# ---------------------------------------------------------------------------
# RuleComposer.hs — Composable rule algebra with combinators
# ---------------------------------------------------------------------------

def haskell_evaluate_rules(
    tool_name: str,
    description: str = "",
    safety_level: str = "",
    category: str = "",
    profile: str = "default",
) -> dict[str, Any] | None:
    """Evaluate Dharma rules using Haskell's composable rule algebra.

    Returns:
        Dict with {rule_name, action, matched, priority}, or None.

    """
    bridge = _get_bridge()
    if bridge is None:
        return None

    try:
        lib = bridge._lib
        if not hasattr(lib, "c_evaluate_rules"):
            return None

        request = json.dumps({
            "tool_name": tool_name,
            "description": description,
            "safety_level": safety_level,
            "category": category,
            "profile": profile,
        })
        result_ptr = lib.c_evaluate_rules(request.encode("utf-8"))
        if result_ptr:
            parsed = json.loads(result_ptr.decode("utf-8"))
            if isinstance(parsed, dict):
                return parsed
    except Exception as e:
        logger.debug("Haskell evaluate_rules failed: %s", e)

    return None


def haskell_detect_rule_conflicts(
    profile: str = "default",
) -> list[dict[str, Any]] | None:
    """Detect conflicting rules in a profile using Haskell.

    Returns:
        List of conflict dicts {rule_a, rule_b, reason}, or None.

    """
    bridge = _get_bridge()
    if bridge is None:
        return None

    try:
        lib = bridge._lib
        if not hasattr(lib, "c_detect_conflicts"):
            return None

        result_ptr = lib.c_detect_conflicts(profile.encode("utf-8"))
        if result_ptr:
            parsed = json.loads(result_ptr.decode("utf-8"))
            if isinstance(parsed, list):
                return [item for item in parsed if isinstance(item, dict)]
    except Exception as e:
        logger.debug("Haskell detect_rule_conflicts failed: %s", e)

    return None


# ---------------------------------------------------------------------------
# Status
# ---------------------------------------------------------------------------

def haskell_bridge_status() -> dict[str, Any]:
    """Get Haskell bridge status for new modules."""
    _get_bridge()
    bridge = _bridge
    has_boundary = False
    has_maturity = False
    has_rules = False

    if bridge is not None:
        try:
            lib = bridge._lib
            has_boundary = hasattr(lib, "c_check_boundaries")
            has_maturity = hasattr(lib, "c_maturity_assess")
            has_rules = hasattr(lib, "c_evaluate_rules")
        except Exception:
            pass

    return {
        "has_haskell": _HAS_HASKELL,
        "modules": {
            "boundary_detection": has_boundary,
            "maturity_gate": has_maturity,
            "rule_composer": has_rules,
            "dharma_rules": _HAS_HASKELL,  # From original bridge
            "dep_graph": _HAS_HASKELL,
        },
        "backend": "haskell_ffi" if _HAS_HASKELL else "python_fallback",
    }
