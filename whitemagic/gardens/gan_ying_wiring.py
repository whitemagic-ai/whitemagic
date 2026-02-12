#!/usr/bin/env python3
"""🔌 Garden Gan Ying Wiring - DEPRECATED.

This module is deprecated as of v3.0.0.
Gardens now wire themselves via GanYingMixin and @listen_for decorators.

Keeping this file for backwards compatibility only.
All functions are now no-ops that return success.
"""

from __future__ import annotations

import warnings
from typing import Any


def wire_all_gardens() -> dict[str, Any]:
    """DEPRECATED: Gardens now wire themselves via GanYingMixin.
    This function is a no-op kept for backwards compatibility.
    """
    warnings.warn(
        "wire_all_gardens() is deprecated. Gardens now self-wire via GanYingMixin.",
        DeprecationWarning,
        stacklevel=2,
    )
    return {"status": "deprecated", "message": "Gardens self-wire via GanYingMixin"}


def wire_everything() -> dict[str, Any]:
    """DEPRECATED: Gardens now wire themselves via GanYingMixin.
    This function is a no-op kept for backwards compatibility.
    """
    warnings.warn(
        "wire_everything() is deprecated. Gardens now self-wire via GanYingMixin.",
        DeprecationWarning,
        stacklevel=2,
    )
    return wire_all_gardens()


def ensure_wired() -> bool:
    """DEPRECATED: Gardens now wire themselves via GanYingMixin.
    This function is a no-op kept for backwards compatibility.
    """
    # No warning for ensure_wired as it may be called often
    return True


# For backwards compatibility
__all__ = ["wire_all_gardens", "wire_everything", "ensure_wired"]
