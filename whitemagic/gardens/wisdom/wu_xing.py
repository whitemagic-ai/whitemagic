"""Wu Xing - Compatibility Import.
================================

This module has been consolidated to intelligence/wisdom/wu_xing.py
This file maintains backwards compatibility for existing imports.

For new code, import directly from:
    from whitemagic.core.intelligence.wisdom.wu_xing import WuXingSystem, Element, get_wu_xing
"""

# Backwards-compatible imports
from whitemagic.core.intelligence.wisdom.wu_xing import (
    Element,
    WuXingSystem,
    check_wu_xing_balance,
    get_element_for_action,
    get_wu_xing,
    identify_task_element,
)

__all__ = [
    "Element",
    "WuXingSystem",
    "get_wu_xing",
    "identify_task_element",
    "check_wu_xing_balance",
    "get_element_for_action",
]
