"""Harmony Module - Yin-Yang Balance, Harmony Vector & Sustainable Productivity

Philosophy:
-----------
陰陽調和 (Yīn Yáng Tiáohé) - Yin Yang Harmony

Balance between action (Yang) and reflection (Yin) is essential for
sustainable productivity and creative insight.

Too much Yang → Burnout, mistakes, diminishing returns
Too much Yin → Analysis paralysis, stagnation
Optimal balance → Sustainable flow, deep work, creative breakthroughs

The Harmony Vector (inspired by MandalaOS's Lakshmi Subsystem) extends this
into a multi-dimensional health metric: balance, throughput, latency,
error_rate, dharma, karma_debt, and energy — all normalized to [0,1].
"""

from whitemagic.harmony.vector import (
    Guna,
    HarmonySnapshot,
    HarmonyVector,
    get_harmony_vector,
)
from whitemagic.harmony.compat import (
    BalanceMonitor,
    get_balance_monitor,
)
from whitemagic.harmony.yin_yang_tracker import (
    ActivityType,
    BalanceMetrics,
    YinYangBalanceTracker,
    get_tracker,
)

__all__ = [
    "ActivityType",
    "BalanceMonitor",
    "BalanceMetrics",
    "Guna",
    "HarmonySnapshot",
    "HarmonyVector",
    "YinYangBalanceTracker",
    "get_balance_monitor",
    "get_harmony_vector",
    "get_tracker",
]
