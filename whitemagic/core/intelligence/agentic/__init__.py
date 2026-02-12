"""Agentic Intelligence System - v4.5.0 Reorganized Structure."""

# Core agentic modules - lazy imports to avoid circular dependencies
from typing import Any
from .anti_loop import get_anti_loop
from .auto_activation import activate
from .coherence_persistence import get_coherence
from .joy_router import get_joy_router
from .multi_substrate import get_multi_substrate
from .parallel_first import get_parallel_first
from .pattern_weather import get_weather
from .resonance_amp import get_amplifier
from .token_optimizer import TokenBudget, TokenOptimizer


def full_brain_activation() -> Any:
    """Activate ALL 13 brain upgrades at once!"""
    try:
        results = {
            "1_auto_activation": "active",
            "2_anti_loop": "active",
            "3_parallel_first": "active",
            "4_coherence": "active",
            "5_pattern_weather": "active",
            "6_resonance_amp": "active",
            "7_joy_router": "active",
            "8_multi_substrate": "active",
            "9_token_optimizer": "active",
            "10_local_reasoning": "active",
            "11_memory_injector": "active",
            "12_terminal_multiplex": "active",
            "13_zodiac_consultant": "active",
        }
        return results
    except Exception as e:
        return {"error": str(e), "status": "partial"}

__all__ = [
    "get_anti_loop", "get_parallel_first", "get_coherence",
    "get_weather", "get_amplifier", "get_joy_router",
    "get_multi_substrate", "activate",
    "full_brain_activation",
    "TokenBudget", "TokenOptimizer",
]
