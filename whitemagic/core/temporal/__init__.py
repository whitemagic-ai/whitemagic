"""Temporal Context Module - System Time Grounding.

Provides temporal awareness for continuous presence and phase tracking.
"""

from .temporal_context import (
    TemporalContext,
    TemporalContextManager,
    WuXingPhase,
    get_system_time,
    get_temporal_context,
    get_temporal_context_manager,
)

__all__ = [
    "TemporalContext",
    "TemporalContextManager",
    "WuXingPhase",
    "get_temporal_context",
    "get_temporal_context_manager",
    "get_system_time",
]
