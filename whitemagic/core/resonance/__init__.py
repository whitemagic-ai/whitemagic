# Resonance system - Gan Ying Bus for event propagation
from .gan_ying import (
    CascadeTrigger,
    # Sync bus (default in legacy mode)
    EventType,
    GanYingBus,
    ResonanceEvent,
    disable_async_mode,
    emit_event,
    # Mode switching (v4.9.0)
    enable_async_mode,
    get_active_bus,
    get_bus,
    is_async_mode,
    listen_for,
    listen_to,
)
from .gan_ying_async import (
    AsyncEvent,
    # v4.9.0 Async bus
    AsyncGanYingBus,
    EventSubscriber,
    PerformanceMonitor,
    emit_async,
    get_async_bus,
    start_async_bus,
    stop_async_bus,
)
from .salience_arbiter import (
    SalienceArbiter,
    SalienceScore,
    SalienceScorer,
    get_salience_arbiter,
)
from .temporal_scheduler import (
    LaneConfig,
    TemporalConfig,
    TemporalLane,
    TemporalScheduler,
    classify_event,
    get_temporal_scheduler,
)

__all__ = [
    # Sync bus
    "EventType",
    "ResonanceEvent",
    "CascadeTrigger",
    "GanYingBus",
    "get_bus",
    "emit_event",
    "listen_for",
    "listen_to",
    # Async bus
    "AsyncGanYingBus",
    "AsyncEvent",
    "get_async_bus",
    "emit_async",
    "start_async_bus",
    "stop_async_bus",
    "EventSubscriber",
    "PerformanceMonitor",
    # Mode switching
    "enable_async_mode",
    "disable_async_mode",
    "is_async_mode",
    "get_active_bus",
    # Temporal scheduling (CyberBrains-inspired)
    "TemporalScheduler",
    "TemporalLane",
    "TemporalConfig",
    "LaneConfig",
    "get_temporal_scheduler",
    "classify_event",
    # Salience arbitration (Global Workspace)
    "SalienceArbiter",
    "SalienceScorer",
    "SalienceScore",
    "get_salience_arbiter",
]
