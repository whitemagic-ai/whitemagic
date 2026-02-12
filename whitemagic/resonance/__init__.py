"""Backward-compatibility shim — canonical location is whitemagic.core.resonance."""
from __future__ import annotations

import sys
import warnings as _w

_w.warn(
    "whitemagic.resonance is deprecated; use whitemagic.core.resonance",
    DeprecationWarning,
    stacklevel=2,
)

from whitemagic.core.resonance.gan_ying import GanYingBus as GanYingBus, ResonanceEvent as ResonanceEvent, EventType as EventType  # noqa: E402,F401
from whitemagic.core.resonance.gan_ying_enhanced import get_bus as get_event_bus  # noqa: E402,F401
from whitemagic.core.resonance import (  # noqa: E402
    adapters,
    cascade_protocols,
    emergence_tuned,
    gan_ying,
    gan_ying_enhanced,
    integration_helpers,
)

sys.modules.setdefault(__name__ + ".adapters", adapters)
sys.modules.setdefault(__name__ + ".cascade_protocols", cascade_protocols)
sys.modules.setdefault(__name__ + ".emergence_tuned", emergence_tuned)
sys.modules.setdefault(__name__ + ".gan_ying", gan_ying)
sys.modules.setdefault(__name__ + ".gan_ying_enhanced", gan_ying_enhanced)
sys.modules.setdefault(__name__ + ".integration_helpers", integration_helpers)

__all__ = [
    "GanYingBus",
    "ResonanceEvent",
    "EventType",
    "get_event_bus",
    "adapters",
    "cascade_protocols",
    "emergence_tuned",
    "gan_ying",
    "gan_ying_enhanced",
    "integration_helpers",
]
