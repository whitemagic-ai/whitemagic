"""Backward-compatibility shim — canonical location is whitemagic.core.immune.security_integration."""
import warnings as _w
_w.warn(
    "whitemagic.immune.security_integration is deprecated; use whitemagic.core.immune.security_integration",
    DeprecationWarning,
    stacklevel=2,
)
from whitemagic.core.immune.security_integration import (  # noqa: F401,E402
    SecurityAntibody,
    SecurityAutoimmune,
    SecurityEvent,
    SecurityEventType,
    SecurityImmuneSystem,
    SecurityResponse,
    ThreatLevel,
    ThreatPattern,
    get_security_autoimmune,
    get_security_immune,
    report_threat,
)

__all__ = [
    "SecurityAntibody",
    "SecurityAutoimmune",
    "SecurityEvent",
    "SecurityEventType",
    "SecurityImmuneSystem",
    "SecurityResponse",
    "ThreatLevel",
    "ThreatPattern",
    "get_security_autoimmune",
    "get_security_immune",
    "report_threat",
]
