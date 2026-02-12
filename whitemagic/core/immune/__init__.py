"""Immune System module initialization.
"""

from .antibodies import Antibody, AntibodyLibrary
from .detector import Threat, ThreatDetector, ThreatLevel, ThreatType
from .health_check import HealthCheck, HealthStatus, get_health_check
from .memory import ImmuneMemory, ImmuneMemoryRecord
from .pattern_immunity import ImmuneSystem, get_immune_system
from .response import ImmuneResponse, ResponseOutcome

__all__ = [
    "ImmuneSystem", "get_immune_system",
    "Threat", "ThreatLevel", "ThreatType", "ThreatDetector",
    "Antibody", "AntibodyLibrary",
    "ImmuneResponse", "ResponseOutcome",
    "HealthCheck", "HealthStatus", "get_health_check",
    "ImmuneMemory", "ImmuneMemoryRecord",
]
