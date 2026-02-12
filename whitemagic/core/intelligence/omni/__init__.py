"""Omni-Tool Intelligence Package.
==============================

Contains the components for the Universal Router and Recursive Self-Improvement patterns.
"""

from .autonomy import AutonomyEngine, get_autonomy_engine
from .skill_forge import SkillForge, get_skill_forge
from .universal_router import UniversalRouter, get_universal_router

__all__ = [
    "get_universal_router",
    "UniversalRouter",
    "get_skill_forge",
    "SkillForge",
    "get_autonomy_engine",
    "AutonomyEngine",
]
