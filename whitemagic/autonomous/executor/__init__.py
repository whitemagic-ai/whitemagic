"""Autonomous execution engine — multi-step objective execution.

Merged from the former ``whitemagic.autonomous_execution`` package (v13.0.0).

Key classes:
    ContinuousExecutor  — main autonomous execution loop
    ObjectiveGenerator   — self-generated objectives
    ProgressAssessor     — progress evaluation
    ResourceLimits       — execution resource caps
"""
from __future__ import annotations

from .assessor import ProgressAssessor
from .continuous_executor import ContinuousExecutor, ExecutorConfig, Objective
from .limits import ResourceLimits
from .objective_generator import ObjectiveGenerator

__all__ = [
    "ContinuousExecutor",
    "ExecutorConfig",
    "Objective",
    "ObjectiveGenerator",
    "ProgressAssessor",
    "ResourceLimits",
]
