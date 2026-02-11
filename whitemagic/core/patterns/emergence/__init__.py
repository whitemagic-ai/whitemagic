"""Emergence - Self-modifying and creative features."""

from .guideline_evolution import GuidelineEvolution, example_self_reflection
from .novelty_detector import (
    EmergenceScorer,
    NoveltyDetector,
    NoveltyEvent,
    get_emergence_scorer,
    get_novelty_detector,
)

__all__ = [
    "GuidelineEvolution",
    "example_self_reflection",
    "NoveltyDetector",
    "NoveltyEvent",
    "EmergenceScorer",
    "get_novelty_detector",
    "get_emergence_scorer",
]
