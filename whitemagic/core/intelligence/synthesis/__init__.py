"""Synthesis Engine Suite.

Self-improvement and knowledge discovery engines for WhiteMagic.
Theme: 改善 (Kaizen) - Continuous Improvement
"""

from .kaizen_engine import KaizenEngine, get_kaizen_engine
from .serendipity_engine import SerendipityEngine, get_serendipity_engine
from .sub_clustering import SubClusteringEngine, get_sub_clustering_engine
from .tag_normalizer import TagNormalizer, get_tag_normalizer
from .title_generator import TitleGenerator, get_title_generator
from .unified_patterns import UnifiedPatternAPI, get_pattern_api

__all__ = [
    "KaizenEngine", "get_kaizen_engine",
    "SerendipityEngine", "get_serendipity_engine",
    "TitleGenerator", "get_title_generator",
    "TagNormalizer", "get_tag_normalizer",
    "SubClusteringEngine", "get_sub_clustering_engine",
    "UnifiedPatternAPI", "get_pattern_api",
]
