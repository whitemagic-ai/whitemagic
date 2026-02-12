"""Rosetta Mode — Standard terminology aliasing for enterprise adoption.

When WM_TERMINOLOGY=standard, esoteric names are aliased to corporate-friendly
equivalents in CLI output, logs, and tool descriptions.

Usage:
    from whitemagic.rosetta import t

    # Returns "Tool Group" if WM_TERMINOLOGY=standard, else "Gana"
    print(f"Dispatching to {t('Gana')} handler")

    # Works in f-strings, log messages, CLI output, etc.
    logger.info(f"{t('Dream Cycle')} starting phase 3: {t('Serendipity')}")

The mapping is intentionally one-way (display only). Internal code always uses
esoteric names. Only user-facing strings pass through t().
"""

from __future__ import annotations

import os
from functools import lru_cache

# ---------------------------------------------------------------------------
# Terminology mapping: esoteric → standard
# ---------------------------------------------------------------------------

_ROSETTA: dict[str, str] = {
    # Core concepts
    "Gana": "Tool Group",
    "Ganas": "Tool Groups",
    "PRAT Router": "Meta-Tool Router",
    "PRAT": "Meta-Tool",
    "Prat": "Meta-Tool",

    # Governance & ethics
    "Dharma": "Ethics Policy",
    "Dharma Rules": "Ethics Policy",
    "dharma": "ethics",
    "Karma": "Side-Effect Audit",
    "Karma Ledger": "Side-Effect Audit Log",
    "karma": "side-effect audit",
    "karma_debt": "audit_debt",
    "Karma Debt": "Audit Debt",

    # Health & monitoring
    "Harmony Vector": "Health Metrics",
    "Harmony": "Health",
    "harmony": "health",
    "Guna": "Health Mode",
    "Sattva": "Balanced",
    "Rajas": "Active",
    "Tamas": "Degraded",

    # Memory & lifecycle
    "Galactic Map": "Memory Lifecycle",
    "galactic_map": "memory_lifecycle",
    "Galaxy": "Memory Scope",
    "galaxy": "memory scope",
    "Galactic Zone": "Lifecycle Zone",
    "CORE": "ACTIVE",
    "INNER_RIM": "RECENT",
    "MID_BAND": "STANDARD",
    "OUTER_RIM": "ARCHIVE",
    "FAR_EDGE": "DEEP_ARCHIVE",
    "Holographic": "Spatial",
    "holographic": "spatial",

    # Processes
    "Dream Cycle": "Memory Consolidation",
    "dream_cycle": "memory_consolidation",
    "Serendipity": "Discovery",
    "Kaizen": "Optimization",
    "Oracle": "Forecasting",

    # Communication & coordination
    "Gan Ying": "Event Bus",
    "gan_ying": "event_bus",
    "Sangha": "Agent Swarm",
    "sangha": "agent_swarm",

    # Documentation
    "Grimoire": "AI Orientation Guide",
    "grimoire": "orientation_guide",

    # Security
    "Edgerunner Violet": "Security Monitor",

    # Tool names (Gana prefixes → standard prefixes)
    "gana_ghost": "system_introspection",
    "gana_ox": "memory_operations",
    "gana_girl": "search_recall",
    "gana_room": "memory_maintenance",
    "gana_heart": "memory_creation",
    "gana_tail": "memory_lifecycle",
    "gana_basket": "knowledge_graph",
    "gana_wall": "governance",
    "gana_leg": "health_monitoring",
    "gana_roof": "ai_inference",
    "gana_house": "config_management",
    "gana_star": "meta_tools",
    "gana_fire": "batch_operations",
    "gana_well": "analytics",
    "gana_wing": "agent_coordination",
    "gana_net": "network_mesh",
    "gana_horn": "security",
    "gana_axe": "admin_operations",
    "gana_willow": "creative_tools",
    "gana_chariot": "workflow_automation",
    "gana_door": "gateway_tools",
    "gana_void": "system_reset",
    "gana_danger": "emergency_tools",
    "gana_earth": "grounding_tools",
    "gana_thunder": "performance_tools",
    "gana_wind": "discovery_tools",
    "gana_water": "embedding_tools",
    "gana_mountain": "archive_tools",
    "gana_bond": "relationship_tools",
}


@lru_cache(maxsize=1)
def _is_standard_mode() -> bool:
    """Check if standard terminology mode is enabled."""
    return os.environ.get("WM_TERMINOLOGY", "esoteric").lower() == "standard"


def t(term: str) -> str:
    """Translate an esoteric term to standard if WM_TERMINOLOGY=standard.

    If the term is not in the mapping or mode is esoteric, returns the
    original term unchanged.

    This function is designed to be used in f-strings and log messages:
        logger.info(f"Running {t('Dream Cycle')} phase 3")
    """
    if not _is_standard_mode():
        return term
    return _ROSETTA.get(term, term)


def rosetta_table() -> dict[str, str]:
    """Return the full terminology mapping (for documentation/introspection)."""
    return dict(_ROSETTA)


def current_mode() -> str:
    """Return the current terminology mode: 'esoteric' or 'standard'."""
    return "standard" if _is_standard_mode() else "esoteric"
