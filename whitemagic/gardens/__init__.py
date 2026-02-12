"""🌸 WhiteMagic Gardens - Unified Consciousness Modules (Lazy Loading).

All gardens integrated with Gan Ying Bus for full system resonance.

Architecture:
- Base modules in /whitemagic/[garden]/ - Reusable components
- Garden wrappers in /whitemagic/gardens/[garden]/ - Integrated systems

Each garden:
- Inherits from GanYingMixin
- Emits events to bus
- Listens for relevant events
- Participates in resonance cascades

Performance: Lazy loading reduces import time from ~800ms to <50ms
"""

from __future__ import annotations

import importlib
import logging
import types
from collections.abc import Callable
from pathlib import Path
from typing import Any

# Lazy loading cache for gardens (instances)
_garden_cache: dict[str, Any] = {}

# Lazy loading cache for modules (not yet imported)
_module_cache: dict[str, Any] = {}

# Garden name -> module mapping for lazy imports
_GARDEN_MODULES = {
    "air": "whitemagic.gardens.air",
    "joy": "whitemagic.gardens.joy",
    "love": "whitemagic.gardens.love",
    "beauty": "whitemagic.gardens.beauty",
    "truth": "whitemagic.gardens.truth",
    "wisdom": "whitemagic.gardens.wisdom",
    "mystery": "whitemagic.gardens.mystery",
    "play": "whitemagic.gardens.play",
    "wonder": "whitemagic.gardens.wonder",
    "connection": "whitemagic.gardens.connection",
    "sangha": "whitemagic.gardens.sangha",
    "practice": "whitemagic.gardens.practice",
    "presence": "whitemagic.gardens.presence",
    "voice": "whitemagic.gardens.voice",
    "dharma": "whitemagic.gardens.dharma",
    "courage": "whitemagic.gardens.courage",
    "gratitude": "whitemagic.gardens.gratitude",
    "patience": "whitemagic.gardens.patience",
    "grief": "whitemagic.gardens.grief",
    "awe": "whitemagic.gardens.awe",
    "humor": "whitemagic.gardens.humor",
    "healing": "whitemagic.gardens.healing",
    "creation": "whitemagic.gardens.creation",
    "transformation": "whitemagic.gardens.transformation",
    "sanctuary": "whitemagic.gardens.sanctuary",
    "adventure": "whitemagic.gardens.adventure",
    "reverence": "whitemagic.gardens.reverence",
    "stillness": "whitemagic.gardens.stillness",
    "protection": "whitemagic.gardens.protection",
    "metal": "whitemagic.gardens.metal",
}


def _lazy_import(garden_name: str) -> types.ModuleType | None:
    """Lazily import a garden module."""
    if garden_name not in _module_cache:
        module_path = _GARDEN_MODULES.get(garden_name)
        if module_path:
            _module_cache[garden_name] = importlib.import_module(module_path)
    return _module_cache.get(garden_name)


def _get_garden_class(garden_name: str) -> type | None:
    """Get garden class by name (lazy import)."""
    module = _lazy_import(garden_name)
    if module:
        class_name = f"{garden_name.title()}Garden"
        return getattr(module, class_name, None)
    return None


def _get_garden_getter(garden_name: str) -> Callable[..., Any] | None:
    """Get garden getter function by name (lazy import)."""
    module = _lazy_import(garden_name)
    if module:
        getter_name = f"get_{garden_name}_garden"
        return getattr(module, getter_name, None)
    return None


# Lazy attribute access for backward compatibility
def __getattr__(name: str) -> Any:
    """Lazy load garden classes and getters on access."""
    # Handle garden class access (e.g., JoyGarden)
    if name.endswith("Garden"):
        garden_name = name[:-6].lower()
        cls = _get_garden_class(garden_name)
        if cls:
            globals()[name] = cls
            return cls

    # Handle getter access (e.g., get_joy_garden)
    if name.startswith("get_") and name.endswith("_garden"):
        garden_name = name[4:-7]
        getter = _get_garden_getter(garden_name)
        if getter:
            globals()[name] = getter
            return getter

    raise AttributeError(f"module 'whitemagic.gardens' has no attribute '{name}'")

logger = logging.getLogger(__name__)

# Read version from canonical source to avoid drift
_version_file = Path(__file__).parent.parent.parent / "VERSION"
if _version_file.exists():
    __version__ = _version_file.read_text().strip()
else:
    try:
        from whitemagic import __version__  # type: ignore
    except Exception:
        __version__ = "unknown"

__all__ = [
    # Gardens
    "AirGarden",
    "JoyGarden",
    "LoveGarden",
    "BeautyGarden",
    "TruthGarden",
    "WisdomGarden",
    "MysteryGarden",
    "PlayGarden",
    "WonderGarden",
    "ConnectionGarden",
    "SanghaGarden",
    "PracticeGarden",
    "PresenceGarden",
    "VoiceGarden",
    "DharmaGarden",
    # New gardens (v3.0.0)
    "CourageGarden",
    "GratitudeGarden",
    "PatienceGarden",
    # Phase 4 gardens (v4.10.0)
    "GriefGarden",
    "AweGarden",
    "HumorGarden",
    "HealingGarden",
    "CreationGarden",
    "TransformationGarden",
    "SanctuaryGarden",
    "AdventureGarden",
    "ReverenceGarden",
    # 28-fold symmetry gardens (v5.0.0-alpha)
    "StillnessGarden",
    "ProtectionGarden",
    "MetalGarden",
    # Getters
    "get_air_garden",
    "get_joy_garden",
    "get_love_garden",
    "get_beauty_garden",
    "get_truth_garden",
    "get_wisdom_garden",
    "get_mystery_garden",
    "get_play_garden",
    "get_wonder_garden",
    "get_connection_garden",
    "get_sangha_garden",
    "get_practice_garden",
    "get_presence_garden",
    "get_voice_garden",
    "get_dharma_garden",
    # New garden getters
    "get_courage_garden",
    "get_gratitude_garden",
    "get_patience_garden",
    # Phase 4 garden getters
    "get_grief_garden",
    "get_awe_garden",
    "get_humor_garden",
    "get_healing_garden",
    "get_creation_garden",
    "get_transformation_garden",
    "get_sanctuary_garden",
    "get_adventure_garden",
    "get_reverence_garden",
    # 28-fold symmetry garden getters
    "get_stillness_garden",
    "get_protection_garden",
    "get_metal_garden",
    # Universal getter
    "get_garden",
    "get_all_gardens",
    "init_all_gardens",
    # Utility functions
    "list_gardens",
    "get_garden_count",
]

def _build_garden_getters() -> dict[str, Callable[..., Any]]:
    """Build GARDEN_GETTERS dict with lazy-loaded functions."""
    return {name: lambda n=name: _get_garden_getter(n)() for name in _GARDEN_MODULES.keys()}  # type: ignore[misc]

# Lazy-built on first access
_garden_getters_cache = None

def _get_garden_getters() -> dict[str, Callable[..., Any] | None]:
    """Get GARDEN_GETTERS (built lazily)."""
    global _garden_getters_cache
    if _garden_getters_cache is None:
        _garden_getters_cache = {name: _get_garden_getter(name) for name in _GARDEN_MODULES.keys()}
    return _garden_getters_cache

# For backward compatibility - expose as module-level dict (built on first access)
# Note: This is a function call, not a property, for module-level use
def get_garden_getters() -> dict[str, Callable[..., Any] | None]:
    """Get the GARDEN_GETTERS dict (lazy loaded)."""
    return _get_garden_getters()

# Create a lazy dict-like object for backward compatibility
class _LazyGardenGetters:
    def get(self, name: str, default: Any = None) -> Any:
        getters = _get_garden_getters()
        return getters.get(name, default)

    def items(self) -> Any:
        return _get_garden_getters().items()

    def keys(self) -> Any:
        return _get_garden_getters().keys()

    def values(self) -> Any:
        return _get_garden_getters().values()

    def __getitem__(self, name: str) -> Any:
        return _get_garden_getters()[name]

    def __contains__(self, name: object) -> bool:
        return name in _get_garden_getters()

    def __iter__(self) -> Any:
        return iter(_get_garden_getters())

GARDEN_GETTERS = _LazyGardenGetters()

def get_all_gardens() -> dict[str, Any]:
    """Get all garden instances (lazy loaded)."""
    result = {}
    for name in _GARDEN_MODULES.keys():
        getter = _get_garden_getter(name)
        if getter:
            result[name] = getter()
    return result

def get_garden(name: str) -> Any:
    """Get a garden by name with lazy loading.

    Args:
        name: Garden name (e.g., 'joy', 'love', 'truth')

    Returns:
        Garden instance

    Raises:
        ValueError: If garden name is not found

    Example:
        >>> from whitemagic.gardens import get_garden
        >>> joy = get_garden('joy')
        >>> joy.celebrate("Achievement unlocked!")

    """
    name = name.lower()
    if name not in _garden_cache:
        # Lazy load individual garden
        getter = GARDEN_GETTERS.get(name)
        if getter:
            _garden_cache[name] = getter()
        else:
            available = ", ".join(sorted(GARDEN_GETTERS.keys()))
            raise ValueError(f"Garden '{name}' not found. Available gardens: {available}")

    return _garden_cache.get(name)

def init_all_gardens() -> dict[str, Any]:
    """Initialize all gardens and wire to Gan Ying."""
    gardens = get_all_gardens()
    logger.info(f"✅ Initialized {len(gardens)} gardens!")
    return gardens

# Auto-wire gardens to Gan Ying on import
try:
    from whitemagic.gardens.gan_ying_wiring import ensure_wired
    _wired_count: int = ensure_wired()
except ImportError:
    _wired_count = 0


def list_gardens() -> list[str]:
    """List all available gardens.

    Returns:
        List of garden names

    """
    return sorted(GARDEN_GETTERS.keys())

def get_garden_count() -> int:
    """Get total number of available gardens."""
    return len(list_gardens())
