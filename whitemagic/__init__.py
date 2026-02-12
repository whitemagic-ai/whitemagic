"""
WhiteMagic - Holographic Intelligence Core Package

Core Components:
- core: Essential infrastructure (memory, resonance, patterns, consciousness)
- gardens: 17 operational gardens
- rust: Performance-critical Rust implementations (SIMD, parallel search)
- interfaces: User-facing components (CLI, API, Dashboard)
- core.intelligence: AI/ML features (agentic, wisdom, learning)
- utils: Shared utilities
"""

import importlib
from importlib.metadata import PackageNotFoundError, version as _pkg_version
from pathlib import Path
from typing import Any

# Single source of truth for version:
# 1) Repo `VERSION` file (always fresh for source/editable checkouts)
# 2) Installed package metadata (works for pip installs / wheels)
# 3) "unknown" fallback
_version_file = Path(__file__).resolve().parent.parent / "VERSION"
if _version_file.exists():
    __version__ = _version_file.read_text().strip()
else:
    try:
        __version__ = _pkg_version("whitemagic")
    except PackageNotFoundError:
        __version__ = "unknown"

# Lazy imports to keep base import fast.
_LAZY_MODULES: dict[str, str] = {
    "utils": "whitemagic.utils",
    "memory": "whitemagic.core.memory",
    "resonance": "whitemagic.core.resonance",
    "patterns": "whitemagic.core.patterns",
    "consciousness": "whitemagic.core.consciousness",
    "gardens": "whitemagic.gardens",
    "optimization": "whitemagic.rust.optimization",
    "embeddings": "whitemagic.rust.embeddings",
    "data_lake": "whitemagic.rust.data_lake",
    "bindings": "whitemagic.rust.bindings",
    "cli": "whitemagic.interfaces.cli",
    "api": "whitemagic.interfaces.api",
    "dashboard": "whitemagic.interfaces.dashboard",
    "agentic": "whitemagic.core.intelligence.agentic",
    "wisdom": "whitemagic.core.intelligence.wisdom",
    "learning": "whitemagic.core.intelligence.learning",
    "immune": "whitemagic.core.immune",
    "automation": "whitemagic.automation",
    "monitoring": "whitemagic.core.monitoring",
    "integration": "whitemagic.integration",
    "edge": "whitemagic.edge",
    "parallel": "whitemagic.parallel",
    "autonomous": "whitemagic.autonomous",
}


def __getattr__(name: str) -> Any:
    if name == "MemoryManager":
        from whitemagic.core.memory.manager import MemoryManager
        globals()[name] = MemoryManager
        return MemoryManager

    module_path = _LAZY_MODULES.get(name)
    if module_path is None:
        raise AttributeError(f"module 'whitemagic' has no attribute '{name}'")

    try:
        module = importlib.import_module(module_path)
    except ImportError as exc:
        raise AttributeError(f"Optional module '{name}' unavailable: {exc}") from exc

    globals()[name] = module
    return module


def __dir__() -> list[str]:
    return sorted(list(globals().keys()) + list(_LAZY_MODULES.keys()) + ["MemoryManager"])

__all__ = [
    'memory', 'resonance', 'patterns', 'consciousness',
    'MemoryManager',  # Backward compatibility
    'gardens',
    'optimization', 'embeddings', 'data_lake', 'bindings',
    'cli', 'api', 'dashboard',
    'agentic', 'wisdom', 'learning',
    'immune', 'automation', 'monitoring',
    'integration',
    'edge', 'parallel', 'autonomous',
    'utils'
]
