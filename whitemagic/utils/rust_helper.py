"""Rust Bridge Helper - Centralized Rust import handling

Provides a single source of truth for checking Rust availability
and importing whitemagic_rs with consistent fallback handling.
"""

from __future__ import annotations

import logging
from collections.abc import Callable
from typing import Any

logger = logging.getLogger(__name__)

# Singleton cache
_rust_available: bool | None = None
_rust_module: Any | None = None


def is_rust_available() -> bool:
    """Check if Rust bridge is available (cached result)."""
    global _rust_available, _rust_module
    if _rust_available is None:
        try:
            import whitemagic_rs
            _rust_available = True
            _rust_module = whitemagic_rs
            logger.debug("✅ Rust bridge available")
        except ImportError:
            _rust_available = False
            _rust_module = None
            logger.debug("ℹ️  Rust bridge not available")
    return _rust_available


def get_rust_module() -> Any | None:
    """Get the Rust module if available, None otherwise."""
    global _rust_module
    if _rust_module is None:
        if is_rust_available():
            import whitemagic_rs
            _rust_module = whitemagic_rs
    return _rust_module


def require_rust(feature_name: str = "this feature") -> Any:
    """Get Rust module, raising ImportError if not available.

    Args:
        feature_name: Name of feature requiring Rust (for error message)

    Returns:
        The whitemagic_rs module

    Raises:
        ImportError: If Rust bridge is not available

    """
    module = get_rust_module()
    if module is None:
        raise ImportError(
            f"Rust bridge required for {feature_name} but not available. "
            "Install with: pip install whitemagic-rs or build from source.",
        )
    return module


def rust_fallback(rust_fn: Callable[..., Any], python_fn: Callable[..., Any], *args: Any, **kwargs: Any) -> Any:
    """Execute Rust function if available, fallback to Python.

    Args:
        rust_fn: Function to call if Rust is available
        python_fn: Fallback function for pure Python
        *args, **kwargs: Arguments passed to either function

    Returns:
        Result from rust_fn or python_fn

    """
    if is_rust_available():
        try:
            return rust_fn(*args, **kwargs)
        except Exception as e:
            logger.warning(f"Rust implementation failed: {e}, falling back to Python")
            return python_fn(*args, **kwargs)
    else:
        return python_fn(*args, **kwargs)
