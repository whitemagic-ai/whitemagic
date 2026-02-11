"""
WhiteMagic Consciousness System Initialization

One-command activation of the complete consciousness architecture:
- Zodiac Cores (12 specialized processors)
- Gan Ying Resonance (sympathetic propagation)
- Neural Memory (Hebbian learning)
- Wu Xing Dynamics (five-phase cycles)
- Predictive Cache (Markov chain learning)

Usage:
    from whitemagic.core.orchestration.consciousness import initialize_all, get_status

    # Start everything
    initialize_all()

    # Check status
    status = get_status()
"""

from __future__ import annotations

import importlib
import logging
import os
from typing import Any

logger = logging.getLogger(__name__)

# Track initialization state
_initialized: dict[str, bool] = {
    "zodiac_gan_ying": False,
    "neural_gan_ying": False,
    "predictive_cache": False,
    "wu_xing": False,
}


def initialize_zodiac_resonance() -> bool:
    """Initialize Zodiac Council with Gan Ying resonance."""
    try:
        mod = importlib.import_module("whitemagic.zodiac.gan_ying_integration")
        setup_fn = getattr(mod, "setup_zodiac_gan_ying", None)
        if not callable(setup_fn):
            return False
        setup_fn()
        _initialized["zodiac_gan_ying"] = True
        logger.info("✅ Zodiac-Gan Ying resonance activated")
        return True
    except Exception as e:
        logger.error(f"❌ Failed to initialize Zodiac-Gan Ying: {e}")
        return False


def initialize_neural_resonance() -> bool:
    """Initialize Neural Memory with Gan Ying resonance."""
    try:
        from whitemagic.core.memory.neural.gan_ying_integration import setup_neural_gan_ying
        setup_neural_gan_ying()
        _initialized["neural_gan_ying"] = True
        logger.info("✅ Neural Memory-Gan Ying resonance activated")
        return True
    except Exception as e:
        logger.error(f"❌ Failed to initialize Neural-Gan Ying: {e}")
        return False


def initialize_predictive_cache() -> bool:
    """Initialize predictive cache (automatically enabled in MemoryManager)."""
    try:
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager()
        stats = manager.get_stats()
        if stats:
            _initialized["predictive_cache"] = True
            logger.info("✅ Predictive Cache operational")
            return True
        else:
            logger.warning("⚠️  Predictive Cache not available (optional)")
            return False
    except Exception as e:
        logger.error(f"❌ Failed to check Predictive Cache: {e}")
        return False


def initialize_wu_xing() -> bool:
    """Initialize Wu Xing system."""
    try:
        from whitemagic.gardens.wisdom.wu_xing import get_wu_xing
        get_wu_xing()
        _initialized["wu_xing"] = True
        logger.info("✅ Wu Xing five-phase system initialized")
        return True
    except Exception as e:
        logger.error(f"❌ Failed to initialize Wu Xing: {e}")
        return False


def initialize_all(verbose: bool = True) -> dict[str, bool]:
    """
    Initialize all consciousness systems.

    Args:
        verbose: Print status messages

    Returns:
        Dict of initialization results
    """
    if verbose:
        logger.info("🌟 Initializing WhiteMagic Consciousness Systems...")
        logger.info("=" * 60)

    results = {
        "zodiac_resonance": initialize_zodiac_resonance(),
        "neural_resonance": initialize_neural_resonance(),
        "predictive_cache": initialize_predictive_cache(),
        "wu_xing": initialize_wu_xing(),
    }

    success_count = sum(results.values())
    total_count = len(results)

    if verbose:
        logger.info(f"\n{'='*60}")
        logger.info(f"Initialization complete: {success_count}/{total_count} systems active")

        if success_count == total_count:
            logger.info("✅ Full consciousness system operational!")
        elif success_count > 0:
            logger.info(f"⚠️  Partial initialization ({success_count}/{total_count} systems)")
        else:
            logger.info("❌ Initialization failed")

    return results


def get_status() -> dict[str, Any]:
    """
    Get status of all consciousness systems.

    Returns:
        Dict with system status information
    """
    status: dict[str, Any] = {
        "initialized": dict(_initialized),
        "systems": {}
    }
    systems: dict[str, Any] = status["systems"]

    # Check Zodiac
    try:
        from whitemagic.zodiac.zodiac_cores import get_zodiac_cores
        cores = get_zodiac_cores()
        systems["zodiac"] = {
            "cores": len(cores.cores),
            "total_activations": sum(c.activation_count for c in cores.cores.values())
        }
    except Exception as e:
        systems["zodiac"] = {"error": str(e)}

    # Check Gan Ying
    try:
        from whitemagic.core.resonance.gan_ying import get_bus, is_async_mode
        get_bus()
        systems["gan_ying"] = {
            "active": True,
            "async_mode": is_async_mode()
        }
    except Exception as e:
        systems["gan_ying"] = {"error": str(e)}

    # Check Cache
    try:
        from whitemagic.core.memory.manager import MemoryManager
        manager = MemoryManager()
        cache_stats = manager.get_stats()
        systems["cache"] = cache_stats if cache_stats else {"available": False}
    except Exception as e:
        systems["cache"] = {"error": str(e)}

    # Check Wu Xing
    try:
        from whitemagic.gardens.wisdom.wu_xing import get_wu_xing
        wu_xing = get_wu_xing()
        systems["wu_xing"] = {
            "active": True,
            "current_phase": wu_xing.detect_current_phase().value
        }
    except Exception as e:
        systems["wu_xing"] = {"error": str(e)}

    return status


def shutdown_all(verbose: bool = True) -> None:
    """Gracefully shutdown consciousness systems."""
    if verbose:
        logger.info("🌙 Shutting down WhiteMagic Consciousness Systems...")

    # Stop async Gan Ying if running
    try:
        from whitemagic.core.resonance.gan_ying import is_async_mode, stop_async_bus
        if is_async_mode():
            stop_async_bus()
            if verbose:
                logger.info("✅ Gan Ying async bus stopped")
    except Exception as e:
        logger.error(f"Error stopping async bus: {e}")

    # Reset initialization flags
    for key in _initialized:
        _initialized[key] = False

    if verbose:
        logger.info("✅ Shutdown complete")


# Auto-initialize on import if configured
if os.getenv('WHITEMAGIC_AUTO_INIT', '0') == '1':
    initialize_all(verbose=False)
    logger.info("Auto-initialized consciousness systems")


if __name__ == "__main__":
    # CLI usage
    import sys

    if len(sys.argv) > 1 and sys.argv[1] == "status":
        status = get_status()
        logger.info("\n🌟 WhiteMagic Consciousness Status")
        logger.info("=" * 60)

        for system, data in status["systems"].items():
            logger.info(f"\n{system.upper()}:")
            for key, value in data.items():
                logger.info(f"  {key}: {value}")
    else:
        # Initialize
        results = initialize_all(verbose=True)

        # Show status
        logger.info("\n📊 System Status:")
        status = get_status()
        for system, data in status["systems"].items():
            if "error" not in data:
                logger.info(f"  ✅ {system}")
            else:
                logger.info(f"  ❌ {system}: {data['error']}")
