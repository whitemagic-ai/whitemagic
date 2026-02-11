#!/usr/bin/env python3
"""
Layer 2 Verification Script: Parallel Magic
===========================================
Tests the "Parallel Magic" components:
1. whitemagic-rs (Rust bridge)
2. Mojo Bridge
3. AdaptiveParallelExecutor (Python)
"""

import sys
import os
import time
import asyncio
import logging
from pathlib import Path

# Setup paths to allow imports from staging
ROOT_DIR = Path("PROJECT_ROOT/staging/core_system/whitemagic")
sys.path.append(str(ROOT_DIR.parent))

logging.basicConfig(level=logging.INFO, format='%(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger("BridgeVerifier")

def test_rust_bridge():
    logger.info("--- Testing Rust Bridge (whitemagic-rs) ---")
    try:
        import whitemagic_rs
        logger.info("✅ whitemagic_rs imported successfully!")
        if hasattr(whitemagic_rs, 'SpatialIndex'):
             logger.info("✅ SpatialIndex class found.")
        else:
             logger.warning("⚠️ whitemagic_rs imported but SpatialIndex missing.")
    except ImportError as e:
        logger.warning(f"⚠️ whitemagic_rs import failed: {e}")
        logger.info("ℹ️ Note: This is expected if binaries are missing. Fallbacks should handle this.")

def test_mojo_bridge():
    logger.info("\n--- Testing Mojo Bridge ---")
    try:
        from whitemagic.core.intelligence.hologram.mojo_bridge import get_mojo_encoder
        encoder = get_mojo_encoder()
        logger.info(f"✅ MojoEncoderBridge initialized.")
        logger.info(f"   Mojo available: {encoder.mojo_available}")
        logger.info(f"   Mojo binary: {encoder.mojo_bin}")
        
        # Test encoding (will use fallback if mojo missing)
        dummy_mem = {"id": "test", "content": "Hello World", "tags": ["test"]}
        coords = encoder.encode(dummy_mem)
        logger.info(f"✅ Encoding test result: {coords}")
        
    except Exception as e:
        logger.error(f"❌ Mojo Bridge test failed: {e}")

async def test_parallel_executor():
    logger.info("\n--- Testing AdaptiveParallelExecutor ---")
    try:
        from whitemagic.cascade.advanced_parallel import AdaptiveParallelExecutor, ParallelTask, ParallelTier
        
        executor = AdaptiveParallelExecutor()
        logger.info("✅ AdaptiveParallelExecutor initialized.")
        
        # Define a simple task
        def dummy_task(x):
            time.sleep(0.01)
            return x * x
            
        tasks = [ParallelTask(id=str(i), func=dummy_task, args=(i,)) for i in range(10)]
        logger.info(f"   Created {len(tasks)} dummy tasks.")
        
        # Run them
        start = time.time()
        results = await executor.execute_parallel(tasks, tier=ParallelTier.TIER_0_TRIGRAMS)
        duration = time.time() - start
        
        logger.info(f"✅ Execution complete. Results: {results[:5]}...")
        logger.info(f"   Duration: {duration:.4f}s")
        
    except ImportError as e:
        logger.error(f"❌ Could not import AdaptiveParallelExecutor: {e}")
    except Exception as e:
        logger.error(f"❌ Parallel execution failed: {e}")

if __name__ == "__main__":
    test_rust_bridge()
    test_mojo_bridge()
    asyncio.run(test_parallel_executor())
