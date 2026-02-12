import asyncio
import time
import random
import logging
from typing import List
from whitemagic.core.memory.unified import get_unified_memory, MemoryType
from whitemagic.core.resonance.gan_ying_enhanced import get_bus, EventType, ResonanceEvent
from datetime import datetime

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def generate_spectral_payload(size_kb: int = 1) -> str:
    """Generate a synthetic spectral data block."""
    return "".join(random.choice("ABCDEF0123456789") for _ in range(size_kb * 1024))

async def stress_test_indexing(count: int = 1000):
    """Stress test the indexing of spectral data."""
    um = get_unified_memory()
    logger.info(f"🚀 INITIATING HYPER-BENCHMARK: Indexing {count} spectral blocks...")
    
    start_time = time.perf_counter()
    
    for i in range(count):
        payload = generate_spectral_payload(random.randint(1, 4))
        um.store(
            title=f"SpectralPulse_{i}",
            content=payload,
            tags={"spectral", "stress_test", "benchmark"},
            importance=random.random(),
            memory_type=MemoryType.PATTERN
        )
        if i % 100 == 0:
            logger.info(f"Progress: {i}/{count}")
            
    end_time = time.perf_counter()
    total_time = end_time - start_time
    avg_per_mem = (total_time / count) * 1000
    avg_nanosec = (total_time / count) * 1_000_000_000
    
    logger.info(f"📊 BENCHMARK RESULTS (Indexing):")
    logger.info(f"   Total Time: {total_time:.4f}s")
    logger.info(f"   Average per memory: {avg_per_mem:.4f}ms")
    logger.info(f"   Average per memory: {avg_nanosec:,.0f}ns")
    
    return total_time

async def test_recall_performance(count: int = 100):
    """Test recall performance on indexed data."""
    um = get_unified_memory()
    logger.info(f"🔍 Testing recall performance for {count} random queries...")
    
    start_time = time.perf_counter()
    
    for _ in range(count):
        query = f"SpectralPulse_{random.randint(0, 999)}"
        um.search(query=query, limit=1)
        
    end_time = time.perf_counter()
    total_time = end_time - start_time
    avg_nanosec = (total_time / count) * 1_000_000_000
    
    logger.info(f"📊 BENCHMARK RESULTS (Recall):")
    logger.info(f"   Average Recall Latency: {avg_nanosec:,.0f}ns")

if __name__ == "__main__":
    async def run():
        # First clear/reinit if needed or just use current
        # For a clean benchmark, we might want a fresh DB, but let's test scale on existing.
        await stress_test_indexing(1000)
        await test_recall_performance(100)
        
    asyncio.run(run())
