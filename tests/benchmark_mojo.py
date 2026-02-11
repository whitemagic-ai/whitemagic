#!/usr/bin/env python3
import time
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from whitemagic.optimization.polyglot_router import get_router, Backend

def benchmark():
    router = get_router()
    
    # 1. Prepare batch
    batch_size = 1000
    memories = []
    for i in range(batch_size):
        memories.append({
            "id": f"batch_{i}",
            "content": "This is a test memory for benchmarking holographic encoding speed." * 10,
            "title": f"Benchmark Memory {i}",
            "tags": ["benchmark", "test", "speed"],
            "importance": 0.7,
            "created_timestamp": int(time.time())
        })
    
    current_time = int(time.time())
    
    print(f"Benchmarking {batch_size} memories...")
    
    # 2. Benchmark Python (Native in router)
    start = time.time()
    # Force python by setting prefer_backend if router allows it, 
    # but here we'll just call the python implementation directly if possible 
    # or use the router's current state.
    
    # Actually, we can just use the FastBatchEncoder directly
    from whitemagic.core.intelligence.hologram.batch_encoder import FastBatchEncoder
    py_encoder = FastBatchEncoder()
    py_start = time.time()
    py_results = py_encoder.encode_batch(memories)
    py_end = time.time()
    py_time = (py_end - py_start) * 1000
    print(f"Python (optimized): {py_time:.2f}ms ({batch_size/((py_end-py_start) or 0.001):.0f}/sec)")
    
    # 3. Benchmark Mojo
    mojo_start = time.time()
    mojo_results = router.encode_holographic_batch(memories, current_time)
    mojo_end = time.time()
    mojo_time = (mojo_end - mojo_start) * 1000
    print(f"Mojo (via stdin): {mojo_time:.2f}ms ({batch_size/((mojo_end-mojo_start) or 0.001):.0f}/sec)")
    
    if py_time > 0:
        print(f"Mojo Speedup: {py_time/mojo_time:.2f}x")

if __name__ == "__main__":
    benchmark()
