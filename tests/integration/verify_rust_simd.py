
import time
import sys
import os
import random
import string

# Ensure we can import the router and whitemagic_rs
sys.path.append(os.getcwd())

try:
    from whitemagic.optimization.polyglot_router import get_router, get_performance_stats
    import whitemagic_rs
    
    print("✅ whitemagic_rs imported successfully")
except ImportError as e:
    print(f"❌ Failed to import: {e}")
    sys.exit(1)

def generate_text(size_kb=100):
    chars = string.ascii_letters + string.digits + " "
    return "".join(random.choices(chars, k=size_kb * 1024))

def benchmark_similarity():
    print("\n🏎️  Benchmarking Similarity (Rust vs Python)...")
    
    text1 = generate_text(10) # 10KB
    text2 = generate_text(10) # 10KB (Unrelated)
    
    router = get_router()
    
    # Force Python
    router.prefer_backend = router.prefer_backend.PYTHON
    start_py = time.time()
    for _ in range(10):
        router.similarity(text1, text2)
    py_time = (time.time() - start_py) / 10
    print(f"🐍 Python Avg: {py_time*1000:.2f} ms")
    
    # Force Rust (via Auto/Rust preference)
    # The router defaults to Rust if available in AUTO mode
    router.prefer_backend = router.prefer_backend.AUTO 
    start_rs = time.time()
    for _ in range(10):
        # We need to make sure it actually hits Rust. 
        # The router logic prefers Rust if available.
        # But let's verify by calling the Rust impl directly to be sure for benchmark
        whitemagic_rs.rust_similarity(text1, text2)
    rs_time = (time.time() - start_rs) / 10
    print(f"🦀 Rust Avg:   {rs_time*1000:.2f} ms")
    
    speedup = py_time / rs_time if rs_time > 0 else 0
    print(f"🚀 Speedup: {speedup:.2f}x")

if __name__ == "__main__":
    benchmark_similarity()
    stats = get_performance_stats()
    print(f"\nFinal Router Stats: {stats}")
