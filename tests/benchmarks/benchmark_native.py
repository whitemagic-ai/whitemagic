
import time
import sys
import os

# Ensure we can import the rust module
sys.path.append(os.getcwd())

try:
    import whitemagic_rs
except ImportError:
    print("❌ Could not import whitemagic_rs. Run from project root.")
    sys.exit(1)

def benchmark():
    print("⚡ Benchmarking Native Rust Bridge ⚡")
    print("="*40)
    
    iterations = 1000
    
    # 1. Similarity
    start = time.time()
    for _ in range(iterations):
        whitemagic_rs.fast_similarity("The quick brown fox", "The slow red fox")
    end = time.time()
    
    avg_sim = (end - start) / iterations
    print(f"Fast Similarity Avg: {avg_sim*1000:.4f} ms") # Display in ms
    
    if avg_sim < 0.001:
        print("   ✅ Nanosecond/Microsecond Speed Achieved (<1ms)")
    else:
        print("   ⚠️  Speed Warning (>1ms)")

    # 2. Pattern Extraction
    content = ["def some_code(): pass"] * 10 
    start = time.time()
    # Provide a dummy threshold
    whitemagic_rs.extract_patterns_from_content(content, 0.5)
    end = time.time()
    
    duration = end - start
    print(f"Batch Pattern Extraction (10 lines): {duration*1000:.4f} ms")


if __name__ == "__main__":
    benchmark()
