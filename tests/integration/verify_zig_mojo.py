
import sys
import os
import time

# Ensure we can import the router
sys.path.append(os.getcwd())

try:
    from whitemagic.optimization.polyglot_router import get_router, get_performance_stats, Backend
except ImportError as e:
    print(f"❌ Failed to import router: {e}")
    sys.exit(1)

def verify_all():
    router = get_router()
    print("\n🔍 Verifying Zig & Mojo Integration")
    print("====================================")
    
    # 1. Zig I Ching
    print("\n⚡ [Zig] Casting I Ching...")
    try:
        # Force Auto (should pick Zig)
        router.prefer_backend = Backend.AUTO
        cast = router.cast_iching()
        print(f"   Result: {cast}")
        print("   ✅ Zig cast successful")
    except Exception as e:
        print(f"   ❌ Zig cast failed: {e}")

    # 2. Mojo Holographic Encoding
    print("\n🔥 [Mojo] Holographic Encoding...")
    memory_data = {
        "id": "mem_001",
        "content": "The quick brown fox jumps over the lazy dog.",
        "tags": ["fox", "nature", "agility"],
        "importance": 0.8,
        "created_timestamp": int(time.time())
    }
    
    try:
        start_m = time.time()
        coords = router.encode_holographic(memory_data, int(time.time()))
        dur_m = (time.time() - start_m) * 1000
        print(f"   Coords: {coords}")
        print(f"   Time: {dur_m:.2f}ms")
        
        # Verify it wasn't Python fallback by checking speed or stats?
        # We'll check stats at the end.
        print("   ✅ Mojo encoding successful")
    except Exception as e:
        print(f"   ❌ Mojo encoding failed: {e}")

    # 3. Final Stats
    stats = get_performance_stats()
    print("\n📊 Final Performance Stats:")
    print(stats)
    
    # Validation
    if stats['calls']['zig'] > 0:
        print("   ✅ Zig confirmed active")
    else:
        print("   ⚠️  Zig call count is 0 (Fallback used?)")
        
    if stats['calls']['mojo'] > 0:
        print("   ✅ Mojo confirmed active")
    else:
        print("   ⚠️  Mojo call count is 0 (Fallback used?)")

if __name__ == "__main__":
    verify_all()
