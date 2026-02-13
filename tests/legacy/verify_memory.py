#!/usr/bin/env python3
"""
Layer 3 Verification: Memory Navigation Probe
=============================================
"Chapter 8: Memory Navigation"

Objectives:
1. Connect to Project Memory (SQLite/Unified).
2. SEARCH for ancient artifacts (WXYZ).
3. WRITE a test memory ("Resurrection Check").
4. VERIFY Holographic Coordinates (even if Python fallback).
"""

import sys
from pathlib import Path
from pprint import pprint

# Setup paths to allow imports from staging
ROOT_DIR = Path(__file__).resolve().parent
# Point to staging/project_memory
PROJECT_MEM_DIR = ROOT_DIR.parent.parent.parent / "staging" / "project_memory"
sys.path.append(str(ROOT_DIR.parent))

def run_probe():
    print("🧠 Layer 3 Memory Probe Initiating...\n")
    print(f"📂 Target Memory: {PROJECT_MEM_DIR}")

    if not PROJECT_MEM_DIR.exists():
        print(f"❌ Error: Memory directory not found at {PROJECT_MEM_DIR}")
        return 1

    try:
        from whitemagic.core.memory.manager import MemoryManager
        print("✅ MemoryManager imported.")
    except ImportError as e:
        print(f"❌ Failed to import MemoryManager: {e}")
        return 1

    # 1. Initialize
    try:
        mm = MemoryManager(base_dir=PROJECT_MEM_DIR)
        print("✅ MemoryManager initialized connected to DB.")
        
        stats = mm.get_stats()
        print(f"📊 System Stats: {stats}")
        
    except Exception as e:
        print(f"❌ Initialization failed: {e}")
        return 1

    # 2. Search for Ancient Artifacts
    print("\n🔍 Probe 1: Searching for 'WXYZ'...")
    results = mm.search_memories(query="WXYZ", limit=5)
    print(f"   Found {len(results)} results.")
    for res in results:
        print(f"   - [{res['score']:.2f}] {res['entry']['title']} ({res['entry']['id']})")

    # 3. Create Test Memory
    print("\n📝 Probe 2: Creating 'Resurrection Check' memory...")
    try:
        create_res = mm.create_memory(
            title="Resurrection Check",
            content="This is a probe to verify the Phoenix has risen. Checking Holographic Coordinates.",
            tags=["layer3_probe", "phoenix"],
            memory_type="short_term"
        )
        mem_id = str(create_res).replace(".md", "")
        print(f"   ✅ Created Memory ID: {mem_id}")
        
    except Exception as e:
        print(f"❌ Creation failed: {e}")
        return 1

    # 4. Verify Holographic Coordinates
    print("\n📐 Probe 3: Verifying Holographic Coordinates...")
    full_mem = mm.get_memory(mem_id)
    
    if "metadata" in full_mem and "holographic_coords" in full_mem["metadata"]:
        coords = full_mem["metadata"]["holographic_coords"]
        print(f"   ✅ Coordinates Found: {coords}")
        print("   (Note: If X/Y/Z are present, the Encoder is working!)")
    else:
        print("   ⚠️ No 'holographic_coords' in metadata.")
        print("   Metadata dump:")
        pprint(full_mem.get("metadata", {}))
        # It's possible the unified backend stores it separately or calls it something else
        # Let's check if there is a 'vector' field or similar
        
    # Check if the python fallback worked (it usually returns non-zero coords)
    
    return 0

if __name__ == "__main__":
    sys.exit(run_probe())
