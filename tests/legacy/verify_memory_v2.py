#!/usr/bin/env python3
"""
Layer 3 Verification: Memory Navigation Probe (v2)
==================================================
UPDATES:
1. HolographicMemory natively generates coordinates without Rust index (no patch needed).
2. Ingests usage of 'recovery_abc' files to populate the empty DB.
"""

import sys
import os
import glob
from pathlib import Path
from pprint import pprint

# Setup paths
ROOT_DIR = Path(__file__).resolve().parent
PROJECT_MEM_DIR = ROOT_DIR.parent.parent.parent / "staging" / "project_memory"
sys.path.append(str(ROOT_DIR.parent))

def ingest_sample_files(mm):
    """Ingest a few markdown files from recovery_abc to populate DB."""
    print("\n📥 Ingesting sample artifacts...")
    
    # Target some interesting files
    targets = list(PROJECT_MEM_DIR.glob("recovery_abc/**/*.md"))[:10]
    
    count = 0
    for fpath in targets:
        try:
            content = fpath.read_text()
            title = fpath.stem
            mm.create_memory(
                title=title,
                content=content,
                tags=["ingested", "layer3"],
                memory_type="long_term"
            )
            count += 1
            print(f"   - Ingested: {title}")
        except Exception as e:
            print(f"   ⚠️ Failed to ingest {fpath.name}: {e}")
            
    print(f"   Total ingested: {count}")


def run_probe():
    print("🧠 Layer 3 Memory Probe (v2) Initiating...\n")

    try:
        from whitemagic.core.memory.manager import MemoryManager
        mm = MemoryManager(base_dir=PROJECT_MEM_DIR)
        print("✅ MemoryManager initialized.")
    except Exception as e:
        print(f"❌ Initialization failed: {e}")
        return 1

    # 1. Ingest Data
    ingest_sample_files(mm)

    # 2. Search for Ingested Data
    print("\n🔍 Probe 1: Searching for 'layer3'...")
    results = mm.search_memories(tags=["layer3"], limit=5)
    print(f"   Found {len(results)} results.")

    # 3. Create Test Memory & Verify Coords
    print("\n📝 Probe 2: Creating 'Holographic Check' memory...")
    try:
        create_res = mm.create_memory(
            title="Holographic Check",
            content="Testing monkeypatched coordinate generation.",
            tags=["probe_v2"],
            memory_type="short_term"
        )
        mem_id = str(create_res).replace(".md", "")
        print(f"   ✅ Created Memory ID: {mem_id}")
        
        # 4. Verify Holographic Coordinates
        print("\n📐 Probe 3: Verifying Holographic Coordinates...")
        full_mem = mm.get_memory(mem_id)
        
        coords = full_mem.get("metadata", {}).get("holographic_coords")
        if coords:
            print(f"   ✅ Coordinates Found: {coords}")
        else:
            # Check if we can verify via the unified backend directly
            # Sometimes metadata isn't returned in the dict by default
            print("   ⚠️ No 'holographic_coords' in metadata view. Checking via backend...")
            stored_mem = mm.unified.recall(mem_id)
            # The backend might store coords separately if index is missing?
            # Actually, our patch returns coords, so they SHOULD be in metadata if store() logic worked.
            # Let's look at store() in unified.py:
            # if self.holographic:
            #    coords = self.holographic.index_memory(...)
            #    if coords: self.backend.store_coords(...)
            
            # They are stored in backend.store_coords, not necessarily in memory.metadata object immediately without reload?
            # Let's check backend.
            coords_map = mm.unified.backend.get_all_coords()
            if mem_id in coords_map:
                 print(f"   ✅ Coordinates found in Backend DB: {coords_map[mem_id]}")
            else:
                 print("   ❌ Coordinates missing from Backend DB.")

    except Exception as e:
        print(f"❌ Creation/Verification failed: {e}")
        return 1

    return 0

if __name__ == "__main__":
    run_probe()
