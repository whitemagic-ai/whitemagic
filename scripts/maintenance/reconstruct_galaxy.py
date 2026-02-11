import sys
import logging
import time
from pathlib import Path

# Add project root to path
REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT))

from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.intelligence.hologram.encoder import CoordinateEncoder
from whitemagic.intelligence.learning.relationship_extractor import extract_relationships

# Configure logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")
logger = logging.getLogger("GalaxyReconstruction")

def main():
    logger.info("🌌 Starting Galaxy Reconstruction...")
    
    # 1. Initialize Memory System
    # um = get_unified_memory()
    # stats = um.get_stats()
    # total_memories = stats['total_memories']
    # logger.info(f"Loaded Unified Memory. Total Memories: {total_memories}")
    logger.info("Skipping UM init to prevent locks.")
    
    
    # 2. Re-Index Holographic Coordinates
    # logger.info("📍 Phase 1: Holographic Re-Indexing (Pure Python Mode)")
    # ... (Phase 1 skipped because it is already done and validated)
    logger.info("⏩ Phase 1: Holographic Re-Indexing (Skipping - Already Done)")
    
    # [Pre-existing implementation kept commented out for reference]
    
    if False: # Skipping Phase 1 as it is already complete
        # Monkeymatch CoordinateEncoder to skip router and use pure Python
        # This avoids the 25ms/call overhead of the bridge
        def fast_encode(self, memory):
            # 1. Base calculations (Pure Python)
            x = self._calculate_x(memory)
            y = self._calculate_y(memory)
            z = self._calculate_z(memory)
            w = self._calculate_w(memory)
            
            # Apply garden bias if present
            if self._garden_bias_enabled:
                garden_bias = self._get_garden_bias(memory)
                if garden_bias:
                    x, y, z, w = self._blend_with_garden(x, y, z, w, garden_bias)
                    
            from whitemagic.intelligence.hologram.encoder import HolographicCoordinate
            return HolographicCoordinate(x, y, z, w)
            
        CoordinateEncoder.encode = fast_encode
        encoder = CoordinateEncoder()
        
        # Iterate in batches to avoid memory issues
        batch_size = 1000
        total_processed = 0
        total_coords = 0
        
    
        # Use SQLite cursor for efficient iteration
        batch_size = 1000
        total_processed = 0
        total_coords = 0
        
        import sqlite3
        
        # Get total count
        with um.backend.pool.connection() as conn:
            count = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
            
        logger.info(f"Re-indexing {count} memories in batches of {batch_size}...")
        
        with um.backend.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            
            for offset in range(0, count, batch_size):
                # Log progress every 10 batches
                if (offset // batch_size) % 10 == 0:
                    logger.info(f"Processing batch {offset // batch_size} / {count // batch_size}...")
                # Fetch raw rows
                rows = conn.execute(f"SELECT * FROM memories LIMIT {batch_size} OFFSET {offset}").fetchall()
                if not rows:
                    break
                    
                # Efficiently hydrate
                memories = um.backend._batch_hydrate(rows, conn)
                
                # Prepare batch of coordinates
                coords_to_store = []
                
                for memory in memories:
                    try:
                        # Encode
                        # Memory object has to_dict which works for encoder
                        mem_dict = memory.to_dict()
                        coord = encoder.encode(mem_dict)
                        
                        coords_to_store.append((memory.id, coord.x, coord.y, coord.z, coord.w))
                        total_coords += 1
                    except Exception as e:
                        logger.error(f"Failed to encode {memory.id}: {e}")
                
                # Bulk insert coords
                if coords_to_store:
                    with conn: # Nested transaction for the batch write
                        conn.executemany(
                            "INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w) VALUES (?, ?, ?, ?, ?)",
                            coords_to_store
                        )
                
                total_processed += len(rows)
    
            
        logger.info(f"✅ Re-indexing complete. {total_coords} stars positioned.")
    

    # 3. Extract Relationships
    logger.info("🕸️ Phase 2: Relationship Extraction")
    
    # Resolving path manually to avoid initializing UnifiedMemory (which locks DB)
    from whitemagic.config.paths import WM_ROOT
    base_dir = WM_ROOT / "memory"
    
    logger.info(f"Target DB Directory: {base_dir}")
    rel_stats = extract_relationships(base_path=base_dir, dry_run=False)
    
    logger.info("✅ Galaxy Reconstruction Complete.")
    logger.info(f"Summary: {rel_stats}")

if __name__ == "__main__":
    main()
