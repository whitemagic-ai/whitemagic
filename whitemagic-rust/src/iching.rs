///
/// I Ching Engine - Rust Implementation
/// 
/// Provides deterministic casting based on query hash
/// (consistent answers for consistent questions)
///

use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

/// Calculate hexagram from string query
pub fn cast_hexagram(query: &str) -> (u32, Vec<u32>) {
    let mut hasher = DefaultHasher::new();
    query.hash(&mut hasher);
    let hash = hasher.finish();
    
    // Generate 6 lines from the hash bits
    // Each line is 6, 7, 8, or 9 (moving/stable yin/yang)
    // Simplified: Just getting 1-64 hexagram number for now
    // But let's do it properly with lines
    
    let mut lines = Vec::new();
    let mut hex_val = 0;
    
    for i in 0..6 {
        // Use different parts of the hash for entropy
        let part = (hash >> (i * 4)) & 0xF;
        
        // 6,7,8,9 probability distribution (approximate yarrow stalk)
        // 6 (Old Yin)  : 1/16
        // 7 (Young Yang): 5/16
        // 8 (Young Yin) : 7/16
        // 9 (Old Yang)  : 3/16
        
        let line_val = match part {
            0 => 6,      // Old Yin
            1..=5 => 7,  // Young Yang
            6..=12 => 8, // Young Yin
            _ => 9       // Old Yang
        };
        
        lines.push(line_val);
        
        // Build hexagram value (bottom to top)
        // 7 and 9 are Yang (1), 6 and 8 are Yin (0)
        if line_val == 7 || line_val == 9 {
            hex_val |= 1 << i;
        }
    }
    
    // Convert 0-63 to 1-64
    (hex_val + 1, lines)
}
