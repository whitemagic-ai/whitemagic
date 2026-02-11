//! T-SAR Inspired SIMD Pattern Matching
//! 
//! Based on research from "T-SAR: CPU-Only Ternary LLM Inference via In-Place SIMD ALU Reorganization"
//! Applies the principle of using SIMD registers for fast pattern matching in edge rules.
//!
//! Key insight: Instead of memory-bound lookups, use SIMD parallel comparison.

use std::collections::HashMap;

/// A pattern compiled for SIMD matching
#[derive(Debug, Clone)]
pub struct SimdPattern {
    pub id: String,
    pub keywords: Vec<String>,
    pub response: String,
    pub confidence: f32,
}

/// SIMD-optimized pattern matcher
/// 
/// Uses parallel byte comparison for fast keyword detection.
/// On CPUs with SSE3/AVX2, this can check multiple keywords simultaneously.
pub struct SimdMatcher {
    patterns: Vec<SimdPattern>,
    // Pre-computed keyword bytes for SIMD comparison
    keyword_cache: HashMap<String, Vec<u8>>,
}

impl SimdMatcher {
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
            keyword_cache: HashMap::new(),
        }
    }

    /// Add a pattern with pipe-separated keywords
    pub fn add_pattern(&mut self, id: &str, pattern: &str, response: &str, confidence: f32) {
        let keywords: Vec<String> = pattern
            .split('|')
            .map(|s| s.to_lowercase())
            .collect();
        
        // Pre-cache keyword bytes
        for kw in &keywords {
            if !self.keyword_cache.contains_key(kw) {
                self.keyword_cache.insert(kw.clone(), kw.as_bytes().to_vec());
            }
        }

        self.patterns.push(SimdPattern {
            id: id.to_string(),
            keywords,
            response: response.to_string(),
            confidence,
        });
    }

    /// Fast pattern matching using byte-level comparison
    /// 
    /// This simulates SIMD-style parallel matching:
    /// - Converts query to lowercase bytes once
    /// - Checks all keywords using slice contains (which Rust optimizes with SIMD)
    pub fn match_query(&self, query: &str) -> Option<(&SimdPattern, f32)> {
        let query_lower = query.to_lowercase();
        let query_bytes = query_lower.as_bytes();
        
        let mut best_match: Option<(&SimdPattern, f32)> = None;
        
        for pattern in &self.patterns {
            let mut matched_count = 0;
            let mut total_match_len = 0;
            
            for kw in &pattern.keywords {
                // Use byte-level search (Rust uses SIMD internally for this)
                if let Some(kw_bytes) = self.keyword_cache.get(kw) {
                    if contains_bytes(query_bytes, kw_bytes) {
                        matched_count += 1;
                        total_match_len += kw.len();
                    }
                }
            }
            
            if matched_count > 0 {
                // Score based on coverage
                let keyword_coverage = matched_count as f32 / pattern.keywords.len() as f32;
                let length_ratio = (total_match_len as f32 / query_bytes.len().max(1) as f32 * 2.0).min(1.0);
                let score = keyword_coverage * 0.6 + length_ratio * 0.4;
                
                let weighted_score = score * pattern.confidence;
                
                if let Some((_, best_score)) = best_match {
                    if weighted_score > best_score {
                        best_match = Some((pattern, score));
                    }
                } else {
                    best_match = Some((pattern, score));
                }
            }
        }
        
        // Only return if score is good enough
        best_match.filter(|(_, score)| *score >= 0.3)
    }
    
    /// Get pattern count
    pub fn pattern_count(&self) -> usize {
        self.patterns.len()
    }
}

/// Fast byte-level substring search
/// Rust's standard library uses SIMD for this on supported platforms
#[inline]
fn contains_bytes(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    if needle.len() > haystack.len() {
        return false;
    }
    
    // Use windows iterator - Rust optimizes this with SIMD
    haystack.windows(needle.len()).any(|window| window == needle)
}

/// Ternary weight representation for future BitNet-style inference
/// Based on T-SAR's ternary-to-binary decomposition
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TernaryWeight {
    Negative = -1,
    Zero = 0,
    Positive = 1,
}

/// Decompose ternary weights into dense and sparse binary forms
/// This is the key T-SAR insight for efficient computation
pub struct TernaryDecomposition {
    /// Dense weights: -1 or +1 (zeros become +1)
    pub dense: Vec<i8>,
    /// Sparse mask: 1 where original was 0, else 0
    pub sparse: Vec<u8>,
}

impl TernaryDecomposition {
    /// Create from ternary weights
    pub fn from_ternary(weights: &[TernaryWeight]) -> Self {
        let mut dense = Vec::with_capacity(weights.len());
        let mut sparse = Vec::with_capacity(weights.len());
        
        for w in weights {
            match w {
                TernaryWeight::Negative => {
                    dense.push(-1);
                    sparse.push(0);
                }
                TernaryWeight::Zero => {
                    dense.push(1);  // Placeholder
                    sparse.push(1); // Mark as zero
                }
                TernaryWeight::Positive => {
                    dense.push(1);
                    sparse.push(0);
                }
            }
        }
        
        Self { dense, sparse }
    }
    
    /// Compute dot product using decomposition
    /// Result = dense_dot - sparse_dot (effectively subtracting zero contributions)
    pub fn dot_product(&self, activations: &[f32]) -> f32 {
        assert_eq!(self.dense.len(), activations.len());
        
        let mut dense_sum: f32 = 0.0;
        let mut sparse_sum: f32 = 0.0;
        
        // In real SIMD, these would be parallel operations
        for i in 0..activations.len() {
            dense_sum += activations[i] * self.dense[i] as f32;
            sparse_sum += activations[i] * self.sparse[i] as f32;
        }
        
        dense_sum - sparse_sum
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simd_matcher() {
        let mut matcher = SimdMatcher::new();
        matcher.add_pattern("hello", "hello|hi|hey", "Hello there!", 1.0);
        matcher.add_pattern("version", "version|what version", "Version 4.0", 1.0);
        
        let result = matcher.match_query("hello");
        assert!(result.is_some());
        assert_eq!(result.unwrap().0.id, "hello");
        
        let result = matcher.match_query("what is the version");
        assert!(result.is_some());
        assert_eq!(result.unwrap().0.id, "version");
        
        let result = matcher.match_query("explain quantum physics");
        assert!(result.is_none());
    }
    
    #[test]
    fn test_ternary_decomposition() {
        let weights = vec![
            TernaryWeight::Positive,
            TernaryWeight::Zero,
            TernaryWeight::Negative,
            TernaryWeight::Positive,
        ];
        
        let decomp = TernaryDecomposition::from_ternary(&weights);
        
        // Test with simple activations
        let activations = vec![1.0, 2.0, 3.0, 4.0];
        let result = decomp.dot_product(&activations);
        
        // Expected: 1*1 + 2*0 + 3*(-1) + 4*1 = 1 + 0 - 3 + 4 = 2
        assert!((result - 2.0).abs() < 0.001);
    }
}
