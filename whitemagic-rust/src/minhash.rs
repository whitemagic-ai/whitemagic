//! MinHash — Locality-Sensitive Hashing for approximate set similarity
//!
//! Used by the consolidation engine and association miner to quickly
//! identify near-duplicate memories without computing full Jaccard on
//! every pair. MinHash reduces each keyword set to a fixed-size signature,
//! then compares signatures in O(k) instead of O(|A|+|B|).
//!
//! With 128 hash functions, the Jaccard estimate has standard error ~0.088.

use pyo3::prelude::*;
use rayon::prelude::*;
use serde::Serialize;
use std::collections::HashSet;

const NUM_HASHES: usize = 128;
const LARGE_PRIME: u64 = 4294967311; // Next prime after 2^32

/// A MinHash signature — fixed-size sketch of a keyword set.
#[derive(Debug, Clone, Serialize)]
pub struct MinHashSignature {
    pub values: Vec<u64>,
}

/// Hash parameters: h(x) = (a*x + b) mod p
struct HashParams {
    a: u64,
    b: u64,
}

lazy_static::lazy_static! {
    static ref HASH_PARAMS: Vec<HashParams> = {
        // Deterministic seed for reproducibility
        let mut params = Vec::with_capacity(NUM_HASHES);
        let mut seed: u64 = 0xDEADBEEF_CAFEBABE;
        for _ in 0..NUM_HASHES {
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            let a = (seed >> 16) | 1; // Must be odd
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            let b = seed >> 16;
            params.push(HashParams { a, b });
        }
        params
    };
}

/// Hash a string to u64 using FNV-1a
#[inline]
fn fnv1a_hash(s: &str) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for byte in s.bytes() {
        h ^= byte as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

/// Compute MinHash signature for a set of keywords
pub fn compute_signature(keywords: &HashSet<String>) -> MinHashSignature {
    if keywords.is_empty() {
        return MinHashSignature {
            values: vec![u64::MAX; NUM_HASHES],
        };
    }

    let mut min_values = vec![u64::MAX; NUM_HASHES];

    for kw in keywords {
        let kw_hash = fnv1a_hash(kw);
        for (i, params) in HASH_PARAMS.iter().enumerate() {
            let h = (params.a.wrapping_mul(kw_hash).wrapping_add(params.b)) % LARGE_PRIME;
            if h < min_values[i] {
                min_values[i] = h;
            }
        }
    }

    MinHashSignature { values: min_values }
}

/// Estimate Jaccard similarity from two MinHash signatures
#[inline]
pub fn estimate_jaccard(a: &MinHashSignature, b: &MinHashSignature) -> f64 {
    let matches = a.values.iter().zip(b.values.iter()).filter(|(x, y)| x == y).count();
    matches as f64 / NUM_HASHES as f64
}

/// Result of a near-duplicate detection
#[derive(Debug, Clone, Serialize)]
pub struct DuplicateCandidate {
    pub idx_a: usize,
    pub idx_b: usize,
    pub estimated_jaccard: f64,
}

/// Find near-duplicate pairs from a batch of keyword sets using MinHash.
/// Much faster than full N² Jaccard for large corpora.
pub fn find_near_duplicates(
    keyword_sets: &[HashSet<String>],
    threshold: f64,
    max_results: usize,
) -> Vec<DuplicateCandidate> {
    let n = keyword_sets.len();
    if n < 2 {
        return Vec::new();
    }

    // Compute all signatures in parallel
    let signatures: Vec<MinHashSignature> = keyword_sets
        .par_iter()
        .map(|ks| compute_signature(ks))
        .collect();

    // Compare all pairs (parallelized outer loop)
    let mut candidates: Vec<DuplicateCandidate> = (0..n)
        .into_par_iter()
        .flat_map(|i| {
            let mut local = Vec::new();
            for j in (i + 1)..n {
                let est = estimate_jaccard(&signatures[i], &signatures[j]);
                if est >= threshold {
                    local.push(DuplicateCandidate {
                        idx_a: i,
                        idx_b: j,
                        estimated_jaccard: est,
                    });
                }
            }
            local
        })
        .collect();

    candidates.sort_by(|a, b| b.estimated_jaccard.partial_cmp(&a.estimated_jaccard).unwrap());
    candidates.truncate(max_results);
    candidates
}

// ---------------------------------------------------------------------------
// Python bindings
// ---------------------------------------------------------------------------

/// Find near-duplicate memory pairs using MinHash LSH.
/// Input: JSON array of keyword arrays (one per memory).
/// Output: JSON array of DuplicateCandidate.
#[pyfunction]
pub fn minhash_find_duplicates(
    keywords_json: &str,
    threshold: f64,
    max_results: usize,
) -> PyResult<String> {
    let keyword_lists: Vec<Vec<String>> = serde_json::from_str(keywords_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let sets: Vec<HashSet<String>> = keyword_lists
        .into_iter()
        .map(|kws| kws.into_iter().collect())
        .collect();

    let candidates = find_near_duplicates(&sets, threshold, max_results);

    serde_json::to_string(&candidates)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Compute MinHash signatures for a batch of keyword sets.
/// Input: JSON array of keyword arrays.
/// Output: JSON array of signature value arrays.
#[pyfunction]
pub fn minhash_signatures(keywords_json: &str) -> PyResult<String> {
    let keyword_lists: Vec<Vec<String>> = serde_json::from_str(keywords_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let signatures: Vec<MinHashSignature> = keyword_lists
        .par_iter()
        .map(|kws| {
            let set: HashSet<String> = kws.iter().cloned().collect();
            compute_signature(&set)
        })
        .collect();

    serde_json::to_string(&signatures)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identical_sets() {
        let set: HashSet<String> = ["python", "rust", "memory"]
            .iter().map(|s| s.to_string()).collect();
        let sig_a = compute_signature(&set);
        let sig_b = compute_signature(&set);
        let est = estimate_jaccard(&sig_a, &sig_b);
        assert!((est - 1.0).abs() < 1e-10, "Identical sets should have Jaccard=1.0, got {}", est);
    }

    #[test]
    fn test_disjoint_sets() {
        let set_a: HashSet<String> = ["alpha", "beta", "gamma", "delta", "epsilon"]
            .iter().map(|s| s.to_string()).collect();
        let set_b: HashSet<String> = ["one", "two", "three", "four", "five"]
            .iter().map(|s| s.to_string()).collect();
        let sig_a = compute_signature(&set_a);
        let sig_b = compute_signature(&set_b);
        let est = estimate_jaccard(&sig_a, &sig_b);
        // Should be close to 0 (with some noise from hash collisions)
        assert!(est < 0.2, "Disjoint sets should have low Jaccard, got {}", est);
    }

    #[test]
    fn test_overlapping_sets() {
        let set_a: HashSet<String> = ["python", "rust", "memory", "tools", "mcp"]
            .iter().map(|s| s.to_string()).collect();
        let set_b: HashSet<String> = ["python", "memory", "mcp", "galactic", "hologram"]
            .iter().map(|s| s.to_string()).collect();
        let sig_a = compute_signature(&set_a);
        let sig_b = compute_signature(&set_b);
        let est = estimate_jaccard(&sig_a, &sig_b);
        // True Jaccard = 3/7 ≈ 0.43. Estimate should be in [0.2, 0.7]
        assert!(est > 0.15 && est < 0.75, "Overlapping sets should have moderate Jaccard, got {}", est);
    }

    #[test]
    fn test_find_near_duplicates() {
        let sets: Vec<HashSet<String>> = vec![
            ["python", "rust", "memory"].iter().map(|s| s.to_string()).collect(),
            ["python", "rust", "memory"].iter().map(|s| s.to_string()).collect(), // exact dup
            ["elixir", "haskell", "zig"].iter().map(|s| s.to_string()).collect(),
        ];
        let candidates = find_near_duplicates(&sets, 0.5, 100);
        assert!(!candidates.is_empty(), "Should find the duplicate pair");
        assert_eq!(candidates[0].idx_a, 0);
        assert_eq!(candidates[0].idx_b, 1);
    }

    #[test]
    fn test_empty_set() {
        let empty: HashSet<String> = HashSet::new();
        let sig = compute_signature(&empty);
        assert_eq!(sig.values.len(), NUM_HASHES);
        assert!(sig.values.iter().all(|&v| v == u64::MAX));
    }
}
