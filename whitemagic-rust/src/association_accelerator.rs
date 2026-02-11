//! Association Accelerator — High-performance keyword overlap & Jaccard scoring
//!
//! Ports the hot path from Python's `association_miner.py` into Rust:
//! - Keyword extraction with stop-word filtering
//! - N² pairwise Jaccard overlap via Rayon parallel iteration
//! - Weighted Jaccard with count bonus (matches Python algorithm)
//!
//! The Python AssociationMiner._compute_overlap() runs O(N²) over sampled
//! memories. With N=200, that's 19,900 pairs. Rust + Rayon makes this
//! near-instant even for N=1000 (499,500 pairs).

use pyo3::prelude::*;
use rayon::prelude::*;
use serde::Serialize;
use std::collections::HashSet;

// ---------------------------------------------------------------------------
// Stop words (minimal English set matching Python's _STOP_WORDS)
// ---------------------------------------------------------------------------

const STOP_WORDS: &[&str] = &[
    "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
    "of", "with", "by", "from", "is", "it", "this", "that", "was", "are",
    "were", "be", "been", "being", "have", "has", "had", "do", "does",
    "did", "will", "would", "could", "should", "may", "might", "shall",
    "can", "not", "no", "nor", "so", "if", "then", "than", "too", "very",
    "just", "about", "up", "out", "into", "over", "after", "before",
    "between", "under", "through", "during", "each", "some", "such",
    "only", "other", "new", "now", "way", "all", "both", "when", "where",
    "how", "what", "which", "who", "whom", "its", "also", "more", "most",
    "these", "those", "here", "there", "they", "them", "their", "our",
    "your", "his", "her", "she", "him", "we", "you", "me", "my",
];

lazy_static::lazy_static! {
    static ref STOP_SET: HashSet<&'static str> = {
        STOP_WORDS.iter().copied().collect()
    };
}

// ---------------------------------------------------------------------------
// Keyword extraction
// ---------------------------------------------------------------------------

/// Extract meaningful keywords from text.
/// Matches Python's AssociationMiner._extract_keywords().
pub fn extract_keywords(text: &str, max_keywords: usize) -> HashSet<String> {
    let text_lower = text.to_lowercase();

    // Split on non-alphanumeric boundaries (matching Python's \w+ regex)
    let words: Vec<&str> = text_lower
        .split(|c: char| !c.is_alphanumeric() && c != '_')
        .filter(|w| w.len() > 2 && !STOP_SET.contains(*w))
        .collect();

    let mut keywords: HashSet<String> = words.iter().map(|w| w.to_string()).collect();

    // If too many, keep most frequent
    if keywords.len() > max_keywords {
        let mut freq = std::collections::HashMap::new();
        for w in &words {
            if keywords.contains(*w) {
                *freq.entry(w.to_string()).or_insert(0u32) += 1;
            }
        }
        let mut sorted_kw: Vec<_> = freq.into_iter().collect();
        sorted_kw.sort_by(|a, b| b.1.cmp(&a.1));
        keywords = sorted_kw.into_iter().take(max_keywords).map(|(k, _)| k).collect();
    }

    keywords
}

// ---------------------------------------------------------------------------
// Jaccard overlap scoring
// ---------------------------------------------------------------------------

/// Result of comparing two memory keyword sets.
#[derive(Debug, Clone, Serialize)]
pub struct OverlapResult {
    pub source_idx: usize,
    pub target_idx: usize,
    pub overlap_score: f64,
    pub shared_count: usize,
    pub shared_keywords: Vec<String>,
}

/// Compute weighted Jaccard overlap between two keyword sets.
/// Matches Python's AssociationMiner._compute_overlap().
#[inline]
fn compute_overlap(kw_a: &HashSet<String>, kw_b: &HashSet<String>) -> (f64, Vec<String>) {
    if kw_a.is_empty() || kw_b.is_empty() {
        return (0.0, Vec::new());
    }

    let shared: Vec<String> = kw_a.intersection(kw_b).cloned().collect();
    let union_size = kw_a.union(kw_b).count();

    if union_size == 0 {
        return (0.0, Vec::new());
    }

    // Weighted Jaccard: penalize very small shared sets
    let raw_jaccard = shared.len() as f64 / union_size as f64;
    // Boost if many shared keywords (absolute count matters)
    let count_bonus = (shared.len() as f64 / 5.0).min(1.0) * 0.3;
    let score = (raw_jaccard + count_bonus).min(1.0);

    (score, shared)
}

// ---------------------------------------------------------------------------
// Batch pairwise comparison (parallel)
// ---------------------------------------------------------------------------

/// Compare all pairs of keyword sets and return strong overlaps.
/// This is the N² hot path — Rayon parallelizes the outer loop.
///
/// Args:
///   fingerprints: Vec of keyword sets (one per memory)
///   min_score: Minimum overlap score to include in results
///   max_results: Maximum number of results to return
///
/// Returns: Vec<OverlapResult> sorted by score descending
pub fn batch_pairwise_overlap(
    fingerprints: &[HashSet<String>],
    min_score: f64,
    max_results: usize,
) -> Vec<OverlapResult> {
    let n = fingerprints.len();
    if n < 2 {
        return Vec::new();
    }

    // Parallel: each thread handles a chunk of the outer loop
    let mut all_results: Vec<OverlapResult> = (0..n)
        .into_par_iter()
        .flat_map(|i| {
            let mut local_results = Vec::new();
            for j in (i + 1)..n {
                let (score, shared) = compute_overlap(&fingerprints[i], &fingerprints[j]);
                if score >= min_score {
                    local_results.push(OverlapResult {
                        source_idx: i,
                        target_idx: j,
                        overlap_score: score,
                        shared_count: shared.len(),
                        shared_keywords: shared.into_iter().take(5).collect(),
                    });
                }
            }
            local_results
        })
        .collect();

    // Sort by score descending
    all_results.sort_by(|a, b| b.overlap_score.partial_cmp(&a.overlap_score).unwrap());

    // Cap results
    all_results.truncate(max_results);
    all_results
}

// ---------------------------------------------------------------------------
// Python bindings
// ---------------------------------------------------------------------------

/// Extract keywords from a list of texts (parallel).
/// Input: list of (id, text) tuples as JSON.
/// Output: JSON map of id → [keywords].
#[pyfunction]
pub fn association_extract_keywords(
    texts_json: &str,
    max_keywords: usize,
) -> PyResult<String> {
    let texts: Vec<(String, String)> = serde_json::from_str(texts_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let results: Vec<(String, Vec<String>)> = texts
        .par_iter()
        .map(|(id, text)| {
            let kws = extract_keywords(text, max_keywords);
            let mut sorted: Vec<String> = kws.into_iter().collect();
            sorted.sort();
            (id.clone(), sorted)
        })
        .collect();

    serde_json::to_string(&results)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Compute pairwise overlaps from pre-extracted keyword sets.
/// Input: JSON array of keyword arrays (one per memory).
/// Output: JSON array of OverlapResult.
#[pyfunction]
pub fn association_pairwise_overlap(
    keywords_json: &str,
    min_score: f64,
    max_results: usize,
) -> PyResult<String> {
    let keyword_lists: Vec<Vec<String>> = serde_json::from_str(keywords_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let fingerprints: Vec<HashSet<String>> = keyword_lists
        .into_iter()
        .map(|kws| kws.into_iter().collect())
        .collect();

    let results = batch_pairwise_overlap(&fingerprints, min_score, max_results);

    serde_json::to_string(&results)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// All-in-one: extract keywords from texts, then compute pairwise overlaps.
/// Input: JSON array of [id, text] pairs.
/// Output: JSON object with "keywords" and "overlaps" fields.
#[pyfunction]
pub fn association_mine_fast(
    texts_json: &str,
    max_keywords: usize,
    min_score: f64,
    max_results: usize,
) -> PyResult<String> {
    let texts: Vec<(String, String)> = serde_json::from_str(texts_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    // Extract keywords in parallel
    let fingerprints: Vec<HashSet<String>> = texts
        .par_iter()
        .map(|(_, text)| extract_keywords(text, max_keywords))
        .collect();

    // Pairwise overlap
    let overlaps = batch_pairwise_overlap(&fingerprints, min_score, max_results);

    // Build output with IDs mapped
    #[derive(Serialize)]
    struct MiningOutput {
        memory_count: usize,
        pair_count: usize,
        overlaps: Vec<OverlapWithIds>,
    }

    #[derive(Serialize)]
    struct OverlapWithIds {
        source_id: String,
        target_id: String,
        overlap_score: f64,
        shared_count: usize,
        shared_keywords: Vec<String>,
    }

    let overlaps_with_ids: Vec<OverlapWithIds> = overlaps
        .into_iter()
        .map(|o| OverlapWithIds {
            source_id: texts[o.source_idx].0.clone(),
            target_id: texts[o.target_idx].0.clone(),
            overlap_score: o.overlap_score,
            shared_count: o.shared_count,
            shared_keywords: o.shared_keywords,
        })
        .collect();

    let output = MiningOutput {
        memory_count: texts.len(),
        pair_count: overlaps_with_ids.len(),
        overlaps: overlaps_with_ids,
    };

    serde_json::to_string(&output)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_keywords() {
        let text = "WhiteMagic uses Python for MCP tools and Rust for performance";
        let kws = extract_keywords(text, 50);
        assert!(kws.contains("whitemagic"));
        assert!(kws.contains("python"));
        assert!(kws.contains("rust"));
        assert!(kws.contains("performance"));
        // Stop words filtered
        assert!(!kws.contains("for"));
        assert!(!kws.contains("and"));
    }

    #[test]
    fn test_overlap_scoring() {
        let a: HashSet<String> = ["python", "rust", "memory", "tools", "mcp"]
            .iter().map(|s| s.to_string()).collect();
        let b: HashSet<String> = ["python", "memory", "galactic", "hologram", "mcp"]
            .iter().map(|s| s.to_string()).collect();

        let (score, shared) = compute_overlap(&a, &b);
        assert_eq!(shared.len(), 3); // python, memory, mcp
        assert!(score > 0.3); // Should be significant overlap
    }

    #[test]
    fn test_batch_pairwise() {
        let fp = vec![
            ["python", "rust", "memory"].iter().map(|s| s.to_string()).collect::<HashSet<_>>(),
            ["python", "memory", "galactic"].iter().map(|s| s.to_string()).collect(),
            ["elixir", "haskell", "zig"].iter().map(|s| s.to_string()).collect(),
        ];

        let results = batch_pairwise_overlap(&fp, 0.1, 100);
        // First two should overlap, third should not match well
        assert!(!results.is_empty());
        assert_eq!(results[0].source_idx, 0);
        assert_eq!(results[0].target_idx, 1);
    }

    #[test]
    fn test_empty_inputs() {
        let (score, shared) = compute_overlap(&HashSet::new(), &HashSet::new());
        assert_eq!(score, 0.0);
        assert!(shared.is_empty());

        let results = batch_pairwise_overlap(&[], 0.1, 100);
        assert!(results.is_empty());
    }
}
