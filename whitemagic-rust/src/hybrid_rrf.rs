//! Hybrid Reciprocal Rank Fusion (RRF) — Rust-accelerated scoring.
//!
//! Replaces the Python dict-based RRF scoring loop in CoreAccessLayer.hybrid_recall()
//! with a zero-copy Rust implementation for sub-millisecond fusion of vector and
//! graph search results.
//!
//! RRF formula: score(d) = Σ_r (w_r / (k + rank_r(d)))
//! where k=60 (standard), w_r is per-source weight, rank_r(d) is 1-indexed rank.

#[cfg(feature = "python")]
use pyo3::prelude::*;

use std::collections::HashMap;

/// A single RRF result: memory_id, fused score, source flags.
#[cfg(feature = "python")]
#[pyclass]
#[derive(Clone)]
pub struct RrfResult {
    #[pyo3(get)]
    pub memory_id: String,
    #[pyo3(get)]
    pub score: f64,
    #[pyo3(get)]
    pub from_vector: bool,
    #[pyo3(get)]
    pub from_graph: bool,
}

/// Fuse vector-channel and graph-channel results using Reciprocal Rank Fusion.
///
/// Args:
///   vector_ids: memory IDs from embedding search, ordered by similarity desc.
///   graph_ids: memory IDs from association graph walk, ordered by strength desc.
///   vector_weight: weight for the vector channel (default 0.6).
///   graph_weight: weight for the graph channel (default 0.4).
///   k_rrf: RRF constant (default 60).
///   limit: max results to return.
///
/// Returns:
///   Vec of RrfResult sorted by fused score descending.
#[cfg(feature = "python")]
#[pyfunction]
#[pyo3(signature = (vector_ids, graph_ids, vector_weight=0.6, graph_weight=0.4, k_rrf=60, limit=10))]
pub fn rrf_fuse(
    vector_ids: Vec<String>,
    graph_ids: Vec<String>,
    vector_weight: f64,
    graph_weight: f64,
    k_rrf: usize,
    limit: usize,
) -> Vec<RrfResult> {
    let mut scores: HashMap<String, (f64, bool, bool)> = HashMap::new();

    // Vector channel ranks (1-indexed)
    for (rank_0, mid) in vector_ids.iter().enumerate() {
        let rank = rank_0 + 1;
        let contribution = vector_weight / (k_rrf as f64 + rank as f64);
        let entry = scores.entry(mid.clone()).or_insert((0.0, false, false));
        entry.0 += contribution;
        entry.1 = true; // from_vector
    }

    // Graph channel ranks (1-indexed)
    for (rank_0, mid) in graph_ids.iter().enumerate() {
        let rank = rank_0 + 1;
        let contribution = graph_weight / (k_rrf as f64 + rank as f64);
        let entry = scores.entry(mid.clone()).or_insert((0.0, false, false));
        entry.0 += contribution;
        entry.2 = true; // from_graph
    }

    // Collect and sort by score descending
    let mut results: Vec<RrfResult> = scores
        .into_iter()
        .map(|(mid, (score, from_vector, from_graph))| RrfResult {
            memory_id: mid,
            score,
            from_vector,
            from_graph,
        })
        .collect();

    results.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
    results.truncate(limit);
    results
}

/// Batch RRF fusion for multiple queries at once.
///
/// Args:
///   queries: Vec of (vector_ids, graph_ids) pairs.
///   vector_weight, graph_weight, k_rrf, limit: same as rrf_fuse.
///
/// Returns:
///   Vec of Vec<RrfResult>, one per query.
#[cfg(feature = "python")]
#[pyfunction]
#[pyo3(signature = (queries, vector_weight=0.6, graph_weight=0.4, k_rrf=60, limit=10))]
pub fn rrf_fuse_batch(
    queries: Vec<(Vec<String>, Vec<String>)>,
    vector_weight: f64,
    graph_weight: f64,
    k_rrf: usize,
    limit: usize,
) -> Vec<Vec<RrfResult>> {
    queries
        .into_iter()
        .map(|(vec_ids, graph_ids)| {
            rrf_fuse(vec_ids, graph_ids, vector_weight, graph_weight, k_rrf, limit)
        })
        .collect()
}
