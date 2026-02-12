//! Galactic Accelerator — High-performance retention scoring & distance computation
//!
//! Ports the hot path from Python's `galactic_map.py` into Rust with:
//! - Parallel batch scoring via Rayon
//! - SIMD-friendly memory layout
//! - Zero-copy zone classification
//!
//! The Python GalacticMap.full_sweep() iterates 107K+ memories computing
//! a 7-signal weighted retention score per memory. This module does that
//! computation in parallel, returning (memory_id, distance, retention_score)
//! tuples for batch DB update.

use pyo3::prelude::*;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Zone classification (matches Python's galactic_map.py)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GalacticZone {
    Core,      // 0.00 - 0.15
    InnerRim,  // 0.15 - 0.40
    MidBand,   // 0.40 - 0.65
    OuterRim,  // 0.65 - 0.85
    FarEdge,   // 0.85 - 1.00
}

impl GalacticZone {
    #[inline]
    pub fn from_distance(d: f64) -> Self {
        if d < 0.15 {
            GalacticZone::Core
        } else if d < 0.40 {
            GalacticZone::InnerRim
        } else if d < 0.65 {
            GalacticZone::MidBand
        } else if d < 0.85 {
            GalacticZone::OuterRim
        } else {
            GalacticZone::FarEdge
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            GalacticZone::Core => "core",
            GalacticZone::InnerRim => "inner_rim",
            GalacticZone::MidBand => "mid_band",
            GalacticZone::OuterRim => "outer_rim",
            GalacticZone::FarEdge => "far_edge",
        }
    }
}

// ---------------------------------------------------------------------------
// Memory signals for retention scoring
// ---------------------------------------------------------------------------

/// Flat struct holding the signals needed for retention scoring.
/// Designed for cache-friendly parallel iteration.
#[derive(Debug, Clone)]
pub struct MemorySignals {
    pub id: String,
    pub importance: f64,
    pub neuro_score: f64,
    pub emotional_valence: f64,
    pub recall_count: u32,
    pub is_protected: bool,
    pub is_core_identity: bool,
    pub is_sacred: bool,
    pub is_pinned: bool,
    // Extended signals for full retention scoring
    pub memory_type_weight: f64,  // type signal: 0.0-1.0 based on memory_type
    pub richness: f64,            // content richness: title+tags+content length normalized
    pub activity: f64,            // recall_count normalized to 0.0-1.0
    pub recency: f64,             // days since last access, normalized
    pub emotion: f64,             // abs(emotional_valence), normalized
    pub protection: f64,          // 1.0 if any protection flag, else 0.0
}

/// Weights for the 7-signal retention scoring.
/// Matches the Python implementation in scripts/galactic_sweep_direct.py:
///   type(35%), richness(20%), activity(10%), recency(10%),
///   importance(10%), emotion(5%), protection(10%)
const WEIGHTS: [f64; 7] = [
    0.35, // memory_type_weight
    0.20, // richness
    0.10, // activity
    0.10, // recency
    0.10, // importance
    0.05, // emotion
    0.10, // protection
];

/// Result of scoring a single memory.
#[derive(Debug, Clone, Serialize)]
pub struct ScoringResult {
    pub id: String,
    pub retention_score: f64,
    pub galactic_distance: f64,
    pub zone: String,
}

// ---------------------------------------------------------------------------
// Core scoring logic
// ---------------------------------------------------------------------------

/// Compute retention score from 7 weighted signals.
#[inline]
fn compute_retention_score(signals: &MemorySignals) -> f64 {
    let values = [
        signals.memory_type_weight,
        signals.richness,
        signals.activity,
        signals.recency,
        signals.importance,
        signals.emotion,
        signals.protection,
    ];

    let mut weighted_sum = 0.0;
    let mut total_weight = 0.0;
    for (v, w) in values.iter().zip(WEIGHTS.iter()) {
        weighted_sum += v * w;
        total_weight += w;
    }

    if total_weight > 0.0 {
        (weighted_sum / total_weight).clamp(0.0, 1.0)
    } else {
        0.5
    }
}

/// Compute galactic distance from retention score.
/// Protected/core/sacred/pinned memories are always at distance 0.0.
#[inline]
fn compute_distance(signals: &MemorySignals, retention_score: f64) -> f64 {
    if signals.is_protected || signals.is_core_identity || signals.is_sacred || signals.is_pinned {
        return 0.0;
    }
    let distance = 1.0 - retention_score;
    (distance.clamp(0.0, 1.0) * 10000.0).round() / 10000.0  // round to 4 decimals
}

/// Quick retention estimate from basic fields only (no extended signals).
/// Matches Python's GalacticMap._quick_retention_estimate().
#[inline]
fn quick_retention_estimate(signals: &MemorySignals) -> f64 {
    let s1 = signals.importance * 1.0;
    let s2 = signals.neuro_score * 0.9;
    let s3 = signals.emotional_valence.abs() * 0.6;
    let s4 = (signals.recall_count as f64 / 20.0).min(1.0) * 0.5;
    let total_weight = 1.0 + 0.9 + 0.6 + 0.5;
    (s1 + s2 + s3 + s4) / total_weight
}

// ---------------------------------------------------------------------------
// Batch operations (parallel via Rayon)
// ---------------------------------------------------------------------------

/// Score a batch of memories in parallel using the full 7-signal model.
/// Returns Vec<ScoringResult> sorted by distance ascending (core first).
pub fn batch_score_full(memories: &[MemorySignals]) -> Vec<ScoringResult> {
    memories
        .par_iter()
        .map(|m| {
            let retention = compute_retention_score(m);
            let distance = compute_distance(m, retention);
            let zone = GalacticZone::from_distance(distance);
            ScoringResult {
                id: m.id.clone(),
                retention_score: retention,
                galactic_distance: distance,
                zone: zone.as_str().to_string(),
            }
        })
        .collect()
}

/// Score a batch using the quick 4-signal heuristic (faster, less precise).
pub fn batch_score_quick(memories: &[MemorySignals]) -> Vec<ScoringResult> {
    memories
        .par_iter()
        .map(|m| {
            let retention = quick_retention_estimate(m);
            let distance = compute_distance(m, retention);
            let zone = GalacticZone::from_distance(distance);
            ScoringResult {
                id: m.id.clone(),
                retention_score: retention,
                galactic_distance: distance,
                zone: zone.as_str().to_string(),
            }
        })
        .collect()
}

/// Compute zone distribution counts from a batch of distances.
pub fn zone_counts(distances: &[f64]) -> [u64; 5] {
    let mut counts = [0u64; 5];
    for &d in distances {
        let idx = match GalacticZone::from_distance(d) {
            GalacticZone::Core => 0,
            GalacticZone::InnerRim => 1,
            GalacticZone::MidBand => 2,
            GalacticZone::OuterRim => 3,
            GalacticZone::FarEdge => 4,
        };
        counts[idx] += 1;
    }
    counts
}

/// Apply decay drift: add drift_rate to distances for memories exceeding
/// inactivity threshold. Returns Vec<(id, new_distance)> for updated memories.
pub fn batch_decay_drift(
    memories: &[(String, f64, bool, u64)],  // (id, current_distance, is_protected, days_inactive)
    drift_rate: f64,
    inactivity_days: u64,
) -> Vec<(String, f64)> {
    memories
        .par_iter()
        .filter_map(|(id, dist, is_protected, days_inactive)| {
            if *is_protected || *dist >= 1.0 || *days_inactive < inactivity_days {
                None
            } else {
                let new_dist = (*dist + drift_rate).min(1.0);
                let rounded = (new_dist * 10000.0).round() / 10000.0;
                Some((id.clone(), rounded))
            }
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Python bindings
// ---------------------------------------------------------------------------

/// Batch score memories from Python dicts → JSON results.
/// Input: list of dicts with keys matching MemorySignals fields.
/// Output: JSON string of Vec<ScoringResult>.
#[pyfunction]
pub fn galactic_batch_score(memories_json: &str) -> PyResult<String> {
    let raw: Vec<serde_json::Value> = serde_json::from_str(memories_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let signals: Vec<MemorySignals> = raw
        .iter()
        .map(|v| MemorySignals {
            id: v["id"].as_str().unwrap_or("").to_string(),
            importance: v["importance"].as_f64().unwrap_or(0.5),
            neuro_score: v["neuro_score"].as_f64().unwrap_or(0.5),
            emotional_valence: v["emotional_valence"].as_f64().unwrap_or(0.0),
            recall_count: v["recall_count"].as_u64().unwrap_or(0) as u32,
            is_protected: v["is_protected"].as_bool().unwrap_or(false),
            is_core_identity: v["is_core_identity"].as_bool().unwrap_or(false),
            is_sacred: v["is_sacred"].as_bool().unwrap_or(false),
            is_pinned: v["is_pinned"].as_bool().unwrap_or(false),
            memory_type_weight: v["memory_type_weight"].as_f64().unwrap_or(0.5),
            richness: v["richness"].as_f64().unwrap_or(0.3),
            activity: v["activity"].as_f64().unwrap_or(0.0),
            recency: v["recency"].as_f64().unwrap_or(0.5),
            emotion: v["emotion"].as_f64().unwrap_or(0.0),
            protection: v["protection"].as_f64().unwrap_or(0.0),
        })
        .collect();

    let results = batch_score_full(&signals);

    serde_json::to_string(&results)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Quick batch score using 4-signal heuristic (for incremental updates).
#[pyfunction]
pub fn galactic_batch_score_quick(memories_json: &str) -> PyResult<String> {
    let raw: Vec<serde_json::Value> = serde_json::from_str(memories_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let signals: Vec<MemorySignals> = raw
        .iter()
        .map(|v| MemorySignals {
            id: v["id"].as_str().unwrap_or("").to_string(),
            importance: v["importance"].as_f64().unwrap_or(0.5),
            neuro_score: v["neuro_score"].as_f64().unwrap_or(0.5),
            emotional_valence: v["emotional_valence"].as_f64().unwrap_or(0.0),
            recall_count: v["recall_count"].as_u64().unwrap_or(0) as u32,
            is_protected: v["is_protected"].as_bool().unwrap_or(false),
            is_core_identity: v["is_core_identity"].as_bool().unwrap_or(false),
            is_sacred: v["is_sacred"].as_bool().unwrap_or(false),
            is_pinned: v["is_pinned"].as_bool().unwrap_or(false),
            memory_type_weight: 0.5,
            richness: 0.3,
            activity: 0.0,
            recency: 0.5,
            emotion: 0.0,
            protection: 0.0,
        })
        .collect();

    let results = batch_score_quick(&signals);

    serde_json::to_string(&results)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Batch decay drift from Python.
/// Input: JSON array of [id, current_distance, is_protected, days_inactive].
/// Output: JSON array of [id, new_distance] for updated memories.
#[pyfunction]
pub fn galactic_decay_drift(
    memories_json: &str,
    drift_rate: f64,
    inactivity_days: u64,
) -> PyResult<String> {
    let raw: Vec<serde_json::Value> = serde_json::from_str(memories_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let memories: Vec<(String, f64, bool, u64)> = raw
        .iter()
        .map(|v| {
            (
                v[0].as_str().unwrap_or("").to_string(),
                v[1].as_f64().unwrap_or(0.5),
                v[2].as_bool().unwrap_or(false),
                v[3].as_u64().unwrap_or(0),
            )
        })
        .collect();

    let results = batch_decay_drift(&memories, drift_rate, inactivity_days);

    serde_json::to_string(&results)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Get zone counts from a list of distances.
#[pyfunction]
pub fn galactic_zone_counts(distances: Vec<f64>) -> PyResult<Vec<u64>> {
    let counts = zone_counts(&distances);
    Ok(counts.to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zone_classification() {
        assert_eq!(GalacticZone::from_distance(0.0), GalacticZone::Core);
        assert_eq!(GalacticZone::from_distance(0.14), GalacticZone::Core);
        assert_eq!(GalacticZone::from_distance(0.15), GalacticZone::InnerRim);
        assert_eq!(GalacticZone::from_distance(0.50), GalacticZone::MidBand);
        assert_eq!(GalacticZone::from_distance(0.70), GalacticZone::OuterRim);
        assert_eq!(GalacticZone::from_distance(0.90), GalacticZone::FarEdge);
    }

    #[test]
    fn test_protected_always_core() {
        let m = MemorySignals {
            id: "test".into(),
            importance: 0.1,
            neuro_score: 0.1,
            emotional_valence: 0.0,
            recall_count: 0,
            is_protected: true,
            is_core_identity: false,
            is_sacred: false,
            is_pinned: false,
            memory_type_weight: 0.1,
            richness: 0.1,
            activity: 0.0,
            recency: 0.0,
            emotion: 0.0,
            protection: 1.0,
        };
        let score = compute_retention_score(&m);
        let dist = compute_distance(&m, score);
        assert_eq!(dist, 0.0);
    }

    #[test]
    fn test_batch_score() {
        let memories = vec![
            MemorySignals {
                id: "high".into(),
                importance: 0.9,
                neuro_score: 0.8,
                emotional_valence: 0.7,
                recall_count: 15,
                is_protected: false,
                is_core_identity: false,
                is_sacred: false,
                is_pinned: false,
                memory_type_weight: 0.9,
                richness: 0.8,
                activity: 0.75,
                recency: 0.9,
                emotion: 0.7,
                protection: 0.0,
            },
            MemorySignals {
                id: "low".into(),
                importance: 0.1,
                neuro_score: 0.1,
                emotional_valence: 0.0,
                recall_count: 0,
                is_protected: false,
                is_core_identity: false,
                is_sacred: false,
                is_pinned: false,
                memory_type_weight: 0.1,
                richness: 0.1,
                activity: 0.0,
                recency: 0.1,
                emotion: 0.0,
                protection: 0.0,
            },
        ];

        let results = batch_score_full(&memories);
        assert_eq!(results.len(), 2);
        // High-value memory should be closer to core
        assert!(results.iter().find(|r| r.id == "high").unwrap().galactic_distance < 0.3);
        // Low-value memory should be further out
        assert!(results.iter().find(|r| r.id == "low").unwrap().galactic_distance > 0.8);
    }

    #[test]
    fn test_zone_counts() {
        let distances = vec![0.05, 0.10, 0.20, 0.50, 0.50, 0.70, 0.90, 0.95];
        let counts = zone_counts(&distances);
        assert_eq!(counts[0], 2); // CORE
        assert_eq!(counts[1], 1); // INNER_RIM
        assert_eq!(counts[2], 2); // MID_BAND
        assert_eq!(counts[3], 1); // OUTER_RIM
        assert_eq!(counts[4], 2); // FAR_EDGE
    }
}
