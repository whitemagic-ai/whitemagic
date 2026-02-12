//! 5D Holographic Encoder — Batch coordinate encoding for WhiteMagic memories
//!
//! Ports the hot path from Python's `intelligence/hologram/encoder.py`:
//! - X: Logic ↔ Emotion (-1.0 to +1.0)
//! - Y: Micro ↔ Macro (-1.0 to +1.0)
//! - Z: Time / Chronos (-1.0 to +1.0)
//! - W: Importance / Gravity (0.0 to 2.0+)
//! - V: Vitality / Galactic Distance (0.0=edge to 1.0=core)
//!
//! The Python encoder processes memories one-at-a-time. This Rust version
//! uses Rayon to encode batches in parallel — critical for galactic sweeps
//! over 107K+ memories.

use pyo3::prelude::*;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Signal keywords for X-axis (Logic ↔ Emotion)
// ---------------------------------------------------------------------------

const LOGIC_KEYWORDS: &[&str] = &[
    "algorithm", "function", "class", "method", "variable", "parameter",
    "return", "import", "module", "interface", "struct", "enum", "type",
    "compile", "binary", "integer", "boolean", "array", "hash", "queue",
    "stack", "tree", "graph", "node", "edge", "index", "query", "schema",
    "database", "table", "column", "row", "key", "value", "config",
    "deploy", "build", "test", "debug", "error", "exception", "log",
];

const EMOTION_KEYWORDS: &[&str] = &[
    "feel", "believe", "hope", "fear", "love", "hate", "joy", "anger",
    "sadness", "surprise", "trust", "anticipation", "dream", "wish",
    "inspire", "motivate", "passion", "creative", "intuition", "vision",
    "soul", "spirit", "heart", "beauty", "harmony", "peace", "balance",
    "wisdom", "insight", "revelation", "epiphany", "grateful", "wonder",
    "meaning", "purpose", "destiny", "journey", "growth", "transform",
];

// ---------------------------------------------------------------------------
// Signal keywords for Y-axis (Micro ↔ Macro)
// ---------------------------------------------------------------------------

const MICRO_KEYWORDS: &[&str] = &[
    "byte", "bit", "char", "pixel", "cell", "atom", "detail", "specific",
    "particular", "individual", "single", "one", "local", "narrow", "small",
    "minor", "trivial", "line", "character", "token", "symbol", "field",
];

const MACRO_KEYWORDS: &[&str] = &[
    "system", "architecture", "framework", "platform", "ecosystem",
    "infrastructure", "enterprise", "global", "universal", "broad",
    "comprehensive", "holistic", "strategic", "vision", "roadmap",
    "philosophy", "paradigm", "worldview", "civilization", "cosmos",
];

// ---------------------------------------------------------------------------
// Garden→Element mapping for blending
// ---------------------------------------------------------------------------

fn garden_element(garden: &str) -> &'static str {
    match garden.to_lowercase().as_str() {
        "wood" | "east" => "wood",
        "fire" | "south" => "fire",
        "earth" | "center" => "earth",
        "metal" | "west" => "metal",
        "water" | "north" => "water",
        _ => "earth",
    }
}

fn element_bias(element: &str) -> [f64; 5] {
    // [x_bias, y_bias, z_bias, w_mult, v_mult]
    match element {
        "wood" => [0.1, 0.15, 0.0, 1.0, 1.0],   // Growth → slightly macro, creative
        "fire" => [0.2, 0.1, 0.05, 1.1, 1.0],    // Passion → emotional, important
        "earth" => [0.0, 0.0, 0.0, 1.0, 1.0],    // Neutral center
        "metal" => [-0.15, -0.1, 0.0, 1.0, 1.05], // Precision → logical, micro
        "water" => [0.05, 0.05, -0.1, 0.95, 1.1], // Deep → slightly old, vital
        _ => [0.0, 0.0, 0.0, 1.0, 1.0],
    }
}

// ---------------------------------------------------------------------------
// Core encoding functions
// ---------------------------------------------------------------------------

/// Input for a single memory to encode
#[derive(Debug, Clone, Deserialize)]
pub struct MemoryInput {
    pub id: String,
    pub content: String,
    #[serde(default)]
    pub importance: f64,
    #[serde(default)]
    pub access_count: u32,
    #[serde(default)]
    pub age_days: f64,
    #[serde(default)]
    pub galactic_distance: f64,
    #[serde(default)]
    pub garden: String,
    #[serde(default)]
    pub tags: Vec<String>,
}

/// Output coordinate for a single memory
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Coordinate5D {
    pub id: String,
    pub x: f64,
    pub y: f64,
    pub z: f64,
    pub w: f64,
    pub v: f64,
}

/// Count keyword matches in text (case-insensitive)
#[inline]
fn count_keywords(text: &str, keywords: &[&str]) -> usize {
    let lower = text.to_lowercase();
    keywords.iter().filter(|kw| lower.contains(**kw)).count()
}

/// Calculate X: Logic (-1) ↔ Emotion (+1)
fn calculate_x(content: &str) -> f64 {
    let logic_count = count_keywords(content, LOGIC_KEYWORDS) as f64;
    let emotion_count = count_keywords(content, EMOTION_KEYWORDS) as f64;
    let total = logic_count + emotion_count;
    if total == 0.0 {
        return 0.0;
    }
    // Normalize to [-1, 1]: positive = emotional, negative = logical
    ((emotion_count - logic_count) / total).clamp(-1.0, 1.0)
}

/// Calculate Y: Micro (-1) ↔ Macro (+1)
fn calculate_y(content: &str) -> f64 {
    let micro_count = count_keywords(content, MICRO_KEYWORDS) as f64;
    let macro_count = count_keywords(content, MACRO_KEYWORDS) as f64;
    let total = micro_count + macro_count;
    if total == 0.0 {
        return 0.0;
    }
    ((macro_count - micro_count) / total).clamp(-1.0, 1.0)
}

/// Calculate Z: Time/Chronos — newer = positive, older = negative
fn calculate_z(age_days: f64) -> f64 {
    // Sigmoid-like mapping: 0 days → +1.0, 365 days → ~0.0, very old → -1.0
    if age_days <= 0.0 {
        return 1.0;
    }
    let normalized = 1.0 - (2.0 / (1.0 + (-age_days / 180.0).exp()));
    normalized.clamp(-1.0, 1.0)
}

/// Calculate W: Importance/Gravity
fn calculate_w(importance: f64, access_count: u32, content_len: usize) -> f64 {
    let base = importance.max(0.5);
    let access_boost = (access_count as f64 / 10.0).min(0.5);
    let richness_boost = if content_len > 500 { 0.2 } else if content_len > 200 { 0.1 } else { 0.0 };
    (base + access_boost + richness_boost).min(2.5)
}

/// Calculate V: Vitality — based on galactic distance (0=edge, 1=core)
fn calculate_v(galactic_distance: f64, access_count: u32, importance: f64) -> f64 {
    // Invert galactic distance: core memories (low distance) have high vitality
    let distance_vitality = 1.0 - galactic_distance.clamp(0.0, 1.0);
    let access_vitality = (access_count as f64 / 20.0).min(0.3);
    let importance_vitality = (importance - 0.5).max(0.0) * 0.2;
    (distance_vitality + access_vitality + importance_vitality).clamp(0.0, 1.0)
}

/// Encode a single memory into 5D coordinates
pub fn encode_memory(mem: &MemoryInput) -> Coordinate5D {
    let mut x = calculate_x(&mem.content);
    let mut y = calculate_y(&mem.content);
    let mut z = calculate_z(mem.age_days);
    let mut w = calculate_w(mem.importance, mem.access_count, mem.content.len());
    let mut v = calculate_v(mem.galactic_distance, mem.access_count, mem.importance);

    // Apply garden/element blending
    if !mem.garden.is_empty() {
        let element = garden_element(&mem.garden);
        let bias = element_bias(element);
        x = (x + bias[0]).clamp(-1.0, 1.0);
        y = (y + bias[1]).clamp(-1.0, 1.0);
        z = (z + bias[2]).clamp(-1.0, 1.0);
        w *= bias[3];
        v *= bias[4];
    }

    Coordinate5D {
        id: mem.id.clone(),
        x,
        y,
        z,
        w: w.min(2.5),
        v: v.clamp(0.0, 1.0),
    }
}

/// Batch encode memories in parallel using Rayon
pub fn encode_batch(memories: &[MemoryInput]) -> Vec<Coordinate5D> {
    memories.par_iter().map(encode_memory).collect()
}

/// Compute weighted 5D distance between two coordinates
#[inline]
pub fn distance_5d(a: &Coordinate5D, b: &Coordinate5D, weights: &[f64; 5]) -> f64 {
    let dx = (a.x - b.x) * weights[0];
    let dy = (a.y - b.y) * weights[1];
    let dz = (a.z - b.z) * weights[2];
    let dw = (a.w - b.w) * weights[3];
    let dv = (a.v - b.v) * weights[4];
    (dx * dx + dy * dy + dz * dz + dw * dw + dv * dv).sqrt()
}

// ---------------------------------------------------------------------------
// Python bindings
// ---------------------------------------------------------------------------

/// Batch encode memories from JSON input.
/// Input: JSON array of MemoryInput objects.
/// Output: JSON array of Coordinate5D objects.
#[pyfunction]
pub fn holographic_encode_batch(memories_json: &str) -> PyResult<String> {
    let memories: Vec<MemoryInput> = serde_json::from_str(memories_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let coords = encode_batch(&memories);

    serde_json::to_string(&coords)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Encode a single memory from JSON.
#[pyfunction]
pub fn holographic_encode_single(memory_json: &str) -> PyResult<String> {
    let mem: MemoryInput = serde_json::from_str(memory_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse: {}", e)))?;

    let coord = encode_memory(&mem);

    serde_json::to_string(&coord)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

/// Compute 5D distances from a query point to a batch of coordinates.
/// Returns JSON array of (id, distance) sorted by distance ascending.
#[pyfunction]
pub fn holographic_nearest_5d(
    query_json: &str,
    coords_json: &str,
    k: usize,
    weights_json: Option<&str>,
) -> PyResult<String> {
    let query: Coordinate5D = serde_json::from_str(query_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse query: {}", e)))?;
    let coords: Vec<Coordinate5D> = serde_json::from_str(coords_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse coords: {}", e)))?;

    let weights: [f64; 5] = if let Some(wj) = weights_json {
        serde_json::from_str(wj)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("JSON parse weights: {}", e)))?
    } else {
        [1.0, 1.0, 1.0, 1.0, 1.0]
    };

    let mut distances: Vec<(String, f64)> = coords
        .par_iter()
        .map(|c| (c.id.clone(), distance_5d(&query, c, &weights)))
        .collect();

    distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    distances.truncate(k);

    serde_json::to_string(&distances)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialize: {}", e)))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_x_logical() {
        let text = "This function returns a class method with algorithm parameters";
        let x = calculate_x(text);
        assert!(x < 0.0, "Logical text should have negative X: {}", x);
    }

    #[test]
    fn test_calculate_x_emotional() {
        let text = "I feel inspired by this creative vision of beauty and harmony";
        let x = calculate_x(text);
        assert!(x > 0.0, "Emotional text should have positive X: {}", x);
    }

    #[test]
    fn test_calculate_y_micro() {
        let text = "Check the byte at position and each individual character token";
        let y = calculate_y(text);
        assert!(y < 0.0, "Micro text should have negative Y: {}", y);
    }

    #[test]
    fn test_calculate_y_macro() {
        let text = "The system architecture needs a comprehensive global framework";
        let y = calculate_y(text);
        assert!(y > 0.0, "Macro text should have positive Y: {}", y);
    }

    #[test]
    fn test_calculate_z_fresh() {
        let z = calculate_z(0.0);
        assert!(z > 0.5, "Fresh memory should have high Z: {}", z);
    }

    #[test]
    fn test_calculate_z_old() {
        let z = calculate_z(365.0);
        assert!(z < 0.0, "Old memory should have negative Z: {}", z);
    }

    #[test]
    fn test_encode_batch_parallel() {
        let memories: Vec<MemoryInput> = (0..100)
            .map(|i| MemoryInput {
                id: format!("mem_{}", i),
                content: format!("Memory number {} about algorithm and function", i),
                importance: 0.7,
                access_count: i as u32,
                age_days: i as f64 * 3.0,
                galactic_distance: 0.5,
                garden: "wood".to_string(),
                tags: vec![],
            })
            .collect();

        let coords = encode_batch(&memories);
        assert_eq!(coords.len(), 100);
        // All should have valid ranges
        for c in &coords {
            assert!(c.x >= -1.0 && c.x <= 1.0);
            assert!(c.y >= -1.0 && c.y <= 1.0);
            assert!(c.z >= -1.0 && c.z <= 1.0);
            assert!(c.w >= 0.0 && c.w <= 2.5);
            assert!(c.v >= 0.0 && c.v <= 1.0);
        }
    }

    #[test]
    fn test_distance_5d() {
        let a = Coordinate5D { id: "a".into(), x: 0.0, y: 0.0, z: 0.0, w: 1.0, v: 0.5 };
        let b = Coordinate5D { id: "b".into(), x: 1.0, y: 0.0, z: 0.0, w: 1.0, v: 0.5 };
        let weights = [1.0, 1.0, 1.0, 1.0, 1.0];
        let dist = distance_5d(&a, &b, &weights);
        assert!((dist - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_garden_blending() {
        let mut mem = MemoryInput {
            id: "test".into(),
            content: "neutral content".into(),
            importance: 0.5,
            access_count: 0,
            age_days: 0.0,
            galactic_distance: 0.5,
            garden: String::new(),
            tags: vec![],
        };

        let no_garden = encode_memory(&mem);
        mem.garden = "fire".to_string();
        let with_fire = encode_memory(&mem);

        // Fire element should shift X positive (emotional)
        assert!(with_fire.x > no_garden.x, "Fire should increase X");
    }
}
