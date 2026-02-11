use pyo3::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::fs::File;
use std::io::{BufRead, BufReader};

// Reuse the Coordinate Logic from Python -> Rust Port
// Simple deterministic hashing for coords using simple mixing (no external crate needed for simple distribution)
fn get_coords(text: &str) -> [f64; 4] {
    let mut h: u64 = 0xcbf29ce484222325;
    for byte in text.bytes() {
        h = h ^ (byte as u64);
        h = h.wrapping_mul(0x100000001b3);
    }

    // Split 64-bit hash into 4 x 16-bit chunks
    let h1 = (h & 0xFFFF) as f64;
    let h2 = ((h >> 16) & 0xFFFF) as f64;
    let h3 = ((h >> 32) & 0xFFFF) as f64;
    let h4 = ((h >> 48) & 0xFFFF) as f64;

    // Normalize to -1.0 .. 1.0
    let x = (h1 / 65535.0) * 2.0 - 1.0;
    let y = (h2 / 65535.0) * 2.0 - 1.0;
    let z = (h3 / 65535.0) * 2.0 - 1.0;
    let w = (h4 / 65535.0) * 2.0 - 1.0;

    [x, y, z, w]
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Star {
    pattern: String,
    domain: String,
    confidence: f64,
    file: String,
    file_path: String,
    coords: [f64; 4],
    score: f64,
}

// For min-heap to keep top N
#[derive(Debug)]
struct ScoredStar {
    score: f64,
    star: Star,
}

impl PartialEq for ScoredStar {
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score
    }
}
impl Eq for ScoredStar {}

// We want a MIN heap, so we order by score.
// Standard BinaryHeap is max-heap.
// If we want to keep TOP N, we use a Min Heap of size N.
// If new item > min_heap.peek(), pop min and push new.
impl PartialOrd for ScoredStar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Reverse ordering for Min-Heap behavior?
        // Actually, let's use standard ordering and BinaryHeap as MinHeap by using Reverse wrapper
        // OR just simple:
        self.score.partial_cmp(&other.score)
    }
}
impl Ord for ScoredStar {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

// Reversed wrapper for MinHeap
#[derive(Debug)]
struct MinScoredStar(ScoredStar);

impl PartialEq for MinScoredStar {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl Eq for MinScoredStar {}
impl PartialOrd for MinScoredStar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Reverse order so smallest is at top
        other.0.partial_cmp(&self.0)
    }
}
impl Ord for MinScoredStar {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

/// Cluster stars into K constellations (Simple K-Means)
fn cluster_stars(stars: Vec<Star>, k: usize) -> Vec<(String, Vec<Star>)> {
    if stars.is_empty() {
        return vec![];
    }

    // Init centroids
    let mut centroids: Vec<[f64; 4]> = stars.iter().take(k).map(|s| s.coords).collect();

    // If not enough stars, duplicate last
    while centroids.len() < k {
        centroids.push(centroids.last().cloned().unwrap_or([0.0; 4]));
    }

    let mut assignments: Vec<Vec<Star>> = vec![vec![]; k];

    // 5 Iterations of K-Means is usually enough for rough semantic grouping
    for _ in 0..5 {
        // Clear assignments
        for vec in &mut assignments {
            vec.clear();
        }

        // Assign
        for star in &stars {
            let mut min_dist = f64::MAX;
            let mut best_idx = 0;

            for (i, c) in centroids.iter().enumerate() {
                let d = (star.coords[0] - c[0]).powi(2)
                    + (star.coords[1] - c[1]).powi(2)
                    + (star.coords[2] - c[2]).powi(2)
                    + (star.coords[3] - c[3]).powi(2);
                if d < min_dist {
                    min_dist = d;
                    best_idx = i;
                }
            }
            assignments[best_idx].push(star.clone());
        }

        // Recalculate centroids
        for (i, group) in assignments.iter().enumerate() {
            if group.is_empty() {
                continue;
            }
            let mut sum = [0.0; 4];
            for s in group {
                sum[0] += s.coords[0];
                sum[1] += s.coords[1];
                sum[2] += s.coords[2];
                sum[3] += s.coords[3];
            }
            let len = group.len() as f64;
            centroids[i] = [sum[0] / len, sum[1] / len, sum[2] / len, sum[3] / len];
        }
    }

    // Naming
    let names = vec![
        "The Weaver",
        "The Forge",
        "The Void",
        "The River",
        "The Mountain",
        "The Thunder",
        "The Wind",
        "The Lake",
        "The Fire",
        "The Earth",
    ];

    let mut results = Vec::new();
    for (i, group) in assignments.into_iter().enumerate() {
        if group.is_empty() {
            continue;
        }

        // Find dominant domain
        let mut counts = HashMap::new();
        for s in &group {
            *counts.entry(&s.domain).or_insert(0) += 1;
        }
        let top_domain = counts
            .into_iter()
            .max_by_key(|&(_, c)| c)
            .map(|(k, _)| k)
            .unwrap_or(&"unknown".to_string())
            .clone();

        let base_name = names[i % names.len()];
        let full_name = format!("{} ({})", base_name, top_domain);

        results.push((full_name, group));
    }

    results
}

#[pyfunction]
pub fn ignite_constellations(
    stream_path: String,
    keywords: Vec<String>,
    limit: usize,
    k: usize,
) -> PyResult<String> {
    let file = File::open(&stream_path)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    let reader = BufReader::new(file);

    let keywords_lower: Vec<String> = keywords.iter().map(|s| s.to_lowercase()).collect();

    // Min-Heap to track top N best scores
    let mut heap = BinaryHeap::new();

    for line in reader.lines() {
        let line_str = line.unwrap_or_default();
        if line_str.is_empty() {
            continue;
        }

        let v: Value = serde_json::from_str(&line_str).unwrap_or(Value::Null);
        if v == Value::Null {
            continue;
        }

        // Extract fields safely
        let pattern = v["pattern"].as_str().unwrap_or("").to_string();
        let domain = v["domain"].as_str().unwrap_or("").to_string();
        let file_name = v["file"].as_str().unwrap_or("").to_string();
        let file_path_str = v["file_path"].as_str().unwrap_or("").to_string();
        let confidence = v["confidence"].as_f64().unwrap_or(0.5);

        // Text for matching
        // Optimization: Don't alloc new string, check linearly
        // But for simplicity of matching logic:
        let text = format!("{} {} {}", pattern, domain, file_name).to_lowercase();

        let mut matches = 0.0;
        for kw in &keywords_lower {
            if text.contains(kw) {
                matches += 1.0;
            }
        }

        if matches > 0.0 {
            let score = matches + confidence;
            let star = Star {
                pattern,
                domain,
                confidence,
                file: file_name,
                file_path: file_path_str,
                coords: get_coords(&v["pattern"].as_str().unwrap_or("")),
                score,
            };

            let entry = MinScoredStar(ScoredStar { score, star });

            if heap.len() < limit {
                heap.push(entry);
            } else if let Some(min) = heap.peek() {
                if score > min.0.score {
                    heap.pop();
                    heap.push(entry);
                }
            }
        }
    }

    // Collected Stars
    let stars: Vec<Star> = heap.into_iter().map(|s| s.0.star).collect();

    // Cluster
    let constellations = cluster_stars(stars, k);

    // Return JSON
    // We construct a simple JSON structure manually or via serde_json
    // Vec<(Name, Vec<Star>)>

    #[derive(Serialize)]
    struct ConstellationOutput {
        name: String,
        stars: Vec<Star>,
    }

    let outputs: Vec<ConstellationOutput> = constellations
        .into_iter()
        .map(|(name, stars)| ConstellationOutput { name, stars })
        .collect();

    let json_out = serde_json::to_string(&outputs)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    Ok(json_out)
}
