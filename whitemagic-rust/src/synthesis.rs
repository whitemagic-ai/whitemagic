//! Synthesis Engine - Rust Acceleration Layer
//! 
//! High-performance backends for:
//! - Serendipity sampling (weighted random from large datasets)
//! - Pattern correlation (cross-engine matching)
//! - Spatial clustering (KD-tree based sub-clustering)
//! - Tag normalization (fast string similarity)

use pyo3::prelude::*;
use rayon::prelude::*;
use std::collections::HashMap;
use rand::prelude::*;
use rand::distributions::WeightedIndex;

/// Weighted random sampling for serendipity engine
#[pyfunction]
pub fn weighted_sample(
    ids: Vec<String>,
    weights: Vec<f64>,
    count: usize,
    seed: Option<u64>,
) -> PyResult<Vec<String>> {
    if ids.len() != weights.len() {
        return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
            "ids and weights must have same length"
        ));
    }
    
    if ids.is_empty() {
        return Ok(vec![]);
    }
    
    let mut rng = match seed {
        Some(s) => StdRng::seed_from_u64(s),
        None => StdRng::from_entropy(),
    };
    
    // Normalize weights
    let total: f64 = weights.iter().sum();
    let normalized: Vec<f64> = weights.iter().map(|w| w / total).collect();
    
    // Create weighted distribution
    let dist = match WeightedIndex::new(&normalized) {
        Ok(d) => d,
        Err(_) => return Ok(vec![]),
    };
    
    // Sample without replacement
    let mut sampled = Vec::new();
    let mut used = std::collections::HashSet::new();
    
    while sampled.len() < count && sampled.len() < ids.len() {
        let idx = dist.sample(&mut rng);
        if !used.contains(&idx) {
            used.insert(idx);
            sampled.push(ids[idx].clone());
        }
    }
    
    Ok(sampled)
}

/// Fast string similarity for tag normalization (Levenshtein distance)
#[pyfunction]
pub fn find_similar_strings(
    strings: Vec<String>,
    max_distance: usize,
) -> PyResult<Vec<(String, String, usize)>> {
    let results: Vec<(String, String, usize)> = strings
        .par_iter()
        .enumerate()
        .flat_map(|(i, s1)| {
            strings[i+1..]
                .iter()
                .filter_map(|s2| {
                    let dist = levenshtein_distance(s1, s2);
                    if dist <= max_distance && dist > 0 {
                        Some((s1.clone(), s2.clone(), dist))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect();
    
    Ok(results)
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    
    if len1 == 0 { return len2; }
    if len2 == 0 { return len1; }
    
    let s1: Vec<char> = s1.chars().collect();
    let s2: Vec<char> = s2.chars().collect();
    
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];
    
    for i in 0..=len1 { matrix[i][0] = i; }
    for j in 0..=len2 { matrix[0][j] = j; }
    
    for i in 1..=len1 {
        for j in 1..=len2 {
            let cost = if s1[i-1] == s2[j-1] { 0 } else { 1 };
            matrix[i][j] = std::cmp::min(
                std::cmp::min(matrix[i-1][j] + 1, matrix[i][j-1] + 1),
                matrix[i-1][j-1] + cost
            );
        }
    }
    
    matrix[len1][len2]
}

/// Parallel pattern correlation across engines
#[pyfunction]
pub fn correlate_patterns(
    patterns: Vec<(String, String, Vec<String>)>, // (id, engine, keywords)
    min_overlap: usize,
) -> PyResult<Vec<(String, String, usize)>> {
    let correlations: Vec<(String, String, usize)> = patterns
        .par_iter()
        .enumerate()
        .flat_map(|(i, (id1, engine1, kw1))| {
            patterns[i+1..]
                .iter()
                .filter_map(|(id2, engine2, kw2)| {
                    if engine1 == engine2 { return None; }
                    
                    let overlap: usize = kw1.iter()
                        .filter(|k| kw2.contains(k))
                        .count();
                    
                    if overlap >= min_overlap {
                        Some((id1.clone(), id2.clone(), overlap))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect();
    
    Ok(correlations)
}

/// Fast 4D spatial clustering using grid-based approach
#[pyfunction]
pub fn spatial_cluster(
    points: Vec<(String, f64, f64, f64, f64)>, // (id, x, y, z, w)
    grid_size: f64,
) -> PyResult<HashMap<String, Vec<String>>> {
    let mut clusters: HashMap<String, Vec<String>> = HashMap::new();
    
    for (id, x, y, z, w) in points {
        // Grid cell key
        let gx = (x / grid_size).floor() as i32;
        let gy = (y / grid_size).floor() as i32;
        let gz = (z / grid_size).floor() as i32;
        let gw = (w / grid_size).floor() as i32;
        let key = format!("{}_{}_{}_{}", gx, gy, gz, gw);
        
        clusters.entry(key).or_insert_with(Vec::new).push(id);
    }
    
    Ok(clusters)
}

/// Batch gravity calculation for memory importance
#[pyfunction]
pub fn batch_calculate_gravity(
    memories: Vec<(String, usize, usize, usize, String)>, // (id, content_len, access_count, age_days, mem_type)
) -> PyResult<Vec<(String, f64)>> {
    let results: Vec<(String, f64)> = memories
        .par_iter()
        .map(|(id, content_len, access_count, age_days, mem_type)| {
            let mut gravity = 0.3; // Base
            
            // Content richness (0-0.15)
            gravity += (*content_len as f64 / 2000.0).min(0.15);
            
            // Access frequency (0-0.15)
            gravity += ((*access_count as f64).ln() * 0.05).min(0.15);
            
            // Recency bonus (0-0.1)
            gravity += if *age_days < 7 { 0.1 } else if *age_days < 30 { 0.05 } else { 0.0 };
            
            // Type bonus
            gravity += if mem_type == "LONG_TERM" { 0.1 } else { 0.0 };
            
            (id.clone(), gravity.min(1.0))
        })
        .collect();
    
    Ok(results)
}

/// Module registration
pub fn register(_py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(weighted_sample, m)?)?;
    m.add_function(wrap_pyfunction!(find_similar_strings, m)?)?;
    m.add_function(wrap_pyfunction!(correlate_patterns, m)?)?;
    m.add_function(wrap_pyfunction!(spatial_cluster, m)?)?;
    m.add_function(wrap_pyfunction!(batch_calculate_gravity, m)?)?;
    Ok(())
}
