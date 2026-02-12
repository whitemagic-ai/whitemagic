//! Embedding Cache - Pre-computed vector representations
//! 
//! Uses simple hash-based embeddings for fast similarity search.
//! For production, integrate with sentence-transformers or similar.

use pyo3::prelude::*;
use rayon::prelude::*;
use std::fs;
use std::path::PathBuf;

/// Simple hash-based embedding (for demonstration)
/// In production, use sentence-transformers
fn simple_embed(text: &str, dim: usize) -> Vec<f32> {
    let mut embedding = vec![0.0f32; dim];
    
    // Hash each word and distribute across dimensions
    for word in text.split_whitespace() {
        let hash = word.bytes().fold(0u64, |acc, b| {
            acc.wrapping_mul(31).wrapping_add(b as u64)
        });
        
        for i in 0..dim {
            let idx = ((hash >> (i % 8)) as usize) % dim;
            embedding[idx] += 1.0 / (1.0 + (i as f32));
        }
    }
    
    // Normalize
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    if norm > 0.0 {
        for v in &mut embedding {
            *v /= norm;
        }
    }
    
    embedding
}

/// Cosine similarity between two vectors
fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() {
        return 0.0;
    }
    
    let dot: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let norm_a: f32 = a.iter().map(|x| x * x).sum::<f32>().sqrt();
    let norm_b: f32 = b.iter().map(|x| x * x).sum::<f32>().sqrt();
    
    if norm_a > 0.0 && norm_b > 0.0 {
        dot / (norm_a * norm_b)
    } else {
        0.0
    }
}

/// Build embedding cache for all files in directory
#[pyfunction]
pub fn build_embedding_cache(
    root_path: String,
    extensions: Vec<String>,
    embedding_dim: usize,
) -> PyResult<Vec<(String, Vec<f32>)>> {
    let root = PathBuf::from(&root_path);
    let mut files: Vec<PathBuf> = Vec::new();
    
    collect_files(&root, &extensions, &mut files);
    
    let embeddings: Vec<(String, Vec<f32>)> = files
        .par_iter()
        .filter_map(|path| {
            if let Ok(content) = fs::read_to_string(path) {
                let rel_path = path.strip_prefix(&root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string();
                
                let embedding = simple_embed(&content, embedding_dim);
                Some((rel_path, embedding))
            } else {
                None
            }
        })
        .collect();
    
    Ok(embeddings)
}

/// Find similar files using embedding similarity
#[pyfunction]
pub fn find_similar(
    query_text: String,
    embeddings: Vec<(String, Vec<f32>)>,
    embedding_dim: usize,
    top_k: usize,
) -> PyResult<Vec<(String, f32)>> {
    let query_embedding = simple_embed(&query_text, embedding_dim);
    
    let mut similarities: Vec<(String, f32)> = embeddings
        .par_iter()
        .map(|(path, emb)| {
            let sim = cosine_similarity(&query_embedding, emb);
            (path.clone(), sim)
        })
        .collect();
    
    // Sort by similarity (descending)
    similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    
    // Return top-k
    Ok(similarities.into_iter().take(top_k).collect())
}

/// Build similarity matrix for all files
#[pyfunction]
pub fn build_similarity_matrix(
    embeddings: Vec<(String, Vec<f32>)>,
    threshold: f32,
) -> PyResult<Vec<(String, String, f32)>> {
    let n = embeddings.len();
    
    // Parallel similarity computation
    let pairs: Vec<(String, String, f32)> = (0..n)
        .into_par_iter()
        .flat_map(|i| {
            let mut local_pairs = Vec::new();
            for j in (i + 1)..n {
                let sim = cosine_similarity(&embeddings[i].1, &embeddings[j].1);
                if sim >= threshold {
                    local_pairs.push((
                        embeddings[i].0.clone(),
                        embeddings[j].0.clone(),
                        sim,
                    ));
                }
            }
            local_pairs
        })
        .collect();
    
    Ok(pairs)
}

// Helper to collect files
fn collect_files(dir: &std::path::Path, extensions: &[String], files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().unwrap_or_default().to_string_lossy();
                if !name.starts_with('.') && 
                   name != "node_modules" && 
                   name != "__pycache__" &&
                   name != "target" {
                    collect_files(&path, extensions, files);
                }
            } else if path.is_file() {
                if let Some(ext) = path.extension() {
                    if extensions.iter().any(|e| e == ext.to_string_lossy().as_ref()) {
                        files.push(path);
                    }
                }
            }
        }
    }
}

// SIMD-accelerated dot product (when available)
#[cfg(target_arch = "x86_64")]
pub fn simd_dot_product(a: &[f32], b: &[f32]) -> f32 {
    // For now, use standard implementation
    // Full SIMD requires nightly features or explicit intrinsics
    a.iter().zip(b.iter()).map(|(x, y)| x * y).sum()
}

/// Batch similarity computation (SIMD-ready)
#[pyfunction]
pub fn batch_similarities(
    query_embedding: Vec<f32>,
    all_embeddings: Vec<Vec<f32>>,
) -> PyResult<Vec<f32>> {
    let similarities: Vec<f32> = all_embeddings
        .par_iter()
        .map(|emb| cosine_similarity(&query_embedding, emb))
        .collect();
    
    Ok(similarities)
}
