///
/// Auto-consolidation system for WhiteMagic v2.3.1
/// 
/// Takes 60 short-term memories and consolidates into 30 best long-term memories
/// using parallel processing and similarity detection.
///

use pyo3::prelude::*;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryMetadata {
    pub action_count: usize,
    pub timestamp: String,
    pub tags: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScoredMemory {
    pub id: String,
    pub metadata: MemoryMetadata,
    pub content: String,
    pub importance_score: f64,
    pub novelty_score: f64,
    pub reusability_score: f64,
    pub total_score: f64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MemoryCluster {
    pub representative: ScoredMemory,
    pub similar_memories: Vec<ScoredMemory>,
    pub merged_content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ConsolidationReport {
    pub short_term_count: usize,
    pub long_term_created: usize,
    pub archived_count: usize,
    pub clusters_found: usize,
    pub duration_seconds: f64,
    pub top_memories: Vec<String>,
    // List of (Merged Content, List of Source IDs)
    pub clusters: Vec<(String, Vec<String>)>,
}

use crate::zig_bridge::ZigTransientAllocator;

/// Python wrapper for directory-based memory consolidation
#[pyfunction]
pub fn consolidate_memories(
    short_term_dir: String,
    top_n: Option<usize>,
    similarity_threshold: Option<f64>,
) -> PyResult<(usize, usize, usize, f64, Vec<String>)> {
    // Initialize Zig allocator for this thread/operation
    ZigTransientAllocator::init();
    
    let top_n = top_n.unwrap_or(20);
    let similarity_threshold = similarity_threshold.unwrap_or(0.7);
    let report = auto_consolidate(
        Path::new(&short_term_dir),
        top_n,
        similarity_threshold,
    )
    .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e))?;
    
    // Reset allocator after use
    ZigTransientAllocator::reset();
    
    Ok((
        report.short_term_count,
        report.long_term_created,
        report.clusters_found,
        report.duration_seconds,
        report.top_memories,
    ))
}

/// Python wrapper for in-memory consolidation
#[pyfunction(name = "consolidate_memories_from_content")]
pub fn consolidate_memories_from_content_py(
    memories: Vec<(String, String)>, // (id, content)
    top_n: Option<usize>,
    similarity_threshold: Option<f64>,
) -> PyResult<(usize, usize, usize, f64, Vec<String>, Vec<(String, Vec<String>)>)> {
    let top_n = top_n.unwrap_or(20);
    let similarity_threshold = similarity_threshold.unwrap_or(0.7);
    
    let start = Instant::now();
    
    // Convert inputs to ScoredMemory
    let scored_memories: Vec<ScoredMemory> = memories
        .into_par_iter()
        .map(|(id, content)| score_memory(id, content))
        .collect();

    let report = process_memories(scored_memories, top_n, similarity_threshold, start)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e))?;

    Ok((
        report.short_term_count,
        report.long_term_created,
        report.clusters_found,
        report.duration_seconds,
        report.top_memories,
        report.clusters,
    ))
}


/// Load all short-term memories in parallel (Directory based)
pub fn load_memories_parallel(dir: &Path) -> Result<Vec<ScoredMemory>, String> {
    let entries: Vec<PathBuf> = fs::read_dir(dir)
        .map_err(|e| format!("Failed to read directory: {}", e))?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("md"))
        .collect();
    
    // Parallel loading
    let memories: Vec<ScoredMemory> = entries
        .par_iter()
        .filter_map(|path| load_and_score_memory(path).ok())
        .collect();
    
    Ok(memories)
}


/// Load a single memory file and compute scores
fn load_and_score_memory(path: &Path) -> Result<ScoredMemory, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
    
    let id = path.file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown")
        .to_string();

    Ok(score_memory(id, content))
}

/// Score a memory based on its content
fn score_memory(id: String, content: String) -> ScoredMemory {
    // Extract metadata if present
    let metadata = extract_metadata(&content).unwrap_or(MemoryMetadata {
        action_count: 0,
        timestamp: String::new(),
        tags: Vec::new(),
    });
    
    // Compute scores
    let importance_score = score_importance(&content);
    let novelty_score = score_novelty(&content);
    let reusability_score = score_reusability(&content);
    
    let total_score = (importance_score + novelty_score + reusability_score) / 3.0;
    
    ScoredMemory {
        id,
        metadata,
        content,
        importance_score,
        novelty_score,
        reusability_score,
        total_score,
    }
}


/// Extract metadata from YAML frontmatter
fn extract_metadata(content: &str) -> Result<MemoryMetadata, String> {
    let lines: Vec<&str> = content.lines().collect();
    
    // Simple frontmatter parsing (assumes YAML between ---)
    let mut in_frontmatter = false;
    let mut action_count = 0;
    let mut timestamp = String::new();
    let mut tags = Vec::new();
    
    for line in lines {
        let trimmed = line.trim();
        
        if trimmed == "---" {
            if in_frontmatter {
                break; // End of frontmatter
            }
            in_frontmatter = true;
            continue;
        }
        
        if in_frontmatter {
            if trimmed.starts_with("action_count:") {
                if let Some(value) = trimmed.split(':').nth(1) {
                    action_count = value.trim().parse().unwrap_or(0);
                }
            } else if trimmed.starts_with("timestamp:") {
                if let Some(value) = trimmed.split(':').nth(1) {
                    timestamp = value.trim().to_string();
                }
            } else if trimmed.starts_with("tags:") {
                // Simple tags extraction
                if let Some(value) = trimmed.split(':').nth(1) {
                    let tags_str = value.trim().trim_matches(|c| c == '[' || c == ']');
                    tags = tags_str.split(',')
                        .map(|s| s.trim().trim_matches('"').to_string())
                        .collect();
                }
            }
        }
    }
    
    Ok(MemoryMetadata {
        action_count,
        timestamp,
        tags,
    })
}


/// Score memory importance (did it change direction?)
fn score_importance(content: &str) -> f64 {
    let lower = content.to_lowercase();
    let mut score: f64 = 0.5; // Base score
    
    // Keywords indicating importance
    if lower.contains("critical") || lower.contains("breakthrough") {
        score += 0.3;
    }
    if lower.contains("decision") || lower.contains("chose") {
        score += 0.2;
    }
    if lower.contains("changed") || lower.contains("pivoted") {
        score += 0.2;
    }
    if lower.contains("error") || lower.contains("bug") {
        score += 0.15;
    }
    
    score.min(1.0)
}


/// Score novelty (did we learn something new?)
fn score_novelty(content: &str) -> f64 {
    let lower = content.to_lowercase();
    let mut score: f64 = 0.5;
    
    if lower.contains("learned") || lower.contains("discovered") {
        score += 0.3;
    }
    if lower.contains("new") || lower.contains("novel") {
        score += 0.2;
    }
    if lower.contains("insight") || lower.contains("realization") {
        score += 0.2;
    }
    
    score.min(1.0)
}


/// Score reusability (will we use this again?)
fn score_reusability(content: &str) -> f64 {
    let lower = content.to_lowercase();
    let mut score: f64 = 0.5;
    
    if lower.contains("pattern") || lower.contains("approach") {
        score += 0.3;
    }
    if lower.contains("solution") || lower.contains("fix") {
        score += 0.2;
    }
    if lower.contains("template") || lower.contains("framework") {
        score += 0.2;
    }
    
    // Length indicates detail (more reusable)
    if content.len() > 1000 {
        score += 0.1;
    }
    
    score.min(1.0)
}


/// Select top N memories by total score
pub fn select_top(mut memories: Vec<ScoredMemory>, n: usize) -> Vec<ScoredMemory> {
    memories.sort_by(|a, b| b.total_score.partial_cmp(&a.total_score).unwrap_or(std::cmp::Ordering::Equal));
    memories.into_iter().take(n).collect()
}


/// Find similar memories using Jaccard similarity
pub fn cluster_similar(memories: &[ScoredMemory], threshold: f64) -> Vec<MemoryCluster> {
    let mut clusters: Vec<MemoryCluster> = Vec::new();
    let mut used: Vec<bool> = vec![false; memories.len()];
    
    for i in 0..memories.len() {
        if used[i] {
            continue;
        }
        
        let mut cluster = MemoryCluster {
            representative: memories[i].clone(),
            similar_memories: Vec::new(),
            merged_content: String::new(),
        };
        
        used[i] = true;
        
        // Find similar memories
        for j in (i + 1)..memories.len() {
            if used[j] {
                continue;
            }
            
            let similarity = jaccard_similarity(&memories[i].content, &memories[j].content);
            
            if similarity >= threshold {
                cluster.similar_memories.push(memories[j].clone());
                used[j] = true;
            }
        }
        
        // Merge content
        cluster.merged_content = merge_similar_memories(&cluster);
        
        clusters.push(cluster);
    }
    
    clusters
}


/// Compute Jaccard similarity between two texts
fn jaccard_similarity(text1: &str, text2: &str) -> f64 {
    let words1: Vec<&str> = text1.split_whitespace().collect();
    let words2: Vec<&str> = text2.split_whitespace().collect();
    
    let set1: std::collections::HashSet<_> = words1.iter().collect();
    let set2: std::collections::HashSet<_> = words2.iter().collect();
    
    let intersection = set1.intersection(&set2).count() as f64;
    let union = set1.union(&set2).count() as f64;
    
    if union == 0.0 {
        0.0
    } else {
        intersection / union
    }
}


/// Merge similar memories into consolidated content
fn merge_similar_memories(cluster: &MemoryCluster) -> String {
    let mut merged = cluster.representative.content.clone();
    
    if !cluster.similar_memories.is_empty() {
        merged.push_str("\n\n## Related Memories\n\n");
        
        for (i, mem) in cluster.similar_memories.iter().enumerate() {
            merged.push_str(&format!("### Similar Memory {}\n", i + 1));
            
            // Extract key points (lines with keywords)
            let lines: Vec<&str> = mem.content.lines()
                .filter(|l| {
                    let lower = l.to_lowercase();
                    lower.contains("learned") || 
                    lower.contains("outcome") ||
                    lower.contains("next")
                })
                .collect();
            
            for line in lines {
                merged.push_str(&format!("{}\n", line));
            }
            merged.push('\n');
        }
    }
    
    merged
}


/// Main consolidation function (Directory based)
pub fn auto_consolidate(
    short_term_dir: &Path,
    top_n: usize,
    similarity_threshold: f64
) -> Result<ConsolidationReport, String> {
    let start = std::time::Instant::now();
    
    // 1. Load all short-term memories
    let memories = load_memories_parallel(short_term_dir)?;
    
    process_memories(memories, top_n, similarity_threshold, start)
}

/// Core consolidation logic (shared)
pub fn process_memories(
    memories: Vec<ScoredMemory>,
    top_n: usize,
    similarity_threshold: f64,
    start_time: std::time::Instant,
) -> Result<ConsolidationReport, String> {
    let short_term_count = memories.len();
    
    // 2. Select top N by score
    let top_memories = select_top(memories, top_n);
    
    // 3. Cluster similar memories
    let clusters = cluster_similar(&top_memories, similarity_threshold);
    
    // 4. Create report
    let report = ConsolidationReport {
        short_term_count,
        long_term_created: clusters.len(),
        archived_count: short_term_count,
        clusters_found: clusters.len(),
        duration_seconds: start_time.elapsed().as_secs_f64(),
        top_memories: top_memories.iter()
            .map(|m| m.id.clone())
            .collect(),
        clusters: clusters.iter().map(|c| {
            (
                c.merged_content.clone(),
                c.similar_memories.iter().map(|m| m.id.clone()).collect()
            )
        }).collect(),
    };
    
    Ok(report)
}
