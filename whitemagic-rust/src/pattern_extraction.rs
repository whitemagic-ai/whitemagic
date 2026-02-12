///
/// Pattern Extraction Engine for WhiteMagic v2.3.1
/// 
/// Analyzes long-term memories to extract:
/// - Common solutions (when X happens, do Y)
/// - Anti-patterns (avoid Z because...)
/// - Heuristics (if condition, then action)
/// - Optimization strategies (proven approaches)
///

use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use pyo3::prelude::*;


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pattern {
    pub pattern_type: PatternType,
    pub title: String,
    pub description: String,
    pub examples: Vec<String>,
    pub confidence: f64,
    pub frequency: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternType {
    Solution,      // "When X, do Y"
    AntiPattern,   // "Never do Z"
    Heuristic,     // "If condition, then action"
    Optimization,  // "Approach A is faster than B"
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PatternReport {
    pub total_memories: usize,
    pub patterns_found: usize,
    pub solutions: Vec<Pattern>,
    pub anti_patterns: Vec<Pattern>,
    pub heuristics: Vec<Pattern>,
    pub optimizations: Vec<Pattern>,
    pub duration_seconds: f64,
}

/// Python wrapper for pattern extraction
#[pyfunction(name = "extract_patterns")]
pub fn extract_patterns_py(
    long_term_dir: String,
    min_confidence: Option<f64>,
) -> PyResult<(usize, usize, Vec<String>, Vec<String>, Vec<String>, Vec<String>, f64)> {
    let min_confidence = min_confidence.unwrap_or(0.7);
    let report = extract_patterns(Path::new(&long_term_dir), min_confidence)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e))?;
    
    let solutions = report.solutions.iter().map(|p| p.description.clone()).collect();
    let anti_patterns = report.anti_patterns.iter().map(|p| p.description.clone()).collect();
    let heuristics = report.heuristics.iter().map(|p| p.description.clone()).collect();
    let optimizations = report.optimizations.iter().map(|p| p.description.clone()).collect();
    
    Ok((
        report.total_memories,
        report.patterns_found,
        solutions,
        anti_patterns,
        heuristics,
        optimizations,
        report.duration_seconds,
    ))
}

/// Python wrapper for in-memory pattern extraction
#[pyfunction(name = "extract_patterns_from_content")]
pub fn extract_patterns_from_content_py(
    memories: Vec<String>,
    min_confidence: Option<f64>,
) -> PyResult<(usize, usize, Vec<String>, Vec<String>, Vec<String>, Vec<String>, f64)> {
    let min_confidence = min_confidence.unwrap_or(0.7);
    let report = extract_patterns_from_content(memories, min_confidence)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e))?;
    
    let solutions = report.solutions.iter().map(|p| p.description.clone()).collect();
    let anti_patterns = report.anti_patterns.iter().map(|p| p.description.clone()).collect();
    let heuristics = report.heuristics.iter().map(|p| p.description.clone()).collect();
    let optimizations = report.optimizations.iter().map(|p| p.description.clone()).collect();
    
    Ok((
        report.total_memories,
        report.patterns_found,
        solutions,
        anti_patterns,
        heuristics,
        optimizations,
        report.duration_seconds,
    ))
}


/// Extract all patterns from long-term memories (Directory based)
pub fn extract_patterns(
    long_term_dir: &Path,
    min_confidence: f64
) -> Result<PatternReport, String> {
    // Load all long-term memories
    let memories = load_memories(long_term_dir)?;
    extract_patterns_from_content(memories, min_confidence)
}

/// Extract all patterns from list of memory contents
pub fn extract_patterns_from_content(
    memories: Vec<String>,
    min_confidence: f64
) -> Result<PatternReport, String> {
    let start = std::time::Instant::now();
    let total = memories.len();
    
    // Extract patterns in parallel
    let all_patterns: Vec<Pattern> = memories
        .par_iter()
        .flat_map(|content| extract_from_memory(content))
        .collect();
    
    // Filter by confidence
    let filtered: Vec<Pattern> = all_patterns
        .into_iter()
        .filter(|p| p.confidence >= min_confidence)
        .collect();
    
    // Group by type
    let mut solutions = Vec::new();
    let mut anti_patterns = Vec::new();
    let mut heuristics = Vec::new();
    let mut optimizations = Vec::new();
    
    for pattern in filtered {
        match pattern.pattern_type {
            PatternType::Solution => solutions.push(pattern),
            PatternType::AntiPattern => anti_patterns.push(pattern),
            PatternType::Heuristic => heuristics.push(pattern),
            PatternType::Optimization => optimizations.push(pattern),
        }
    }
    
    let patterns_found = solutions.len() + anti_patterns.len() + 
                        heuristics.len() + optimizations.len();
    
    Ok(PatternReport {
        total_memories: total,
        patterns_found,
        solutions,
        anti_patterns,
        heuristics,
        optimizations,
        duration_seconds: start.elapsed().as_secs_f64(),
    })
}


/// Load all memory files
fn load_memories(dir: &Path) -> Result<Vec<String>, String> {
    let entries: Vec<String> = fs::read_dir(dir)
        .map_err(|e| format!("Failed to read directory: {}", e))?
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .and_then(|s| s.to_str())
                == Some("md")
        })
        .filter_map(|e| fs::read_to_string(e.path()).ok())
        .collect();
    
    Ok(entries)
}


/// Extract patterns from a single memory
fn extract_from_memory(content: &str) -> Vec<Pattern> {
    let mut patterns = Vec::new();
    
    // Extract solutions
    patterns.extend(extract_solutions(content));
    
    // Extract anti-patterns
    patterns.extend(extract_anti_patterns(content));
    
    // Extract heuristics
    patterns.extend(extract_heuristics(content));
    
    // Extract optimizations
    patterns.extend(extract_optimizations(content));
    
    patterns
}


/// Extract solution patterns ("when X, do Y")
fn extract_solutions(content: &str) -> Vec<Pattern> {
    let mut solutions = Vec::new();
    let _lower = content.to_lowercase();
    
    // Look for solution keywords
    let solution_indicators = [
        ("solved", "fix", "solution"),
        ("worked", "success", "resolved"),
        ("approach", "method", "technique"),
    ];
    
    for line in content.lines() {
        let line_lower = line.to_lowercase();
        
        // Check if line contains solution pattern
        for (indicator1, indicator2, indicator3) in &solution_indicators {
            if line_lower.contains(indicator1) || 
               line_lower.contains(indicator2) || 
               line_lower.contains(indicator3) {
                
                // Extract context  
                let char_len = line.chars().count();
                if char_len > 20 && char_len < 200 {
                    solutions.push(Pattern {
                        pattern_type: PatternType::Solution,
                        title: extract_title(line),
                        description: line.trim().to_string(),
                        examples: vec![line.trim().to_string()],
                        confidence: calculate_confidence(line, &["solved", "worked", "success"]),
                        frequency: 1,
                    });
                }
                break;
            }
        }
    }
    
    // Deduplicate similar solutions
    deduplicate_patterns(solutions)
}


/// Extract anti-patterns ("never do X", "avoid Y")
fn extract_anti_patterns(content: &str) -> Vec<Pattern> {
    let mut anti_patterns = Vec::new();
    let _lower = content.to_lowercase();
    
    let warning_keywords = [
        "never", "don't", "avoid", "prevent", "error", "bug", "failed", "wrong"
    ];
    
    for line in content.lines() {
        let line_lower = line.to_lowercase();
        
        for keyword in &warning_keywords {
            let char_len = line.chars().count();
            if line_lower.contains(keyword) && char_len > 20 && char_len < 200 {
                anti_patterns.push(Pattern {
                    pattern_type: PatternType::AntiPattern,
                    title: format!("Avoid: {}", extract_title(line)),
                    description: line.trim().to_string(),
                    examples: vec![line.trim().to_string()],
                    confidence: calculate_confidence(line, &warning_keywords),
                    frequency: 1,
                });
                break;
            }
        }
    }
    
    deduplicate_patterns(anti_patterns)
}


/// Extract heuristics ("if X, then Y")
fn extract_heuristics(content: &str) -> Vec<Pattern> {
    let mut heuristics = Vec::new();
    
    let heuristic_patterns = [
        "if", "when", "whenever", "in case", "should", "recommend"
    ];
    
    for line in content.lines() {
        let line_lower = line.to_lowercase();
        
        for pattern in &heuristic_patterns {
            let char_len = line.chars().count();
            if line_lower.contains(pattern) && char_len > 30 && char_len < 200 {
                heuristics.push(Pattern {
                    pattern_type: PatternType::Heuristic,
                    title: extract_title(line),
                    description: line.trim().to_string(),
                    examples: vec![line.trim().to_string()],
                    confidence: calculate_confidence(line, &heuristic_patterns),
                    frequency: 1,
                });
                break;
            }
        }
    }
    
    deduplicate_patterns(heuristics)
}


/// Extract optimization patterns ("X is faster than Y")
fn extract_optimizations(content: &str) -> Vec<Pattern> {
    let mut optimizations = Vec::new();
    let _lower = content.to_lowercase();
    
    let optimization_keywords = [
        "faster", "slower", "optimiz", "performance", "speed", "efficien"
    ];
    
    for line in content.lines() {
        let line_lower = line.to_lowercase();
        
        for keyword in &optimization_keywords {
            let char_len = line.chars().count();
            if line_lower.contains(keyword) && char_len > 30 && char_len < 200 {
                optimizations.push(Pattern {
                    pattern_type: PatternType::Optimization,
                    title: extract_title(line),
                    description: line.trim().to_string(),
                    examples: vec![line.trim().to_string()],
                    confidence: calculate_confidence(line, &optimization_keywords),
                    frequency: 1,
                });
                break;
            }
        }
    }
    
    deduplicate_patterns(optimizations)
}


/// Extract a title from a line of text
fn extract_title(line: &str) -> String {
    // Remove markdown formatting
    let clean = line
        .trim()
        .trim_start_matches('#')
        .trim_start_matches('*')
        .trim_start_matches('-')
        .trim();
    
    // Take first 80 chars (UTF-8 safe)
    let char_count: usize = clean.chars().count();
    if char_count > 80 {
        let truncated: String = clean.chars().take(77).collect();
        format!("{}...", truncated)
    } else {
        clean.to_string()
    }
}


/// Calculate confidence score based on keyword presence
fn calculate_confidence(text: &str, keywords: &[&str]) -> f64 {
    let lower = text.to_lowercase();
    let mut score = 0.5; // Base confidence
    
    // Check keyword density
    let matches = keywords.iter().filter(|k| lower.contains(*k)).count();
    score += matches as f64 * 0.1;
    
    // Length indicates detail
    if text.len() > 50 {
        score += 0.1;
    }
    if text.len() > 100 {
        score += 0.1;
    }
    
    // Punctuation indicates complete thought
    if text.contains('.') || text.contains('!') {
        score += 0.1;
    }
    
    score.min(1.0)
}


/// Deduplicate similar patterns
fn deduplicate_patterns(patterns: Vec<Pattern>) -> Vec<Pattern> {
    let mut unique: HashMap<String, Pattern> = HashMap::new();
    
    for pattern in patterns {
        let key = pattern.title.to_lowercase();
        
        if let Some(existing) = unique.get_mut(&key) {
            // Merge with existing
            existing.frequency += 1;
            existing.confidence = (existing.confidence + pattern.confidence) / 2.0;
            existing.examples.extend(pattern.examples);
        } else {
            unique.insert(key, pattern);
        }
    }
    
    unique.into_values().collect()
}


/// Find cause-effect relationships in text
pub fn find_relationships(content: &str) -> Vec<(String, String)> {
    let mut relationships = Vec::new();
    
    let cause_effect_patterns = [
        ("because", "therefore"),
        ("since", "so"),
        ("if", "then"),
        ("when", "result"),
    ];
    
    for line in content.lines() {
        let lower = line.to_lowercase();
        
        for (cause_word, _effect_word) in &cause_effect_patterns {
            if lower.contains(cause_word) {
                // Try to extract cause and effect
                if let Some(cause_idx) = lower.find(cause_word) {
                    let before = &line[..cause_idx].trim();
                    let after = &line[cause_idx + cause_word.len()..].trim();
                    
                    if !before.is_empty() && !after.is_empty() {
                        relationships.push((
                            before.to_string(),
                            after.to_string()
                        ));
                    }
                }
            }
        }
    }
    
    relationships
}
