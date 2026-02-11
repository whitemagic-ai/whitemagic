//! Pattern Extraction Engine - Rust Implementation
//!
//! High-performance pattern extraction from memory content.
//! Detects: solutions, anti-patterns, heuristics, optimizations

use pyo3::prelude::*;
use rayon::prelude::*;
use std::time::Instant;

/// Keywords for pattern classification.
/// Kept specific: common words ("if", "when", "better") were generating
/// tens of thousands of false positives and have been removed.
const SOLUTION_KEYWORDS: &[&str] = &[
    "solution",
    "solved",
    "resolved",
    "the fix",
    "fixed by",
    "the answer",
    "how to",
    "workaround",
    "the trick",
];

const ANTI_PATTERN_KEYWORDS: &[&str] = &[
    "avoid",
    "pitfall",
    "gotcha",
    "anti-pattern",
    "bad practice",
    "don't do",
    "never do",
    "mistake",
    "beware",
    "wrong approach",
    "footgun",
    "trap",
];

const HEURISTIC_KEYWORDS: &[&str] = &[
    "rule of thumb",
    "best practice",
    "guideline",
    "principle",
    "heuristic",
    "recommend",
    "convention",
    "idiom",
    "always",
    "prefer",
    "should",
    "must",
];

const OPTIMIZATION_KEYWORDS: &[&str] = &[
    "optimize",
    "performance",
    "bottleneck",
    "overhead",
    "cache",
    "parallel",
    "async",
    "latency",
    "throughput",
    "complexity",
    "batch processing",
    "reduce overhead",
];

/// Extract patterns from a single content string
fn extract_from_content(
    content: &str,
    min_confidence: f64,
) -> (Vec<String>, Vec<String>, Vec<String>, Vec<String>) {
    let mut solutions = Vec::new();
    let mut anti_patterns = Vec::new();
    let mut heuristics = Vec::new();
    let mut optimizations = Vec::new();

    // Split into sentences for granular extraction
    let sentences: Vec<&str> = content
        .split(|c| c == '.' || c == '\n')
        .filter(|s| s.len() > 20) // Filter out tiny fragments
        .collect();

    for sentence in sentences {
        let sentence_lower = sentence.to_lowercase();
        // Count keyword matches for confidence scoring
        let solution_score: usize = SOLUTION_KEYWORDS
            .iter()
            .filter(|k| sentence_lower.contains(*k))
            .count();
        let anti_score: usize = ANTI_PATTERN_KEYWORDS
            .iter()
            .filter(|k| sentence_lower.contains(*k))
            .count();
        let heuristic_score: usize = HEURISTIC_KEYWORDS
            .iter()
            .filter(|k| sentence_lower.contains(*k))
            .count();
        let opt_score: usize = OPTIMIZATION_KEYWORDS
            .iter()
            .filter(|k| sentence_lower.contains(*k))
            .count();

        // Classify by highest score
        let max_score = solution_score
            .max(anti_score)
            .max(heuristic_score)
            .max(opt_score);

        if max_score > 0 {
            let confidence = ((max_score as f64) / 3.0).min(1.0);

            if confidence >= min_confidence {
                let trimmed = sentence.trim().to_string();

                if solution_score == max_score && !solutions.contains(&trimmed) {
                    solutions.push(trimmed);
                } else if anti_score == max_score && !anti_patterns.contains(&trimmed) {
                    anti_patterns.push(trimmed);
                } else if heuristic_score == max_score && !heuristics.contains(&trimmed) {
                    heuristics.push(trimmed);
                } else if opt_score == max_score && !optimizations.contains(&trimmed) {
                    optimizations.push(trimmed);
                }
            }
        }
    }

    (solutions, anti_patterns, heuristics, optimizations)
}

/// Extract patterns from content list (Python callable)
///
/// Returns: (total_memories, patterns_found, solutions, anti_patterns, heuristics, optimizations, duration_secs)
#[pyfunction]
pub fn extract_patterns_from_content(
    contents: Vec<String>,
    min_confidence: f64,
) -> PyResult<(
    usize,
    usize,
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
    f64,
)> {
    let start = Instant::now();
    let total = contents.len();

    // Parallel extraction using rayon
    let results: Vec<_> = contents
        .par_iter()
        .map(|content| extract_from_content(content, min_confidence))
        .collect();

    // Aggregate results
    let mut all_solutions = Vec::new();
    let mut all_anti = Vec::new();
    let mut all_heuristics = Vec::new();
    let mut all_opts = Vec::new();

    for (solutions, anti, heuristics, opts) in results {
        all_solutions.extend(solutions);
        all_anti.extend(anti);
        all_heuristics.extend(heuristics);
        all_opts.extend(opts);
    }

    // Deduplicate
    all_solutions.sort();
    all_solutions.dedup();
    all_anti.sort();
    all_anti.dedup();
    all_heuristics.sort();
    all_heuristics.dedup();
    all_opts.sort();
    all_opts.dedup();

    let found = all_solutions.len() + all_anti.len() + all_heuristics.len() + all_opts.len();
    let duration = start.elapsed().as_secs_f64();

    Ok((
        total,
        found,
        all_solutions,
        all_anti,
        all_heuristics,
        all_opts,
        duration,
    ))
}

/// Score pattern strings by average IDF weight of their constituent words.
/// Patterns with rare vocabulary (high IDF) score higher than common ones.
///
/// # Args
/// - `patterns` — the pattern strings to score
/// - `idf_words` — vocabulary words (lowercased)
/// - `idf_scores` — IDF scores aligned 1:1 with `idf_words`
///
/// # Returns
/// Vec of (pattern_text, avg_idf_score), same order as input `patterns`.
#[pyfunction]
pub fn score_patterns_idf(
    patterns: Vec<String>,
    idf_words: Vec<String>,
    idf_scores: Vec<f64>,
) -> PyResult<Vec<(String, f64)>> {
    use std::collections::HashMap;

    let idf_map: HashMap<String, f64> = idf_words.into_iter().zip(idf_scores.into_iter()).collect();

    let scored: Vec<(String, f64)> = patterns
        .into_iter()
        .map(|p| {
            let idf_vals: Vec<f64> = p
                .split_whitespace()
                .filter_map(|w| {
                    let clean: String = w
                        .chars()
                        .filter(|c| c.is_alphanumeric())
                        .collect::<String>()
                        .to_lowercase();
                    if clean.is_empty() {
                        None
                    } else {
                        idf_map.get(&clean).copied()
                    }
                })
                .collect();
            let avg = if idf_vals.is_empty() {
                0.0
            } else {
                idf_vals.iter().sum::<f64>() / idf_vals.len() as f64
            };
            (p, avg)
        })
        .collect();

    Ok(scored)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_solution() {
        let contents = vec![
            "The solution was to add a cache layer.".to_string(),
            "Avoid using global variables.".to_string(),
        ];

        let (total, found, solutions, anti, _, _opts, _) =
            extract_patterns_from_content(contents, 0.1).unwrap();

        assert_eq!(total, 2);
        assert!(found > 0);
        assert!(!solutions.is_empty());
        assert!(!anti.is_empty());
    }
}
