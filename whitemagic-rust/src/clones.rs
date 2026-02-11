///
/// Clone Army - Rust Implementation
///
/// Provides massively parallel search capabilities using Rayon.
///

use std::path::Path;
use std::fs;
use rayon::prelude::*;
use walkdir::WalkDir;
use aho_corasick::AhoCorasick;

#[derive(Debug, Clone)]
pub struct SearchMatch {
    pub file_path: String,
    pub line_number: usize,
    pub content: String,
    pub relevance: f64,
    pub context: String,
}

/// Search a single file for query terms using Aho-Corasick
fn search_file(path: &Path, ac: &AhoCorasick, term_count: usize) -> Option<Vec<SearchMatch>> {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return None,
    };

    let mut matches = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let lower_content = content.to_lowercase();
    let lower_lines: Vec<&str> = lower_content.lines().collect();

    for (i, line) in lower_lines.iter().enumerate() {
        // Count unique matches in the line
        let hit_count = ac.find_iter(line).count();

        if hit_count > 0 {
            // Rough relevance: matches / term_count (capped at 1.0)
            let relevance = (hit_count as f64 / term_count as f64).min(1.0);
            
            // Get context (2 lines before and after)
            let start = if i >= 2 { i - 2 } else { 0 };
            let end = std::cmp::min(lines.len(), i + 3);
            let context = lines[start..end].join("\n");

            matches.push(SearchMatch {
                file_path: path.to_string_lossy().into_owned(),
                line_number: i + 1,
                content: lines[i].to_string(), // Original case
                relevance,
                context,
            });
        }
    }

    if matches.is_empty() {
        None
    } else {
        Some(matches)
    }
}

/// Deploy the clone army (parallel search)
pub fn deploy_search(
    root_path: &Path, 
    query: &str, 
    extensions: &[String],
    max_results: usize
) -> Vec<SearchMatch> {
    let query_lower = query.to_lowercase();
    let query_terms: Vec<&str> = query_lower.split_whitespace().collect();

    if query_terms.is_empty() {
        return Vec::new();
    }

    // Build Aho-Corasick automaton for O(n) multi-pattern search
    let ac = match AhoCorasick::new(&query_terms) {
        Ok(ac) => ac,
        Err(_) => return Vec::new(),
    };

    // 1. Gather all candidate files
    let files: Vec<_> = WalkDir::new(root_path)
        .into_iter()
        .filter_entry(|e| {
            let path = e.path();
            let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            
            // Exclude hidden directories and common vendor folders
            if e.file_type().is_dir() {
                if file_name.starts_with('.') && file_name != "." {
                    return false;
                }
                if file_name == "node_modules" || file_name == "target" || file_name == "__pycache__" || file_name == "venv" || file_name == ".venv" {
                    return false;
                }
            }
            true
        })
        .filter_map(|e| e.ok())
        .filter(|e| {
            if !e.file_type().is_file() {
                return false;
            }
            
            // Check extension
            if let Some(ext) = e.path().extension() {
                if let Some(ext_str) = ext.to_str() {
                    return extensions.contains(&ext_str.to_string()) || extensions.contains(&"*".to_string());
                }
            }
            false
        })
        .map(|e| e.path().to_owned())
        .collect();

    // 2. Parallel processing with Rayon
    let mut all_matches: Vec<SearchMatch> = files.par_iter()
        .filter_map(|path| search_file(path, &ac, query_terms.len()))
        .flatten()
        .collect();

    // 3. Sort by relevance (descending)
    all_matches.sort_by(|a, b| b.relevance.partial_cmp(&a.relevance).unwrap_or(std::cmp::Ordering::Equal));

    all_matches.truncate(max_results);
    all_matches
}
