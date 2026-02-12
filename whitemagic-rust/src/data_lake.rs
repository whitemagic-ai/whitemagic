//! Data Lake - High-speed pattern extraction across entire codebase
//! 
//! This module enables scanning tens of thousands of files in seconds,
//! extracting patterns, and building comprehensive indices.

use pyo3::prelude::*;
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use regex::Regex;
use walkdir::WalkDir;
use lazy_static::lazy_static;

lazy_static! {
    static ref DEF_REGEX: Regex = Regex::new(r"^(def|class|async def)\s+(\w+)").expect("DEF_REGEX");
    static ref WORD_REGEX: Regex = Regex::new(r"\b[a-zA-Z_][a-zA-Z0-9_]*\b").expect("WORD_REGEX");
    static ref TODO_REGEX: Regex = Regex::new(r"(?i)(TODO|FIXME|NOTE|HACK|XXX):\s*(.*)").expect("TODO_REGEX");
}

const DEFAULT_MAX_FILE_SIZE: usize = 2_000_000;

fn default_extensions() -> Vec<String> {
    vec![
        "py", "rs", "md", "txt", "json", "yaml", "yml", "toml", "js", "ts", "tsx", "jsx", "c",
        "cpp", "h", "hpp", "sh",
    ]
    .into_iter()
    .map(|ext| ext.to_string())
    .collect()
}

fn normalize_extensions(extensions: Option<Vec<String>>) -> Vec<String> {
    match extensions {
        Some(exts) if !exts.is_empty() => exts,
        _ => default_extensions(),
    }
}

/// Scan entire directory tree and extract all patterns
#[pyfunction]
pub fn scan_codebase(
    root_path: String,
    extensions: Vec<String>,
    max_file_size: usize,
) -> PyResult<(usize, usize, Vec<(String, String, usize)>)> {
    let root = PathBuf::from(&root_path);
    let mut all_files: Vec<PathBuf> = Vec::new();
    
    // Collect all matching files
    collect_files(&root, &extensions, &mut all_files);
    
    let total_files = all_files.len();
    
    // Process files in parallel
    let results: Vec<(String, String, usize)> = all_files
        .par_iter()
        .filter_map(|path| {
            if let Ok(metadata) = fs::metadata(path) {
                if metadata.len() as usize > max_file_size {
                    return None;
                }
            }
            
            if let Ok(content) = fs::read_to_string(path) {
                let lines = content.lines().count();
                let rel_path = path.strip_prefix(&root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string();
                
                // Extract first line as preview
                let preview = content.lines().next().unwrap_or("").to_string();
                
                Some((rel_path, preview, lines))
            } else {
                None
            }
        })
        .collect();
    
    let total_lines: usize = results.iter().map(|(_, _, l)| l).sum();
    
    Ok((total_files, total_lines, results))
}

/// Extract all function/class definitions from Python files
#[pyfunction]
pub fn extract_definitions(
    root_path: String,
) -> PyResult<Vec<(String, String, String, usize)>> {
    let root = PathBuf::from(&root_path);
    let mut py_files: Vec<PathBuf> = Vec::new();
    collect_files(&root, &vec!["py".to_string()], &mut py_files);
    
    let results: Vec<(String, String, String, usize)> = py_files
        .par_iter()
        .flat_map(|path| {
            let mut defs = Vec::new();
            if let Ok(content) = fs::read_to_string(path) {
                let rel_path = path.strip_prefix(&root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string();

                for (line_num, line) in content.lines().enumerate() {
                    if let Some(caps) = DEF_REGEX.captures(line) {
                        if let (Some(kind_match), Some(name_match)) = (caps.get(1), caps.get(2)) {
                            let kind = kind_match.as_str().to_string();
                            let name = name_match.as_str().to_string();
                            defs.push((rel_path.clone(), kind, name, line_num + 1));
                        }
                    }
                }
            }
            defs
        })
        .collect();
    
    Ok(results)
}

/// Build word frequency index across all files
#[pyfunction]
pub fn build_word_index(
    root_path: String,
    extensions: Vec<String>,
    min_word_length: usize,
) -> PyResult<Vec<(String, usize)>> {
    let root = PathBuf::from(&root_path);
    let mut files: Vec<PathBuf> = Vec::new();
    collect_files(&root, &extensions, &mut files);
    
    // Parallel word counting
    let word_counts: HashMap<String, usize> = files
        .par_iter()
        .map(|path| {
            let mut local_counts: HashMap<String, usize> = HashMap::new();
            if let Ok(content) = fs::read_to_string(path) {
                for cap in WORD_REGEX.find_iter(&content) {
                    let word = cap.as_str().to_lowercase();
                    if word.len() >= min_word_length {
                        *local_counts.entry(word).or_insert(0) += 1;
                    }
                }
            }
            local_counts
        })
        .reduce(HashMap::new, |mut acc, map| {
            for (word, count) in map {
                *acc.entry(word).or_insert(0) += count;
            }
            acc
        });
    
    // Sort by frequency
    let mut sorted: Vec<(String, usize)> = word_counts.into_iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(&a.1));
    
    Ok(sorted)
}

/// Fast file search across a directory tree
#[pyfunction]
#[pyo3(signature = (root_path, pattern, max_results=100, extensions=None, max_file_size=None))]
pub fn fast_search(
    root_path: String,
    pattern: String,
    max_results: usize,
    extensions: Option<Vec<String>>,
    max_file_size: Option<usize>,
) -> PyResult<Vec<(String, f64)>> {
    let root = PathBuf::from(&root_path);
    let regex = Regex::new(&pattern).ok();
    let extensions = normalize_extensions(extensions);
    let max_file_size = max_file_size.unwrap_or(DEFAULT_MAX_FILE_SIZE);
    let mut files: Vec<PathBuf> = Vec::new();
    
    for entry in WalkDir::new(&root).into_iter().filter_map(|e| e.ok()) {
        if entry.file_type().is_file() {
            let path = entry.path();
            if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                if !extensions.iter().any(|e| e == ext) {
                    continue;
                }
            } else {
                continue;
            }
            files.push(path.to_path_buf());
        }
    }
    
    let mut results: Vec<(String, f64)> = files
        .par_iter()
        .filter_map(|path| {
            if let Ok(metadata) = fs::metadata(path) {
                if metadata.len() as usize > max_file_size {
                    return None;
                }
            }
            let content = fs::read_to_string(path).ok()?;
            let score = if let Some(re) = &regex {
                re.find_iter(&content).count() as f64
            } else {
                content.matches(&pattern).count() as f64
            };
            
            if score > 0.0 {
                let rel_path = path.strip_prefix(&root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string();
                Some((rel_path, score))
            } else {
                None
            }
        })
        .collect();
    
    results.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    results.truncate(max_results);
    
    Ok(results)
}

/// Find all files containing a pattern (parallel grep)
#[pyfunction]
#[pyo3(signature = (root_path, pattern, extensions=None, context_lines=0, max_file_size=None))]
pub fn parallel_grep(
    root_path: String,
    pattern: String,
    extensions: Option<Vec<String>>,
    context_lines: usize,
    max_file_size: Option<usize>,
) -> PyResult<Vec<(String, usize, String)>> {
    let root = PathBuf::from(&root_path);
    let mut files: Vec<PathBuf> = Vec::new();
    let extensions = normalize_extensions(extensions);
    collect_files(&root, &extensions, &mut files);
    
    let regex = match Regex::new(&pattern) {
        Ok(r) => r,
        Err(_) => return Ok(vec![]),
    };
    let max_file_size = max_file_size.unwrap_or(DEFAULT_MAX_FILE_SIZE);
    
    let results: Vec<(String, usize, String)> = files
        .par_iter()
        .flat_map(|path| {
            let mut matches = Vec::new();
            if let Ok(metadata) = fs::metadata(path) {
                if metadata.len() as usize > max_file_size {
                    return matches;
                }
            }
            if let Ok(content) = fs::read_to_string(path) {
                let rel_path = path.strip_prefix(&root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string();
                
                let lines: Vec<&str> = content.lines().collect();
                for (i, line) in lines.iter().enumerate() {
                    if regex.is_match(line) {
                        // Get context
                        let start = i.saturating_sub(context_lines);
                        let end = (i + context_lines + 1).min(lines.len());
                        let context: String = lines[start..end].join("\n");
                        matches.push((rel_path.clone(), i + 1, context));
                    }
                }
            }
            matches
        })
        .collect();
    
    Ok(results)
}

/// Extract all TODO/FIXME/NOTE comments
#[pyfunction]
pub fn extract_todos(root_path: String) -> PyResult<Vec<(String, usize, String, String)>> {
    let root = PathBuf::from(&root_path);
    let mut files: Vec<PathBuf> = Vec::new();
    collect_files(&root, &vec!["py".to_string(), "rs".to_string(), "js".to_string(), "ts".to_string()], &mut files);
    
    let results: Vec<(String, usize, String, String)> = files
        .par_iter()
        .flat_map(|path| {
            let mut todos = Vec::new();
            if let Ok(content) = fs::read_to_string(path) {
                let rel_path = path.strip_prefix(&root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string();

                for (line_num, line) in content.lines().enumerate() {
                    if let Some(caps) = TODO_REGEX.captures(line) {
                        if let (Some(kind_match), Some(text_match)) = (caps.get(1), caps.get(2)) {
                            let kind = kind_match.as_str().to_uppercase();
                            let text = text_match.as_str().trim().to_string();
                            todos.push((rel_path.clone(), line_num + 1, kind, text));
                        }
                    }
                }
            }
            todos
        })
        .collect();
    
    Ok(results)
}

// Helper function to collect files recursively
fn collect_files(dir: &Path, extensions: &[String], files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // Skip hidden directories and common ignores
                let name = path.file_name().unwrap_or_default().to_string_lossy();
                let name_str = name.as_ref();
                let parent_name = path.parent()
                    .and_then(|parent| parent.file_name())
                    .unwrap_or_default()
                    .to_string_lossy();
                let skip_dir =
                    name_str.starts_with('.') ||
                    name_str.starts_with("whitemagic_backup_") ||
                    name_str == "bitnet" ||
                    (name_str == "whitemagic" && parent_name == "whitemagic-mcp") ||
                    matches!(
                        name_str,
                        "node_modules" |
                        "__pycache__" |
                        "target" |
                        ".git" |
                        "build" |
                        "dist" |
                        "tmp" |
                        "htmlcov" |
                        "logs" |
                        "data" |
                        "reports" |
                        "archive" |
                        "backups" |
                        "venv" |
                        ".venv" |
                        "whitemagic-public"
                    );
                if !skip_dir {
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
