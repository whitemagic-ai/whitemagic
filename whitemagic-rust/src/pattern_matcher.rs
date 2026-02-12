//! Fast Pattern Matching Module
//! 
//! High-performance regex-based pattern matching using Rust's regex crate.
//! 10-100x faster than Python's re module for large codebases.

use pyo3::prelude::*;
use regex::Regex;
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use lazy_static::lazy_static;

/// A matched pattern with location info
#[derive(Clone)]
pub struct PatternMatch {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub matched_text: String,
    pub pattern_name: String,
}

/// Pattern definition
pub struct PatternDef {
    pub name: String,
    pub regex: Regex,
    pub description: String,
}

lazy_static! {
    static ref RE_TODO: Regex = Regex::new(r"(?i)#\s*TODO[:\s]*(.*)").expect("RE_TODO");
    static ref RE_FIXME: Regex = Regex::new(r"(?i)#\s*FIXME[:\s]*(.*)").expect("RE_FIXME");
    static ref RE_HACK: Regex = Regex::new(r"(?i)#\s*HACK[:\s]*(.*)").expect("RE_HACK");
    static ref RE_FUNC_DEF: Regex = Regex::new(r"^\s*(?:async\s+)?def\s+(\w+)\s*\(").expect("RE_FUNC_DEF");
    static ref RE_CLASS_DEF: Regex = Regex::new(r"^\s*class\s+(\w+)").expect("RE_CLASS_DEF");
    static ref RE_IMPORT: Regex = Regex::new(r"^(?:from\s+(\S+)\s+)?import\s+(.+)").expect("RE_IMPORT");
    static ref RE_DECORATOR: Regex = Regex::new(r"^\s*@(\w+)").expect("RE_DECORATOR");
    static ref RE_TYPE_HINT: Regex = Regex::new(r":\s*(\w+(?:\[[\w\[\], ]+\])?)(?:\s*=|$)").expect("RE_TYPE_HINT");
    static ref RE_ASYNC_AWAIT: Regex = Regex::new(r"\bawait\s+").expect("RE_ASYNC_AWAIT");
    static ref RE_ERROR_HANDLING: Regex = Regex::new(r"^\s*(?:try|except|raise|finally):?").expect("RE_ERROR_HANDLING");
}

/// Built-in patterns for code analysis
pub fn get_builtin_patterns() -> Vec<PatternDef> {
    vec![
        PatternDef {
            name: "TODO".to_string(),
            regex: RE_TODO.clone(),
            description: "TODO comments".to_string(),
        },
        PatternDef {
            name: "FIXME".to_string(),
            regex: RE_FIXME.clone(),
            description: "FIXME comments".to_string(),
        },
        PatternDef {
            name: "HACK".to_string(),
            regex: RE_HACK.clone(),
            description: "HACK comments".to_string(),
        },
        PatternDef {
            name: "function_def".to_string(),
            regex: RE_FUNC_DEF.clone(),
            description: "Python function definitions".to_string(),
        },
        PatternDef {
            name: "class_def".to_string(),
            regex: RE_CLASS_DEF.clone(),
            description: "Python class definitions".to_string(),
        },
        PatternDef {
            name: "import".to_string(),
            regex: RE_IMPORT.clone(),
            description: "Import statements".to_string(),
        },
        PatternDef {
            name: "decorator".to_string(),
            regex: RE_DECORATOR.clone(),
            description: "Decorators".to_string(),
        },
        PatternDef {
            name: "type_hint".to_string(),
            regex: RE_TYPE_HINT.clone(),
            description: "Type hints".to_string(),
        },
        PatternDef {
            name: "async_await".to_string(),
            regex: RE_ASYNC_AWAIT.clone(),
            description: "Async/await usage".to_string(),
        },
        PatternDef {
            name: "error_handling".to_string(),
            regex: RE_ERROR_HANDLING.clone(),
            description: "Error handling".to_string(),
        },
    ]
}

/// Match a single pattern across a file's content
fn match_pattern_in_content(
    content: &str,
    file_path: &str,
    pattern: &PatternDef,
) -> Vec<PatternMatch> {
    let mut matches = Vec::new();
    
    for (line_num, line) in content.lines().enumerate() {
        for cap in pattern.regex.captures_iter(line) {
            if let Some(m) = cap.get(0) {
                matches.push(PatternMatch {
                    file: file_path.to_string(),
                    line: line_num + 1,
                    column: m.start() + 1,
                    matched_text: m.as_str().to_string(),
                    pattern_name: pattern.name.clone(),
                });
            }
        }
    }
    
    matches
}

/// Scan a directory for patterns in parallel
#[pyfunction]
pub fn scan_patterns(
    directory: String,
    pattern_names: Vec<String>,
    extensions: Vec<String>,
) -> PyResult<Vec<(String, usize, usize, String, String)>> {
    let dir_path = PathBuf::from(&directory);
    
    // Get patterns to use
    let all_patterns = get_builtin_patterns();
    let patterns: Vec<&PatternDef> = if pattern_names.is_empty() {
        all_patterns.iter().collect()
    } else {
        all_patterns.iter()
            .filter(|p| pattern_names.contains(&p.name))
            .collect()
    };
    
    // Collect all files
    let files: Vec<PathBuf> = walkdir::WalkDir::new(&dir_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            if extensions.is_empty() {
                true
            } else {
                e.path()
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| extensions.contains(&ext.to_string()))
                    .unwrap_or(false)
            }
        })
        .map(|e| e.path().to_path_buf())
        .collect();
    
    // Process files in parallel
    let results: Vec<PatternMatch> = files
        .par_iter()
        .flat_map(|file_path| {
            let content = match fs::read_to_string(file_path) {
                Ok(c) => c,
                Err(_) => return Vec::new(),
            };
            
            let file_str = file_path.to_string_lossy().to_string();
            
            patterns.iter()
                .flat_map(|pattern| match_pattern_in_content(&content, &file_str, pattern))
                .collect::<Vec<_>>()
        })
        .collect();
    
    // Convert to Python-friendly format
    Ok(results.into_iter()
        .map(|m| (m.file, m.line, m.column, m.matched_text, m.pattern_name))
        .collect())
}

/// Count patterns by type across a codebase
#[pyfunction]
pub fn count_patterns(
    directory: String,
    extensions: Vec<String>,
) -> PyResult<HashMap<String, usize>> {
    let matches = scan_patterns(directory, vec![], extensions)?;
    
    let mut counts: HashMap<String, usize> = HashMap::new();
    for (_, _, _, _, pattern_name) in matches {
        *counts.entry(pattern_name).or_insert(0) += 1;
    }
    
    Ok(counts)
}

/// Fast regex search across files (like grep but faster)
#[pyfunction]
pub fn fast_regex_search(
    directory: String,
    pattern: String,
    extensions: Vec<String>,
    case_sensitive: bool,
) -> PyResult<Vec<(String, usize, String)>> {
    let dir_path = PathBuf::from(&directory);
    
    let regex = if case_sensitive {
        Regex::new(&pattern)
    } else {
        Regex::new(&format!("(?i){}", pattern))
    }.map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(
        format!("Invalid regex: {}", e)
    ))?;
    
    // Collect files
    let files: Vec<PathBuf> = walkdir::WalkDir::new(&dir_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            if extensions.is_empty() {
                true
            } else {
                e.path()
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| extensions.contains(&ext.to_string()))
                    .unwrap_or(false)
            }
        })
        .map(|e| e.path().to_path_buf())
        .collect();
    
    // Search in parallel
    let results: Vec<(String, usize, String)> = files
        .par_iter()
        .flat_map(|file_path| {
            let content = match fs::read_to_string(file_path) {
                Ok(c) => c,
                Err(_) => return Vec::new(),
            };
            
            let file_str = file_path.to_string_lossy().to_string();
            
            content.lines()
                .enumerate()
                .filter(|(_, line)| regex.is_match(line))
                .map(|(line_num, line)| {
                    (file_str.clone(), line_num + 1, line.to_string())
                })
                .collect::<Vec<_>>()
        })
        .collect();
    
    Ok(results)
}
