//! Heaven's Net v6: The Intelligent Dragnet (Rust Edition)
//!
//! Features:
//! - Tree scanning with metadata (size, mtime)
//! - 4 Modes: Safe, Balanced, Deep, UltraDeep
//! - Intelligent file prioritization
//! - Batched parallel processing

use pyo3::prelude::*;
use rayon::prelude::*;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::SystemTime;
use walkdir::WalkDir;

// Mode Configurations
#[derive(Debug, Clone, Copy)]
pub enum ScanMode {
    Safe,      // Fast, conservative limits
    Balanced,  // Medium speed, reasonable limits
    Deep,      // Slow, high limits
    UltraDeep, // No limits (use with caution)
}

impl ScanMode {
    fn max_file_size(&self) -> u64 {
        match self {
            ScanMode::Safe => 512 * 1024,      // 512 KB
            ScanMode::Balanced => 1024 * 1024, // 1 MB
            ScanMode::Deep => 5 * 1024 * 1024, // 5 MB
            ScanMode::UltraDeep => u64::MAX,   // No limit
        }
    }

    fn batch_size(&self) -> usize {
        match self {
            ScanMode::Safe => 100,
            ScanMode::Balanced => 200,
            ScanMode::Deep => 50,      // Smaller batches, more data per file
            ScanMode::UltraDeep => 25, // Very small batches
        }
    }

    fn tech_limit_per_file(&self) -> usize {
        match self {
            ScanMode::Safe => 10,
            ScanMode::Balanced => 15,
            ScanMode::Deep => 50,
            ScanMode::UltraDeep => usize::MAX,
        }
    }
}

// Tree Node for folder structure
#[derive(Debug, Serialize, Deserialize, Clone)]
#[pyclass]
pub struct TreeNode {
    #[pyo3(get)]
    pub path: String,
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub is_dir: bool,
    #[pyo3(get)]
    pub size_bytes: u64,
    #[pyo3(get)]
    pub file_count: usize,
    #[pyo3(get)]
    pub mtime_epoch: u64, // Seconds since epoch
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[pyclass]
pub struct Concept {
    #[pyo3(get)]
    pub pattern: String,
    #[pyo3(get)]
    pub domain: String,
    #[pyo3(get)]
    pub confidence: f64,
    #[pyo3(get)]
    pub file_path: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[pyclass]
pub struct DragnetResult {
    #[pyo3(get)]
    pub total_files: usize,
    #[pyo3(get)]
    pub skipped_files: usize,
    #[pyo3(get)]
    pub features: Vec<Concept>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[pyclass]
pub struct TreeScanResult {
    #[pyo3(get)]
    pub root_path: String,
    #[pyo3(get)]
    pub total_dirs: usize,
    #[pyo3(get)]
    pub total_files: usize,
    #[pyo3(get)]
    pub total_size_bytes: u64,
    #[pyo3(get)]
    pub top_dirs: Vec<TreeNode>, // Top-level directories with stats
}

lazy_static::lazy_static! {
    static ref RE_ENTITY: Regex = Regex::new(r"[A-Z][a-z]+(?:\s+[A-Z][a-z]+)+").expect("Invalid Entity Regex");
    static ref RE_TECH: Regex = Regex::new(r"\b[a-zA-Z][a-zA-Z0-9_]*[A-Z_][a-zA-Z0-9_]*\b").expect("Invalid Tech Regex");
    static ref RE_ENGINE: Regex = Regex::new(r"(?i)\b(pattern[_\s]?engine|graph[_\s]?engine|dream[_\s]?state|dream[_\s]?cycle|insight|decision|emotion|kaizen|serendipity|predictive|narrative|ethics|hologram|constellation|resonance|wu[_\s]?xing|gan[_\s]?ying|zodiac|autonomous|synthesis|emergence|consciousness)\b").expect("Invalid Engine Regex");
    static ref RE_ARCH: Regex = Regex::new(r"(?i)\b(architecture|diagram|subsystem|component|pipeline|workflow|integration|orchestration)\b").expect("Invalid Arch Regex");
}

fn get_mtime(path: &std::path::Path) -> u64 {
    fs::metadata(path)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Scan tree structure with metadata
#[pyfunction]
pub fn scan_tree(root_path: String) -> PyResult<TreeScanResult> {
    let mut total_dirs = 0usize;
    let mut total_files = 0usize;
    let mut total_size = 0u64;
    let mut top_level_stats: HashMap<String, (u64, usize, u64)> = HashMap::new(); // (size, count, mtime)

    for entry in WalkDir::new(&root_path)
        .min_depth(1)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        let depth = entry.depth();

        if entry.file_type().is_dir() {
            total_dirs += 1;
        } else if entry.file_type().is_file() {
            total_files += 1;
            let size = fs::metadata(path).map(|m| m.len()).unwrap_or(0);
            total_size += size;

            // Track top-level directory stats
            if depth >= 1 {
                let components: Vec<_> = path
                    .strip_prefix(&root_path)
                    .unwrap_or(path)
                    .components()
                    .collect();
                if let Some(first) = components.first() {
                    let top_dir = first.as_os_str().to_string_lossy().to_string();
                    let mtime = get_mtime(path);
                    let entry = top_level_stats.entry(top_dir).or_insert((0, 0, 0));
                    entry.0 += size;
                    entry.1 += 1;
                    if mtime > entry.2 {
                        entry.2 = mtime;
                    }
                }
            }
        }
    }

    // Build top dirs vector
    let mut top_dirs: Vec<TreeNode> = top_level_stats
        .iter()
        .map(|(name, (size, count, mtime))| TreeNode {
            path: format!("{}/{}", root_path, name),
            name: name.clone(),
            is_dir: true,
            size_bytes: *size,
            file_count: *count,
            mtime_epoch: *mtime,
        })
        .collect();

    // Sort by size descending
    top_dirs.sort_by(|a, b| b.size_bytes.cmp(&a.size_bytes));

    Ok(TreeScanResult {
        root_path,
        total_dirs,
        total_files,
        total_size_bytes: total_size,
        top_dirs,
    })
}

fn is_supported_extension(path: &str) -> bool {
    let exts = [
        ".py", ".rs", ".md", ".txt", ".ex", ".exs", ".js", ".ts", ".toml", ".yaml", ".json",
        ".mojo", ".zig",
    ];
    exts.iter().any(|ext| path.ends_with(ext))
}

/// Cast Heaven's Net v6 with mode selection
/// mode: "safe", "balanced", "deep", "ultradeep"
#[pyfunction]
pub fn cast_heavens_net(root_path: String, mode: Option<String>) -> PyResult<DragnetResult> {
    let scan_mode = match mode.as_deref() {
        Some("safe") => ScanMode::Safe,
        Some("balanced") => ScanMode::Balanced,
        Some("deep") => ScanMode::Deep,
        Some("ultradeep") => ScanMode::UltraDeep,
        _ => ScanMode::Balanced, // Default
    };

    cast_with_mode(root_path, scan_mode)
}

fn cast_with_mode(root_path: String, mode: ScanMode) -> PyResult<DragnetResult> {
    let skipped = AtomicUsize::new(0);
    let max_size = mode.max_file_size();
    let batch_size = mode.batch_size();
    let tech_limit = mode.tech_limit_per_file();

    // Collect files with size check
    let files: Vec<String> = WalkDir::new(&root_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter_map(|e| {
            let path = e.path();
            let path_str = path.to_string_lossy().into_owned();

            if !is_supported_extension(&path_str) {
                return None;
            }

            if let Ok(meta) = fs::metadata(path) {
                if meta.len() > max_size {
                    skipped.fetch_add(1, Ordering::Relaxed);
                    return None;
                }
            }

            Some(path_str)
        })
        .collect();

    let total_files = files.len();
    let mut all_features: Vec<Concept> = Vec::new();

    for batch in files.chunks(batch_size) {
        let batch_features: Vec<Concept> = batch
            .par_iter()
            .flat_map(|path| mine_file(path, tech_limit))
            .collect();
        all_features.extend(batch_features);
    }

    Ok(DragnetResult {
        total_files,
        skipped_files: skipped.load(Ordering::Relaxed),
        features: all_features,
    })
}

fn mine_file(path: &str, tech_limit: usize) -> Vec<Concept> {
    let mut concepts = Vec::new();

    if let Ok(content) = fs::read_to_string(path) {
        let mut seen = HashSet::new();

        // Cognitive Engines
        for cap in RE_ENGINE.captures_iter(&content) {
            let term = cap[1].to_lowercase().replace(" ", "_");
            if seen.contains(&term) {
                continue;
            }
            seen.insert(term.clone());

            concepts.push(Concept {
                pattern: term,
                domain: "cognitive_engine".to_string(),
                confidence: 0.98,
                file_path: path.to_string(),
            });
        }

        // Architecture
        for cap in RE_ARCH.captures_iter(&content) {
            let term = cap[1].to_lowercase();
            if seen.contains(&term) {
                continue;
            }
            seen.insert(term.clone());

            concepts.push(Concept {
                pattern: term,
                domain: "architecture".to_string(),
                confidence: 0.90,
                file_path: path.to_string(),
            });
        }

        // Entities
        for mat in RE_ENTITY.find_iter(&content) {
            let term = mat.as_str().to_string();
            if term.len() < 5 {
                continue;
            }
            if seen.contains(&term) {
                continue;
            }
            seen.insert(term.clone());

            concepts.push(Concept {
                pattern: term,
                domain: "entity".to_string(),
                confidence: 0.8,
                file_path: path.to_string(),
            });
        }

        // Tech (with limit)
        let mut tech_count = 0;
        for mat in RE_TECH.find_iter(&content) {
            if tech_count >= tech_limit {
                break;
            }

            let term = mat.as_str().to_string();
            if term.len() < 5 {
                continue;
            }
            if seen.contains(&term) {
                continue;
            }
            seen.insert(term.clone());

            concepts.push(Concept {
                pattern: term,
                domain: "tech".to_string(),
                confidence: 0.85,
                file_path: path.to_string(),
            });
            tech_count += 1;
        }
    }

    concepts
}

/// Cast Heaven's Net directly to a JSONL file (Zero-Copy for Python)
#[pyfunction]
pub fn cast_heavens_net_to_file(
    root_path: String,
    output_path: String,
    mode: Option<String>,
) -> PyResult<(usize, usize)> {
    let scan_mode = match mode.as_deref() {
        Some("safe") => ScanMode::Safe,
        Some("balanced") => ScanMode::Balanced,
        Some("deep") => ScanMode::Deep,
        Some("ultradeep") => ScanMode::UltraDeep,
        _ => ScanMode::Balanced,
    };

    let file = fs::File::create(&output_path)?;
    let mut writer = std::io::BufWriter::new(file);

    let max_size = scan_mode.max_file_size();
    let tech_limit = scan_mode.tech_limit_per_file();

    let mut total_files = 0;
    let mut total_concepts = 0;

    // Sequential walk to ensure clean writing (Par iter with mutex writer is also possible but sequential is safer for IO bound)
    // Actually, locking stdout/file in parallel is fine, but let's stick to Rayon with a collect or Mutex write.
    // For max speed, we should gather batch features then write.

    // Simplest robust approach: Walk, filter, mine, write.
    for entry in WalkDir::new(&root_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        let path = entry.path();
        let path_str = path.to_string_lossy().into_owned();

        if !is_supported_extension(&path_str) {
            continue;
        }
        if let Ok(meta) = fs::metadata(path) {
            if meta.len() > max_size {
                continue;
            }
        }

        let concepts = mine_file(&path_str, tech_limit);
        if !concepts.is_empty() {
            for c in concepts {
                let json = serde_json::to_string(&c).unwrap_or_default();
                if !json.is_empty() {
                    use std::io::Write;
                    writeln!(writer, "{}", json)?;
                    total_concepts += 1;
                }
            }
        }
        total_files += 1;
    }

    Ok((total_files, total_concepts))
}
