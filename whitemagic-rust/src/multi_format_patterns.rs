/// Multi-format pattern extraction
/// Supports: .md, .hs, .rs, .py, .yml, .json, .sh

use rayon::prelude::*;
use std::path::{Path, PathBuf};
use std::fs;
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct CodePattern {
    pub file_type: String,
    pub pattern_type: String,  // function, struct, algorithm, etc.
    pub content: String,
    pub confidence: f64,
    pub location: String,
}

/// Extract patterns from multiple file types
pub fn extract_multi_format(dir: &Path, extensions: Vec<&str>) -> Result<Vec<CodePattern>, String> {
    let mut all_patterns = Vec::new();
    
    for ext in extensions {
        let patterns = match ext {
            ".rs" => extract_rust_patterns(dir)?,
            ".hs" => extract_haskell_patterns(dir)?,
            ".py" => extract_python_patterns(dir)?,
            ".sh" => extract_shell_patterns(dir)?,
            ".yml" | ".json" => extract_config_patterns(dir, ext)?,
            _ => vec\![]
        };
        all_patterns.extend(patterns);
    }
    
    Ok(all_patterns)
}

fn extract_rust_patterns(dir: &Path) -> Result<Vec<CodePattern>, String> {
    let mut patterns = Vec::new();
    
    for entry in fs::read_dir(dir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            
            // Extract function patterns
            for line in content.lines() {
                if line.trim().starts_with("pub fn") || line.trim().starts_with("fn ") {
                    patterns.push(CodePattern {
                        file_type: "rust".to_string(),
                        pattern_type: "function".to_string(),
                        content: line.trim().to_string(),
                        confidence: 0.8,
                        location: path.display().to_string(),
                    });
                }
            }
        }
    }
    
    Ok(patterns)
}

fn extract_haskell_patterns(dir: &Path) -> Result<Vec<CodePattern>, String> {
    let mut patterns = Vec::new();
    
    for entry in fs::read_dir(dir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("hs") {
            let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            
            // Extract type signatures and functions
            for line in content.lines() {
                if line.contains("::") && \!line.trim().starts_with("--") {
                    patterns.push(CodePattern {
                        file_type: "haskell".to_string(),
                        pattern_type: "type_signature".to_string(),
                        content: line.trim().to_string(),
                        confidence: 0.9,  // Haskell types are very informative
                        location: path.display().to_string(),
                    });
                }
            }
        }
    }
    
    Ok(patterns)
}

fn extract_python_patterns(dir: &Path) -> Result<Vec<CodePattern>, String> {
    let mut patterns = Vec::new();
    
    for entry in fs::read_dir(dir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("py") {
            let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            
            // Extract class and function definitions
            for line in content.lines() {
                if line.trim().starts_with("def ") || line.trim().starts_with("class ") {
                    patterns.push(CodePattern {
                        file_type: "python".to_string(),
                        pattern_type: if line.contains("class") { "class" } else { "function" }.to_string(),
                        content: line.trim().to_string(),
                        confidence: 0.75,
                        location: path.display().to_string(),
                    });
                }
            }
        }
    }
    
    Ok(patterns)
}

fn extract_shell_patterns(dir: &Path) -> Result<Vec<CodePattern>, String> {
    let mut patterns = Vec::new();
    
    for entry in fs::read_dir(dir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("sh") {
            let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            
            // Extract shell techniques
            for line in content.lines() {
                // Look for pipes, redirects, command chains
                if line.contains("|") || line.contains("<<") || line.contains("&&") {
                    patterns.push(CodePattern {
                        file_type: "shell".to_string(),
                        pattern_type: "technique".to_string(),
                        content: line.trim().to_string(),
                        confidence: 0.85,  // Shell patterns are often reusable
                        location: path.display().to_string(),
                    });
                }
            }
        }
    }
    
    Ok(patterns)
}

fn extract_config_patterns(dir: &Path, ext: &str) -> Result<Vec<CodePattern>, String> {
    // Configuration patterns - structural knowledge
    Ok(vec\![])  // Simplified for now
}
