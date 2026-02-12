//! File Operations - High-performance file I/O

use pyo3::prelude::*;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

/// Validate path is safe (within CWD or allowed paths)
fn validate_path(path: &str) -> PyResult<PathBuf> {
    let path = Path::new(path);

    // 1. Resolve absolute path
    let absolute_path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?
            .join(path)
    };

    // 2. Canonicalize (resolve symlinks, .., etc)
    let canonical_path = absolute_path.canonicalize().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyIOError, _>(format!("Invalid path: {}", e))
    })?;

    // 3. Check against CWD/Allowed Root
    let cwd = std::env::current_dir()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?
        .canonicalize()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;

    // Allow usage of WM_BASE_PATH if set, otherwise default to CWD
    let allowed_root = if let Ok(base) = std::env::var("WM_BASE_PATH") {
        Path::new(&base).canonicalize().unwrap_or_else(|_| cwd.clone())
    } else {
        cwd
    };

    if !canonical_path.starts_with(&allowed_root) {
        return Err(PyErr::new::<pyo3::exceptions::PyPermissionError, _>(
            format!(
                "Access denied: Path outside allowed root ({:?})",
                allowed_root
            ),
        ));
    }

    Ok(canonical_path)
}

/// Read file with maximum performance
#[pyfunction]
pub fn read_file(path: &str) -> PyResult<String> {
    let safe_path = validate_path(path)?;
    fs::read_to_string(safe_path)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))
}

/// Read file with maximum performance (alias)
#[pyfunction]
pub fn read_file_fast(path: &str) -> PyResult<String> {
    read_file(path)
}

/// Write file with maximum performance (alias)
#[pyfunction]
pub fn write_file(path: &str, content: &str) -> PyResult<usize> {
    write_file_fast(path, content)
}

/// Write file with maximum performance
#[pyfunction]
pub fn write_file_fast(path: &str, content: &str) -> PyResult<usize> {
    let safe_path = validate_path(path)?;

    let mut file = File::create(safe_path)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;

    let bytes = file
        .write(content.as_bytes())
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;

    Ok(bytes)
}

/// Write file with compression (gzip)
#[pyfunction]
pub fn write_file_compressed(path: &str, content: &str) -> PyResult<(usize, usize)> {
    use flate2::write::GzEncoder;
    use flate2::Compression;

    let safe_path = validate_path(path)?;

    let file = File::create(safe_path)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;

    let mut encoder = GzEncoder::new(file, Compression::best());
    let original_size = content.as_bytes().len();
    encoder
        .write_all(content.as_bytes())
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;

    let compressed_file = encoder
        .finish()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    let compressed_size = compressed_file
        .metadata()
        .map(|m| m.len() as usize)
        .unwrap_or(0);

    Ok((original_size, compressed_size))
}
