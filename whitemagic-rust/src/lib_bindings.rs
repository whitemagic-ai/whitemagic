//! Bindings for legacy compatibility
use pyo3::prelude::*;
use std::path::PathBuf;

/// Legacy wrapper for consolidation
#[pyfunction]
pub fn consolidate_memories(
    dir: String,
) -> PyResult<String> {
    // Determine path
    let path = PathBuf::from(dir);
    
    // For now, return success message. Real consolidation calls consolidation::consolidate_parallel
    if path.exists() {
        Ok(format!("Consolidation logic available for {:?}", path))
    } else {
        Err(PyErr::new::<pyo3::exceptions::PyFileNotFoundError, _>(
            format!("Path not found: {:?}", path)
        ))
    }
}
