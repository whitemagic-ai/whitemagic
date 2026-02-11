use kdtree::KdTree;
use kdtree::distance::squared_euclidean;
use pyo3::prelude::*;
use std::sync::{Arc, RwLock};

#[pyclass]
#[derive(Clone)]
pub struct SpatialIndex {
    // 4 dimensions: x (Logic-Emotion), y (Micro-Macro), z (Time), w (Importance)
    // Stores index (usize) as the data
    tree: Arc<RwLock<KdTree<f32, usize, [f32; 4]>>>,
    // Map internal index to external string ID AND vector for retrieval
    storage: Arc<RwLock<Vec<(String, [f32; 4])>>>,
}

#[pymethods]
impl SpatialIndex {
    #[new]
    pub fn new() -> Self {
        SpatialIndex {
            tree: Arc::new(RwLock::new(KdTree::new(4))),
            storage: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub fn add(&self, id: String, vector: [f32; 4]) -> PyResult<usize> {
        let mut tree = self.tree.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        let mut storage = self.storage.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        
        let idx = storage.len();
        storage.push((id, vector));
        
        // kdtree add returns Result, we should handle it
        match tree.add(vector, idx) {
            Ok(_) => Ok(idx),
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Failed to add point: {}", e)))
        }
    }

    pub fn query_nearest(&self, vector: [f32; 4], n: usize) -> PyResult<Vec<(String, f32)>> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        let storage = self.storage.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        
        match tree.nearest(&vector, n, &squared_euclidean) {
            Ok(nearest) => {
                let mut results = Vec::new();
                for (distance, &idx) in nearest {
                    if idx < storage.len() {
                        results.push((storage[idx].0.clone(), distance));
                    }
                }
                Ok(results)
            },
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Search failed: {}", e)))
        }
    }
    
    pub fn size(&self) -> PyResult<usize> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(tree.size())
    }

    pub fn get_snapshot(&self) -> PyResult<Vec<(String, [f32; 4])>> {
        let storage = self.storage.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(storage.clone())
    }
}
