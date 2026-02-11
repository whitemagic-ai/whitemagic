use kdtree::distance::squared_euclidean;
use kdtree::KdTree;
use pyo3::prelude::*;
use std::sync::Mutex;

/// A 4D Spatial Index for Holographic Memories
#[pyclass]
pub struct HolographicIndex {
    // kdtree: dim=4, item=String (memory_id), type=f64
    tree: KdTree<f64, String, [f64; 4]>,
    count: usize,
}

#[pymethods]
impl HolographicIndex {
    #[new]
    pub fn new() -> Self {
        HolographicIndex {
            tree: KdTree::new(4),
            count: 0,
        }
    }

    /// Add a memory vector to the 4D index
    pub fn add(&mut self, id: String, x: f64, y: f64, z: f64, w: f64) -> PyResult<bool> {
        self.tree.add([x, y, z, w], id).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("KD-Tree add error: {}", e))
        })?;
        self.count += 1;
        Ok(true)
    }

    /// Query the k nearest neighbors
    pub fn query_nearest(
        &self,
        x: f64,
        y: f64,
        z: f64,
        w: f64,
        k: usize,
    ) -> PyResult<Vec<(String, f64)>> {
        let results = self
            .tree
            .nearest(&[x, y, z, w], k, &squared_euclidean)
            .map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!(
                    "KD-Tree query error: {}",
                    e
                ))
            })?;

        Ok(results
            .into_iter()
            .map(|(dist, id)| (id.clone(), dist))
            .collect())
    }

    /// Query all memories within a specific radius
    pub fn query_radius(
        &self,
        x: f64,
        y: f64,
        z: f64,
        w: f64,
        radius: f64,
    ) -> PyResult<Vec<(String, f64)>> {
        let results = self
            .tree
            .within(&[x, y, z, w], radius, &squared_euclidean)
            .map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!(
                    "KD-Tree radius query error: {}",
                    e
                ))
            })?;

        Ok(results
            .into_iter()
            .map(|(dist, id)| (id.clone(), dist))
            .collect())
    }

    /// Clear the index
    pub fn clear(&mut self) {
        self.tree = KdTree::new(4);
        self.count = 0;
    }

    /// Get current stats
    pub fn stats(&self, py: Python) -> PyResult<PyObject> {
        let dict = pyo3::types::PyDict::new_bound(py);
        dict.set_item("count", self.count)?;
        dict.set_item("dimensions", 4)?;
        dict.set_item("backend", "rust_kdtree")?;
        Ok(dict.into())
    }

    /// Save index metadata to disk (serializes count; KdTree rebuild on load).
    pub fn save(&self, path: String) -> PyResult<bool> {
        use std::io::Write;
        let data = format!("{{\"count\":{},\"dimensions\":4}}", self.count);
        let mut file = std::fs::File::create(&path).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyIOError, _>(format!("Save failed: {}", e))
        })?;
        file.write_all(data.as_bytes()).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyIOError, _>(format!("Write failed: {}", e))
        })?;
        Ok(true)
    }
}

// Global default store for non-OO usage
lazy_static::lazy_static! {
    static ref GLOBAL_INDEX: Mutex<HolographicIndex> = Mutex::new(HolographicIndex::new());
}

#[pyfunction]
pub fn add_embedding(id: String, vector: Vec<f64>) -> PyResult<bool> {
    if vector.len() != 4 {
        return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
            "Vector must be 4D",
        ));
    }
    let mut index = GLOBAL_INDEX.lock().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Mutex lock error: {}", e))
    })?;
    index.add(id, vector[0], vector[1], vector[2], vector[3])
}

#[pyfunction]
pub fn search_embedding(query: Vec<f64>, k: usize) -> PyResult<Vec<(String, f64)>> {
    if query.len() != 4 {
        return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
            "Query must be 4D",
        ));
    }
    let index = GLOBAL_INDEX.lock().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Mutex lock error: {}", e))
    })?;
    index.query_nearest(query[0], query[1], query[2], query[3], k)
}
