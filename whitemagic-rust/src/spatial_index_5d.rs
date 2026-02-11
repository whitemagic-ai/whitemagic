//! 5D Spatial Index — Extends the KD-tree to include the V (Vitality) dimension
//!
//! The existing `spatial_index.rs` uses a 4D KD-tree (X, Y, Z, W).
//! The V dimension (Galactic Distance / Vitality) was added in Python only.
//! This module provides a proper 5D spatial index so that nearest-neighbor
//! queries account for all holographic dimensions.
//!
//! The 5D coordinate system:
//!   X: Logic ↔ Emotion      (-1.0 to +1.0)
//!   Y: Micro ↔ Macro        (-1.0 to +1.0)
//!   Z: Time / Chronos       (-1.0 to +1.0)
//!   W: Importance / Gravity  (0.0 to 2.0+)
//!   V: Vitality / Distance   (0.0=edge to 1.0=core)

use kdtree::distance::squared_euclidean;
use kdtree::KdTree;
use pyo3::prelude::*;
use rayon::prelude::*;
use std::sync::{Arc, RwLock};

// ---------------------------------------------------------------------------
// 5D Spatial Index
// ---------------------------------------------------------------------------

#[pyclass]
#[derive(Clone)]
pub struct SpatialIndex5D {
    tree: Arc<RwLock<KdTree<f32, usize, [f32; 5]>>>,
    storage: Arc<RwLock<Vec<(String, [f32; 5])>>>,
}

#[pymethods]
impl SpatialIndex5D {
    #[new]
    pub fn new() -> Self {
        SpatialIndex5D {
            tree: Arc::new(RwLock::new(KdTree::new(5))),
            storage: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Add a memory's 5D coordinates to the index.
    pub fn add(&self, id: String, vector: [f32; 5]) -> PyResult<usize> {
        let mut tree = self.tree.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        let mut storage = self.storage.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;

        let idx = storage.len();
        storage.push((id, vector));

        match tree.add(vector, idx) {
            Ok(_) => Ok(idx),
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                "Failed to add point: {}",
                e
            ))),
        }
    }

    /// Batch add multiple points (faster than individual adds).
    pub fn add_batch(&self, items: Vec<(String, [f32; 5])>) -> PyResult<usize> {
        let mut tree = self.tree.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        let mut storage = self.storage.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;

        let start_idx = storage.len();
        let count = items.len();

        for (i, (id, vector)) in items.into_iter().enumerate() {
            let idx = start_idx + i;
            storage.push((id, vector));
            if let Err(e) = tree.add(vector, idx) {
                return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                    "Failed to add point {}: {}",
                    idx, e
                )));
            }
        }

        Ok(count)
    }

    /// Query the N nearest neighbors to a 5D point.
    /// Returns Vec<(memory_id, squared_distance)>.
    pub fn query_nearest(&self, vector: [f32; 5], n: usize) -> PyResult<Vec<(String, f32)>> {
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
            }
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                "Search failed: {}",
                e
            ))),
        }
    }

    /// Query neighbors within a given radius (squared distance).
    pub fn query_within_radius(
        &self,
        vector: [f32; 5],
        radius_sq: f32,
    ) -> PyResult<Vec<(String, f32)>> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        let storage = self.storage.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;

        // Use within to find all points within the radius
        match tree.within(&vector, radius_sq, &squared_euclidean) {
            Ok(points) => {
                let results: Vec<(String, f32)> = points
                    .into_iter()
                    .filter_map(|(dist, &idx)| {
                        if idx < storage.len() {
                            Some((storage[idx].0.clone(), dist))
                        } else {
                            None
                        }
                    })
                    .collect();
                Ok(results)
            }
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                "Radius search failed: {}",
                e
            ))),
        }
    }

    /// Get the number of points in the index.
    pub fn size(&self) -> PyResult<usize> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(tree.size())
    }

    /// Get a full snapshot of all stored points.
    pub fn get_snapshot(&self) -> PyResult<Vec<(String, [f32; 5])>> {
        let storage = self.storage.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(storage.clone())
    }

    /// Clear the index and rebuild empty.
    pub fn clear(&self) -> PyResult<()> {
        let mut tree = self.tree.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        let mut storage = self.storage.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        *tree = KdTree::new(5);
        storage.clear();
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Standalone functions for batch operations
// ---------------------------------------------------------------------------

/// Batch nearest-neighbor: given a list of query points, find nearest
/// neighbors for each (parallel via Rayon).
/// Returns Vec<(query_idx, Vec<(memory_id, distance)>)>.
#[pyfunction]
pub fn batch_nearest_5d(
    index: &SpatialIndex5D,
    queries: Vec<[f32; 5]>,
    n: usize,
) -> PyResult<Vec<Vec<(String, f32)>>> {
    let tree = index.tree.read().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
    })?;
    let storage = index.storage.read().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
    })?;

    let results: Vec<Vec<(String, f32)>> = queries
        .par_iter()
        .map(|query| {
            match tree.nearest(query, n, &squared_euclidean) {
                Ok(nearest) => nearest
                    .into_iter()
                    .filter_map(|(dist, &idx)| {
                        if idx < storage.len() {
                            Some((storage[idx].0.clone(), dist))
                        } else {
                            None
                        }
                    })
                    .collect(),
                Err(_) => Vec::new(),
            }
        })
        .collect();

    Ok(results)
}

/// Density estimation: for each point, count how many neighbors are within
/// a given radius. Useful for constellation detection.
#[pyfunction]
pub fn density_map_5d(
    index: &SpatialIndex5D,
    radius_sq: f32,
) -> PyResult<Vec<(String, usize)>> {
    let tree = index.tree.read().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
    })?;
    let storage = index.storage.read().map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
    })?;

    let results: Vec<(String, usize)> = storage
        .par_iter()
        .map(|(id, coords)| {
            let count = match tree.within(coords, radius_sq, &squared_euclidean) {
                Ok(neighbors) => neighbors.len().saturating_sub(1), // exclude self
                Err(_) => 0,
            };
            (id.clone(), count)
        })
        .collect();

    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_and_query() {
        let idx = SpatialIndex5D::new();
        idx.add("mem_a".into(), [0.0, 0.0, 0.0, 1.0, 0.5]).unwrap();
        idx.add("mem_b".into(), [0.1, 0.1, 0.0, 1.0, 0.5]).unwrap();
        idx.add("mem_c".into(), [5.0, 5.0, 5.0, 5.0, 5.0]).unwrap();

        let nearest = idx.query_nearest([0.0, 0.0, 0.0, 1.0, 0.5], 2).unwrap();
        assert_eq!(nearest.len(), 2);
        assert_eq!(nearest[0].0, "mem_a"); // closest
        assert_eq!(nearest[1].0, "mem_b"); // second closest
    }

    #[test]
    fn test_batch_add() {
        let idx = SpatialIndex5D::new();
        let items = vec![
            ("a".into(), [0.0, 0.0, 0.0, 0.0, 0.0]),
            ("b".into(), [1.0, 1.0, 1.0, 1.0, 1.0]),
        ];
        let count = idx.add_batch(items).unwrap();
        assert_eq!(count, 2);
        assert_eq!(idx.size().unwrap(), 2);
    }

    #[test]
    fn test_within_radius() {
        let idx = SpatialIndex5D::new();
        idx.add("near".into(), [0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
        idx.add("mid".into(), [0.5, 0.5, 0.0, 0.0, 0.0]).unwrap();
        idx.add("far".into(), [5.0, 5.0, 5.0, 5.0, 5.0]).unwrap();

        let within = idx.query_within_radius([0.0, 0.0, 0.0, 0.0, 0.0], 1.0).unwrap();
        assert_eq!(within.len(), 2); // near + mid (0.5 sq dist)
    }

    #[test]
    fn test_clear() {
        let idx = SpatialIndex5D::new();
        idx.add("a".into(), [0.0; 5]).unwrap();
        assert_eq!(idx.size().unwrap(), 1);
        idx.clear().unwrap();
        assert_eq!(idx.size().unwrap(), 0);
    }
}
