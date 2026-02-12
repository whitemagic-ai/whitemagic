//! Holographic Spatial Memory Backend
//! Implements a 4D spatial index for WhiteMagic memories using K-D Trees.
//! Coordinates: [x (Logic-Emotion), y (Micro-Macro), z (Time), w (Importance)]

use kdtree::distance::squared_euclidean;
use kdtree::KdTree;
use pyo3::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[pyclass]
#[derive(Debug, Clone)]
pub struct HolographicCoordinate {
    #[pyo3(get, set)]
    pub x: f64,
    #[pyo3(get, set)]
    pub y: f64,
    #[pyo3(get, set)]
    pub z: f64,
    #[pyo3(get, set)]
    pub w: f64,
}

#[pymethods]
impl HolographicCoordinate {
    #[new]
    fn new(x: f64, y: f64, z: f64, w: f64) -> Self {
        Self { x, y, z, w }
    }

    fn to_list(&self) -> Vec<f64> {
        vec![self.x, self.y, self.z, self.w]
    }
}

type Point = [f64; 4];

use crate::zig_bridge::{ZigTransientAllocator, ZigUnifiedMemory};

#[pyclass]
pub struct HolographicIndex {
    tree: Arc<RwLock<KdTree<f64, String, Point>>>,
    coords: Arc<RwLock<HashMap<String, Point>>>,
    #[pyo3(get)]
    pub use_zig: bool,
}

#[pymethods]
impl HolographicIndex {
    #[new]
    fn new() -> Self {
        Self {
            tree: Arc::new(RwLock::new(KdTree::new(4))),
            coords: Arc::new(RwLock::new(HashMap::new())),
            use_zig: true,
        }
    }

    /// Add or update a memory coordinate in the hologram
    fn add(&self, memory_id: String, x: f64, y: f64, z: f64, w: f64) -> PyResult<()> {
        let point: Point = [x, y, z, w];

        if self.use_zig {
            // Use Zig Unified Memory for coordinate storage tracking
            // We use a hash of the memory_id as the ID for Zig
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            memory_id.hash(&mut hasher);
            let zig_id = hasher.finish();

            // Just a demonstration of using Zig allocation for "thought" persistence
            // In a real high-perf scenario, we'd store the actual KdTree nodes in Zig memory.
            ZigUnifiedMemory::alloc(zig_id, std::mem::size_of::<Point>(), 8);
        }

        let mut tree = self.tree.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock write error: {}", e))
        })?;
        let mut coords = self.coords.write().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock write error: {}", e))
        })?;

        // If exists, removing from KdTree is O(N) or hard without rebalancing.
        // For 0.7, we might just add duplicates or need to rebuild if updates are frequent.
        // But kdtree 0.7 supports `remove` if we know the point.
        if let Some(old_point) = coords.get(&memory_id) {
            // Try to remove old entry if possible
            // remove needs the point and the value matching.
            if let Ok(_) = tree.remove(old_point, &memory_id) {
                // Removed old successfully
            }
        }

        // Add new
        tree.add(point, memory_id.clone()).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("KdTree error: {}", e))
        })?;

        coords.insert(memory_id, point);
        Ok(())
    }

    /// Find N nearest memories to a coordinate point
    fn query_nearest(
        &self,
        x: f64,
        y: f64,
        z: f64,
        w: f64,
        n: usize,
    ) -> PyResult<Vec<(String, f64)>> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock read error: {}", e))
        })?;
        let point: Point = [x, y, z, w];

        match tree.nearest(&point, n, &squared_euclidean) {
            Ok(results) => {
                let mut out = Vec::new();
                for (dist, id) in results {
                    out.push((id.clone(), dist));
                }
                Ok(out)
            }
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                "Query error: {}",
                e
            ))),
        }
    }

    /// Find all memories within a radius (hyper-sphere)
    fn query_radius(
        &self,
        x: f64,
        y: f64,
        z: f64,
        w: f64,
        radius: f64,
    ) -> PyResult<Vec<(String, f64)>> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock read error: {}", e))
        })?;
        let point: Point = [x, y, z, w];

        // within returns Vec<(f64, &String)>
        match tree.within(&point, radius, &squared_euclidean) {
            Ok(results) => {
                let mut out = Vec::new();
                for (dist, id) in results {
                    out.push((id.clone(), dist));
                }
                // Sort by distance (nearest first)
                out.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
                Ok(out)
            }
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                "Query error: {}",
                e
            ))),
        }
    }

    /// Get stats about the index
    fn stats(&self) -> PyResult<HashMap<String, usize>> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock read error: {}", e))
        })?;
        let mut map = HashMap::new();
        map.insert("count".to_string(), tree.size());
        Ok(map)
    }

    /// Find clusters of memories within a radius
    /// Returns list of clusters, where each cluster is (Center Point [x,y,z,w], List of Memory IDs)
    fn find_clusters(
        &self,
        radius: f64,
        min_size: usize,
    ) -> PyResult<Vec<(Vec<f64>, Vec<String>)>> {
        let tree = self.tree.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock read error: {}", e))
        })?;
        let coords_guard = self.coords.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock read error: {}", e))
        })?;

        let mut clusters = Vec::new();
        let mut visited = std::collections::HashSet::new();

        // Get all points
        // We iterate over the HashMap because KdTree doesn't expose iter easily in 0.7 without ownership issues?
        // Actually, we can just iterate the HashMap keys (Memory IDs)

        for (mem_id, point) in coords_guard.iter() {
            if visited.contains(mem_id) {
                continue;
            }

            // Query neighbors
            match tree.within(point, radius, &squared_euclidean) {
                Ok(neighbors) => {
                    // Check density
                    if neighbors.len() >= min_size {
                        let mut cluster_ids = Vec::new();
                        let mut center_sum = [0.0; 4];
                        let mut count = 0.0;

                        for (_dist, neighbor_id) in neighbors {
                            if !visited.contains(neighbor_id) {
                                visited.insert(neighbor_id.clone());
                                cluster_ids.push(neighbor_id.clone());

                                // Add to center calc
                                if let Some(p) = coords_guard.get(neighbor_id) {
                                    center_sum[0] += p[0];
                                    center_sum[1] += p[1];
                                    center_sum[2] += p[2];
                                    center_sum[3] += p[3];
                                    count += 1.0;
                                }
                            }
                        }

                        if !cluster_ids.is_empty() {
                            let center = vec![
                                center_sum[0] / count,
                                center_sum[1] / count,
                                center_sum[2] / count,
                                center_sum[3] / count,
                            ];
                            clusters.push((center, cluster_ids));
                        }
                    }
                }
                Err(e) => {
                    return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                        "Query error: {}",
                        e
                    )))
                }
            }
        }

        Ok(clusters)
    }

    /// v7.0 Materialization: Load Shadowclones from Registry and project them into 4D space
    fn materialize_shadowclones(&self, registry_path: String) -> PyResult<usize> {
        use rand::Rng;
        use serde::Deserialize;
        use std::fs::File;
        use std::io::BufReader;

        #[derive(Debug, Deserialize)]
        struct CloneDistribution {
            explorers: usize,
            analysts: usize,
            creators: usize,
            synthesizers: usize,
            validators: usize,
            coordinators: usize,
        }

        #[derive(Debug, Deserialize)]
        struct Registry {
            distribution: CloneDistribution,
        }

        let file = File::open(registry_path).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyFileNotFoundError, _>(format!(
                "Failed to open registry: {}",
                e
            ))
        })?;
        let reader = BufReader::new(file);
        let registry: Registry = serde_json::from_reader(reader).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyValueError, _>(format!(
                "Failed to parse registry: {}",
                e
            ))
        })?;

        let mut rng = rand::thread_rng();
        let mut count = 0;

        // Helper to spawn clones in a sector with random jitter
        let mut spawn_clones = |role: &str, num: usize, base: Point, jitter: f64| -> PyResult<()> {
            for i in 0..num {
                let id = format!("clone:{}:{}", role, i);
                let x = base[0] + rng.gen_range(-jitter..jitter);
                let y = base[1] + rng.gen_range(-jitter..jitter);
                let z = base[2] + rng.gen_range(-jitter..jitter);
                let w = base[3] + rng.gen_range(-jitter..jitter);

                // We use self.add logic directly but avoid the pyo3 wrapper overhead if internal?
                // No, just call the internal add logic.
                // But wait, add is not pub or internal ref refactoring needed?
                // add is defined above in impl, we can call it.
                self.add(id, x, y, z, w)?;
                count += 1;
            }
            Ok(())
        };

        // Explorers: Macro (Y>0), Future (Z>0). Broad search.
        spawn_clones(
            "explorer",
            registry.distribution.explorers,
            [0.0, 0.8, 0.5, 0.5],
            0.2,
        )?;

        // Analysts: Micro (Y<0), Logic (X<0). Deep analysis.
        spawn_clones(
            "analyst",
            registry.distribution.analysts,
            [-0.8, -0.8, 0.0, 0.8],
            0.1,
        )?;

        // Creators: Emotion (X>0), Future (Z>0). Synthesis.
        spawn_clones(
            "creator",
            registry.distribution.creators,
            [0.8, 0.0, 0.8, 0.7],
            0.2,
        )?;

        // Synthesizers: Balanced (X~0), Macro (Y>0).
        spawn_clones(
            "synthesizer",
            registry.distribution.synthesizers,
            [0.0, 0.5, 0.0, 0.9],
            0.1,
        )?;

        // Validators: Logic (X<0), Past (Z<0). Verification.
        spawn_clones(
            "validator",
            registry.distribution.validators,
            [-0.9, -0.5, -0.5, 0.9],
            0.05,
        )?;

        // Coordinators: High Importance (W~1).
        spawn_clones(
            "coordinator",
            registry.distribution.coordinators,
            [0.0, 0.0, 0.0, 1.0],
            0.05,
        )?;

        Ok(count)
    }

    /// v8.0 Harmonic Gravity: Calculate the aggregate gravitational pull of governance clusters at a point
    fn calculate_resonance_field(&self, x: f64, y: f64, z: f64, w: f64) -> PyResult<f64> {
        let coords_guard = self.coords.read().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("RwLock read error: {}", e))
        })?;
        let mut total_field = 0.0;

        for (id, point) in coords_guard.iter() {
            if id.starts_with("gov:") {
                // Harmonic Gravity: Field strength = Resonance / Distance^2
                // (Using 4D squared distance)
                let dist_sq = (x - point[0]).powi(2)
                    + (y - point[1]).powi(2)
                    + (z - point[2]).powi(2)
                    + (w - point[3]).powi(2);

                // Add a small epsilon to avoid division by zero
                let resonance = point[3]; // We use W as the resonance intensity bias
                total_field += resonance / (dist_sq + 0.1);
            }
        }

        Ok(total_field)
    }
}

/// Module registration
pub fn register(_py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Initialize Zig Metabolic Core
    ZigUnifiedMemory::init();
    ZigTransientAllocator::init();

    m.add_class::<HolographicCoordinate>()?;
    m.add_class::<HolographicIndex>()?;
    Ok(())
}
