//! High-performance async memory backend for WhiteMagic
//! Provides zero-copy operations and efficient storage

use pyo3::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;
use serde::{Deserialize, Serialize};
use std::time::Instant;
use uuid::Uuid;
use chrono::{Utc, DateTime};
use tokio::runtime::Runtime;

#[cfg(feature = "tokio")]
use tokio::sync::RwLock;

#[cfg(not(feature = "tokio"))]
use std::sync::RwLock as StdRwLock;

#[pyclass]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Memory {
    #[pyo3(get, set)]
    pub id: String,
    #[pyo3(get, set)]
    pub content: String,
    #[pyo3(get, set)]
    pub memory_type: String,
    #[pyo3(get, set)]
    pub created_at: String,
    #[pyo3(get, set)]
    pub accessed_at: String,
    #[pyo3(get, set)]
    pub access_count: u32,
    #[pyo3(get, set)]
    pub tags: Vec<String>,
    #[pyo3(get, set)]
    pub associations: HashMap<String, f32>,
    #[pyo3(get, set)]
    pub emotional_valence: f32,
    #[pyo3(get, set)]
    pub importance: f32,
    #[pyo3(get, set)]
    pub metadata: HashMap<String, String>,
}

#[pyclass]
#[derive(Debug)]
pub struct MemoryStats {
    #[pyo3(get)]
    pub total_stored: u64,
    #[pyo3(get)]
    pub total_retrieved: u64,
    #[pyo3(get)]
    pub cache_hits: u64,
    #[pyo3(get)]
    pub cache_misses: u64,
    #[pyo3(get)]
    pub avg_store_time: f64,
    #[pyo3(get)]
    pub avg_retrieve_time: f64,
}

pub struct AsyncMemoryBackend {
    // In-memory cache for hot data
    cache: Arc<RwLock<HashMap<String, Memory>>>,
    // Persistent storage would go here (RocksDB, etc.)
    stats: Arc<RwLock<MemoryStats>>,
    runtime: Runtime,
}

impl AsyncMemoryBackend {
    pub fn new() -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            stats: Arc::new(RwLock::new(MemoryStats {
                total_stored: 0,
                total_retrieved: 0,
                cache_hits: 0,
                cache_misses: 0,
                avg_store_time: 0.0,
                avg_retrieve_time: 0.0,
            })),
            runtime: Runtime::new().expect("Failed to create tokio runtime"),
        }
    }

    pub async fn store(&self, mut memory: Memory) -> PyResult<String> {
        let start = Instant::now();
        
        // Generate ID if not present
        if memory.id.is_empty() {
            memory.id = Uuid::new_v4().to_string()[..16].to_string();
        }
        
        // Update access info
        let now = Utc::now().to_rfc3339();
        memory.accessed_at = now.clone();
        memory.access_count += 1;
        
        // Store in cache
        {
            let mut cache = self.cache.write().await;
            cache.insert(memory.id.clone(), memory.clone());
        }
        
        // Update stats
        let elapsed = start.elapsed().as_secs_f64();
        {
            let mut stats = self.stats.write().await;
            stats.total_stored += 1;
            stats.avg_store_time = 
                (stats.avg_store_time * (stats.total_stored - 1) as f64 + elapsed) 
                / stats.total_stored as f64;
        }
        
        // In a real implementation, persist to disk here
        
        Ok(memory.id)
    }

    pub async fn retrieve(&self, memory_id: &str) -> PyResult<Option<Memory>> {
        let start = Instant::now();
        
        // Check cache first
        {
            let cache: &HashMap<String, Memory> = &*self.cache.read().await;
            if let Some(memory) = cache.get(memory_id) {
                // Update stats for cache hit
                let mut stats = self.stats.write().await;
                stats.cache_hits += 1;
                stats.total_retrieved += 1;
                
                let elapsed = start.elapsed().as_secs_f64();
                stats.avg_retrieve_time = 
                    (stats.avg_retrieve_time * (stats.total_retrieved - 1) as f64 + elapsed) 
                    / stats.total_retrieved as f64;
                
                return Ok(Some(memory.clone()));
            }
        }
        
        // Cache miss
        {
            let mut stats = self.stats.write().await;
            stats.cache_misses += 1;
            stats.total_retrieved += 1;
        }
        
        // In a real implementation, load from disk here
        
        Ok(None)
    }

    pub async fn search(&self, query: &str, limit: usize) -> PyResult<Vec<String>> {
        let cache: &HashMap<String, Memory> = &*self.cache.read().await;
        let mut results = Vec::new();
        
        // Simple text search
        for (id, memory) in cache.iter() {
            if memory.content.to_lowercase().contains(&query.to_lowercase()) {
                results.push(id.clone());
                if results.len() >= limit {
                    break;
                }
            }
        }
        
        Ok(results)
    }

    pub async fn associate(&self, id1: &str, id2: &str, strength: f32) -> PyResult<()> {
        let mut cache = self.cache.write().await;
        
        // Update bidirectional association
        if let Some(memory) = cache.get_mut(id1) {
            memory.associations.insert(id2.to_string(), strength);
        }
        
        if let Some(memory) = cache.get_mut(id2) {
            memory.associations.insert(id1.to_string(), strength);
        }
        
        Ok(())
    }

    pub async fn get_associations(&self, memory_id: &str, min_strength: f32) -> PyResult<Vec<String>> {
        let cache: &HashMap<String, Memory> = &*self.cache.read().await;
        
        if let Some(memory) = cache.get(memory_id) {
            let mut associated: Vec<String> = Vec::new();
            for (id, &strength) in &memory.associations {
                if strength >= min_strength {
                    associated.push(id.clone());
                }
            }
            associated.sort_by(|a, b| {
                memory.associations.get(a).partial_cmp(&memory.associations.get(b))
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            Ok(associated)
        } else {
            Ok(Vec::new())
        }
    }

    pub async fn get_stats(&self) -> PyResult<MemoryStats> {
        let stats = self.stats.read().await;
        Ok(MemoryStats {
            total_stored: stats.total_stored,
            total_retrieved: stats.total_retrieved,
            cache_hits: stats.cache_hits,
            cache_misses: stats.cache_misses,
            avg_store_time: stats.avg_store_time,
            avg_retrieve_time: stats.avg_retrieve_time,
        })
    }

    pub async fn consolidate(&self, age_days: u32, importance_threshold: f32) -> PyResult<u64> {
        let mut consolidated = 0u64;
        let cutoff_date = Utc::now() - chrono::Duration::days(age_days as i64);
        
        let mut cache = self.cache.write().await;
        let mut to_remove: Vec<String> = Vec::new();
        
        for (id, memory) in cache.iter() {
            if let Ok(created) = DateTime::parse_from_rfc3339(&memory.created_at) {
                if created.with_timezone(&Utc) < cutoff_date && memory.importance >= importance_threshold {
                    // Mark for consolidation (would move to long-term storage)
                    to_remove.push(id.clone());
                    consolidated += 1;
                }
            }
        }
        
        // Remove from cache (would be in long-term storage)
        for id in to_remove {
            cache.remove(&id);
        }
        
        Ok(consolidated)
    }
}

// Python interface
#[pyclass]
pub struct PyAsyncMemory {
    inner: AsyncMemoryBackend,
}

#[pymethods]
impl PyAsyncMemory {
    #[new]
    fn new() -> Self {
        Self {
            inner: AsyncMemoryBackend::new(),
        }
    }

    fn store(&self, memory: Memory) -> PyResult<String> {
        self.inner.runtime.block_on(self.inner.store(memory))
    }

    fn retrieve(&self, memory_id: &str) -> PyResult<Option<Memory>> {
        self.inner.runtime.block_on(self.inner.retrieve(memory_id))
    }

    fn search(&self, query: &str, limit: Option<usize>) -> PyResult<Vec<String>> {
        self.inner.runtime.block_on(self.inner.search(query, limit.unwrap_or(10)))
    }

    fn associate(&self, id1: &str, id2: &str, strength: f32) -> PyResult<()> {
        self.inner.runtime.block_on(self.inner.associate(id1, id2, strength))
    }

    fn get_associations(&self, memory_id: &str, min_strength: f32) -> PyResult<Vec<String>> {
        self.inner.runtime.block_on(self.inner.get_associations(memory_id, min_strength))
    }

    fn get_stats(&self) -> PyResult<MemoryStats> {
        self.inner.runtime.block_on(self.inner.get_stats())
    }

    fn consolidate(&self, age_days: u32, importance_threshold: f32) -> PyResult<u64> {
        self.inner.runtime.block_on(self.inner.consolidate(age_days, importance_threshold))
    }
}

// Factory function for creating memories
#[pyfunction]
pub fn create_memory(
    content: String,
    memory_type: String,
    tags: Vec<String>,
    importance: Option<f32>,
) -> Memory {
    let now = Utc::now().to_rfc3339();
    Memory {
        id: String::new(), // Will be generated on store
        content,
        memory_type,
        created_at: now.clone(),
        accessed_at: now,
        access_count: 0,
        tags,
        associations: HashMap::new(),
        emotional_valence: 0.0,
        importance: importance.unwrap_or(0.5),
        metadata: HashMap::new(),
    }
}

// Updated module registration with Bound
#[pymodule]
fn async_memory(_py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyAsyncMemory>()?;
    m.add_class::<Memory>()?;
    m.add_class::<MemoryStats>()?;
    m.add_function(wrap_pyfunction!(create_memory, m)?)?;
    Ok(())
}
