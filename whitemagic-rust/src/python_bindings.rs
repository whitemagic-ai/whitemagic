//! Python Bindings for Rust Event Processor
//! 
//! Exposes high-performance Rust implementations to Python via PyO3

use pyo3::prelude::*;
use std::sync::Arc;
use std::sync::Mutex;
use crate::event_processor::{EventProcessor, Event, EventType};
use crate::simd_search;

/// Python-accessible event processor
#[pyclass]
pub struct RustEventProcessor {
    processor: Arc<Mutex<EventProcessor>>,
}

#[pymethods]
impl RustEventProcessor {
    #[new]
    fn new(batch_size: Option<usize>) -> Self {
        let processor = EventProcessor::new(batch_size.unwrap_or(100));
        Self {
            processor: Arc::new(Mutex::new(processor)),
        }
    }
    
    /// Emit single event
    fn emit(&self, event_type: u32, source: String, data: String, timestamp: u64) -> PyResult<()> {
        let event_type = EventType::from_u32(event_type)
            .ok_or_else(|| PyErr::new::<pyo3::exceptions::PyValueError, _>("Invalid event type"))?;

        let event = Event {
            event_type,
            source,
            data,
            timestamp,
        };

        let processor = self.processor.lock().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        processor.emit(event);

        Ok(())
    }

    /// Emit batch of events
    fn emit_batch(&self, events: Vec<(u32, String, String, u64)>) -> PyResult<()> {
        let rust_events: Result<Vec<Event>, _> = events.into_iter()
            .map(|(event_type, source, data, timestamp)| {
                EventType::from_u32(event_type)
                    .map(|et| Event {
                        event_type: et,
                        source,
                        data,
                        timestamp,
                    })
                    .ok_or_else(|| PyErr::new::<pyo3::exceptions::PyValueError, _>("Invalid event type"))
            })
            .collect();

        let processor = self.processor.lock().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        processor.emit_batch(rust_events?);

        Ok(())
    }

    /// Process pending events
    fn process_batch(&self) -> PyResult<usize> {
        let processor = self.processor.lock().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(processor.process_batch())
    }

    /// Flush all pending events
    fn flush(&self) -> PyResult<usize> {
        let processor = self.processor.lock().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(processor.flush())
    }

    /// Get statistics
    fn get_stats(&self) -> PyResult<(u64, u64, f64)> {
        let processor = self.processor.lock().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(processor.get_stats())
    }

    /// Get queue size
    fn queue_size(&self) -> PyResult<usize> {
        let processor = self.processor.lock().map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Lock error: {}", e))
        })?;
        Ok(processor.queue_size())
    }
}

/// SIMD memory search (Python exposed)
#[pyfunction]
pub fn rust_search_memories(
    query: String,
    memories: Vec<(String, String)>,  // (id, content)
    threshold: f64,
    limit: usize,
) -> PyResult<Vec<(String, f64)>> {
    Ok(simd_search::parallel_search(&query, &memories, threshold, limit))
}

/// Fast similarity calculation
#[pyfunction]
pub fn rust_similarity(text1: String, text2: String) -> PyResult<f64> {
    Ok(simd_search::simd_similarity(&text1, &text2))
}

/// Fast similarity calculation (alias)
#[pyfunction]
pub fn fast_similarity(text1: String, text2: String) -> PyResult<f64> {
    rust_similarity(text1, text2)
}

/// Register Python module functions
pub fn register_python_functions(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<RustEventProcessor>()?;
    m.add_function(wrap_pyfunction!(rust_search_memories, m)?)?;
    m.add_function(wrap_pyfunction!(rust_similarity, m)?)?;
    m.add_function(wrap_pyfunction!(fast_similarity, m)?)?;
    Ok(())
}
