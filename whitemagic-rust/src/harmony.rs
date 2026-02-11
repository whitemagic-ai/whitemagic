//! Harmony Vector - Real-time system state monitoring
//! Maps to MandalaOS "Prana Allocation" concept

use serde::{Deserialize, Serialize};
use std::time::Instant;
use sysinfo::System;

#[cfg(feature = "python")]
use pyo3::prelude::*;

/// Harmony Vector - composite system health metric
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "python", pyclass)]
pub struct HarmonyVector {
    pub cpu_load: f32,
    pub memory_pressure: f32,
    pub thermal_celsius: f32,
    pub io_wait: f32,
    pub harmony_score: f32,
    pub timestamp_ms: u128,
}

#[cfg_attr(feature = "python", pymethods)]
impl HarmonyVector {
    #[cfg(feature = "python")]
    #[new]
    pub fn new() -> Self {
        Self::measure_internal()
    }
    
    #[cfg(feature = "python")]
    #[staticmethod]
    pub fn measure() -> Self {
        Self::measure_internal()
    }
    
    #[cfg(feature = "python")]
    #[getter]
    pub fn cpu_load(&self) -> f32 {
        self.cpu_load
    }
    
    #[cfg(feature = "python")]
    #[getter]
    pub fn memory_pressure(&self) -> f32 {
        self.memory_pressure
    }
    
    #[cfg(feature = "python")]
    #[getter]
    pub fn thermal_celsius(&self) -> f32 {
        self.thermal_celsius
    }
    
    #[cfg(feature = "python")]
    #[getter]
    pub fn io_wait(&self) -> f32 {
        self.io_wait
    }
    
    #[cfg(feature = "python")]
    #[getter]
    pub fn harmony_score(&self) -> f32 {
        self.harmony_score
    }
    
    pub fn is_harmonious(&self) -> bool {
        self.harmony_score > 0.7
    }
    
    pub fn guna_tag(&self) -> String {
        match self.harmony_score {
            s if s > 0.7 => "Sattva".to_string(),   // Lowered from 0.8 for IDE workload
            s if s > 0.45 => "Rajas".to_string(),   // Lowered from 0.5
            _ => "Tamas".to_string(),
        }
    }
}

impl HarmonyVector {
    fn measure_internal() -> Self {
        let mut sys = System::new_all();
        sys.refresh_all();
        
        let cpu_load = sys.global_cpu_info().cpu_usage() / 100.0;
        let memory_pressure = sys.used_memory() as f32 / sys.total_memory() as f32;
        let thermal_celsius = 45.0;
        
        let load_avg = System::load_average();
        let cpu_count = sys.cpus().len().max(1) as f32;
        let io_wait = (load_avg.one as f32 / cpu_count).min(1.0);
        
        let mut hv = Self {
            cpu_load,
            memory_pressure,
            thermal_celsius,
            io_wait,
            harmony_score: 0.0,
            timestamp_ms: Instant::now().elapsed().as_millis(),
        };
        hv.calculate_harmony();
        hv
    }
    
    fn calculate_harmony(&mut self) {
        let cpu_factor = 1.0 - self.cpu_load;
        let mem_factor = 1.0 - self.memory_pressure;
        let thermal_factor = self.thermal_health();
        let io_factor = 1.0 - self.io_wait;
        
        // Adjusted weights for development environment
        // CPU and memory more important than transient I/O
        self.harmony_score = 
            (cpu_factor * 0.30) +      // CPU priority
            (mem_factor * 0.35) +      // Memory priority
            (thermal_factor * 0.25) +  // Thermal unchanged
            (io_factor * 0.10);        // I/O less critical
    }
    
    fn thermal_health(&self) -> f32 {
        match self.thermal_celsius {
            t if t < 30.0 => 1.0,
            t if t < 60.0 => 1.0 - (t - 30.0) / 30.0 * 0.2,
            t if t < 80.0 => 0.8 - (t - 60.0) / 20.0 * 0.6,
            _ => 0.2,
        }
    }
}

#[cfg(feature = "python")]
#[pyfunction]
pub fn get_harmony_vector() -> PyResult<HarmonyVector> {
    Ok(HarmonyVector::measure_internal())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_harmony_basics() {
        let hv = HarmonyVector::measure_internal();
        assert!(hv.harmony_score >= 0.0);
        assert!(hv.harmony_score <= 1.0);
    }
}
