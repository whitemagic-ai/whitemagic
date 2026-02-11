//! Fast compression using flate2 (gzip)
//! 
//! flate2 provides fast compression with good ratios:
//! - Standard gzip/zlib compression
//! - Good balance of speed and compression
//! - Ideal for memory archives

use flate2::write::{GzEncoder, GzDecoder};
use flate2::Compression;
use pyo3::prelude::*;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

/// Compress a file using gzip
pub fn compress_file(input: &Path, output: &Path) -> PyResult<u64> {
    let mut input_file = File::open(input)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    // Use gzip compression (fast level)
    let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
    encoder
        .write_all(&input_data)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    
    let compressed = encoder
        .finish()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    
    let mut output_file = File::create(output)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    output_file
        .write_all(&compressed)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    Ok(compressed.len() as u64)
}

/// Decompress a gzip file
pub fn decompress_file(input: &Path, output: &Path) -> PyResult<u64> {
    let mut input_file = File::open(input)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    let mut compressed_data = Vec::new();
    input_file
        .read_to_end(&mut compressed_data)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    // Decompress with gzip
    let mut decoder = GzDecoder::new(Vec::new());
    decoder
        .write_all(&compressed_data)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    
    let decompressed = decoder
        .finish()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    
    let mut output_file = File::create(output)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    output_file
        .write_all(&decompressed)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(e.to_string()))?;
    
    Ok(decompressed.len() as u64)
}

/// Compress a file using gzip (Python-friendly wrapper)
#[pyfunction(name = "compress_file")]
pub fn compress_file_py(input_path: String, output_path: String) -> PyResult<u64> {
    compress_file(Path::new(&input_path), Path::new(&output_path))
}

/// Decompress a gzip file (Python-friendly wrapper)
#[pyfunction(name = "decompress_file")]
pub fn decompress_file_py(input_path: String, output_path: String) -> PyResult<u64> {
    decompress_file(Path::new(&input_path), Path::new(&output_path))
}

/// Compress in-memory data using gzip
#[pyfunction]
pub fn fast_compress(data: &str) -> PyResult<Vec<u8>> {
    let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
    encoder
        .write_all(data.as_bytes())
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    let compressed = encoder
        .finish()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    Ok(compressed)
}

/// Decompress in-memory gzip data
#[pyfunction]
pub fn fast_decompress(data: Vec<u8>) -> PyResult<String> {
    let mut decoder = GzDecoder::new(Vec::new());
    decoder
        .write_all(&data)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    let decompressed = decoder
        .finish()
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    String::from_utf8(decompressed)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(e.to_string()))
}
