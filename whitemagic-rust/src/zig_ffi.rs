//! Safe Rust bindings to the Zig unified memory manager and auxiliary modules.
//!
//! Note: When the Zig library (libwhitemagic-zig.a) is not available,
//! this module provides stub implementations that return sensible defaults.

use pyo3::prelude::*;

// ---------------------------------------------------------------------------
// Mirrored C structs
// ---------------------------------------------------------------------------
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct WmCoordinate {
    pub x: f32, // logic
    pub y: f32, // micro
    pub z: f32, // time
    pub w: f32, // importance
}

// ---------------------------------------------------------------------------
// Stub implementations (Zig library not linked)
// ---------------------------------------------------------------------------

/// Initialise the Zig memory manager. Stub when Zig not available.
pub fn zig_memory_init() {
    // Stub: no-op
}

/// Tear down the Zig memory manager. Stub when Zig not available.
pub fn zig_memory_deinit() {
    // Stub: no-op
}

/// Allocate or re-allocate a named block. Stub returns None.
pub fn zig_memory_alloc(_id: u64, _size: usize, _alignment: u32) -> Option<*mut u8> {
    None // Stub: Zig library not available
}

/// Free a block by ID. Stub is no-op.
pub fn zig_memory_free(_id: u64) {
    // Stub: no-op
}

/// Compact the heap. Stub returns false.
pub fn zig_memory_rearrange() -> bool {
    false // Stub: Zig library not available
}

/// Number of live blocks. Stub returns 0.
pub fn zig_memory_block_count() -> usize {
    0 // Stub: Zig library not available
}

/// Resolve a handle to its current pointer. Stub returns None.
pub fn zig_memory_resolve(_id: u64) -> Option<*mut u8> {
    None // Stub: Zig library not available
}

/// Dump a JSON stats file. Stub returns false.
pub fn zig_memory_dump_stats(_path: &str) -> bool {
    false // Stub: Zig library not available
}

/// Project a vector into 4-D holographic space. Stub returns None.
pub fn zig_holographic_project(_input: &[f32], _result_id: u64) -> Option<WmCoordinate> {
    None // Stub: Zig library not available
}

/// One-step metabolic flux simulation. Stub returns 0.0.
pub fn zig_genomics_flux(_rate: f32, _concentration: f32) -> f32 {
    0.0 // Stub: Zig library not available
}

/// Entropy-seeded hexagram cast. Stub returns all zeros.
pub fn zig_iching_cast() -> [u8; 6] {
    [0u8; 6] // Stub: Zig library not available
}

// ---------------------------------------------------------------------------
// PyO3 exposure — thin wrappers callable from Python
// ---------------------------------------------------------------------------

#[pyfunction]
/// Initialise the Zig memory manager from Python.
pub fn zig_py_init() {
    zig_memory_init();
}

#[pyfunction]
/// Return the current live block count.
pub fn zig_py_block_count() -> usize {
    zig_memory_block_count()
}

#[pyfunction]
/// Compact the Zig heap. Returns True on success.
pub fn zig_py_rearrange() -> bool {
    zig_memory_rearrange()
}

#[pyfunction]
/// Dump Zig memory stats to a JSON file at *path*.
pub fn zig_py_dump_stats(path: &str) -> bool {
    zig_memory_dump_stats(path)
}

#[pyfunction]
/// Project a list of floats into 4-D holographic coordinates.
/// Returns (x, y, z, w) or None.
pub fn zig_py_holographic_project(input: Vec<f32>, result_id: u64) -> Option<(f32, f32, f32, f32)> {
    zig_holographic_project(&input, result_id).map(|c| (c.x, c.y, c.z, c.w))
}

#[pyfunction]
/// One-step metabolic flux.
pub fn zig_py_genomics_flux(rate: f32, concentration: f32) -> f32 {
    zig_genomics_flux(rate, concentration)
}

#[pyfunction]
/// Entropy-seeded I Ching cast. Returns list of 6 ints (0 or 1).
pub fn zig_py_iching_cast() -> Vec<u8> {
    zig_iching_cast().to_vec()
}
