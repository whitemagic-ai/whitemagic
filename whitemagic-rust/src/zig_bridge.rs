//! Zig Bridge - wrapper around Zig library functions
//!
//! This module provides safe wrappers around Zig FFI functions.
//! The actual FFI declarations are in zig_ffi.rs.
//!
//! Note: These are stubs when Zig library is not available.
//! The build will proceed without Zig acceleration.

use std::ffi::c_void;

#[repr(C)]
pub struct ProjectionResult {
    pub ptr: *mut c_void,
    pub count: usize,
    pub error_code: u32,
}

/// A wrapper for the Zig transient allocator to ensure safe usage in Rust.
/// Currently a stub when Zig library is not available.
pub struct ZigTransientAllocator;

impl ZigTransientAllocator {
    pub fn init() {
        // Stub: Zig library not linked
    }

    pub fn alloc(_size: usize) -> *mut c_void {
        // Stub: return null when Zig library not linked
        std::ptr::null_mut()
    }

    pub fn reset() {
        // Stub: Zig library not linked
    }

    pub fn deinit() {
        // Stub: Zig library not linked
    }
}

pub struct ZigUnifiedMemory;

impl ZigUnifiedMemory {
    pub fn init() {
        // Stub: Zig library not linked
    }

    pub fn deinit() {
        // Stub: Zig library not linked
    }

    pub fn alloc(_id: u64, _size: usize, _alignment: u32) -> *mut c_void {
        // Stub: return null when Zig library not linked
        std::ptr::null_mut()
    }

    pub fn free(_id: u64) {
        // Stub: Zig library not linked
    }
}

pub struct ZigHolographic;

impl ZigHolographic {
    pub fn project(_input: &[f32], _result_id: u64) -> Result<(*mut c_void, usize), String> {
        // Stub: Zig library not linked
        Err("Zig holographic projection not available (library not linked)".to_string())
    }
}
