//! Shared memory (mmap) state for cross-process coordination (v13.3.2)
//!
//! Maps rate limiter counters and pipeline metadata into a memory-mapped
//! file at ~/.whitemagic/cache/shared_state.bin. Multiple Python workers
//! (or any process loading whitemagic_rs) see the same atomic counters
//! without any IPC overhead.
//!
//! Layout (64 bytes per slot, 256 slots = 16KB total):
//!   [0..8]   magic: u64 = 0x574D_5348_4152_4544 ("WMSHARED")
//!   [8..16]  version: u64 = 1
//!   [16..24] total_checks: AtomicU64
//!   [24..32] total_allowed: AtomicU64
//!   [32..40] total_denied: AtomicU64
//!   [40..48] last_update_ms: AtomicU64
//!   [48..56] pipeline_calls: AtomicU64
//!   [56..64] reserved
//!   [64..16384] per-tool counters (253 slots × 64 bytes)

use pyo3::prelude::*;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Once;

const MAGIC: u64 = 0x574D_5348_4152_4544; // "WMSHARED"
const VERSION: u64 = 1;
const FILE_SIZE: usize = 16384; // 16KB
const _HEADER_SIZE: usize = 64;

/// Global counters offset in the mmap
const OFF_TOTAL_CHECKS: usize = 16;
const OFF_TOTAL_ALLOWED: usize = 24;
const OFF_TOTAL_DENIED: usize = 32;
const OFF_LAST_UPDATE: usize = 40;
const OFF_PIPELINE_CALLS: usize = 48;

static INIT: Once = Once::new();
static mut MMAP_PTR: *mut u8 = std::ptr::null_mut();
static mut MMAP_LEN: usize = 0;

fn shared_state_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    let wm_root = std::env::var("WM_STATE_ROOT")
        .unwrap_or_else(|_| format!("{}/.whitemagic", home));
    PathBuf::from(wm_root).join("cache").join("shared_state.bin")
}

fn ensure_file(path: &PathBuf) -> std::io::Result<File> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    if !path.exists() || std::fs::metadata(path)?.len() < FILE_SIZE as u64 {
        let mut f = File::create(path)?;
        let buf = vec![0u8; FILE_SIZE];
        f.write_all(&buf)?;
        f.sync_all()?;
        // Write magic + version
        let f = OpenOptions::new().read(true).write(true).open(path)?;
        return Ok(f);
    }

    OpenOptions::new().read(true).write(true).open(path)
}

fn init_mmap() {
    INIT.call_once(|| {
        let path = shared_state_path();
        match ensure_file(&path) {
            Ok(file) => {
                unsafe {
                    let fd = {
                        use std::os::unix::io::AsRawFd;
                        file.as_raw_fd()
                    };
                    let ptr = libc::mmap(
                        std::ptr::null_mut(),
                        FILE_SIZE,
                        libc::PROT_READ | libc::PROT_WRITE,
                        libc::MAP_SHARED,
                        fd,
                        0,
                    );
                    if ptr == libc::MAP_FAILED {
                        eprintln!("shared_state: mmap failed");
                        return;
                    }
                    MMAP_PTR = ptr as *mut u8;
                    MMAP_LEN = FILE_SIZE;

                    // Write magic + version if not present
                    let magic_ptr = MMAP_PTR as *mut u64;
                    if *magic_ptr != MAGIC {
                        *magic_ptr = MAGIC;
                        *(magic_ptr.add(1)) = VERSION;
                    }
                }
            }
            Err(e) => {
                eprintln!("shared_state: failed to create file: {}", e);
            }
        }
    });
}

/// Get an atomic u64 reference at a byte offset in the mmap.
/// Returns None if mmap is not initialized.
fn get_atomic(offset: usize) -> Option<&'static AtomicU64> {
    unsafe {
        if MMAP_PTR.is_null() || offset + 8 > MMAP_LEN {
            return None;
        }
        Some(&*(MMAP_PTR.add(offset) as *const AtomicU64))
    }
}

fn now_ms() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Record a rate check event in shared memory.
pub fn record_check(allowed: bool) {
    init_mmap();
    if let Some(total) = get_atomic(OFF_TOTAL_CHECKS) {
        total.fetch_add(1, Ordering::Relaxed);
    }
    if allowed {
        if let Some(a) = get_atomic(OFF_TOTAL_ALLOWED) {
            a.fetch_add(1, Ordering::Relaxed);
        }
    } else {
        if let Some(d) = get_atomic(OFF_TOTAL_DENIED) {
            d.fetch_add(1, Ordering::Relaxed);
        }
    }
    if let Some(ts) = get_atomic(OFF_LAST_UPDATE) {
        ts.store(now_ms(), Ordering::Relaxed);
    }
}

/// Record a pipeline execution.
pub fn record_pipeline_call() {
    init_mmap();
    if let Some(p) = get_atomic(OFF_PIPELINE_CALLS) {
        p.fetch_add(1, Ordering::Relaxed);
    }
    if let Some(ts) = get_atomic(OFF_LAST_UPDATE) {
        ts.store(now_ms(), Ordering::Relaxed);
    }
}

// ---------------------------------------------------------------------------
// PyO3 bindings
// ---------------------------------------------------------------------------

/// Get shared state statistics as JSON.
/// Returns counters visible to ALL processes sharing the mmap file.
#[pyfunction]
pub fn shared_state_stats() -> PyResult<String> {
    init_mmap();
    let total_checks = get_atomic(OFF_TOTAL_CHECKS)
        .map(|a| a.load(Ordering::Relaxed))
        .unwrap_or(0);
    let total_allowed = get_atomic(OFF_TOTAL_ALLOWED)
        .map(|a| a.load(Ordering::Relaxed))
        .unwrap_or(0);
    let total_denied = get_atomic(OFF_TOTAL_DENIED)
        .map(|a| a.load(Ordering::Relaxed))
        .unwrap_or(0);
    let last_update = get_atomic(OFF_LAST_UPDATE)
        .map(|a| a.load(Ordering::Relaxed))
        .unwrap_or(0);
    let pipeline_calls = get_atomic(OFF_PIPELINE_CALLS)
        .map(|a| a.load(Ordering::Relaxed))
        .unwrap_or(0);

    Ok(format!(
        r#"{{"total_checks":{},"total_allowed":{},"total_denied":{},"last_update_ms":{},"pipeline_calls":{},"mmap_active":{}}}"#,
        total_checks, total_allowed, total_denied, last_update, pipeline_calls,
        unsafe { !MMAP_PTR.is_null() }
    ))
}

/// Reset all shared state counters to zero.
#[pyfunction]
pub fn shared_state_reset() -> PyResult<()> {
    init_mmap();
    for off in [OFF_TOTAL_CHECKS, OFF_TOTAL_ALLOWED, OFF_TOTAL_DENIED, OFF_LAST_UPDATE, OFF_PIPELINE_CALLS] {
        if let Some(a) = get_atomic(off) {
            a.store(0, Ordering::Relaxed);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shared_state_path() {
        let path = shared_state_path();
        assert!(path.to_string_lossy().contains("shared_state.bin"));
    }
}
