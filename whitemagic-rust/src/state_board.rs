//! StateBoard — Shared-Memory Blackboard for WhiteMagic Nervous System (Leap 7a)
//!
//! A contiguous, mmap-readable memory region holding the system's real-time
//! vital signs. Python reads it via zero-copy pointer arithmetic; Rust tools
//! write to it with atomic operations.
//!
//! Layout (4096 bytes total):
//!   [0..8]     magic: u64 = 0x574D_424F_4152_4400 ("WMBOARD\0")
//!   [8..16]    version: u64 = 1
//!   [16..24]   tick: AtomicU64 (monotonic sequence counter)
//!
//!   --- Harmony Vector (7 × f64 = 56 bytes) ---
//!   [64..72]   balance: f64
//!   [72..80]   throughput: f64
//!   [80..88]   latency: f64
//!   [88..96]   error_rate: f64
//!   [96..104]  dharma: f64
//!   [104..112] karma_debt: f64
//!   [112..120] energy: f64
//!
//!   --- System State (64 bytes) ---
//!   [128..136] guna: u64 (0=sattvic, 1=rajasic, 2=tamasic)
//!   [136..144] wu_xing_phase: u64 (0=wood, 1=fire, 2=earth, 3=metal, 4=water)
//!   [144..152] current_gana: u64 (1-28)
//!   [152..160] predecessor_gana: u64 (1-28)
//!   [160..168] successor_gana: u64 (1-28)
//!   [168..176] quadrant: u64 (0=east, 1=south, 2=west, 3=north)
//!   [176..184] active_engines: u64 (bitmask, 28 bits)
//!   [184..192] last_update_ns: u64
//!
//!   --- Circuit Breakers (64 slots × 16 bytes = 1024 bytes) ---
//!   [256..1280] per-tool breaker state
//!     each slot: [state: u64 (0=closed, 1=open, 2=half_open), failure_count: u64]
//!
//!   --- Event Counters (64 slots × 8 bytes = 512 bytes) ---
//!   [1280..1792] per-tool call counters (for rate limiting)
//!
//!   --- Reserved ---
//!   [1792..4096] future expansion

use pyo3::prelude::*;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Once;

const MAGIC: u64 = 0x574D_424F_4152_4400; // "WMBOARD\0"
const VERSION: u64 = 1;
const FILE_SIZE: usize = 4096;

// Offsets
const OFF_TICK: usize = 16;

// Harmony Vector offsets (cache-line aligned at 64)
const OFF_HV_BALANCE: usize = 64;
const OFF_HV_THROUGHPUT: usize = 72;
const OFF_HV_LATENCY: usize = 80;
const OFF_HV_ERROR_RATE: usize = 88;
const OFF_HV_DHARMA: usize = 96;
const OFF_HV_KARMA_DEBT: usize = 104;
const OFF_HV_ENERGY: usize = 112;

// System state offsets (cache-line aligned at 128)
const OFF_GUNA: usize = 128;
const OFF_WU_XING: usize = 136;
const OFF_CURRENT_GANA: usize = 144;
const OFF_PREDECESSOR: usize = 152;
const OFF_SUCCESSOR: usize = 160;
const OFF_QUADRANT: usize = 168;
const OFF_ACTIVE_ENGINES: usize = 176;
const OFF_LAST_UPDATE: usize = 184;

// Circuit breaker region
const OFF_BREAKERS: usize = 256;
const BREAKER_SLOT_SIZE: usize = 16;
const MAX_BREAKER_SLOTS: usize = 64;

// Rate counter region
const OFF_COUNTERS: usize = 1280;
const MAX_COUNTER_SLOTS: usize = 64;

static INIT: Once = Once::new();
static mut MMAP_PTR: *mut u8 = std::ptr::null_mut();

fn board_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    let wm_root =
        std::env::var("WM_STATE_ROOT").unwrap_or_else(|_| format!("{}/.whitemagic", home));
    PathBuf::from(wm_root).join("cache").join("state_board.bin")
}

fn ensure_file(path: &PathBuf) -> std::io::Result<File> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    if !path.exists() || std::fs::metadata(path)?.len() < FILE_SIZE as u64 {
        let mut f = File::create(path)?;
        f.write_all(&vec![0u8; FILE_SIZE])?;
        f.sync_all()?;
    }
    OpenOptions::new().read(true).write(true).open(path)
}

fn init_board() {
    INIT.call_once(|| {
        let primary = board_path();
        let file = match ensure_file(&primary) {
            Ok(f) => Some(f),
            Err(primary_err) => {
                // Fallback to a temp-rooted state path if HOME/WM_STATE_ROOT is not writable.
                let fallback_root = std::env::temp_dir().join("whitemagic_state");
                std::env::set_var("WM_STATE_ROOT", fallback_root.to_string_lossy().to_string());
                let fallback = board_path();
                match ensure_file(&fallback) {
                    Ok(f) => Some(f),
                    Err(fallback_err) => {
                        eprintln!(
                            "state_board: failed to create file at {} ({}) and fallback {} ({})",
                            primary.display(),
                            primary_err,
                            fallback.display(),
                            fallback_err
                        );
                        None
                    }
                }
            }
        };

        if let Some(file) = file {
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
                    eprintln!("state_board: mmap failed");
                    return;
                }
                MMAP_PTR = ptr as *mut u8;
                let magic_ptr = MMAP_PTR as *mut u64;
                if *magic_ptr != MAGIC {
                    *magic_ptr = MAGIC;
                    *(magic_ptr.add(1)) = VERSION;
                }
            }
        }
    });
}

#[inline]
fn get_atomic(offset: usize) -> Option<&'static AtomicU64> {
    unsafe {
        if MMAP_PTR.is_null() || offset + 8 > FILE_SIZE {
            return None;
        }
        Some(&*(MMAP_PTR.add(offset) as *const AtomicU64))
    }
}

#[inline]
fn read_f64(offset: usize) -> f64 {
    unsafe {
        if MMAP_PTR.is_null() || offset + 8 > FILE_SIZE {
            return 0.0;
        }
        f64::from_bits((*(MMAP_PTR.add(offset) as *const AtomicU64)).load(Ordering::Relaxed))
    }
}

#[inline]
fn write_f64(offset: usize, val: f64) {
    if let Some(a) = get_atomic(offset) {
        a.store(val.to_bits(), Ordering::Relaxed);
    }
}

#[inline]
fn read_u64(offset: usize) -> u64 {
    get_atomic(offset)
        .map(|a| a.load(Ordering::Relaxed))
        .unwrap_or(0)
}

#[inline]
fn write_u64(offset: usize, val: u64) {
    if let Some(a) = get_atomic(offset) {
        a.store(val, Ordering::Relaxed);
    }
}

fn now_ns() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64
}

// ---------------------------------------------------------------------------
// Public Rust API (used by other Rust modules)
// ---------------------------------------------------------------------------

/// Advance the tick counter and update timestamp. Returns new tick.
pub fn tick() -> u64 {
    init_board();
    let new_tick = get_atomic(OFF_TICK)
        .map(|a| a.fetch_add(1, Ordering::AcqRel) + 1)
        .unwrap_or(0);
    write_u64(OFF_LAST_UPDATE, now_ns());
    new_tick
}

/// Write the full Harmony Vector in one call.
pub fn write_harmony(
    balance: f64,
    throughput: f64,
    latency: f64,
    error_rate: f64,
    dharma: f64,
    karma_debt: f64,
    energy: f64,
) {
    init_board();
    write_f64(OFF_HV_BALANCE, balance);
    write_f64(OFF_HV_THROUGHPUT, throughput);
    write_f64(OFF_HV_LATENCY, latency);
    write_f64(OFF_HV_ERROR_RATE, error_rate);
    write_f64(OFF_HV_DHARMA, dharma);
    write_f64(OFF_HV_KARMA_DEBT, karma_debt);
    write_f64(OFF_HV_ENERGY, energy);
    tick();
}

/// Update a circuit breaker slot.
pub fn write_breaker(tool_slot: usize, state: u64, failures: u64) {
    if tool_slot >= MAX_BREAKER_SLOTS {
        return;
    }
    init_board();
    let base = OFF_BREAKERS + tool_slot * BREAKER_SLOT_SIZE;
    write_u64(base, state);
    write_u64(base + 8, failures);
}

/// Read a circuit breaker slot. Returns (state, failure_count).
pub fn read_breaker(tool_slot: usize) -> (u64, u64) {
    if tool_slot >= MAX_BREAKER_SLOTS {
        return (0, 0);
    }
    init_board();
    let base = OFF_BREAKERS + tool_slot * BREAKER_SLOT_SIZE;
    (read_u64(base), read_u64(base + 8))
}

/// Increment a per-tool call counter (for rate limiting).
pub fn increment_counter(tool_slot: usize) -> u64 {
    if tool_slot >= MAX_COUNTER_SLOTS {
        return 0;
    }
    init_board();
    let off = OFF_COUNTERS + tool_slot * 8;
    get_atomic(off)
        .map(|a| a.fetch_add(1, Ordering::Relaxed) + 1)
        .unwrap_or(0)
}

// ---------------------------------------------------------------------------
// PyO3 bindings
// ---------------------------------------------------------------------------

/// Write the full Harmony Vector from Python.
#[pyfunction]
pub fn board_write_harmony(
    balance: f64,
    throughput: f64,
    latency: f64,
    error_rate: f64,
    dharma: f64,
    karma_debt: f64,
    energy: f64,
) -> PyResult<u64> {
    write_harmony(balance, throughput, latency, error_rate, dharma, karma_debt, energy);
    Ok(read_u64(OFF_TICK))
}

/// Read the Harmony Vector as a dict.
#[pyfunction]
pub fn board_read_harmony() -> PyResult<HashMap<String, f64>> {
    init_board();
    let mut hv = HashMap::new();
    hv.insert("balance".to_string(), read_f64(OFF_HV_BALANCE));
    hv.insert("throughput".to_string(), read_f64(OFF_HV_THROUGHPUT));
    hv.insert("latency".to_string(), read_f64(OFF_HV_LATENCY));
    hv.insert("error_rate".to_string(), read_f64(OFF_HV_ERROR_RATE));
    hv.insert("dharma".to_string(), read_f64(OFF_HV_DHARMA));
    hv.insert("karma_debt".to_string(), read_f64(OFF_HV_KARMA_DEBT));
    hv.insert("energy".to_string(), read_f64(OFF_HV_ENERGY));
    Ok(hv)
}

/// Write resonance state (current gana, predecessor, successor, quadrant, wu_xing).
#[pyfunction]
pub fn board_write_resonance(
    current_gana: u64,
    predecessor: u64,
    successor: u64,
    quadrant: u64,
    wu_xing: u64,
    guna: u64,
) -> PyResult<u64> {
    init_board();
    write_u64(OFF_CURRENT_GANA, current_gana);
    write_u64(OFF_PREDECESSOR, predecessor);
    write_u64(OFF_SUCCESSOR, successor);
    write_u64(OFF_QUADRANT, quadrant);
    write_u64(OFF_WU_XING, wu_xing);
    write_u64(OFF_GUNA, guna);
    Ok(tick())
}

/// Read the full board state as a dict.
#[pyfunction]
pub fn board_read_state() -> PyResult<HashMap<String, u64>> {
    init_board();
    let mut state = HashMap::new();
    state.insert("tick".to_string(), read_u64(OFF_TICK));
    state.insert("guna".to_string(), read_u64(OFF_GUNA));
    state.insert("wu_xing_phase".to_string(), read_u64(OFF_WU_XING));
    state.insert("current_gana".to_string(), read_u64(OFF_CURRENT_GANA));
    state.insert("predecessor_gana".to_string(), read_u64(OFF_PREDECESSOR));
    state.insert("successor_gana".to_string(), read_u64(OFF_SUCCESSOR));
    state.insert("quadrant".to_string(), read_u64(OFF_QUADRANT));
    state.insert("active_engines".to_string(), read_u64(OFF_ACTIVE_ENGINES));
    state.insert("last_update_ns".to_string(), read_u64(OFF_LAST_UPDATE));
    Ok(state)
}

/// Write a circuit breaker state from Python.
#[pyfunction]
pub fn board_write_breaker(tool_slot: usize, state: u64, failures: u64) -> PyResult<()> {
    write_breaker(tool_slot, state, failures);
    Ok(())
}

/// Read a circuit breaker state from Python. Returns (state, failures).
#[pyfunction]
pub fn board_read_breaker(tool_slot: usize) -> PyResult<(u64, u64)> {
    Ok(read_breaker(tool_slot))
}

/// Increment a tool call counter. Returns new count.
#[pyfunction]
pub fn board_increment_counter(tool_slot: usize) -> PyResult<u64> {
    Ok(increment_counter(tool_slot))
}

/// Set the active engines bitmask (28 bits, one per engine).
#[pyfunction]
pub fn board_set_active_engines(mask: u64) -> PyResult<()> {
    init_board();
    write_u64(OFF_ACTIVE_ENGINES, mask);
    Ok(())
}

/// Get the mmap file path (for Python mmap fallback).
#[pyfunction]
pub fn board_get_path() -> PyResult<String> {
    Ok(board_path().to_string_lossy().to_string())
}

/// Reset the entire board to zeros (preserving magic + version).
#[pyfunction]
pub fn board_reset() -> PyResult<()> {
    init_board();
    unsafe {
        if !MMAP_PTR.is_null() {
            // Zero everything after magic+version (16 bytes)
            std::ptr::write_bytes(MMAP_PTR.add(16), 0, FILE_SIZE - 16);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_board_path() {
        let path = board_path();
        assert!(path.to_string_lossy().contains("state_board.bin"));
    }

    #[test]
    fn test_f64_roundtrip() {
        init_board();
        let val = 0.42_f64;
        write_f64(OFF_HV_BALANCE, val);
        let read = read_f64(OFF_HV_BALANCE);
        assert!((read - val).abs() < f64::EPSILON);
    }

    #[test]
    fn test_tick_monotonic() {
        init_board();
        let t1 = tick();
        let t2 = tick();
        assert!(t2 > t1);
    }

    #[test]
    fn test_breaker_roundtrip() {
        init_board();
        write_breaker(0, 1, 5); // OPEN, 5 failures
        let (state, failures) = read_breaker(0);
        assert_eq!(state, 1);
        assert_eq!(failures, 5);
    }

    #[test]
    fn test_counter_increment() {
        init_board();
        // Reset counter first
        write_u64(OFF_COUNTERS, 0);
        let c1 = increment_counter(0);
        let c2 = increment_counter(0);
        assert_eq!(c2, c1 + 1);
    }
}
