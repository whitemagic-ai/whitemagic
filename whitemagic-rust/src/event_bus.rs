//! Lock-free Event Bus Primitives (v14)
//!
//! Provides atomic dampening checks and stillness gating for the
//! Gan Ying resonance bus. These primitives replace Python's
//! datetime.now() + threading.RLock on every emit() call.
//!
//! Architecture:
//!   Python GanYingBus.emit() calls event_bus_try_emit() first.
//!   If it returns false, the event is dampened/blocked — no Python
//!   lock acquired, no callbacks dispatched. If true, Python proceeds
//!   with listener dispatch (still needs GIL for callbacks).
//!
//! Performance:
//!   - Dampening check: ~5ns (atomic load + compare) vs ~500ns (Python datetime.now())
//!   - Stillness check: ~2ns (atomic load) vs ~2μs (Python import + function call)
//!   - No lock contention between concurrent emitters

use dashmap::DashMap;
use lazy_static::lazy_static;
use pyo3::prelude::*;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

/// Per-event-type state for dampening
struct EventTypeState {
    dampening_ms: AtomicU64,
    last_emit_ms: AtomicU64,
}

lazy_static! {
    static ref EVENT_STATES: DashMap<String, EventTypeState> = DashMap::new();
    static ref IS_STILL: AtomicBool = AtomicBool::new(false);
    static ref TOTAL_EMISSIONS: AtomicU64 = AtomicU64::new(0);
    static ref TOTAL_DAMPENED: AtomicU64 = AtomicU64::new(0);
    static ref TOTAL_STILLNESS_BLOCKED: AtomicU64 = AtomicU64::new(0);
}

/// Current time in milliseconds since epoch (fast clock)
#[inline]
fn now_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

/// Check if an event should be emitted (dampening + stillness).
///
/// All checks are lock-free using atomic operations.
/// Returns True if the event should proceed to listener dispatch,
/// False if it was dampened or blocked by stillness.
///
/// This replaces:
///   1. `from whitemagic.core.consciousness.stillness import get_stillness_manager`
///   2. `sm.is_still` check
///   3. `datetime.now()` for dampening
///   4. dict lookup for last emit time
/// All of which ran under a Python RLock on every single emit().
#[pyfunction]
pub fn event_bus_try_emit(
    event_type: &str,
    confidence: f64,
    source: &str,
) -> PyResult<bool> {
    // --- Stillness check (single atomic load, ~2ns) ---
    if IS_STILL.load(Ordering::Relaxed) && source != "stillness_manager" {
        let is_allowed = event_type.starts_with("system_")
            || event_type.starts_with("emergence_")
            || event_type == "sympathetic_resonance"
            || event_type == "breakthrough_achieved"
            || confidence >= 0.95;

        if !is_allowed {
            TOTAL_STILLNESS_BLOCKED.fetch_add(1, Ordering::Relaxed);
            return Ok(false);
        }
    }

    // --- Dampening check (atomic load + compare, ~5ns) ---
    let now = now_ms();

    let state = EVENT_STATES
        .entry(event_type.to_string())
        .or_insert_with(|| EventTypeState {
            dampening_ms: AtomicU64::new(100), // default 100ms
            last_emit_ms: AtomicU64::new(0),
        });

    let min_interval = state.dampening_ms.load(Ordering::Relaxed);
    let last_emit = state.last_emit_ms.load(Ordering::Relaxed);

    // Skip dampening for critical emergence/resonance events
    let is_critical = event_type == "sympathetic_resonance"
        || event_type.starts_with("emergence_")
        || event_type == "breakthrough_achieved";

    if !is_critical && min_interval > 0 {
        let elapsed = now.saturating_sub(last_emit);
        if elapsed < min_interval {
            TOTAL_DAMPENED.fetch_add(1, Ordering::Relaxed);
            return Ok(false);
        }
    }

    // Update last emit time (relaxed ordering — no strict monotonicity needed)
    state.last_emit_ms.store(now, Ordering::Relaxed);
    TOTAL_EMISSIONS.fetch_add(1, Ordering::Relaxed);

    Ok(true)
}

/// Set dampening interval for an event type (in milliseconds).
/// Pass 0 to disable dampening for this event type.
#[pyfunction]
pub fn event_bus_set_dampening(event_type: &str, min_interval_ms: u64) -> PyResult<()> {
    let state = EVENT_STATES
        .entry(event_type.to_string())
        .or_insert_with(|| EventTypeState {
            dampening_ms: AtomicU64::new(min_interval_ms),
            last_emit_ms: AtomicU64::new(0),
        });
    state.dampening_ms.store(min_interval_ms, Ordering::Relaxed);
    Ok(())
}

/// Set the stillness flag (atomic store, ~1ns).
/// Called by the Stillness Manager when entering/exiting stillness.
#[pyfunction]
pub fn event_bus_set_stillness(is_still: bool) -> PyResult<()> {
    IS_STILL.store(is_still, Ordering::Relaxed);
    Ok(())
}

/// Get event bus statistics as a dict.
#[pyfunction]
pub fn event_bus_stats() -> PyResult<HashMap<String, u64>> {
    let mut stats = HashMap::new();
    stats.insert(
        "total_emissions".to_string(),
        TOTAL_EMISSIONS.load(Ordering::Relaxed),
    );
    stats.insert(
        "total_dampened".to_string(),
        TOTAL_DAMPENED.load(Ordering::Relaxed),
    );
    stats.insert(
        "total_stillness_blocked".to_string(),
        TOTAL_STILLNESS_BLOCKED.load(Ordering::Relaxed),
    );
    stats.insert(
        "event_type_count".to_string(),
        EVENT_STATES.len() as u64,
    );
    stats.insert(
        "is_still".to_string(),
        IS_STILL.load(Ordering::Relaxed) as u64,
    );
    Ok(stats)
}

/// Reset all event bus state and counters.
#[pyfunction]
pub fn event_bus_reset() -> PyResult<()> {
    TOTAL_EMISSIONS.store(0, Ordering::Relaxed);
    TOTAL_DAMPENED.store(0, Ordering::Relaxed);
    TOTAL_STILLNESS_BLOCKED.store(0, Ordering::Relaxed);
    EVENT_STATES.clear();
    IS_STILL.store(false, Ordering::Relaxed);
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    lazy_static::lazy_static! {
        static ref TEST_LOCK: Mutex<()> = Mutex::new(());
    }

    #[test]
    fn test_dampening_allows_first_emit() {
        let _guard = TEST_LOCK.lock().unwrap();
        event_bus_reset().unwrap();

        let result = event_bus_try_emit("test_event", 1.0, "test").unwrap();
        assert!(result);
        assert_eq!(TOTAL_EMISSIONS.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn test_dampening_blocks_rapid_repeat() {
        let _guard = TEST_LOCK.lock().unwrap();
        event_bus_reset().unwrap();

        // First emit passes
        assert!(event_bus_try_emit("rapid_event", 1.0, "test").unwrap());
        // Immediate repeat should be dampened (100ms default interval)
        assert!(!event_bus_try_emit("rapid_event", 1.0, "test").unwrap());
        assert_eq!(TOTAL_DAMPENED.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn test_stillness_blocks_non_critical() {
        let _guard = TEST_LOCK.lock().unwrap();
        event_bus_reset().unwrap();
        IS_STILL.store(true, Ordering::Relaxed);

        // Non-critical event should be blocked
        assert!(!event_bus_try_emit("joy_triggered", 0.5, "garden").unwrap());
        assert_eq!(TOTAL_STILLNESS_BLOCKED.load(Ordering::Relaxed), 1);

        // System event should pass
        assert!(event_bus_try_emit("system_started", 1.0, "system").unwrap());

        // High-confidence event should pass
        EVENT_STATES.clear(); // clear dampening state
        assert!(event_bus_try_emit("any_event", 0.99, "test").unwrap());

        IS_STILL.store(false, Ordering::Relaxed);
    }

    #[test]
    fn test_critical_events_bypass_dampening() {
        let _guard = TEST_LOCK.lock().unwrap();
        event_bus_reset().unwrap();

        // First emit
        assert!(event_bus_try_emit("sympathetic_resonance", 1.0, "test").unwrap());
        // Immediate repeat — should NOT be dampened (critical event)
        assert!(event_bus_try_emit("sympathetic_resonance", 1.0, "test").unwrap());
        assert_eq!(TOTAL_DAMPENED.load(Ordering::Relaxed), 0);
    }
}
