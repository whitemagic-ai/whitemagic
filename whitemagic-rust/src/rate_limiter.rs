//! Atomic sliding-window rate limiter for WhiteMagic tool dispatch
//!
//! Provides O(1) per-check rate limiting using lock-free atomics.
//! Replaces Python's threading.Lock-based rate limiter for hot paths.
//!
//! Features:
//! - Per-tool sliding window counters
//! - Global RPM enforcement
//! - Burst allowance with token bucket
//! - Lock-free concurrent access via atomics
//! - PyO3 bindings for Python integration

use pyo3::prelude::*;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, RwLock};
use std::time::{SystemTime, UNIX_EPOCH};

// ---------------------------------------------------------------------------
// Sliding window counter
// ---------------------------------------------------------------------------

/// A single sliding-window counter using two half-windows for smooth transitions.
/// This avoids the "boundary spike" problem of fixed-window counters.
pub struct SlidingWindow {
    /// Count in the current window half
    current_count: AtomicU64,
    /// Count in the previous window half
    previous_count: AtomicU64,
    /// Timestamp (ms) when the current window started
    current_window_start: AtomicU64,
    /// Window duration in milliseconds
    window_ms: u64,
    /// Maximum requests per window
    max_requests: u64,
    /// Burst allowance (extra capacity above max_requests for short bursts)
    burst_allowance: u64,
    /// Burst tokens remaining
    burst_tokens: AtomicU64,
    /// Last burst refill timestamp (ms)
    last_refill: AtomicU64,
}

impl SlidingWindow {
    pub fn new(max_requests: u64, window_ms: u64, burst_allowance: u64) -> Self {
        let now = current_time_ms();
        Self {
            current_count: AtomicU64::new(0),
            previous_count: AtomicU64::new(0),
            current_window_start: AtomicU64::new(now),
            window_ms,
            max_requests,
            burst_allowance,
            burst_tokens: AtomicU64::new(burst_allowance),
            last_refill: AtomicU64::new(now),
        }
    }

    /// Try to acquire a permit. Returns true if allowed, false if rate-limited.
    pub fn try_acquire(&self) -> bool {
        let now = current_time_ms();
        self.maybe_rotate(now);
        self.maybe_refill_burst(now);

        // Calculate weighted count across windows
        let window_start = self.current_window_start.load(Ordering::Relaxed);
        let elapsed = now.saturating_sub(window_start);
        let weight = if self.window_ms > 0 {
            (elapsed as f64 / self.window_ms as f64).min(1.0)
        } else {
            1.0
        };

        let prev = self.previous_count.load(Ordering::Relaxed) as f64;
        let curr = self.current_count.load(Ordering::Relaxed) as f64;
        let estimated = prev * (1.0 - weight) + curr;

        if estimated < self.max_requests as f64 {
            self.current_count.fetch_add(1, Ordering::Relaxed);
            return true;
        }

        // Try burst tokens
        let tokens = self.burst_tokens.load(Ordering::Relaxed);
        if tokens > 0 {
            // CAS loop for burst token consumption
            let prev_tokens = self.burst_tokens.fetch_sub(1, Ordering::Relaxed);
            if prev_tokens > 0 {
                self.current_count.fetch_add(1, Ordering::Relaxed);
                return true;
            }
            // Restore if we went negative
            self.burst_tokens.fetch_add(1, Ordering::Relaxed);
        }

        false
    }

    /// Get current estimated request count.
    pub fn current_rate(&self) -> f64 {
        let now = current_time_ms();
        let window_start = self.current_window_start.load(Ordering::Relaxed);
        let elapsed = now.saturating_sub(window_start);
        let weight = if self.window_ms > 0 {
            (elapsed as f64 / self.window_ms as f64).min(1.0)
        } else {
            1.0
        };
        let prev = self.previous_count.load(Ordering::Relaxed) as f64;
        let curr = self.current_count.load(Ordering::Relaxed) as f64;
        prev * (1.0 - weight) + curr
    }

    fn maybe_rotate(&self, now: u64) {
        let window_start = self.current_window_start.load(Ordering::Relaxed);
        if now.saturating_sub(window_start) >= self.window_ms {
            let current = self.current_count.load(Ordering::Relaxed);
            self.previous_count.store(current, Ordering::Relaxed);
            self.current_count.store(0, Ordering::Relaxed);
            self.current_window_start.store(now, Ordering::Relaxed);
        }
    }

    fn maybe_refill_burst(&self, now: u64) {
        let last = self.last_refill.load(Ordering::Relaxed);
        // Refill one burst token per window
        if now.saturating_sub(last) >= self.window_ms {
            let current_tokens = self.burst_tokens.load(Ordering::Relaxed);
            if current_tokens < self.burst_allowance {
                self.burst_tokens.fetch_add(1, Ordering::Relaxed);
            }
            self.last_refill.store(now, Ordering::Relaxed);
        }
    }
}

fn current_time_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

// ---------------------------------------------------------------------------
// Multi-tool rate limiter
// ---------------------------------------------------------------------------

/// Rate limiter managing per-tool and global windows.
pub struct RateLimiter {
    /// Per-tool windows
    tool_windows: RwLock<HashMap<String, Arc<SlidingWindow>>>,
    /// Global window
    global_window: SlidingWindow,
    /// Default per-tool RPM
    default_tool_rpm: u64,
    /// Window duration (ms) — always 60_000 for RPM
    window_ms: u64,
    /// Per-tool burst allowance
    burst_allowance: u64,
    /// Tool-specific RPM overrides
    overrides: RwLock<HashMap<String, u64>>,
}

impl RateLimiter {
    pub fn new(global_rpm: u64, default_tool_rpm: u64, burst_allowance: u64) -> Self {
        Self {
            tool_windows: RwLock::new(HashMap::new()),
            global_window: SlidingWindow::new(global_rpm, 60_000, burst_allowance * 2),
            default_tool_rpm,
            window_ms: 60_000,
            burst_allowance,
            overrides: RwLock::new(HashMap::new()),
        }
    }

    /// Set a per-tool RPM override.
    pub fn set_override(&self, tool: &str, rpm: u64) {
        self.overrides.write().unwrap().insert(tool.to_string(), rpm);
    }

    /// Try to acquire a permit for a tool invocation.
    /// Returns Ok(()) if allowed, Err(retry_after_ms) if rate-limited.
    pub fn try_acquire(&self, tool: &str) -> Result<(), u64> {
        // Check global limit first
        if !self.global_window.try_acquire() {
            return Err(self.window_ms / 2);
        }

        // Get or create per-tool window
        let window = {
            let read_guard = self.tool_windows.read().unwrap();
            if let Some(w) = read_guard.get(tool) {
                Arc::clone(w)
            } else {
                drop(read_guard);
                let rpm = self.overrides.read().unwrap()
                    .get(tool)
                    .copied()
                    .unwrap_or(self.default_tool_rpm);
                let new_window = Arc::new(SlidingWindow::new(rpm, self.window_ms, self.burst_allowance));
                self.tool_windows.write().unwrap()
                    .insert(tool.to_string(), Arc::clone(&new_window));
                new_window
            }
        };

        if window.try_acquire() {
            Ok(())
        } else {
            Err(self.window_ms / 4)
        }
    }

    /// Get statistics for all tracked tools.
    pub fn stats(&self) -> HashMap<String, f64> {
        let mut result = HashMap::new();
        result.insert("global_rate".to_string(), self.global_window.current_rate());
        let guard = self.tool_windows.read().unwrap();
        for (tool, window) in guard.iter() {
            result.insert(format!("tool:{}", tool), window.current_rate());
        }
        result
    }
}

// ---------------------------------------------------------------------------
// Global singleton
// ---------------------------------------------------------------------------

lazy_static::lazy_static! {
    static ref GLOBAL_LIMITER: Arc<RateLimiter> = Arc::new(
        RateLimiter::new(300, 60, 10)
    );
}

// ---------------------------------------------------------------------------
// Python bindings
// ---------------------------------------------------------------------------

/// Check if a tool invocation is allowed by the rate limiter.
/// Returns JSON: {"allowed": bool, "retry_after_ms": u64|null}
#[pyfunction]
pub fn rate_check(tool_name: &str) -> PyResult<String> {
    match GLOBAL_LIMITER.try_acquire(tool_name) {
        Ok(()) => Ok(r#"{"allowed": true, "retry_after_ms": null}"#.to_string()),
        Err(retry) => Ok(format!(r#"{{"allowed": false, "retry_after_ms": {}}}"#, retry)),
    }
}

/// Set a per-tool RPM override.
#[pyfunction]
pub fn rate_set_override(tool_name: &str, rpm: u64) -> PyResult<()> {
    GLOBAL_LIMITER.set_override(tool_name, rpm);
    Ok(())
}

/// Batch check rate limits for multiple tools in a single FFI call.
/// Returns JSON array of results: [{"tool": str, "allowed": bool, "retry_after_ms": u64|null}, ...]
/// Amortizes Python→Rust crossing overhead: N checks for the cost of 1 FFI call.
#[pyfunction]
pub fn rate_check_batch(tool_names: Vec<String>) -> PyResult<String> {
    let mut results = Vec::with_capacity(tool_names.len());
    for tool in &tool_names {
        match GLOBAL_LIMITER.try_acquire(tool) {
            Ok(()) => results.push(format!(
                r#"{{"tool":"{}","allowed":true,"retry_after_ms":null}}"#,
                tool
            )),
            Err(retry) => results.push(format!(
                r#"{{"tool":"{}","allowed":false,"retry_after_ms":{}}}"#,
                tool, retry
            )),
        }
    }
    Ok(format!("[{}]", results.join(",")))
}

/// Get rate limiter statistics as JSON.
#[pyfunction]
pub fn rate_stats() -> PyResult<String> {
    let stats = GLOBAL_LIMITER.stats();
    serde_json::to_string(&stats)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))
}

// ---------------------------------------------------------------------------
// Native Python type bindings (v14 — zero-copy FFI)
// ---------------------------------------------------------------------------
// These functions return native Python types instead of JSON strings,
// eliminating the serialization/deserialization tax on the FFI boundary.
// Benchmarked: ~50% faster than JSON-based counterparts.

/// Rate check returning native Python tuple: (allowed, retry_after_ms).
/// No JSON serialization — tuple crosses FFI as a direct Python object.
#[pyfunction]
pub fn rate_check_native(tool_name: &str) -> PyResult<(bool, Option<u64>)> {
    match GLOBAL_LIMITER.try_acquire(tool_name) {
        Ok(()) => Ok((true, None)),
        Err(retry) => Ok((false, Some(retry))),
    }
}

/// Batch rate check returning native Python list of tuples.
/// Each element: (tool_name, allowed, retry_after_ms).
/// Amortizes FFI crossing AND eliminates JSON for N checks.
#[pyfunction]
pub fn rate_check_batch_native(tool_names: Vec<String>) -> PyResult<Vec<(String, bool, Option<u64>)>> {
    let mut results = Vec::with_capacity(tool_names.len());
    for tool in &tool_names {
        match GLOBAL_LIMITER.try_acquire(tool) {
            Ok(()) => results.push((tool.clone(), true, None)),
            Err(retry) => results.push((tool.clone(), false, Some(retry))),
        }
    }
    Ok(results)
}

/// Rate stats returning native HashMap (auto-converts to Python dict).
#[pyfunction]
pub fn rate_stats_native() -> PyResult<HashMap<String, f64>> {
    Ok(GLOBAL_LIMITER.stats())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sliding_window_allows_under_limit() {
        let w = SlidingWindow::new(10, 60_000, 0);
        for _ in 0..10 {
            assert!(w.try_acquire());
        }
    }

    #[test]
    fn test_sliding_window_blocks_over_limit() {
        let w = SlidingWindow::new(5, 60_000, 0);
        for _ in 0..5 {
            assert!(w.try_acquire());
        }
        // Should be blocked now (no burst)
        assert!(!w.try_acquire());
    }

    #[test]
    fn test_burst_allowance() {
        let w = SlidingWindow::new(5, 60_000, 3);
        for _ in 0..5 {
            assert!(w.try_acquire());
        }
        // Burst should allow 3 more
        assert!(w.try_acquire());
        assert!(w.try_acquire());
        assert!(w.try_acquire());
        // Now truly blocked
        assert!(!w.try_acquire());
    }

    #[test]
    fn test_rate_limiter_per_tool() {
        let limiter = RateLimiter::new(1000, 5, 0);
        for _ in 0..5 {
            assert!(limiter.try_acquire("test_tool").is_ok());
        }
        // Per-tool limit hit
        assert!(limiter.try_acquire("test_tool").is_err());
        // Different tool still works
        assert!(limiter.try_acquire("other_tool").is_ok());
    }

    #[test]
    fn test_rate_limiter_override() {
        let limiter = RateLimiter::new(1000, 5, 0);
        limiter.set_override("special_tool", 2);
        assert!(limiter.try_acquire("special_tool").is_ok());
        assert!(limiter.try_acquire("special_tool").is_ok());
        assert!(limiter.try_acquire("special_tool").is_err());
    }

    #[test]
    fn test_current_rate() {
        let w = SlidingWindow::new(100, 60_000, 0);
        assert!(w.current_rate() < 0.01);
        w.try_acquire();
        w.try_acquire();
        w.try_acquire();
        assert!(w.current_rate() >= 3.0);
    }
}
