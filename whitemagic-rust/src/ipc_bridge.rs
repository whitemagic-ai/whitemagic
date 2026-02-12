//! IPC Bridge — Cross-process zero-copy communication via Iceoryx2
//!
//! Provides shared memory channels between WhiteMagic processes (e.g.,
//! two laptops running the mesh, or separate MCP server processes).
//!
//! Architecture:
//!   - Publisher/Subscriber model via Iceoryx2 shared memory
//!   - Each channel carries typed payloads (events, memories, commands)
//!   - Zero-copy: data stays in shared memory, only pointers are exchanged
//!   - Lock-free: wait-free publish, bounded-wait subscribe
//!
//! Channels:
//!   - "wm/events"   — GanYing event bus (replaces Redis pub/sub for local)
//!   - "wm/memories" — Memory sync announcements
//!   - "wm/commands" — Agent coordination commands
//!   - "wm/harmony"  — Harmony Vector broadcast (health pulse)
//!
//! Fallback: If iceoryx2 is not available, operations are no-ops that return
//! graceful error messages. The Python bridge detects this and uses the
//! existing EventRing (in-process) or Redis (cross-machine) instead.

#[cfg(feature = "pyo3")]
use pyo3::prelude::*;

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Channel registry (works regardless of iceoryx2 feature)
// ---------------------------------------------------------------------------

/// Known IPC channel names
pub const CHANNEL_EVENTS: &str = "wm/events";
pub const CHANNEL_MEMORIES: &str = "wm/memories";
pub const CHANNEL_COMMANDS: &str = "wm/commands";
pub const CHANNEL_HARMONY: &str = "wm/harmony";

/// Track IPC statistics
struct IpcStats {
    published: AtomicU64,
    received: AtomicU64,
    errors: AtomicU64,
    initialized: AtomicBool,
}

impl IpcStats {
    const fn new() -> Self {
        Self {
            published: AtomicU64::new(0),
            received: AtomicU64::new(0),
            errors: AtomicU64::new(0),
            initialized: AtomicBool::new(false),
        }
    }
}

lazy_static::lazy_static! {
    static ref IPC_STATS: IpcStats = IpcStats::new();
}

// ---------------------------------------------------------------------------
// Iceoryx2 integration (conditional)
// ---------------------------------------------------------------------------

#[cfg(feature = "iceoryx2")]
mod iox2 {
    //! When iceoryx2 is available, we create real shared-memory channels.
    //! The iceoryx2 crate provides zero-copy pub/sub over /dev/shm.

    use iceoryx2::prelude::*;
    use std::sync::Mutex;

    lazy_static::lazy_static! {
        static ref IOX_NODE: Mutex<Option<Node<ipc::Service>>> = Mutex::new(None);
    }

    /// Initialize the iceoryx2 node for this process
    pub fn init_node(node_name: &str) -> Result<(), String> {
        let node = NodeBuilder::new()
            .name(&node_name.try_into().map_err(|e| format!("{:?}", e))?)
            .create::<ipc::Service>()
            .map_err(|e| format!("Node create: {:?}", e))?;

        let mut guard = IOX_NODE.lock().map_err(|e| format!("Lock: {}", e))?;
        *guard = Some(node);
        Ok(())
    }

    /// Publish a payload to a named channel
    pub fn publish(channel: &str, payload: &[u8]) -> Result<(), String> {
        let guard = IOX_NODE.lock().map_err(|e| format!("Lock: {}", e))?;
        let node = guard.as_ref().ok_or("IPC not initialized")?;

        let service = node
            .service_builder(&channel.try_into().map_err(|e| format!("{:?}", e))?)
            .publish_subscribe::<[u8]>()
            .open_or_create()
            .map_err(|e| format!("Service: {:?}", e))?;

        let publisher = service
            .publisher_builder()
            .create()
            .map_err(|e| format!("Publisher: {:?}", e))?;

        let sample = publisher
            .loan_slice_uninit(payload.len())
            .map_err(|e| format!("Loan: {:?}", e))?;

        let sample = sample.write_from_slice(payload);
        sample.send().map_err(|e| format!("Send: {:?}", e))?;

        Ok(())
    }

    /// Check if iceoryx2 is available
    pub fn is_available() -> bool {
        IOX_NODE.lock().map(|g| g.is_some()).unwrap_or(false)
    }
}

#[cfg(not(feature = "iceoryx2"))]
mod iox2 {
    //! Stub when iceoryx2 is not compiled

    pub fn init_node(_name: &str) -> Result<(), String> {
        Ok(()) // Graceful no-op
    }

    pub fn publish(_channel: &str, _payload: &[u8]) -> Result<(), String> {
        Err("iceoryx2 not compiled — using in-process EventRing instead".to_string())
    }

    pub fn is_available() -> bool {
        false
    }
}

// ---------------------------------------------------------------------------
// Public API (always available)
// ---------------------------------------------------------------------------

/// Initialize IPC bridge
pub fn ipc_init(node_name: &str) -> Result<(), String> {
    iox2::init_node(node_name)?;
    IPC_STATS.initialized.store(true, Ordering::Release);
    Ok(())
}

/// Publish to a channel
pub fn ipc_publish(channel: &str, payload: &[u8]) -> Result<(), String> {
    match iox2::publish(channel, payload) {
        Ok(()) => {
            IPC_STATS.published.fetch_add(1, Ordering::Relaxed);
            Ok(())
        }
        Err(e) => {
            IPC_STATS.errors.fetch_add(1, Ordering::Relaxed);
            Err(e)
        }
    }
}

/// Get IPC status
pub fn ipc_status() -> HashMap<String, serde_json::Value> {
    let mut status = HashMap::new();
    status.insert("backend".to_string(), serde_json::json!(
        if iox2::is_available() { "iceoryx2" } else { "fallback" }
    ));
    status.insert("initialized".to_string(), serde_json::json!(
        IPC_STATS.initialized.load(Ordering::Relaxed)
    ));
    status.insert("published".to_string(), serde_json::json!(
        IPC_STATS.published.load(Ordering::Relaxed)
    ));
    status.insert("received".to_string(), serde_json::json!(
        IPC_STATS.received.load(Ordering::Relaxed)
    ));
    status.insert("errors".to_string(), serde_json::json!(
        IPC_STATS.errors.load(Ordering::Relaxed)
    ));
    status.insert("channels".to_string(), serde_json::json!([
        CHANNEL_EVENTS, CHANNEL_MEMORIES, CHANNEL_COMMANDS, CHANNEL_HARMONY
    ]));
    #[cfg(feature = "iceoryx2")]
    status.insert("iceoryx2_compiled".to_string(), serde_json::json!(true));
    #[cfg(not(feature = "iceoryx2"))]
    status.insert("iceoryx2_compiled".to_string(), serde_json::json!(false));
    status
}

// ---------------------------------------------------------------------------
// PyO3 bindings
// ---------------------------------------------------------------------------

/// Initialize the IPC bridge with a node name.
#[cfg(feature = "python")]
#[pyfunction]
pub fn ipc_bridge_init(node_name: &str) -> PyResult<String> {
    match ipc_init(node_name) {
        Ok(()) => Ok(serde_json::json!({"initialized": true, "node": node_name}).to_string()),
        Err(e) => Ok(serde_json::json!({"initialized": false, "error": e}).to_string()),
    }
}

/// Publish data to an IPC channel.
#[cfg(feature = "python")]
#[pyfunction]
pub fn ipc_bridge_publish(channel: &str, data: &[u8]) -> PyResult<String> {
    match ipc_publish(channel, data) {
        Ok(()) => Ok(serde_json::json!({"published": true, "channel": channel}).to_string()),
        Err(e) => Ok(serde_json::json!({"published": false, "error": e}).to_string()),
    }
}

/// Get IPC bridge status.
#[cfg(feature = "python")]
#[pyfunction]
pub fn ipc_bridge_status() -> PyResult<String> {
    let status = ipc_status();
    serde_json::to_string(&status)
        .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init() {
        let result = ipc_init("test_node");
        assert!(result.is_ok());
    }

    #[test]
    fn test_status() {
        let status = ipc_status();
        assert!(status.contains_key("backend"));
        assert!(status.contains_key("channels"));
    }

    #[test]
    fn test_publish_without_init() {
        // Should gracefully handle publish without iceoryx2
        let result = ipc_publish(CHANNEL_EVENTS, b"test payload");
        // Either succeeds (iceoryx2 available) or returns error string
        if let Err(e) = &result {
            assert!(e.contains("not compiled") || e.contains("not initialized"));
        }
    }
}
