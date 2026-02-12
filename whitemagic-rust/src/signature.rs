use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Serialize, Deserialize, Debug)]
pub struct SignedSignal {
    pub payload: String,
    pub signature: String,
    pub timestamp: u64,
    pub node_id: String,
}

/// Signs a message with a deterministic node secret.
/// In production, this would use Ed25519.
pub fn sign_signal(payload: &str, node_id: &str, secret: &str) -> SignedSignal {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    let mut hasher = Sha256::new();
    hasher.update(payload.as_bytes());
    hasher.update(node_id.as_bytes());
    hasher.update(secret.as_bytes());
    hasher.update(timestamp.to_le_bytes());

    let signature = hex::encode(hasher.finalize());

    SignedSignal {
        payload: payload.to_string(),
        signature,
        timestamp,
        node_id: node_id.to_string(),
    }
}

/// Verifies a signed signal.
pub fn verify_signal(signal: &SignedSignal, secret: &str) -> bool {
    let mut hasher = Sha256::new();
    hasher.update(signal.payload.as_bytes());
    hasher.update(signal.node_id.as_bytes());
    hasher.update(secret.as_bytes());
    hasher.update(signal.timestamp.to_le_bytes());

    let expected_signature = hex::encode(hasher.finalize());
    expected_signature == signal.signature
}

#[cfg(feature = "python")]
use pyo3::prelude::*;

/// Signs a message with a deterministic node secret (Python binding).
#[cfg(feature = "python")]
#[pyfunction]
pub fn sign_signal_py(payload: &str, node_id: &str, secret: &str) -> PyResult<String> {
    let signed = sign_signal(payload, node_id, secret);
    Ok(serde_json::to_string(&signed)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?)
}

/// Verifies a signed signal (Python binding).
#[cfg(feature = "python")]
#[pyfunction]
pub fn verify_signal_py(signal_json: &str, secret: &str) -> PyResult<bool> {
    let signal: SignedSignal = serde_json::from_str(signal_json)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    Ok(verify_signal(&signal, secret))
}
