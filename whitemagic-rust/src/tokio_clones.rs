//! Tokio Clone Army — Rust async task pool for massively parallel exploration
//!
//! Replaces Python's asyncio-based AsyncThoughtCloneArmy with native Rust
//! tokio tasks. Key advantages:
//!   - No GIL: true parallel execution across CPU cores
//!   - ~100ns task spawn overhead (vs ~1ms Python asyncio)
//!   - Millions of concurrent tasks (vs Python's ~16K limit)
//!   - Structured concurrency with JoinSet
//!
//! Performance targets:
//!   - 1000 clones: <10ms (vs Python's 664ms — 66× faster)
//!   - 5000 clones: <50ms (vs Python's 1.7s — 34× faster)
//!   - 16000 clones: <200ms (Python can't even do this reliably)
//!
//! Architecture:
//!   - Each clone is a tokio::task with a CloneStrategy
//!   - Results are collected via JoinSet
//!   - Consensus voting picks the best result
//!   - PyO3 bridge exposes deploy_clones() to Python

#[cfg(feature = "python")]
use pyo3::prelude::*;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Strategy that each clone uses for exploration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CloneStrategy {
    /// Direct answer generation
    Direct,
    /// Chain-of-thought reasoning
    ChainOfThought,
    /// Adversarial / devil's advocate
    Adversarial,
    /// Creative / lateral thinking
    Creative,
    /// Analytical / structured decomposition
    Analytical,
    /// Synthesis of multiple approaches
    Synthesis,
    /// Memory-grounded reasoning
    MemoryGrounded,
    /// Stateless burn — Ralph Wiggum / Dare-to-Die Corps (Pu 朴)
    /// Zero context, zero history, pure execution. Performance or death.
    StatelessBurn,
    /// Custom strategy with description
    Custom(String),
}

impl CloneStrategy {
    fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "direct" => Self::Direct,
            "chain_of_thought" | "cot" => Self::ChainOfThought,
            "adversarial" => Self::Adversarial,
            "creative" => Self::Creative,
            "analytical" => Self::Analytical,
            "synthesis" => Self::Synthesis,
            "memory_grounded" | "memory" => Self::MemoryGrounded,
            "stateless_burn" | "ralph" | "dare_to_die" | "pu" => Self::StatelessBurn,
            other => Self::Custom(other.to_string()),
        }
    }

    fn name(&self) -> &str {
        match self {
            Self::Direct => "direct",
            Self::ChainOfThought => "chain_of_thought",
            Self::Adversarial => "adversarial",
            Self::Creative => "creative",
            Self::Analytical => "analytical",
            Self::Synthesis => "synthesis",
            Self::MemoryGrounded => "memory_grounded",
            Self::StatelessBurn => "stateless_burn",
            Self::Custom(s) => s.as_str(),
        }
    }

    /// Simulate exploration work (in real deployment, this calls an LLM)
    fn explore(&self, prompt: &str, clone_id: usize) -> CloneResult {
        // Deterministic "thinking" based on strategy
        let confidence = match self {
            Self::Direct => 0.7 + (clone_id as f64 * 0.001).sin().abs() * 0.2,
            Self::ChainOfThought => 0.75 + (clone_id as f64 * 0.002).cos().abs() * 0.2,
            Self::Adversarial => 0.5 + (clone_id as f64 * 0.003).sin().abs() * 0.3,
            Self::Creative => 0.6 + (clone_id as f64 * 0.004).cos().abs() * 0.3,
            Self::Analytical => 0.8 + (clone_id as f64 * 0.001).sin().abs() * 0.15,
            Self::Synthesis => 0.85 + (clone_id as f64 * 0.002).cos().abs() * 0.1,
            Self::MemoryGrounded => 0.7 + (clone_id as f64 * 0.005).sin().abs() * 0.25,
            // StatelessBurn: high variance, no memory bias — pure Pu (朴)
            Self::StatelessBurn => 0.4 + (clone_id as f64 * 0.007).sin().abs() * 0.5,
            Self::Custom(_) => 0.6 + (clone_id as f64 * 0.006).cos().abs() * 0.3,
        };

        CloneResult {
            clone_id,
            strategy: self.name().to_string(),
            response: format!(
                "[Clone {} / {}] Explored: {}",
                clone_id,
                self.name(),
                &prompt[..prompt.len().min(50)]
            ),
            confidence,
            tokens_used: 50 + (clone_id % 200) as u32,
        }
    }
}

/// Result from a single clone's exploration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CloneResult {
    pub clone_id: usize,
    pub strategy: String,
    pub response: String,
    pub confidence: f64,
    pub tokens_used: u32,
}

/// Consensus result after voting
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsensusResult {
    pub winner: CloneResult,
    pub total_clones: usize,
    pub strategy_votes: HashMap<String, usize>,
    pub avg_confidence: f64,
    pub total_tokens: u64,
    pub elapsed_ms: f64,
}

/// Statistics for the clone army
static TOTAL_DEPLOYMENTS: AtomicU64 = AtomicU64::new(0);
static TOTAL_CLONES_SPAWNED: AtomicU64 = AtomicU64::new(0);

// ---------------------------------------------------------------------------
// Core async logic (tokio)
// ---------------------------------------------------------------------------

#[cfg(feature = "tokio")]
/// Deploy N clones with specified strategies, collect results, vote on consensus.
/// This is the Rust replacement for Python's AsyncThoughtCloneArmy.
pub async fn deploy_clones_async(
    prompt: String,
    num_clones: usize,
    strategies: Vec<CloneStrategy>,
) -> ConsensusResult {
    use tokio::task::JoinSet;

    let start = Instant::now();
    let prompt = Arc::new(prompt);
    let mut join_set = JoinSet::new();

    // Spawn all clones as tokio tasks
    for i in 0..num_clones {
        let strategy = strategies[i % strategies.len()].clone();
        let p = Arc::clone(&prompt);
        join_set.spawn(async move {
            // Each clone does its exploration work
            strategy.explore(&p, i)
        });
    }

    // Collect all results
    let mut results: Vec<CloneResult> = Vec::with_capacity(num_clones);
    while let Some(res) = join_set.join_next().await {
        if let Ok(clone_result) = res {
            results.push(clone_result);
        }
    }

    // Update global stats
    TOTAL_DEPLOYMENTS.fetch_add(1, Ordering::Relaxed);
    TOTAL_CLONES_SPAWNED.fetch_add(num_clones as u64, Ordering::Relaxed);

    // Consensus voting: pick highest confidence
    let mut strategy_votes: HashMap<String, usize> = HashMap::new();
    let mut total_tokens: u64 = 0;
    let mut confidence_sum: f64 = 0.0;

    for r in &results {
        *strategy_votes.entry(r.strategy.clone()).or_insert(0) += 1;
        total_tokens += r.tokens_used as u64;
        confidence_sum += r.confidence;
    }

    let avg_confidence = if results.is_empty() {
        0.0
    } else {
        confidence_sum / results.len() as f64
    };

    let winner = results
        .into_iter()
        .max_by(|a, b| a.confidence.partial_cmp(&b.confidence).unwrap())
        .unwrap_or(CloneResult {
            clone_id: 0,
            strategy: "none".to_string(),
            response: "No results".to_string(),
            confidence: 0.0,
            tokens_used: 0,
        });

    let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;

    ConsensusResult {
        winner,
        total_clones: num_clones,
        strategy_votes,
        avg_confidence,
        total_tokens,
        elapsed_ms,
    }
}

// ---------------------------------------------------------------------------
// Synchronous wrapper (for non-async callers)
// ---------------------------------------------------------------------------

#[cfg(feature = "tokio")]
/// Deploy clones synchronously by running the tokio runtime.
pub fn deploy_clones_sync(
    prompt: &str,
    num_clones: usize,
    strategy_names: &[String],
) -> ConsensusResult {
    let strategies: Vec<CloneStrategy> = if strategy_names.is_empty() {
        vec![
            CloneStrategy::Direct,
            CloneStrategy::ChainOfThought,
            CloneStrategy::Analytical,
            CloneStrategy::Creative,
            CloneStrategy::Synthesis,
        ]
    } else {
        strategy_names.iter().map(|s| CloneStrategy::from_str(s)).collect()
    };

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(num_cpus())
        .enable_all()
        .build()
        .expect("Failed to create tokio runtime");

    rt.block_on(deploy_clones_async(prompt.to_string(), num_clones, strategies))
}

fn num_cpus() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
}

// ---------------------------------------------------------------------------
// PyO3 bindings
// ---------------------------------------------------------------------------

/// Deploy a Rust tokio clone army. Returns JSON with consensus result.
///
/// Args:
///   prompt: The exploration prompt
///   num_clones: Number of clones to deploy (1-100,000)
///   strategies: List of strategy names (default: mixed)
///
/// Returns:
///   JSON string with ConsensusResult
#[cfg(feature = "python")]
#[pyfunction]
pub fn tokio_deploy_clones(
    prompt: &str,
    num_clones: usize,
    strategies: Vec<String>,
) -> PyResult<String> {
    #[cfg(feature = "tokio")]
    {
        let result = deploy_clones_sync(prompt, num_clones, &strategies);
        serde_json::to_string(&result)
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))
    }
    #[cfg(not(feature = "tokio"))]
    {
        Ok(r#"{"error": "tokio not compiled"}"#.to_string())
    }
}

/// Benchmark: deploy N clones and return timing.
/// Returns (elapsed_ms, clones_per_second).
#[cfg(feature = "python")]
#[pyfunction]
pub fn tokio_clone_bench(num_clones: usize) -> PyResult<(f64, f64)> {
    #[cfg(feature = "tokio")]
    {
        let start = Instant::now();
        let _result = deploy_clones_sync(
            "Benchmark exploration prompt for clone army performance testing",
            num_clones,
            &[],
        );
        let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
        let cps = num_clones as f64 / (elapsed_ms / 1000.0);
        Ok((elapsed_ms, cps))
    }
    #[cfg(not(feature = "tokio"))]
    {
        Ok((0.0, 0.0))
    }
}

/// Get clone army statistics.
#[cfg(feature = "python")]
#[pyfunction]
pub fn tokio_clone_stats() -> PyResult<String> {
    Ok(serde_json::json!({
        "total_deployments": TOTAL_DEPLOYMENTS.load(Ordering::Relaxed),
        "total_clones_spawned": TOTAL_CLONES_SPAWNED.load(Ordering::Relaxed),
        "available_cpus": num_cpus(),
        "backend": "tokio",
    }).to_string())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[cfg(feature = "tokio")]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_deploy_4_clones() {
        let result = deploy_clones_async(
            "test prompt".to_string(),
            4,
            vec![CloneStrategy::Direct, CloneStrategy::Analytical],
        ).await;
        assert_eq!(result.total_clones, 4);
        assert!(result.elapsed_ms < 100.0); // Should be < 1ms
        assert!(result.avg_confidence > 0.0);
    }

    #[tokio::test]
    async fn test_deploy_1000_clones() {
        let result = deploy_clones_async(
            "large scale test".to_string(),
            1000,
            vec![
                CloneStrategy::Direct,
                CloneStrategy::ChainOfThought,
                CloneStrategy::Creative,
                CloneStrategy::Analytical,
                CloneStrategy::Synthesis,
            ],
        ).await;
        assert_eq!(result.total_clones, 1000);
        assert!(result.elapsed_ms < 500.0, "1000 clones took {}ms", result.elapsed_ms);
    }

    #[tokio::test]
    async fn test_deploy_5000_clones() {
        let result = deploy_clones_async(
            "massive scale test".to_string(),
            5000,
            vec![CloneStrategy::Direct, CloneStrategy::Synthesis],
        ).await;
        assert_eq!(result.total_clones, 5000);
        // Should be WAY faster than Python's 1.7s
        assert!(result.elapsed_ms < 1000.0, "5000 clones took {}ms", result.elapsed_ms);
    }

    #[test]
    fn test_sync_wrapper() {
        let result = deploy_clones_sync("sync test", 100, &[]);
        assert_eq!(result.total_clones, 100);
        assert!(result.elapsed_ms < 200.0);
    }

    #[test]
    fn test_consensus_voting() {
        let result = deploy_clones_sync("consensus test", 50, &[]);
        assert!(!result.strategy_votes.is_empty());
        assert!(result.winner.confidence > 0.0);
        assert!(result.total_tokens > 0);
    }
}
