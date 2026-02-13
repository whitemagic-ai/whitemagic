//! WhiteMagic Rust Core - Minimal stable build
//! Python fallback available for all features

#[cfg(feature = "python")]
use pyo3::prelude::*;
#[cfg(feature = "python")]
use pyo3::wrap_pyfunction;

// ── WASM-only module (browser / edge runtime) ────────────────────────
#[cfg(feature = "wasm")]
pub mod wasm;

// ── Native modules (require rayon, libc, memmap2, sysinfo, etc.) ─────
// These do NOT compile for wasm32 targets.
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::new_without_default)]
pub mod async_memory;
#[cfg(not(target_arch = "wasm32"))]
pub mod audit;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::empty_line_after_doc_comments, clippy::implicit_saturating_sub)]
pub mod clones;
#[cfg(not(target_arch = "wasm32"))]
pub mod compression;
#[cfg(not(target_arch = "wasm32"))]
pub mod consolidation;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::type_complexity, clippy::useless_vec)]
pub mod data_lake;
#[cfg(not(target_arch = "wasm32"))]
pub mod embeddings;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::unwrap_or_default)]
pub mod event_processor;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::needless_as_bytes)]
pub mod file_ops;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::empty_line_after_doc_comments)]
pub mod graph;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::new_without_default)]
pub mod harmony;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::new_without_default)]
pub mod hologram;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::redundant_pattern_matching)]
pub mod holographic;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::empty_line_after_doc_comments)]
pub mod iching;
#[cfg(not(target_arch = "wasm32"))]
#[allow(
    clippy::empty_line_after_doc_comments,
    clippy::type_complexity,
    clippy::redundant_closure
)]
pub mod memory_consolidation;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::new_without_default)]
pub mod pattern_engine;
#[cfg(not(target_arch = "wasm32"))]
#[allow(
    clippy::empty_line_after_doc_comments,
    clippy::type_complexity,
    clippy::redundant_closure
)]
pub mod pattern_extraction;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::type_complexity)]
pub mod pattern_matcher;
#[cfg(not(target_arch = "wasm32"))]
#[allow(
    clippy::manual_pattern_char_comparison,
    clippy::type_complexity,
    clippy::useless_conversion
)]
pub mod patterns;
#[cfg(not(target_arch = "wasm32"))]
pub mod python_bindings;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::needless_range_loop)]
pub mod search;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::needless_question_mark)]
pub mod signature;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::new_without_default, clippy::needless_range_loop)]
pub mod simd_inference;
#[cfg(not(target_arch = "wasm32"))]
pub mod simd_search;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::type_complexity, clippy::new_without_default)]
pub mod spatial_index;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::needless_range_loop, clippy::unwrap_or_default)]
pub mod synthesis;
#[cfg(not(target_arch = "wasm32"))]
pub mod zig_bridge;
#[cfg(not(target_arch = "wasm32"))]
pub mod zig_ffi;

// v12.3 Accelerators
#[cfg(not(target_arch = "wasm32"))]
pub mod galactic_accelerator;
#[cfg(not(target_arch = "wasm32"))]
pub mod association_accelerator;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::type_complexity, clippy::new_without_default)]
pub mod spatial_index_5d;

// v13 Polyglot Expansion
#[cfg(not(target_arch = "wasm32"))]
pub mod rate_limiter;
#[cfg(not(target_arch = "wasm32"))]
pub mod event_bus;
#[cfg(not(target_arch = "wasm32"))]
pub mod holographic_encoder_5d;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::redundant_closure)]
pub mod minhash;
#[cfg(feature = "rusqlite")]
#[allow(clippy::redundant_closure)]
pub mod sqlite_accel;
#[cfg(not(target_arch = "wasm32"))]
pub mod keyword_extract;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::ptr_arg)]
pub mod retrieval_pipeline;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::collapsible_else_if)]
pub mod shared_state;
#[cfg(not(target_arch = "wasm32"))]
pub mod state_board;
#[cfg(not(target_arch = "wasm32"))]
#[allow(clippy::type_complexity)]
pub mod event_ring;

// v14 — Hybrid RRF + Association Walk accelerators
#[cfg(not(target_arch = "wasm32"))]
pub mod hybrid_rrf;
#[cfg(feature = "rusqlite")]
pub mod association_walk;

// v14.5 — Arrow IPC, Tokio Clone Army, Iceoryx2 IPC
#[cfg(not(target_arch = "wasm32"))]
pub mod arrow_bridge;
#[cfg(not(target_arch = "wasm32"))]
pub mod tokio_clones;
#[cfg(not(target_arch = "wasm32"))]
pub mod ipc_bridge;

// Python bindings - minimal export
#[cfg(feature = "python")]
#[pymodule]
fn whitemagic_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("__version__", "14.5.0")?;
    // Audit utilities for fast parallel reads
    m.add_function(wrap_pyfunction!(audit::audit_directory, m)?)?;
    m.add_function(wrap_pyfunction!(audit::read_files_fast, m)?)?;
    m.add_function(wrap_pyfunction!(audit::extract_summaries, m)?)?;
    // File operations
    m.add_function(wrap_pyfunction!(file_ops::read_file, m)?)?;
    m.add_function(wrap_pyfunction!(file_ops::read_file_fast, m)?)?;
    m.add_function(wrap_pyfunction!(file_ops::write_file, m)?)?;
    m.add_function(wrap_pyfunction!(file_ops::write_file_fast, m)?)?;
    m.add_function(wrap_pyfunction!(file_ops::write_file_compressed, m)?)?;
    // Compression utilities
    m.add_function(wrap_pyfunction!(compression::compress_file_py, m)?)?;
    m.add_function(wrap_pyfunction!(compression::decompress_file_py, m)?)?;
    m.add_function(wrap_pyfunction!(compression::fast_compress, m)?)?;
    m.add_function(wrap_pyfunction!(compression::fast_decompress, m)?)?;
    // Data lake scanning/search
    m.add_function(wrap_pyfunction!(data_lake::scan_codebase, m)?)?;
    m.add_function(wrap_pyfunction!(data_lake::extract_definitions, m)?)?;
    m.add_function(wrap_pyfunction!(data_lake::build_word_index, m)?)?;
    m.add_function(wrap_pyfunction!(data_lake::parallel_grep, m)?)?;
    m.add_function(wrap_pyfunction!(data_lake::extract_todos, m)?)?;
    m.add_function(wrap_pyfunction!(data_lake::fast_search, m)?)?;
    // Memory consolidation + pattern extraction
    m.add_function(wrap_pyfunction!(
        memory_consolidation::consolidate_memories,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(
        memory_consolidation::consolidate_memories_from_content_py,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(
        pattern_extraction::extract_patterns_py,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(
        pattern_extraction::extract_patterns_from_content_py,
        m
    )?)?;
    // Patterns v2 (Satkona v3 support)
    m.add_function(wrap_pyfunction!(
        patterns::extract_patterns_from_content,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(patterns::score_patterns_idf, m)?)?;
    // Pattern Engine
    m.add_class::<pattern_engine::PatternEngine>()?;
    // Core Python bindings (event processor, SIMD search, similarity)
    python_bindings::register_python_functions(m)?;
    // Event processing (RustEventProcessor has PyClass)
    m.add_class::<python_bindings::RustEventProcessor>()?;
    // Harmony/Embodiment
    m.add_class::<harmony::HarmonyVector>()?;
    m.add_function(wrap_pyfunction!(harmony::get_harmony_vector, m)?)?;
    // Spatial Index (Hologram)
    m.add_class::<spatial_index::SpatialIndex>()?;

    // Cryptographic signatures
    m.add_function(wrap_pyfunction!(signature::sign_signal_py, m)?)?;
    m.add_function(wrap_pyfunction!(signature::verify_signal_py, m)?)?;

    // Async Memory Submodule
    let async_memory = PyModule::new_bound(m.py(), "async_memory")?;
    async_memory.add_class::<async_memory::PyAsyncMemory>()?;
    async_memory.add_class::<async_memory::Memory>()?;
    async_memory.add_class::<async_memory::MemoryStats>()?;
    async_memory.add_function(wrap_pyfunction!(
        async_memory::create_memory,
        &async_memory
    )?)?;
    m.add_submodule(&async_memory)?;

    // Consolidation Submodule
    let consolidation = PyModule::new_bound(m.py(), "consolidation")?;
    consolidation.add_class::<consolidation::MemoryConsolidator>()?;
    m.add_submodule(&consolidation)?;

    // Holographic Memory Submodule
    let holographic = PyModule::new_bound(m.py(), "holographic")?;
    holographic::register(m.py(), &holographic)?;
    m.add_submodule(&holographic)?;

    // Synthesis Engine Submodule
    let synthesis_mod = PyModule::new_bound(m.py(), "synthesis")?;
    synthesis::register(m.py(), &synthesis_mod)?;
    m.add_submodule(&synthesis_mod)?;

    // Zig FFI Exports
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_init, m)?)?;
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_block_count, m)?)?;
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_rearrange, m)?)?;
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_dump_stats, m)?)?;
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_holographic_project, m)?)?;
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_genomics_flux, m)?)?;
    m.add_function(wrap_pyfunction!(zig_ffi::zig_py_iching_cast, m)?)?;

    // v12.3 Accelerators — Galactic Map
    m.add_function(wrap_pyfunction!(galactic_accelerator::galactic_batch_score, m)?)?;
    m.add_function(wrap_pyfunction!(galactic_accelerator::galactic_batch_score_quick, m)?)?;
    m.add_function(wrap_pyfunction!(galactic_accelerator::galactic_decay_drift, m)?)?;
    m.add_function(wrap_pyfunction!(galactic_accelerator::galactic_zone_counts, m)?)?;

    // v12.3 Accelerators — Association Miner
    m.add_function(wrap_pyfunction!(association_accelerator::association_extract_keywords, m)?)?;
    m.add_function(wrap_pyfunction!(association_accelerator::association_pairwise_overlap, m)?)?;
    m.add_function(wrap_pyfunction!(association_accelerator::association_mine_fast, m)?)?;

    // v12.3 Accelerators — 5D Spatial Index
    m.add_class::<spatial_index_5d::SpatialIndex5D>()?;
    m.add_function(wrap_pyfunction!(spatial_index_5d::batch_nearest_5d, m)?)?;
    m.add_function(wrap_pyfunction!(spatial_index_5d::density_map_5d, m)?)?;

    // v13 — Full-text Search Engine (BM25 + fuzzy + boolean)
    m.add_function(wrap_pyfunction!(search::search_build_index, m)?)?;
    m.add_function(wrap_pyfunction!(search::search_query, m)?)?;
    m.add_function(wrap_pyfunction!(search::search_fuzzy, m)?)?;
    m.add_function(wrap_pyfunction!(search::search_and_query, m)?)?;
    m.add_function(wrap_pyfunction!(search::search_stats, m)?)?;

    // v13 — Atomic Rate Limiter
    m.add_function(wrap_pyfunction!(rate_limiter::rate_check, m)?)?;
    m.add_function(wrap_pyfunction!(rate_limiter::rate_check_batch, m)?)?;
    m.add_function(wrap_pyfunction!(rate_limiter::rate_set_override, m)?)?;
    m.add_function(wrap_pyfunction!(rate_limiter::rate_stats, m)?)?;

    // v14 — Native FFI (zero-copy, no JSON serialization)
    m.add_function(wrap_pyfunction!(rate_limiter::rate_check_native, m)?)?;
    m.add_function(wrap_pyfunction!(rate_limiter::rate_check_batch_native, m)?)?;
    m.add_function(wrap_pyfunction!(rate_limiter::rate_stats_native, m)?)?;

    // v13.1 — 5D Holographic Encoder
    m.add_function(wrap_pyfunction!(holographic_encoder_5d::holographic_encode_batch, m)?)?;
    m.add_function(wrap_pyfunction!(holographic_encoder_5d::holographic_encode_single, m)?)?;
    m.add_function(wrap_pyfunction!(holographic_encoder_5d::holographic_nearest_5d, m)?)?;

    // v13.1 — MinHash LSH
    m.add_function(wrap_pyfunction!(minhash::minhash_find_duplicates, m)?)?;
    m.add_function(wrap_pyfunction!(minhash::minhash_signatures, m)?)?;

    // v13.1 — SQLite Accelerator
    #[cfg(feature = "rusqlite")]
    {
        m.add_function(wrap_pyfunction!(sqlite_accel::sqlite_batch_update_galactic, m)?)?;
        m.add_function(wrap_pyfunction!(sqlite_accel::sqlite_decay_drift, m)?)?;
        m.add_function(wrap_pyfunction!(sqlite_accel::sqlite_fts_search, m)?)?;
        m.add_function(wrap_pyfunction!(sqlite_accel::sqlite_zone_stats, m)?)?;
        m.add_function(wrap_pyfunction!(sqlite_accel::sqlite_export_for_mining, m)?)?;
    }

    // v13.3.2 — Fast keyword extraction (replaces Zig ctypes path)
    m.add_function(wrap_pyfunction!(keyword_extract::keyword_extract, m)?)?;
    m.add_function(wrap_pyfunction!(keyword_extract::keyword_extract_batch, m)?)?;

    // v13.3.2 — Multi-pass retrieval pipeline (composable micro-queries)
    m.add_function(wrap_pyfunction!(retrieval_pipeline::retrieval_pipeline, m)?)?;
    m.add_function(wrap_pyfunction!(retrieval_pipeline::retrieval_pipeline_native, m)?)?;

    // v13.3.2 — Shared memory (mmap) cross-process state
    m.add_function(wrap_pyfunction!(shared_state::shared_state_stats, m)?)?;
    m.add_function(wrap_pyfunction!(shared_state::shared_state_reset, m)?)?;

    // v14 — Lock-free event bus primitives
    m.add_function(wrap_pyfunction!(event_bus::event_bus_try_emit, m)?)?;
    m.add_function(wrap_pyfunction!(event_bus::event_bus_set_dampening, m)?)?;
    m.add_function(wrap_pyfunction!(event_bus::event_bus_set_stillness, m)?)?;
    m.add_function(wrap_pyfunction!(event_bus::event_bus_stats, m)?)?;
    m.add_function(wrap_pyfunction!(event_bus::event_bus_reset, m)?)?;

    // Leap 7a — StateBoard (shared-memory blackboard)
    m.add_function(wrap_pyfunction!(state_board::board_write_harmony, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_read_harmony, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_write_resonance, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_read_state, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_write_breaker, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_read_breaker, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_increment_counter, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_set_active_engines, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_get_path, m)?)?;
    m.add_function(wrap_pyfunction!(state_board::board_reset, m)?)?;

    // Leap 7b — EventRing (LMAX Disruptor ring buffer)
    m.add_function(wrap_pyfunction!(event_ring::ring_publish, m)?)?;
    m.add_function(wrap_pyfunction!(event_ring::ring_register_consumer, m)?)?;
    m.add_function(wrap_pyfunction!(event_ring::ring_poll, m)?)?;
    m.add_function(wrap_pyfunction!(event_ring::ring_stats, m)?)?;
    m.add_function(wrap_pyfunction!(event_ring::ring_reset, m)?)?;

    // v14 — Hybrid RRF (Reciprocal Rank Fusion)
    m.add_class::<hybrid_rrf::RrfResult>()?;
    m.add_function(wrap_pyfunction!(hybrid_rrf::rrf_fuse, m)?)?;
    m.add_function(wrap_pyfunction!(hybrid_rrf::rrf_fuse_batch, m)?)?;

    // v14 — Association Walk (BFS in rusqlite)
    #[cfg(feature = "rusqlite")]
    {
        m.add_class::<association_walk::WalkNode>()?;
        m.add_function(wrap_pyfunction!(association_walk::association_walk, m)?)?;
    }

    // v14.5 — Arrow IPC Bridge (zero-copy columnar interchange)
    m.add_function(wrap_pyfunction!(arrow_bridge::arrow_encode_memories, m)?)?;
    m.add_function(wrap_pyfunction!(arrow_bridge::arrow_decode_memories, m)?)?;
    m.add_function(wrap_pyfunction!(arrow_bridge::arrow_schema_info, m)?)?;
    m.add_function(wrap_pyfunction!(arrow_bridge::arrow_roundtrip_bench, m)?)?;

    // v14.5 — Tokio Clone Army (massively parallel exploration)
    m.add_function(wrap_pyfunction!(tokio_clones::tokio_deploy_clones, m)?)?;
    m.add_function(wrap_pyfunction!(tokio_clones::tokio_clone_bench, m)?)?;
    m.add_function(wrap_pyfunction!(tokio_clones::tokio_clone_stats, m)?)?;

    // v14.5 — Iceoryx2 IPC Bridge (cross-process zero-copy)
    m.add_function(wrap_pyfunction!(ipc_bridge::ipc_bridge_init, m)?)?;
    m.add_function(wrap_pyfunction!(ipc_bridge::ipc_bridge_publish, m)?)?;
    m.add_function(wrap_pyfunction!(ipc_bridge::ipc_bridge_status, m)?)?;

    Ok(())
}
