//! WhiteMagic Rust Core - Minimal stable build
//! Python fallback available for all features

#[cfg(feature = "python")]
use pyo3::prelude::*;
#[cfg(feature = "python")]
use pyo3::wrap_pyfunction;

// Core modules
pub mod async_memory;
pub mod audit;
pub mod clones;
pub mod compression;
pub mod consolidation;
pub mod data_lake;
pub mod embeddings;
pub mod event_processor;
pub mod file_ops;
pub mod graph;
pub mod harmony;
pub mod hologram;
pub mod holographic;
pub mod iching;
pub mod memory_consolidation;
pub mod pattern_engine;
pub mod pattern_extraction;
pub mod pattern_matcher;
pub mod patterns;
pub mod python_bindings;
pub mod search;
pub mod signature;
pub mod simd_inference;
pub mod simd_search;
pub mod spatial_index;
pub mod synthesis;
pub mod zig_bridge;
pub mod zig_ffi;

// v12.3 Accelerators
pub mod galactic_accelerator;
pub mod association_accelerator;
pub mod spatial_index_5d;

// v13 Polyglot Expansion
pub mod rate_limiter;
pub mod event_bus;
pub mod holographic_encoder_5d;
pub mod minhash;
#[cfg(feature = "rusqlite")]
pub mod sqlite_accel;
pub mod keyword_extract;
pub mod retrieval_pipeline;
pub mod shared_state;
pub mod state_board;
pub mod event_ring;

// v14 — Hybrid RRF + Association Walk accelerators
pub mod hybrid_rrf;
#[cfg(feature = "rusqlite")]
pub mod association_walk;

// Python bindings - minimal export
#[cfg(feature = "python")]
#[pymodule]
fn whitemagic_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("__version__", "14.1.0")?;
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

    Ok(())
}
