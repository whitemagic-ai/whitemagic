/*
 * whitemagic.h  —  C FFI surface for the Zig unified memory manager
 *                   and auxiliary compute modules.
 *
 * Consumed by:
 *   - Rust  (rust_core/src/zig_ffi.rs  via  #[link(name = "whitemagic-zig")])
 *   - Python (ctypes / cffi)
 *   - Any future C/C++ consumer
 *
 * Build the shared library first:
 *   cd whitemagic-zig && zig build
 *   # produces zig-out/lib/libwhitemagic.so  (dynamic)
 *   #          zig-out/lib/libwhitemagic-zig.a (static, for Rust)
 */

#ifndef WHITEMAGIC_H
#define WHITEMAGIC_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* =========================================================================
 * Memory Management  (unified.zig)
 * =========================================================================
 * Thread-safe 16 MB compacting heap.  All IDs are caller-assigned u64s;
 * zero is reserved (invalid).  Alignment must be a power of two.
 * ========================================================================= */

/** Initialise the global memory manager.  Idempotent — safe to call more than once. */
void wm_memory_init(void);

/** Release all heap memory and destroy the manager.  After this call
 *  wm_memory_init() must be called before any other wm_memory_* function. */
void wm_memory_deinit(void);

/**
 * Allocate (or re-allocate) a block identified by @id.
 *
 * @param id        Caller-assigned handle (must be > 0).
 * @param size      Requested size in bytes (must be > 0).
 * @param alignment Power-of-two alignment (e.g. 8, 16, 64).
 * @return          Pointer to the block, or NULL on failure / OOM.
 *
 * If @id already exists the block is relocated and old contents are
 * preserved up to min(old_size, new_size).
 */
uint8_t* wm_memory_alloc(uint64_t id, size_t size, uint32_t alignment);

/** Free the block identified by @id.  No-op if @id is unknown or zero. */
void wm_memory_free(uint64_t id);

/**
 * Compact the heap: slide all live blocks to the front, reclaiming
 * fragmented gaps.  Returns true on success.
 */
bool wm_memory_rearrange(void);

/** Return the number of live blocks currently tracked. */
size_t wm_memory_get_block_count(void);

/**
 * Resolve a handle back to its current heap pointer.
 * MUST be called again after any wm_memory_alloc or wm_memory_rearrange
 * because compaction can move blocks.
 *
 * @return Pointer to the block, or NULL if @id is unknown.
 */
uint8_t* wm_memory_resolve_handle(uint64_t id);

/**
 * Dump a JSON stats snapshot to the file at @path_ptr[@path_len].
 * JSON keys: used, heap_size, timestamp, blocks[].
 *
 * @return true on success.
 */
bool wm_memory_dump_stats_json(const uint8_t* path_ptr, size_t path_len);

/* =========================================================================
 * Transient Allocator  (transient.zig)
 * =========================================================================
 * Stack-style bump allocator — allocate fast, reset everything at once.
 * Useful for per-frame / per-request scratch space.
 * ========================================================================= */

/** Initialise the transient allocator (allocates backing buffer). */
void zig_init_transient_allocator(void);

/** Bump-allocate @size bytes.  Returns NULL if the arena is exhausted. */
void* zig_transient_alloc(size_t size);

/** Reset the bump pointer to zero — all previous transient allocations
 *  are implicitly freed.  Does NOT free the backing buffer. */
void zig_transient_reset(void);

/** Free the backing buffer and tear down the transient allocator. */
void zig_deinit_transient_allocator(void);

/* =========================================================================
 * Holographic Projection  (compute/holographic.zig)
 * =========================================================================
 * Projects an N-dimensional input vector into the 4-D holographic
 * coordinate space [logic, micro, time, importance].
 * ========================================================================= */

/** 4-D holographic coordinate. */
typedef struct {
    float x;   /**< Logic axis        */
    float y;   /**< Micro axis        */
    float z;   /**< Time axis         */
    float w;   /**< Importance axis   */
} wm_coordinate_t;

/**
 * Project @input_len floats into a single 4-D coordinate and store the
 * result in the unified heap under @result_id.
 *
 * @param input_ptr  Input vector (row-major floats).
 * @param input_len  Number of elements in the input vector.
 * @param result_id  Heap ID under which the resulting coordinate is stored.
 * @return           Pointer to the stored wm_coordinate_t, or NULL on error.
 */
wm_coordinate_t* wm_holographic_project(const float* input_ptr, size_t input_len, uint64_t result_id);

/* =========================================================================
 * Genomics / Flux Simulation  (genomics/metabolic.zig)
 * ========================================================================= */

/**
 * Simulate one step of metabolic flux.
 *
 * @param rate          Reaction rate constant.
 * @param concentration Substrate concentration.
 * @return              Resulting flux value.
 */
float wm_genomics_simulate_flux(float rate, float concentration);

/* =========================================================================
 * I Ching Oracle  (main.zig)
 * =========================================================================
 * Entropy-seeded hexagram cast.  Writes 6 bytes (one per line, 0=Yin 1=Yang)
 * into the caller-supplied buffer.
 * ========================================================================= */

/**
 * Cast a hexagram using system entropy.
 *
 * @param out_ptr  Caller-allocated buffer of at least 6 bytes.
 *                 Each byte is 0 (Yin) or 1 (Yang), bottom line first.
 */
void wm_iching_cast(uint8_t* out_ptr);

#endif /* WHITEMAGIC_H */
