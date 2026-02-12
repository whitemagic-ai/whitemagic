const std = @import("std");
pub const memory = struct {
    pub const unified = @import("memory/unified.zig");
    pub const transient = @import("memory/transient.zig");
    pub const arena = @import("memory/arena.zig");
};
pub const compute = struct {
    pub const holographic = @import("compute/holographic.zig");
    pub const simd_cosine = @import("compute/simd_cosine.zig");
    pub const holographic_5d = @import("compute/holographic_5d.zig");
    pub const constellation = @import("compute/constellation.zig");
    pub const vector_batch = @import("compute/vector_batch.zig");
    pub const keyword_extract = @import("compute/keyword_extract.zig");
    pub const distance_matrix = @import("compute/distance_matrix.zig");
};
pub const iching = @import("iching.zig");
pub const genomics = struct {
    pub const metabolic = @import("genomics/metabolic.zig");
};
pub const io = struct {
    pub const bridge = @import("io/bridge.zig");
    pub const network = @import("io/network.zig");
};
pub const dispatch_core = @import("dispatch_core.zig");

// --- Re-exporting from submodules to ensure FFI symbols are visible ---

// From memory/unified.zig
export fn wm_memory_init() void {
    memory.unified.wm_memory_init();
}
export fn wm_memory_deinit() void {
    memory.unified.wm_memory_deinit();
}
export fn wm_memory_alloc(id: u64, size: usize, alignment: u32) ?[*]u8 {
    return memory.unified.wm_memory_alloc(id, size, alignment);
}
export fn wm_memory_free(id: u64) void {
    memory.unified.wm_memory_free(id);
}

export fn wm_memory_rearrange() bool {
    return memory.unified.wm_memory_rearrange();
}

export fn wm_memory_get_block_count() usize {
    return memory.unified.wm_memory_get_block_count();
}

export fn wm_memory_resolve_handle(id: u64) ?[*]u8 {
    return memory.unified.wm_memory_resolve_handle(id);
}

export fn wm_memory_dump_stats_json(path_ptr: [*]const u8, path_len: usize) bool {
    return memory.unified.wm_memory_dump_stats_json(path_ptr, path_len);
}

// From compute/holographic.zig
export fn wm_holographic_project(input_ptr: [*]const f32, input_len: usize, result_id: u64) ?[*]compute.holographic.Coordinate {
    const result = compute.holographic.wm_holographic_project(input_ptr, input_len, result_id);
    if (result.error_code != 0) return null;
    return result.ptr;
}

// From memory/transient.zig
export fn zig_init_transient_allocator() void {
    memory.transient.zig_init_transient_allocator();
}
export fn zig_transient_alloc(size: usize) ?*anyopaque {
    return memory.transient.zig_transient_alloc(size);
}
export fn zig_transient_reset() void {
    memory.transient.zig_transient_reset();
}
export fn zig_deinit_transient_allocator() void {
    memory.transient.zig_deinit_transient_allocator();
}

// From genomics/metabolic.zig
export fn wm_genomics_simulate_flux(rate: f32, conc: f32) f32 {
    return genomics.metabolic.wm_genomics_simulate_flux(rate, conc);
}

// From iching.zig
export fn wm_iching_cast(out_ptr: [*]u8) void {
    var engine = iching.IChingEngine.init();
    const cast = engine.cast_hexagram();
    var i: usize = 0;
    while (i < 6) : (i += 1) {
        out_ptr[i] = cast[i];
    }
}

// From compute/keyword_extract.zig
export fn wm_extract_keywords(
    text_ptr: [*]const u8,
    text_len: usize,
    out_ptr: [*]u8,
    out_capacity: usize,
    max_keywords: usize,
) usize {
    return compute.keyword_extract.wm_extract_keywords(text_ptr, text_len, out_ptr, out_capacity, max_keywords);
}

// From compute/distance_matrix.zig
export fn wm_distance_matrix(
    vectors_ptr: [*]const f32,
    n: usize,
    dim: usize,
    out_ptr: [*]f32,
) void {
    compute.distance_matrix.wm_distance_matrix(vectors_ptr, n, dim, out_ptr);
}

export fn wm_cosine_similarity(
    a_ptr: [*]const f32,
    b_ptr: [*]const f32,
    dim: usize,
) f32 {
    return compute.distance_matrix.wm_cosine_similarity(a_ptr, b_ptr, dim);
}

// Force inclusion of all modules and their exported symbols
comptime {
    _ = memory.unified;
    _ = memory.transient;
    _ = memory.arena;
    _ = compute.holographic;
    _ = compute.simd_cosine;
    _ = compute.holographic_5d;
    _ = compute.constellation;
    _ = compute.vector_batch;
    _ = compute.keyword_extract;
    _ = compute.distance_matrix;
    _ = iching;
    _ = genomics.metabolic;
    _ = io.bridge;
    _ = io.network;
    _ = dispatch_core;
}
