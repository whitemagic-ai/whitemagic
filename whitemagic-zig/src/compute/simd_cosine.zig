const std = @import("std");
const math = std.math;

/// SIMD-accelerated cosine similarity for embedding vectors.
/// Uses @Vector for auto-vectorization on supported hardware.
/// Falls back to scalar loop for remainder elements.

const LANE_WIDTH = 8; // Process 8 floats at a time (AVX2-friendly)
const VecF32 = @Vector(LANE_WIDTH, f32);

/// Compute cosine similarity between two f32 slices using SIMD.
/// Returns similarity in [-1.0, 1.0]. Returns 0.0 if either vector has zero norm.
pub fn cosine_similarity(a: []const f32, b: []const f32) f32 {
    const len = @min(a.len, b.len);
    if (len == 0) return 0.0;

    var dot: f32 = 0.0;
    var norm_a: f32 = 0.0;
    var norm_b: f32 = 0.0;

    // SIMD bulk processing
    const simd_len = len - (len % LANE_WIDTH);
    var i: usize = 0;

    var dot_acc: VecF32 = @splat(0.0);
    var na_acc: VecF32 = @splat(0.0);
    var nb_acc: VecF32 = @splat(0.0);

    while (i < simd_len) : (i += LANE_WIDTH) {
        const va: VecF32 = a[i..][0..LANE_WIDTH].*;
        const vb: VecF32 = b[i..][0..LANE_WIDTH].*;
        dot_acc += va * vb;
        na_acc += va * va;
        nb_acc += vb * vb;
    }

    // Horizontal reduce
    dot += @reduce(.Add, dot_acc);
    norm_a += @reduce(.Add, na_acc);
    norm_b += @reduce(.Add, nb_acc);

    // Scalar remainder
    while (i < len) : (i += 1) {
        dot += a[i] * b[i];
        norm_a += a[i] * a[i];
        norm_b += b[i] * b[i];
    }

    const denom = @sqrt(norm_a) * @sqrt(norm_b);
    if (denom == 0.0) return 0.0;
    return dot / denom;
}

/// Batch cosine similarity: compare a query vector against N vectors.
/// Returns array of similarity scores.
/// `vectors` is a flat array of N*dim floats. `scores_out` must have space for N scores.
pub fn batch_cosine(
    query: []const f32,
    vectors: []const f32,
    dim: usize,
    scores_out: []f32,
) void {
    if (dim == 0) return;
    const n = @min(vectors.len / dim, scores_out.len);
    for (0..n) |idx| {
        const offset = idx * dim;
        const end = offset + dim;
        if (end > vectors.len) break;
        scores_out[idx] = cosine_similarity(query, vectors[offset..end]);
    }
}

// --------------------------------------------------------------------------
// C-ABI exports for FFI (Python ctypes / Rust FFI)
// --------------------------------------------------------------------------

/// Compute cosine similarity between two float arrays.
/// Returns similarity score in [-1.0, 1.0].
pub export fn wm_simd_cosine(
    a_ptr: [*]const f32,
    b_ptr: [*]const f32,
    len: usize,
) f32 {
    if (len == 0) return 0.0;
    const a = a_ptr[0..len];
    const b = b_ptr[0..len];
    return cosine_similarity(a, b);
}

/// Batch cosine similarity: compare query against N vectors.
/// `vectors_ptr` points to N*dim contiguous floats.
/// `scores_ptr` must point to space for N floats.
pub export fn wm_simd_batch_cosine(
    query_ptr: [*]const f32,
    vectors_ptr: [*]const f32,
    dim: usize,
    n: usize,
    scores_ptr: [*]f32,
) void {
    if (dim == 0 or n == 0) return;
    const query = query_ptr[0..dim];
    const vectors = vectors_ptr[0 .. n * dim];
    const scores = scores_ptr[0..n];
    batch_cosine(query, vectors, dim, scores);
}

/// Return the SIMD lane width being used (useful for diagnostics).
pub export fn wm_simd_lane_width() usize {
    return LANE_WIDTH;
}

// --------------------------------------------------------------------------
// Tests
// --------------------------------------------------------------------------

test "cosine_similarity identical vectors" {
    const a = [_]f32{ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 };
    const b = [_]f32{ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 };
    const sim = cosine_similarity(&a, &b);
    try std.testing.expectApproxEqAbs(sim, 1.0, 0.0001);
}

test "cosine_similarity orthogonal vectors" {
    const a = [_]f32{ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    const b = [_]f32{ 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    const sim = cosine_similarity(&a, &b);
    try std.testing.expectApproxEqAbs(sim, 0.0, 0.0001);
}

test "cosine_similarity opposite vectors" {
    const a = [_]f32{ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 };
    const b = [_]f32{ -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0 };
    const sim = cosine_similarity(&a, &b);
    try std.testing.expectApproxEqAbs(sim, -1.0, 0.0001);
}

test "batch_cosine" {
    const query = [_]f32{ 1.0, 0.0, 0.0, 0.0 };
    const vectors = [_]f32{
        1.0, 0.0, 0.0, 0.0, // identical
        0.0, 1.0, 0.0, 0.0, // orthogonal
        0.5, 0.5, 0.0, 0.0, // partial
    };
    var scores: [3]f32 = undefined;
    batch_cosine(&query, &vectors, 4, &scores);
    try std.testing.expectApproxEqAbs(scores[0], 1.0, 0.0001);
    try std.testing.expectApproxEqAbs(scores[1], 0.0, 0.0001);
    try std.testing.expect(scores[2] > 0.5);
}
