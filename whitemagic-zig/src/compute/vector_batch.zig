const std = @import("std");
const math = std.math;
const simd_cosine = @import("simd_cosine.zig");

/// Batch vector operations for WhiteMagic memory search.
///
/// Provides:
///   - Top-K similarity search (find K most similar vectors)
///   - Batch dot product computation
///   - L2 normalization
///   - Centroid computation
///   - Pairwise distance matrix (upper triangle)
///
/// All operations use SIMD via simd_cosine's vectorized primitives.

const LANE_WIDTH = 8;
const VecF32 = @Vector(LANE_WIDTH, f32);

/// Result of a top-K search: index + similarity score.
pub const TopKResult = extern struct {
    index: u32,
    score: f32,
};

/// Find the top-K most similar vectors to a query using cosine similarity.
/// Results are sorted by descending similarity.
pub fn top_k_cosine(
    query: []const f32,
    vectors: []const f32,
    dim: usize,
    k: usize,
    results: []TopKResult,
) usize {
    if (dim == 0 or query.len < dim) return 0;
    const n = vectors.len / dim;
    const actual_k = @min(k, @min(n, results.len));
    if (actual_k == 0) return 0;

    // Initialize results with worst scores
    for (0..actual_k) |i| {
        results[i] = TopKResult{ .index = 0, .score = -2.0 };
    }

    // Scan all vectors
    for (0..n) |i| {
        const offset = i * dim;
        const end = offset + dim;
        if (end > vectors.len) break;

        const sim = simd_cosine.cosine_similarity(query, vectors[offset..end]);

        // Check if this should enter top-K
        if (sim > results[actual_k - 1].score) {
            // Insertion sort into results
            var pos = actual_k - 1;
            while (pos > 0 and sim > results[pos - 1].score) {
                results[pos] = results[pos - 1];
                pos -= 1;
            }
            results[pos] = TopKResult{ .index = @intCast(i), .score = sim };
        }
    }

    return actual_k;
}

/// L2-normalize a vector in place.
pub fn l2_normalize(vec: []f32) void {
    if (vec.len == 0) return;

    var sum_sq: f32 = 0.0;
    const simd_len = vec.len - (vec.len % LANE_WIDTH);
    var i: usize = 0;

    var acc: VecF32 = @splat(0.0);
    while (i < simd_len) : (i += LANE_WIDTH) {
        const v: VecF32 = vec[i..][0..LANE_WIDTH].*;
        acc += v * v;
    }
    sum_sq = @reduce(.Add, acc);

    // Scalar remainder
    while (i < vec.len) : (i += 1) {
        sum_sq += vec[i] * vec[i];
    }

    const norm = @sqrt(sum_sq);
    if (norm == 0.0) return;

    const inv_norm: VecF32 = @splat(1.0 / norm);
    i = 0;
    while (i < simd_len) : (i += LANE_WIDTH) {
        const v: VecF32 = vec[i..][0..LANE_WIDTH].*;
        const normalized = v * inv_norm;
        inline for (0..LANE_WIDTH) |lane| {
            vec[i + lane] = normalized[lane];
        }
    }
    while (i < vec.len) : (i += 1) {
        vec[i] /= norm;
    }
}

/// Compute centroid of N vectors (average vector).
pub fn compute_vector_centroid(
    vectors: []const f32,
    dim: usize,
    centroid_out: []f32,
) void {
    if (dim == 0 or centroid_out.len < dim) return;
    const n = vectors.len / dim;
    if (n == 0) return;

    // Zero the output
    for (0..dim) |d| {
        centroid_out[d] = 0.0;
    }

    // Sum all vectors
    for (0..n) |i| {
        const offset = i * dim;
        for (0..dim) |d| {
            if (offset + d < vectors.len) {
                centroid_out[d] += vectors[offset + d];
            }
        }
    }

    // Divide by N
    const n_f: f32 = @floatFromInt(n);
    for (0..dim) |d| {
        centroid_out[d] /= n_f;
    }
}

/// Batch dot product: compute dot products of a query against N vectors.
pub fn batch_dot_product(
    query: []const f32,
    vectors: []const f32,
    dim: usize,
    dots_out: []f32,
) void {
    if (dim == 0) return;
    const n = @min(vectors.len / dim, dots_out.len);
    for (0..n) |i| {
        const offset = i * dim;
        var dot: f32 = 0.0;
        const simd_dim = dim - (dim % LANE_WIDTH);
        var d: usize = 0;

        var acc: VecF32 = @splat(0.0);
        while (d < simd_dim) : (d += LANE_WIDTH) {
            const vq: VecF32 = query[d..][0..LANE_WIDTH].*;
            const vv: VecF32 = vectors[offset + d ..][0..LANE_WIDTH].*;
            acc += vq * vv;
        }
        dot = @reduce(.Add, acc);

        while (d < dim) : (d += 1) {
            dot += query[d] * vectors[offset + d];
        }
        dots_out[i] = dot;
    }
}

// ---------------------------------------------------------------------------
// C-ABI exports
// ---------------------------------------------------------------------------

/// Top-K cosine similarity search. Returns actual K found.
pub export fn wm_vector_top_k(
    query_ptr: [*]const f32,
    vectors_ptr: [*]const f32,
    dim: usize,
    n_vectors: usize,
    k: usize,
    results_ptr: [*]TopKResult,
) usize {
    if (dim == 0 or n_vectors == 0 or k == 0) return 0;
    const query = query_ptr[0..dim];
    const vectors = vectors_ptr[0 .. n_vectors * dim];
    const results = results_ptr[0..k];
    return top_k_cosine(query, vectors, dim, k, results);
}

/// Batch dot product via C-ABI.
pub export fn wm_vector_batch_dot(
    query_ptr: [*]const f32,
    vectors_ptr: [*]const f32,
    dim: usize,
    n: usize,
    dots_ptr: [*]f32,
) void {
    if (dim == 0 or n == 0) return;
    const query = query_ptr[0..dim];
    const vectors = vectors_ptr[0 .. n * dim];
    const dots = dots_ptr[0..n];
    batch_dot_product(query, vectors, dim, dots);
}

/// L2 normalize a vector in-place via C-ABI.
pub export fn wm_vector_normalize(vec_ptr: [*]f32, len: usize) void {
    if (len == 0) return;
    l2_normalize(vec_ptr[0..len]);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "top_k_cosine basic" {
    const query = [_]f32{ 1.0, 0.0, 0.0, 0.0 };
    const vectors = [_]f32{
        0.0,  1.0, 0.0, 0.0, // orthogonal
        1.0,  0.0, 0.0, 0.0, // identical
        0.5,  0.5, 0.0, 0.0, // partial
        -1.0, 0.0, 0.0, 0.0, // opposite
    };
    var results: [2]TopKResult = undefined;
    const found = top_k_cosine(&query, &vectors, 4, 2, &results);
    try std.testing.expectEqual(found, 2);
    try std.testing.expectEqual(results[0].index, 1); // identical = highest
    try std.testing.expectApproxEqAbs(results[0].score, 1.0, 0.001);
}

test "l2_normalize" {
    var vec = [_]f32{ 3.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    l2_normalize(&vec);
    // Should be [0.6, 0.8, 0, ...]
    try std.testing.expectApproxEqAbs(vec[0], 0.6, 0.001);
    try std.testing.expectApproxEqAbs(vec[1], 0.8, 0.001);
}

test "batch_dot_product" {
    const query = [_]f32{ 1.0, 2.0, 3.0, 4.0 };
    const vectors = [_]f32{
        1.0, 1.0, 1.0, 1.0,
        2.0, 2.0, 2.0, 2.0,
    };
    var dots: [2]f32 = undefined;
    batch_dot_product(&query, &vectors, 4, &dots);
    try std.testing.expectApproxEqAbs(dots[0], 10.0, 0.001); // 1+2+3+4
    try std.testing.expectApproxEqAbs(dots[1], 20.0, 0.001); // 2+4+6+8
}

test "compute_vector_centroid" {
    const vectors = [_]f32{
        0.0, 0.0, 0.0, 0.0,
        2.0, 4.0, 6.0, 8.0,
    };
    var centroid: [4]f32 = undefined;
    compute_vector_centroid(&vectors, 4, &centroid);
    try std.testing.expectApproxEqAbs(centroid[0], 1.0, 0.001);
    try std.testing.expectApproxEqAbs(centroid[1], 2.0, 0.001);
    try std.testing.expectApproxEqAbs(centroid[2], 3.0, 0.001);
    try std.testing.expectApproxEqAbs(centroid[3], 4.0, 0.001);
}
