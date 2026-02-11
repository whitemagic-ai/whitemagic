const std = @import("std");
const math = std.math;

/// SIMD-accelerated pairwise distance matrix computation.
///
/// Computes cosine similarity between all pairs of N vectors of dimension D.
/// Uses blocked computation for cache efficiency and SIMD for the inner dot product.
///
/// For N=1000, D=128: 499,500 pairs. SIMD processes D dimensions in parallel.
/// Blocked tiling (TILE_SIZE × TILE_SIZE) maximizes L1/L2 cache utilization.

const LANE_WIDTH = 8; // 8 floats (AVX2-friendly)
const VecF32 = @Vector(LANE_WIDTH, f32);
const TILE_SIZE = 64; // Block size for cache-friendly tiling

// ---------------------------------------------------------------------------
// SIMD cosine similarity (internal)
// ---------------------------------------------------------------------------

/// Compute cosine similarity between two f32 vectors using SIMD.
inline fn simd_cosine(a: []const f32, b: []const f32) f32 {
    const len = @min(a.len, b.len);
    if (len == 0) return 0.0;

    var dot_acc: VecF32 = @splat(0.0);
    var na_acc: VecF32 = @splat(0.0);
    var nb_acc: VecF32 = @splat(0.0);

    const simd_len = len - (len % LANE_WIDTH);
    var i: usize = 0;

    while (i < simd_len) : (i += LANE_WIDTH) {
        const va: VecF32 = a[i..][0..LANE_WIDTH].*;
        const vb: VecF32 = b[i..][0..LANE_WIDTH].*;
        dot_acc += va * vb;
        na_acc += va * va;
        nb_acc += vb * vb;
    }

    var dot: f32 = @reduce(.Add, dot_acc);
    var norm_a: f32 = @reduce(.Add, na_acc);
    var norm_b: f32 = @reduce(.Add, nb_acc);

    // Scalar remainder
    while (i < len) : (i += 1) {
        dot += a[i] * b[i];
        norm_a += a[i] * a[i];
        norm_b += b[i] * b[i];
    }

    const denom = @sqrt(norm_a) * @sqrt(norm_b);
    if (denom < 1e-12) return 0.0;
    return dot / denom;
}

// ---------------------------------------------------------------------------
// Pairwise distance matrix (upper triangle, blocked)
// ---------------------------------------------------------------------------

/// Compute the upper triangle of the pairwise cosine similarity matrix.
/// Only stores (i, j) where i < j to avoid redundancy.
///
/// vectors: flat array of N×D f32 values (row-major)
/// n: number of vectors
/// dim: dimensionality of each vector
/// out_matrix: flat array of N×N f32 values (row-major), filled with similarities
///
/// The matrix is symmetric: out[i*n+j] = out[j*n+i] for i<j.
/// Diagonal is set to 1.0.
pub fn compute_distance_matrix(
    vectors: []const f32,
    n: usize,
    dim: usize,
    out_matrix: []f32,
) void {
    if (n == 0 or dim == 0) return;

    // Set diagonal to 1.0
    for (0..n) |i| {
        out_matrix[i * n + i] = 1.0;
    }

    // Blocked computation: process TILE_SIZE × TILE_SIZE blocks
    var ti: usize = 0;
    while (ti < n) : (ti += TILE_SIZE) {
        const ti_end = @min(ti + TILE_SIZE, n);
        var tj: usize = ti;
        while (tj < n) : (tj += TILE_SIZE) {
            const tj_end = @min(tj + TILE_SIZE, n);

            // Process block (ti..ti_end, tj..tj_end)
            for (ti..ti_end) |i| {
                const start_j = if (ti == tj) i + 1 else tj;
                for (start_j..tj_end) |j| {
                    const vec_i = vectors[i * dim .. (i + 1) * dim];
                    const vec_j = vectors[j * dim .. (j + 1) * dim];
                    const sim = simd_cosine(vec_i, vec_j);
                    out_matrix[i * n + j] = sim;
                    out_matrix[j * n + i] = sim; // Mirror
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Top-K nearest neighbors from distance matrix
// ---------------------------------------------------------------------------

/// Result of a top-K search
pub const TopKResult = struct {
    index: usize,
    similarity: f32,
};

/// Find the K nearest neighbors for a given vector index from the distance matrix.
/// Returns results sorted by similarity descending (most similar first).
pub fn top_k_from_matrix(
    matrix: []const f32,
    n: usize,
    query_idx: usize,
    k: usize,
    out_results: []TopKResult,
) usize {
    if (n == 0 or k == 0) return 0;

    const effective_k = @min(k, n - 1);
    const row = matrix[query_idx * n .. (query_idx + 1) * n];

    // Simple insertion sort for small k (typical: k < 20)
    var count: usize = 0;

    for (0..n) |j| {
        if (j == query_idx) continue;
        const sim = row[j];

        if (count < effective_k) {
            // Insert
            var pos = count;
            while (pos > 0 and out_results[pos - 1].similarity < sim) : (pos -= 1) {
                out_results[pos] = out_results[pos - 1];
            }
            out_results[pos] = .{ .index = j, .similarity = sim };
            count += 1;
        } else if (sim > out_results[count - 1].similarity) {
            // Replace minimum
            var pos = count - 1;
            while (pos > 0 and out_results[pos - 1].similarity < sim) : (pos -= 1) {
                out_results[pos] = out_results[pos - 1];
            }
            out_results[pos] = .{ .index = j, .similarity = sim };
        }
    }

    return count;
}

// ---------------------------------------------------------------------------
// FFI exports
// ---------------------------------------------------------------------------

/// Compute pairwise cosine distance matrix for N vectors of dimension D.
/// vectors_ptr: pointer to N*D f32 values (row-major)
/// out_ptr: pointer to N*N f32 values (output, row-major)
pub fn wm_distance_matrix(
    vectors_ptr: [*]const f32,
    n: usize,
    dim: usize,
    out_ptr: [*]f32,
) void {
    if (n == 0 or dim == 0) return;
    const vectors = vectors_ptr[0 .. n * dim];
    const out = out_ptr[0 .. n * n];
    compute_distance_matrix(vectors, n, dim, out);
}

/// Find top-K nearest neighbors for a query vector index.
/// Returns count of results written.
pub fn wm_top_k_nearest(
    matrix_ptr: [*]const f32,
    n: usize,
    query_idx: usize,
    k: usize,
    out_indices: [*]usize,
    out_sims: [*]f32,
) usize {
    if (n == 0 or k == 0) return 0;
    const matrix = matrix_ptr[0 .. n * n];
    const effective_k = @min(k, n - 1);

    // Use stack buffer for results
    var results: [256]TopKResult = undefined;
    const actual_k = @min(effective_k, 256);

    const count = top_k_from_matrix(matrix, n, query_idx, actual_k, results[0..actual_k]);

    // Copy to output arrays
    for (0..count) |i| {
        out_indices[i] = results[i].index;
        out_sims[i] = results[i].similarity;
    }

    return count;
}

/// Compute a single cosine similarity between two vectors.
pub fn wm_cosine_similarity(
    a_ptr: [*]const f32,
    b_ptr: [*]const f32,
    dim: usize,
) f32 {
    if (dim == 0) return 0.0;
    return simd_cosine(a_ptr[0..dim], b_ptr[0..dim]);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "simd_cosine_identical" {
    const v = [_]f32{ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 };
    const sim = simd_cosine(&v, &v);
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), sim, 1e-5);
}

test "simd_cosine_orthogonal" {
    const a = [_]f32{ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    const b = [_]f32{ 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    const sim = simd_cosine(&a, &b);
    try std.testing.expectApproxEqAbs(@as(f32, 0.0), sim, 1e-5);
}

test "distance_matrix_3x3" {
    const dim = 4;
    const vectors = [_]f32{
        1.0, 0.0, 0.0, 0.0, // v0: unit x
        0.0, 1.0, 0.0, 0.0, // v1: unit y
        1.0, 1.0, 0.0, 0.0, // v2: x+y (similar to both)
    };
    var matrix: [9]f32 = undefined;

    compute_distance_matrix(&vectors, 3, dim, &matrix);

    // Diagonal should be 1.0
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), matrix[0], 1e-5); // [0,0]
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), matrix[4], 1e-5); // [1,1]
    try std.testing.expectApproxEqAbs(@as(f32, 1.0), matrix[8], 1e-5); // [2,2]

    // v0 and v1 should be orthogonal (0.0)
    try std.testing.expectApproxEqAbs(@as(f32, 0.0), matrix[1], 1e-5); // [0,1]
    try std.testing.expectApproxEqAbs(@as(f32, 0.0), matrix[3], 1e-5); // [1,0] (mirror)

    // v2 should be similar to both v0 and v1 (cos(45°) ≈ 0.707)
    try std.testing.expectApproxEqAbs(@as(f32, 0.7071), matrix[2], 0.01); // [0,2]
}

test "top_k" {
    const matrix = [_]f32{
        1.0, 0.9, 0.3, 0.1,
        0.9, 1.0, 0.5, 0.2,
        0.3, 0.5, 1.0, 0.8,
        0.1, 0.2, 0.8, 1.0,
    };
    var results: [3]TopKResult = undefined;
    const count = top_k_from_matrix(&matrix, 4, 0, 2, &results);

    try std.testing.expectEqual(@as(usize, 2), count);
    try std.testing.expectEqual(@as(usize, 1), results[0].index); // Most similar: idx 1 (0.9)
    try std.testing.expectEqual(@as(usize, 2), results[1].index); // Second: idx 2 (0.3)
}
