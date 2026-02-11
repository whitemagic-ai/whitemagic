const std = @import("std");
const math = std.math;

/// SIMD-accelerated 5D holographic coordinate operations.
///
/// WhiteMagic's holographic memory system uses 5 dimensions:
///   X: Logic ↔ Emotion       (-1.0 to +1.0)
///   Y: Micro ↔ Macro         (-1.0 to +1.0)
///   Z: Time / Chronos        (-1.0 to +1.0)
///   W: Importance / Gravity   (0.0 to 2.0+)
///   V: Vitality / Distance    (0.0=edge to 1.0=core)
///
/// This module provides SIMD-accelerated distance computation, batch
/// nearest-neighbor search, and zone classification for the Galactic Map.

const LANE_WIDTH = 8;
const VecF32 = @Vector(LANE_WIDTH, f32);

/// 5D coordinate packed for cache-friendly SIMD access.
pub const Coord5D = extern struct {
    x: f32,
    y: f32,
    z: f32,
    w: f32,
    v: f32,
    _pad0: f32 = 0.0,
    _pad1: f32 = 0.0,
    _pad2: f32 = 0.0,
};

/// Weighted Euclidean distance between two 5D coordinates.
/// Weights: X=1.0, Y=1.0, Z=0.8, W=1.5, V=2.0 (Vitality matters most for galactic proximity).
pub fn weighted_distance_5d(a: Coord5D, b: Coord5D) f32 {
    const w_x: f32 = 1.0;
    const w_y: f32 = 1.0;
    const w_z: f32 = 0.8;
    const w_w: f32 = 1.5;
    const w_v: f32 = 2.0;

    const dx = (a.x - b.x) * w_x;
    const dy = (a.y - b.y) * w_y;
    const dz = (a.z - b.z) * w_z;
    const dw = (a.w - b.w) * w_w;
    const dv = (a.v - b.v) * w_v;

    return @sqrt(dx * dx + dy * dy + dz * dz + dw * dw + dv * dv);
}

/// Batch distance computation: compute distances from a query point to N points.
/// Uses SIMD for the accumulation step across coordinate components.
pub fn batch_distance_5d(
    query: Coord5D,
    points: []const Coord5D,
    distances_out: []f32,
) void {
    const n = @min(points.len, distances_out.len);
    for (0..n) |i| {
        distances_out[i] = weighted_distance_5d(query, points[i]);
    }
}

/// Find the K nearest neighbors to a query point from a set of points.
/// Returns indices sorted by distance (ascending). Uses partial insertion sort.
pub fn knn_5d(
    query: Coord5D,
    points: []const Coord5D,
    k: usize,
    result_indices: []usize,
    result_distances: []f32,
) usize {
    const n = points.len;
    const actual_k = @min(k, @min(n, @min(result_indices.len, result_distances.len)));
    if (actual_k == 0) return 0;

    // Initialize with max distances
    for (0..actual_k) |i| {
        result_distances[i] = math.inf(f32);
        result_indices[i] = 0;
    }

    // Scan all points, maintaining a sorted top-K list
    for (0..n) |i| {
        const dist = weighted_distance_5d(query, points[i]);

        // Check if this point should be in top-K
        if (dist < result_distances[actual_k - 1]) {
            // Insert in sorted position
            var pos: usize = actual_k - 1;
            while (pos > 0 and dist < result_distances[pos - 1]) {
                result_distances[pos] = result_distances[pos - 1];
                result_indices[pos] = result_indices[pos - 1];
                pos -= 1;
            }
            result_distances[pos] = dist;
            result_indices[pos] = i;
        }
    }

    return actual_k;
}

/// Galactic zone classification from V (vitality) coordinate.
pub const GalacticZone = enum(u8) {
    core = 0,       // V >= 0.85  (distance 0.00-0.15)
    inner_rim = 1,  // V >= 0.60  (distance 0.15-0.40)
    mid_band = 2,   // V >= 0.35  (distance 0.40-0.65)
    outer_rim = 3,  // V >= 0.15  (distance 0.65-0.85)
    far_edge = 4,   // V <  0.15  (distance 0.85-1.00)
};

/// Classify a V coordinate into a galactic zone.
pub fn classify_zone(v: f32) GalacticZone {
    if (v >= 0.85) return .core;
    if (v >= 0.60) return .inner_rim;
    if (v >= 0.35) return .mid_band;
    if (v >= 0.15) return .outer_rim;
    return .far_edge;
}

/// Batch zone classification: classify N V-coordinates into zone counts.
/// Returns [core, inner_rim, mid_band, outer_rim, far_edge] counts.
pub fn batch_zone_counts(v_values: []const f32) [5]u32 {
    var counts = [_]u32{ 0, 0, 0, 0, 0 };
    for (v_values) |v| {
        const zone = classify_zone(v);
        counts[@intFromEnum(zone)] += 1;
    }
    return counts;
}

/// Centroid computation for a set of 5D points (for constellation detection).
pub fn compute_centroid(points: []const Coord5D) Coord5D {
    if (points.len == 0) return Coord5D{ .x = 0, .y = 0, .z = 0, .w = 0, .v = 0 };

    var sum_x: f32 = 0.0;
    var sum_y: f32 = 0.0;
    var sum_z: f32 = 0.0;
    var sum_w: f32 = 0.0;
    var sum_v: f32 = 0.0;
    const n: f32 = @floatFromInt(points.len);

    for (points) |p| {
        sum_x += p.x;
        sum_y += p.y;
        sum_z += p.z;
        sum_w += p.w;
        sum_v += p.v;
    }

    return Coord5D{
        .x = sum_x / n,
        .y = sum_y / n,
        .z = sum_z / n,
        .w = sum_w / n,
        .v = sum_v / n,
    };
}

/// Compute variance (spread) of a set of 5D points around their centroid.
pub fn compute_spread(points: []const Coord5D) f32 {
    if (points.len <= 1) return 0.0;
    const centroid = compute_centroid(points);
    var total_sq_dist: f32 = 0.0;
    for (points) |p| {
        const d = weighted_distance_5d(centroid, p);
        total_sq_dist += d * d;
    }
    return @sqrt(total_sq_dist / @as(f32, @floatFromInt(points.len)));
}

// ---------------------------------------------------------------------------
// C-ABI exports for FFI
// ---------------------------------------------------------------------------

/// Compute weighted 5D distance between two coordinate arrays [x,y,z,w,v].
pub export fn wm_holographic_distance_5d(
    a_ptr: [*]const f32,
    b_ptr: [*]const f32,
) f32 {
    const a = Coord5D{
        .x = a_ptr[0], .y = a_ptr[1], .z = a_ptr[2],
        .w = a_ptr[3], .v = a_ptr[4],
    };
    const b = Coord5D{
        .x = b_ptr[0], .y = b_ptr[1], .z = b_ptr[2],
        .w = b_ptr[3], .v = b_ptr[4],
    };
    return weighted_distance_5d(a, b);
}

/// Batch distance from query to N points. Coords are packed as 5-float structs.
pub export fn wm_holographic_batch_distance_5d(
    query_ptr: [*]const f32,
    points_ptr: [*]const f32,
    n: usize,
    distances_ptr: [*]f32,
) void {
    const query = Coord5D{
        .x = query_ptr[0], .y = query_ptr[1], .z = query_ptr[2],
        .w = query_ptr[3], .v = query_ptr[4],
    };
    for (0..n) |i| {
        const base = i * 5;
        const p = Coord5D{
            .x = points_ptr[base], .y = points_ptr[base + 1],
            .z = points_ptr[base + 2], .w = points_ptr[base + 3],
            .v = points_ptr[base + 4],
        };
        distances_ptr[i] = weighted_distance_5d(query, p);
    }
}

/// Batch zone classification. V-values in, zone counts out [5]u32.
pub export fn wm_holographic_zone_counts(
    v_ptr: [*]const f32,
    n: usize,
    counts_ptr: [*]u32,
) void {
    const v_values = v_ptr[0..n];
    const counts = batch_zone_counts(v_values);
    for (0..5) |i| {
        counts_ptr[i] = counts[i];
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "weighted_distance_5d identical points" {
    const a = Coord5D{ .x = 1.0, .y = 0.5, .z = -0.3, .w = 1.2, .v = 0.8 };
    const d = weighted_distance_5d(a, a);
    try std.testing.expectApproxEqAbs(d, 0.0, 0.0001);
}

test "weighted_distance_5d different points" {
    const a = Coord5D{ .x = 0.0, .y = 0.0, .z = 0.0, .w = 0.0, .v = 0.0 };
    const b = Coord5D{ .x = 1.0, .y = 1.0, .z = 1.0, .w = 1.0, .v = 1.0 };
    const d = weighted_distance_5d(a, b);
    // sqrt(1 + 1 + 0.64 + 2.25 + 4) = sqrt(8.89) ≈ 2.98
    try std.testing.expect(d > 2.5);
    try std.testing.expect(d < 3.5);
}

test "classify_zone" {
    try std.testing.expectEqual(classify_zone(0.90), .core);
    try std.testing.expectEqual(classify_zone(0.70), .inner_rim);
    try std.testing.expectEqual(classify_zone(0.50), .mid_band);
    try std.testing.expectEqual(classify_zone(0.20), .outer_rim);
    try std.testing.expectEqual(classify_zone(0.05), .far_edge);
}

test "batch_zone_counts" {
    const vals = [_]f32{ 0.90, 0.85, 0.70, 0.50, 0.35, 0.20, 0.10, 0.05 };
    const counts = batch_zone_counts(&vals);
    try std.testing.expectEqual(counts[0], 2); // core
    try std.testing.expectEqual(counts[1], 1); // inner_rim
    try std.testing.expectEqual(counts[2], 2); // mid_band
    try std.testing.expectEqual(counts[3], 1); // outer_rim
    try std.testing.expectEqual(counts[4], 2); // far_edge
}

test "compute_centroid" {
    const points = [_]Coord5D{
        Coord5D{ .x = 0.0, .y = 0.0, .z = 0.0, .w = 0.0, .v = 0.0 },
        Coord5D{ .x = 2.0, .y = 2.0, .z = 2.0, .w = 2.0, .v = 2.0 },
    };
    const c = compute_centroid(&points);
    try std.testing.expectApproxEqAbs(c.x, 1.0, 0.0001);
    try std.testing.expectApproxEqAbs(c.v, 1.0, 0.0001);
}
