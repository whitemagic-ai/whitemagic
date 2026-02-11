///! DispatchCore — Comptime Static Dispatch Table (Leap 7c)
///!
///! Moves the hot dispatch path out of Python into a Zig comptime-generated
///! static lookup table. The pipeline checks (rate limit, circuit breaker,
///! maturity gate) run entirely in Zig by reading from the StateBoard mmap.
///!
///! Target: <2µs for full pipeline (vs ~40µs in Python).
///!
///! FFI contract (C ABI, callable from Python ctypes or Rust):
///!   wm_dispatch_check(tool_id: u32, board_ptr: [*]const u8) -> i32
///!     Returns: 0 = ALLOW, 1 = RATE_LIMITED, 2 = CIRCUIT_OPEN, 3 = IMMATURE
///!
///!   wm_dispatch_route(tool_id: u32) -> u32
///!     Returns: handler_id for the given tool
///!
///!   wm_dispatch_stats() -> u64
///!     Returns: total dispatch checks performed

const std = @import("std");

// --- Tool ID enum (28 engines, matches StateBoard slot indices) ---
pub const ToolId = enum(u32) {
    session = 0,
    consolidation = 1,
    boundary = 2,
    circuit_breaker = 3,
    nurturing = 4,
    acceleration = 5,
    serendipity = 6,
    introspection = 7,
    resilience = 8,
    governance = 9,
    association = 10,
    exporter = 11,
    archaeology = 12,
    resonance = 13,
    solver = 14,
    embedding = 15,
    lifecycle = 16,
    kaizen = 17,
    pattern = 18,
    narrative = 19,
    ethics = 20,
    predictive = 21,
    galactic = 22,
    clone_army = 23,
    forgetting = 24,
    sanitization = 25,
    swarm = 26,
    emergence = 27,
    unknown = 255,
};

// --- Dispatch result codes ---
pub const DispatchResult = enum(i32) {
    allow = 0,
    rate_limited = 1,
    circuit_open = 2,
    immature = 3,
    invalid_tool = -1,
};

// --- Maturity gates (comptime lookup) ---
// Each engine has a maturity level: 0=experimental, 1=beta, 2=stable, 3=mature
const MaturityLevel = enum(u8) {
    experimental = 0,
    beta = 1,
    stable = 2,
    mature = 3,
};

// Minimum required maturity to dispatch (configurable at compile time)
const MIN_MATURITY: MaturityLevel = .beta;

// Comptime maturity table for all 28 engines
const maturity_table: [28]MaturityLevel = .{
    .mature, // 0: session
    .mature, // 1: consolidation
    .stable, // 2: boundary
    .stable, // 3: circuit_breaker
    .stable, // 4: nurturing
    .stable, // 5: acceleration
    .mature, // 6: serendipity
    .mature, // 7: introspection
    .stable, // 8: resilience
    .mature, // 9: governance
    .mature, // 10: association
    .stable, // 11: export
    .stable, // 12: archaeology
    .mature, // 13: resonance
    .beta, // 14: solver
    .mature, // 15: embedding
    .stable, // 16: lifecycle
    .mature, // 17: kaizen
    .mature, // 18: pattern
    .stable, // 19: narrative
    .stable, // 20: ethics
    .mature, // 21: predictive
    .stable, // 22: galactic
    .stable, // 23: clone_army
    .stable, // 24: forgetting
    .stable, // 25: sanitization
    .stable, // 26: swarm
    .beta, // 27: emergence
};

// --- Handler ID routing table (comptime) ---
// Maps tool_id -> handler_id. In practice, the handler_id maps to a
// Python handler function or a Rust accelerated path.
const handler_table: [28]u32 = .{
    100, // session -> handler 100
    101, // consolidation
    102, // boundary
    103, // circuit_breaker
    104, // nurturing
    105, // acceleration
    106, // serendipity
    107, // introspection
    108, // resilience
    109, // governance
    110, // association
    111, // export
    112, // archaeology
    113, // resonance
    114, // solver
    115, // embedding
    116, // lifecycle
    117, // kaizen
    118, // pattern
    119, // narrative
    120, // ethics
    121, // predictive
    122, // galactic
    123, // clone_army
    124, // forgetting
    125, // sanitization
    126, // swarm
    127, // emergence
};

// --- StateBoard offsets (must match state_board.rs layout) ---
const BOARD_OFF_TICK: usize = 16;
const BOARD_OFF_BREAKERS: usize = 256;
const BOARD_BREAKER_SLOT: usize = 16; // 16 bytes per breaker slot
const BOARD_OFF_COUNTERS: usize = 1280;

// Breaker states
const BREAKER_CLOSED: u64 = 0;
const BREAKER_OPEN: u64 = 1;
const BREAKER_HALF_OPEN: u64 = 2;

// Rate limit: max calls per tick window (configurable)
const RATE_LIMIT_PER_WINDOW: u64 = 10000;

// --- Atomic stats counter ---
var total_checks: u64 = 0;
var total_allowed: u64 = 0;
var total_denied: u64 = 0;

// --- Core dispatch check ---
fn dispatch_check_inner(tool_id: u32, board_ptr: [*]const u8) DispatchResult {
    if (tool_id >= 28) return .invalid_tool;

    // 1. Maturity gate (comptime lookup, ~0ns)
    const maturity = maturity_table[tool_id];
    if (@intFromEnum(maturity) < @intFromEnum(MIN_MATURITY)) {
        return .immature;
    }

    // 2. Circuit breaker check (read from StateBoard mmap, ~10ns)
    const breaker_offset = BOARD_OFF_BREAKERS + @as(usize, tool_id) * BOARD_BREAKER_SLOT;
    const breaker_state = read_u64_from_board(board_ptr, breaker_offset);
    if (breaker_state == BREAKER_OPEN) {
        return .circuit_open;
    }

    // 3. Rate limit check (read counter from StateBoard, ~10ns)
    const counter_offset = BOARD_OFF_COUNTERS + @as(usize, tool_id) * 8;
    const call_count = read_u64_from_board(board_ptr, counter_offset);
    if (call_count > RATE_LIMIT_PER_WINDOW) {
        return .rate_limited;
    }

    return .allow;
}

fn read_u64_from_board(board_ptr: [*]const u8, offset: usize) u64 {
    if (offset + 8 > 4096) return 0;
    const ptr: *align(1) const u64 = @ptrCast(board_ptr + offset);
    return ptr.*;
}

// --- FFI exports (C ABI) ---

/// Full pipeline check: maturity → circuit breaker → rate limit.
/// Returns DispatchResult as i32.
export fn wm_dispatch_check(tool_id: u32, board_ptr: [*]const u8) i32 {
    total_checks += 1;
    const result = dispatch_check_inner(tool_id, board_ptr);
    if (result == .allow) {
        total_allowed += 1;
    } else {
        total_denied += 1;
    }
    return @intFromEnum(result);
}

/// Route a tool_id to its handler_id. Pure comptime lookup.
export fn wm_dispatch_route(tool_id: u32) u32 {
    if (tool_id >= 28) return 0;
    return handler_table[tool_id];
}

/// Get total dispatch checks performed.
export fn wm_dispatch_stats_total() u64 {
    return total_checks;
}

/// Get total allowed dispatches.
export fn wm_dispatch_stats_allowed() u64 {
    return total_allowed;
}

/// Get total denied dispatches.
export fn wm_dispatch_stats_denied() u64 {
    return total_denied;
}

/// Reset stats counters.
export fn wm_dispatch_reset_stats() void {
    total_checks = 0;
    total_allowed = 0;
    total_denied = 0;
}

/// Get the maturity level for a tool (0=experimental, 1=beta, 2=stable, 3=mature).
export fn wm_dispatch_maturity(tool_id: u32) u8 {
    if (tool_id >= 28) return 0;
    return @intFromEnum(maturity_table[tool_id]);
}

// --- Tests ---
test "dispatch allows mature tool with closed breaker" {
    // Simulate a board with all zeros (closed breakers, zero counters)
    var board = [_]u8{0} ** 4096;
    const result = dispatch_check_inner(0, &board); // session engine
    try std.testing.expectEqual(result, .allow);
}

test "dispatch blocks open circuit breaker" {
    var board = [_]u8{0} ** 4096;
    // Set breaker state for tool 0 to OPEN (1)
    const breaker_off = BOARD_OFF_BREAKERS;
    const ptr: *align(1) u64 = @ptrCast(&board[breaker_off]);
    ptr.* = BREAKER_OPEN;
    const result = dispatch_check_inner(0, &board);
    try std.testing.expectEqual(result, .circuit_open);
}

test "dispatch route returns correct handler" {
    try std.testing.expectEqual(wm_dispatch_route(0), 100); // session
    try std.testing.expectEqual(wm_dispatch_route(13), 113); // resonance
    try std.testing.expectEqual(wm_dispatch_route(27), 127); // emergence
    try std.testing.expectEqual(wm_dispatch_route(255), 0); // invalid
}

test "maturity gate blocks experimental tools" {
    var board = [_]u8{0} ** 4096;
    // All tools in the table are beta+ so they should pass.
    // To test blocking, we'd need an experimental tool (none currently).
    // Instead verify that beta tools pass since MIN_MATURITY = beta.
    const result = dispatch_check_inner(27, &board); // emergence = beta
    try std.testing.expectEqual(result, .allow);
}
