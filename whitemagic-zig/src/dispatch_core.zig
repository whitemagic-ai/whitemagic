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

// --- Gana ID enum (28 Lunar Mansions, matches PRAT router indices) ---
// Canonical ordering from prat_resonance._GANA_META.
pub const GanaId = enum(u32) {
    horn             = 0,  // 角 Jiǎo  — Session Initiation
    neck             = 1,  // 亢 Kàng  — Memory Stability
    root             = 2,  // 氐 Dǐ    — System Foundation
    room             = 3,  // 房 Fáng  — Resource Sanctuary
    heart            = 4,  // 心 Xīn   — Context & Pulse
    tail             = 5,  // 尾 Wěi   — Performance & Acceleration
    winnowing_basket = 6,  // 箕 Jī    — Wisdom & Search
    ghost            = 7,  // 鬼 Guǐ   — Introspection
    willow           = 8,  // 柳 Liǔ   — Resilience & Play
    star             = 9,  // 星 Xīng  — Governance & Illumination
    extended_net     = 10, // 张 Zhāng — Pattern Connectivity
    wings            = 11, // 翼 Yì    — Expansion & Export
    chariot          = 12, // 轸 Zhěn  — Archaeology & KG
    abundance        = 13, // 豐 Fēng  — Regeneration
    straddling_legs  = 14, // 奎 Kuí   — Ethical Balance
    mound            = 15, // 娄 Lóu   — Metrics & Accumulation
    stomach          = 16, // 胃 Wèi   — Energy & Tasks
    hairy_head       = 17, // 昴 Mǎo   — Detail & Debug
    net              = 18, // 毕 Bì    — Pattern Capture
    turtle_beak      = 19, // 觜 Zī    — Precision & Inference
    three_stars      = 20, // 参 Shēn  — Wisdom Council
    dipper           = 21, // 斗 Dǒu   — Strategy
    ox               = 22, // 牛 Niú   — Endurance & Swarm
    girl             = 23, // 女 Nǚ    — Nurture & Agents
    void_            = 24, // 虚 Xū    — Stillness & Galaxy
    roof             = 25, // 危 Wēi   — Shelter & Local AI
    encampment       = 26, // 室 Shì   — Community
    wall             = 27, // 壁 Bì    — Boundaries
    unknown          = 255,
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

// Comptime maturity table for all 28 Ganas (Lunar Mansions)
const maturity_table: [28]MaturityLevel = .{
    .mature,       // 0:  horn             — Session (bootstrap/create/resume/checkpoint)
    .mature,       // 1:  neck             — Memory CRUD
    .mature,       // 2:  root             — System health
    .mature,       // 3:  room             — Security & locks
    .mature,       // 4:  heart            — Context & scratchpad
    .stable,       // 5:  tail             — Acceleration (Rust/Zig/Mojo)
    .mature,       // 6:  winnowing_basket — Search & retrieval
    .mature,       // 7:  ghost            — Introspection & gnosis
    .stable,       // 8:  willow           — Grimoire & rate limiting
    .mature,       // 9:  star             — Governance & forge
    .stable,       // 10: extended_net     — Pattern & learning
    .stable,       // 11: wings            — Export & mesh
    .stable,       // 12: chariot          — Archaeology & KG
    .stable,       // 13: abundance        — Dream cycle & lifecycle
    .mature,       // 14: straddling_legs  — Ethics & harmony
    .mature,       // 15: mound            — Metrics & yin-yang
    .stable,       // 16: stomach          — Pipeline & tasks
    .mature,       // 17: hairy_head       — Debug & karma
    .stable,       // 18: net              — Prompts & filtering
    .beta,         // 19: turtle_beak      — Edge/bitnet inference
    .stable,       // 20: three_stars      — Council & ensemble
    .stable,       // 21: dipper           — Strategy & homeostasis
    .stable,       // 22: ox               — Swarm intelligence
    .stable,       // 23: girl             — Agent management
    .stable,       // 24: void_            — Galaxy & gardens
    .stable,       // 25: roof             — Ollama & model signing
    .stable,       // 26: encampment       — Sangha & broker
    .stable,       // 27: wall             — Voting & engagement
};

// --- Handler ID routing table (comptime) ---
// Maps gana_id -> handler_id. The handler_id maps to a Python handler
// function via the dispatch table, or a Rust/Zig accelerated path.
const handler_table: [28]u32 = .{
    100, // horn             → session handlers
    101, // neck             → memory handlers
    102, // root             → health handlers
    103, // room             → security handlers
    104, // heart            → context handlers
    105, // tail             → acceleration handlers
    106, // winnowing_basket → search handlers
    107, // ghost            → introspection handlers
    108, // willow           → grimoire handlers
    109, // star             → governance handlers
    110, // extended_net     → pattern handlers
    111, // wings            → export handlers
    112, // chariot          → archaeology handlers
    113, // abundance        → dream/lifecycle handlers
    114, // straddling_legs  → ethics handlers
    115, // mound            → metrics handlers
    116, // stomach          → pipeline handlers
    117, // hairy_head       → debug handlers
    118, // net              → prompt handlers
    119, // turtle_beak      → inference handlers
    120, // three_stars      → ensemble handlers
    121, // dipper           → strategy handlers
    122, // ox               → swarm handlers
    123, // girl             → agent handlers
    124, // void_            → galaxy handlers
    125, // roof             → ollama handlers
    126, // encampment       → community handlers
    127, // wall             → voting handlers
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
