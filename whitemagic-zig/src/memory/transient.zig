const std = @import("std");

/// TransientThoughtAllocator: A fixed-buffer allocator for "transient thoughts"
/// Designed to be called from Rust for extremely fast, zero-GC, zero-jitter allocation
/// of temporary data during consolidation and reasoning loops.
pub const TransientThoughtAllocator = struct {
    arena: std.heap.ArenaAllocator,
    backing_allocator: std.mem.Allocator,
    
    pub fn init(backing: std.mem.Allocator) TransientThoughtAllocator {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing),
            .backing_allocator = backing,
        };
    }

    pub fn deinit(self: *TransientThoughtAllocator) void {
        self.arena.deinit();
    }

    pub fn alloc(self: *TransientThoughtAllocator, size: usize) ![]u8 {
        return self.arena.allocator().alloc(u8, size);
    }

    pub fn reset(self: *TransientThoughtAllocator) void {
        _ = self.arena.reset(.retain_capacity);
    }
};

// C-compatible API for Rust FFI
var global_allocator: ?TransientThoughtAllocator = null;
var global_mutex: std.Thread.Mutex = .{};

pub fn zig_init_transient_allocator() void {
    global_mutex.lock();
    defer global_mutex.unlock();
    if (global_allocator == null) {
        global_allocator = TransientThoughtAllocator.init(std.heap.c_allocator);
    }
}

pub fn zig_transient_alloc(size: usize) ?[*]u8 {
    global_mutex.lock();
    defer global_mutex.unlock();
    if (global_allocator) |*allocator| {
        const slice = allocator.alloc(size) catch return null;
        return slice.ptr;
    }
    return null;
}

pub fn zig_transient_reset() void {
    global_mutex.lock();
    defer global_mutex.unlock();
    if (global_allocator) |*allocator| {
        allocator.reset();
    }
}

pub fn zig_deinit_transient_allocator() void {
    global_mutex.lock();
    defer global_mutex.unlock();
    if (global_allocator) |*allocator| {
        allocator.deinit();
        global_allocator = null;
    }
}
