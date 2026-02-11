const std = @import("std");

pub const ArenaManager = struct {
    arena: std.heap.ArenaAllocator,
    
    pub fn init(child_allocator: std.mem.Allocator) ArenaManager {
        return .{
            .arena = std.heap.ArenaAllocator.init(child_allocator),
        };
    }
    
    pub fn deinit(self: *ArenaManager) void {
        self.arena.deinit();
    }
    
    pub fn allocator(self: *ArenaManager) std.mem.Allocator {
        return self.arena.allocator();
    }
};
