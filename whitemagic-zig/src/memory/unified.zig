const std = @import("std");
const Alignment = std.mem.Alignment;

// 16MB Managed Heap for True Compaction
const HEAP_CAPACITY = 16 * 1024 * 1024;

pub const MemoryBlock = struct {
    offset: usize,
    size: usize,
    alignment: u32,
};

// Helper struct for sorting to avoid anonymous struct issues in generic calls
const BlockEntry = struct {
    id: u64,
    blk: MemoryBlock,
};

pub const UnifiedMemoryManager = struct {
    allocator: std.mem.Allocator,
    heap: []u8,
    used_mark: usize,
    
    blocks: std.AutoHashMap(u64, MemoryBlock),
    mutex: std.Thread.Mutex,

    pub fn init(allocator: std.mem.Allocator) !UnifiedMemoryManager {
        const heap_mem = try allocator.alloc(u8, HEAP_CAPACITY);
        // Zero initialization for cleanliness
        @memset(heap_mem, 0);
        
        return .{
            .allocator = allocator,
            .heap = heap_mem,
            .used_mark = 0,
            .blocks = std.AutoHashMap(u64, MemoryBlock).init(allocator),
            .mutex = .{},
        };
    }

    pub fn deinit(self: *UnifiedMemoryManager) void {
        self.blocks.deinit();
        self.allocator.free(self.heap);
    }

    pub fn allocate(self: *UnifiedMemoryManager, id: u64, size: usize, alignment: Alignment) ![*]u8 {
        self.mutex.lock();
        defer self.mutex.unlock();

        // If ID exists, we preserve it (realloc semantics) or overwrite.
        var old_block: ?MemoryBlock = null;
        if (self.blocks.get(id)) |b| {
            old_block = b;
        }

        const align_bytes = alignment.toByteUnits();
        
        // Calculate potential offset at end of used space
        var current_ptr = @intFromPtr(self.heap.ptr) + self.used_mark;
        var aligned_ptr = std.mem.alignForward(usize, current_ptr, align_bytes);
        var padding = aligned_ptr - current_ptr;
        var offset = self.used_mark + padding;

        // Check if we fit
        if (offset + size > self.heap.len) {
            // Not enough space at tail. Try Compaction!
            try self.compact_internal();
            
            // Recalculate after compaction
            current_ptr = @intFromPtr(self.heap.ptr) + self.used_mark;
            aligned_ptr = std.mem.alignForward(usize, current_ptr, align_bytes);
            padding = aligned_ptr - current_ptr;
            offset = self.used_mark + padding;
            
            if (offset + size > self.heap.len) {
                return error.OutOfMemory;
            }
        }

        // Copy old data if this was a realloc
        if (old_block) |_| {
            // We need to fetch updated block info if compaction ran
            if (self.blocks.get(id)) |updated_ob| {
                 const copy_len = @min(updated_ob.size, size);
                 const dest_slice = self.heap[offset .. offset + copy_len];
                 const src_slice = self.heap[updated_ob.offset .. updated_ob.offset + copy_len];
                 std.mem.copyBackwards(u8, dest_slice, src_slice);
            }
        }

        // Update Registry
        try self.blocks.put(id, .{ .offset = offset, .size = size, .alignment = @as(u32, @intCast(align_bytes)) });
        self.used_mark = offset + size;

        return self.heap.ptr + offset;
    }

    pub fn free(self: *UnifiedMemoryManager, id: u64) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        _ = self.blocks.remove(id);
    }

    // True Memory Compaction: Slides all live blocks to the beginning of the heap
    fn compact_internal(self: *UnifiedMemoryManager) !void {
        // 1. Collect all blocks
        // Use std.ArrayList(BlockEntry) which is unmanaged (equivalent to ArrayListUnmanaged in other versions)
        var list = try std.ArrayList(BlockEntry).initCapacity(self.allocator, self.blocks.count());
        defer list.deinit(self.allocator);
        
        var it = self.blocks.iterator();
        while (it.next()) |entry| {
            try list.append(self.allocator, .{ .id = entry.key_ptr.*, .blk = entry.value_ptr.* });
        }
        
        // 2. Sort by offset to ensure we slide left-to-right
        const Sort = struct {
            pub fn lessThan(context: void, a: BlockEntry, b: BlockEntry) bool {
                _ = context;
                return a.blk.offset < b.blk.offset;
            }
        };
        // Use std.sort.block (available in this Zig version)
        std.sort.block(BlockEntry, list.items, {}, Sort.lessThan);
        
        // 3. Compact (Slide Left)
        var write_mark: usize = 0;
        
        for (list.items) |item| {
            const current_ptr = @intFromPtr(self.heap.ptr) + write_mark;
            const align_bytes = item.blk.alignment;
            const aligned_ptr = std.mem.alignForward(usize, current_ptr, align_bytes);
            const padding = aligned_ptr - current_ptr;
            const target_offset = write_mark + padding;
            
            if (target_offset != item.blk.offset) {
                 const dest_slice = self.heap[target_offset .. target_offset + item.blk.size];
                 const src_slice = self.heap[item.blk.offset .. item.blk.offset + item.blk.size];
                 std.mem.copyForwards(u8, dest_slice, src_slice);
                 
                 // Update HashMap with new location
                 if (self.blocks.getPtr(item.id)) |b_ptr| {
                     b_ptr.offset = target_offset;
                 }
            }
            
            write_mark = target_offset + item.blk.size;
        }
        
        self.used_mark = write_mark;
        @memset(self.heap[self.used_mark..], 0);
    }

    pub fn rearrange(self: *UnifiedMemoryManager) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.compact_internal();
    }
    
    // Resolve Handle to Pointer (Pointer Safety Mechanism)
    pub fn get_ptr(self: *UnifiedMemoryManager, id: u64) ?[*]u8 {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.blocks.get(id)) |b| {
            return self.heap.ptr + b.offset;
        }
        return null;
    }
    
    pub fn get_block_count(self: *UnifiedMemoryManager) usize {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.blocks.count();
    }

    pub fn dump_stats_json(self: *UnifiedMemoryManager, file_path: []const u8) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        
        const file = try std.fs.cwd().createFile(file_path, .{});
        defer file.close();
        
        var write_buffer: [4096]u8 = undefined;
        // Format the header
        var slice = try std.fmt.bufPrint(&write_buffer, "{{\"used\": {}, \"heap_size\": {}, \"timestamp\": {}, \"blocks\": [", .{self.used_mark, self.heap.len, std.time.milliTimestamp()});
        try file.writeAll(slice);
        
        var it = self.blocks.iterator();
        var first = true;
        while (it.next()) |entry| {
            if (!first) try file.writeAll(", ");
            // Format entry
            slice = try std.fmt.bufPrint(&write_buffer, "{{\"id\": {}, \"offset\": {}, \"size\": {}, \"alignment\": {}}}", .{
                entry.key_ptr.*, 
                entry.value_ptr.offset, 
                entry.value_ptr.size,
                entry.value_ptr.alignment
            });
            try file.writeAll(slice);
            first = false;
        }
        
        try file.writeAll("]}");
    }
};

// Global manager for FFI access
var global_manager: ?UnifiedMemoryManager = null;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub fn wm_memory_init() void {
    if (global_manager == null) {
        global_manager = UnifiedMemoryManager.init(gpa.allocator()) catch return; 
    }
}

pub fn wm_memory_deinit() void {
    if (global_manager) |*m| {
        m.deinit();
        global_manager = null;
    }
}

pub fn wm_memory_alloc(id: u64, size: usize, alignment_val: u32) ?[*]u8 {
    if (id == 0) return null;
    if (size == 0) return null;
    if (global_manager) |*m| {
        if (!std.math.isPowerOfTwo(alignment_val)) {
            return null;
        }
        const alignment = Alignment.fromByteUnits(alignment_val);
        return m.allocate(id, size, alignment) catch null;
    }
    return null;
}

pub fn wm_memory_free(id: u64) void {
    if (id == 0) return;
    if (global_manager) |*m| {
        // remove() returns true if key was in the map
        _ = m.blocks.remove(id);
    }
}

pub fn wm_memory_rearrange() bool {
    if (global_manager) |*m| {
        m.rearrange() catch return false;
        return true;
    }
    return false;
}

pub fn wm_memory_get_block_count() usize {
    if (global_manager) |*m| {
        return m.get_block_count();
    }
    return 0;
}

pub fn wm_memory_resolve_handle(id: u64) ?[*]u8 {
    if (global_manager) |*m| {
        return m.get_ptr(id);
    }
    return null;
}

pub fn wm_memory_dump_stats_json(path_ptr: [*]const u8, path_len: usize) bool {
    if (global_manager) |*m| {
        const path = path_ptr[0..path_len];
        m.dump_stats_json(path) catch return false;
        return true;
    }
    return false;
}
