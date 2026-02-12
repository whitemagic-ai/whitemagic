const std = @import("std");

var global_rng: ?std.Random.DefaultPrng = null;

pub const Hexagram = struct {
    number: u8,
    name: []const u8,
    lines: [6]u1, // 0 for broken (yin), 1 for solid (yang)

    pub fn init(number: u8, name: []const u8, lines: [6]u1) Hexagram {
        return Hexagram{
            .number = number,
            .name = name,
            .lines = lines,
        };
    }

    pub fn is_yang(self: Hexagram, line_index: usize) bool {
        return self.lines[line_index] == 1;
    }
};

pub const IChingEngine = struct {
    rng: *std.Random.DefaultPrng,

    pub fn init() IChingEngine {
        if (global_rng == null) {
            global_rng = std.Random.DefaultPrng.init(@intCast(@as(i128, std.time.timestamp())));
        }
        return IChingEngine{
            .rng = &global_rng.?,
        };
    }

    pub fn cast_hexagram(self: *IChingEngine) [6]u8 {
        var lines: [6]u8 = undefined;
        var i: usize = 0;
        const random = self.rng.random();
        while (i < 6) : (i += 1) {
            var sum: u8 = 0;
            var j: usize = 0;
            while (j < 3) : (j += 1) {
                sum += if (random.boolean()) @as(u8, 3) else @as(u8, 2);
            }
            lines[i] = sum;
        }
        return lines;
    }
};

pub fn main() !void {
    var engine = IChingEngine.init();
    const cast = engine.cast_hexagram();
    
    const stdout = std.fs.File.stdout();
    const writer = stdout.deprecatedWriter();
    try writer.print("I Ching Cast:\n", .{});
    for (cast, 0..) |line, i| {
        try writer.print("Line {d}: {d}\n", .{i + 1, line});
    }
}
