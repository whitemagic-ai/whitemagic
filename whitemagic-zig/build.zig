const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create the root module
    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Define sub-modules
    const unified_mod = b.createModule(.{
        .root_source_file = b.path("src/memory/unified.zig"),
        .target = target,
        .optimize = optimize,
    });

    const holographic_mod = b.createModule(.{
        .root_source_file = b.path("src/compute/holographic.zig"),
        .target = target,
        .optimize = optimize,
    });

    const transient_mod = b.createModule(.{
        .root_source_file = b.path("src/memory/transient.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add imports to the root module (if used as named modules)
    lib_mod.addImport("unified_memory", unified_mod);
    lib_mod.addImport("holographic_allocator", holographic_mod);
    lib_mod.addImport("transient_allocator", transient_mod);

    // Create the static library (for Rust)
    const lib_static = b.addLibrary(.{
        .name = "whitemagic-zig",
        .root_module = lib_mod,
        .linkage = .static,
    });
    lib_static.linkLibC();
    b.installArtifact(lib_static);

    // Create the shared library (for Mojo)
    const lib_shared = b.addLibrary(.{
        .name = "whitemagic",
        .root_module = lib_mod,
        .linkage = .dynamic,
    });
    lib_shared.linkLibC();
    b.installArtifact(lib_shared);
}
