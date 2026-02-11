use std::env;
use std::path::PathBuf;

fn main() {
    let project_root = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    // Path to the Zig static library
    // In v5.1, we expect whitemagic-zig to be a sibling of the whitemagic repo
    // or at a specific relative path.
    // Correct path: project_root is "whitemagic-rust"
    // We want sibling "whitemagic-zig" in the parent "whitemagic" folder
    let zig_lib_dir = project_root
        .parent()
        .unwrap()
        .join("whitemagic-zig")
        .join("zig-out")
        .join("lib");

    // Fallback: Check if the library is in the root of whitemagic-zig (common in dev/pixi builds)
    let zig_lib_root = project_root.parent().unwrap().join("whitemagic-zig");

    let final_zig_dir = if zig_lib_dir.exists() {
        Some(zig_lib_dir.clone())
    } else if zig_lib_root.join("libwhitemagic.so").exists() {
        Some(zig_lib_root.clone())
    } else {
        None
    };

    if let Some(lib_dir) = final_zig_dir {
        println!("cargo:rustc-link-search=native={}", lib_dir.display());
        // Link DYNAMICALLY to the shared Zig core to allow state sharing with Mojo/Python
        // The shared library is named "libwhitemagic.so" -> link "whitemagic"
        println!("cargo:rustc-link-lib=dylib=whitemagic");

        // Add RPATH so the extension can find the Zig library at runtime without LD_LIBRARY_PATH
        // We use $ORIGIN relative paths assuming specific deployment, or absolute path for dev
        // For dev (Ghost in the Machine test), absolute path is safest.
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib_dir.display());

        // We also need to link libc because Zig's C allocator depends on it
        println!("cargo:rustc-link-lib=dylib=c");
    } else {
        // Fallback or warning if Zig library is not built yet
        // This allows the build to proceed (without Zig acceleration) if needed
        println!("cargo:warning=Zig static library not found at {} or {}. Proceeding without Zig acceleration.", zig_lib_dir.display(), zig_lib_root.display());
    }

    // Rerun if build script changes or if the Zig library changes
    println!("cargo:rerun-if-changed=build.rs");
}
