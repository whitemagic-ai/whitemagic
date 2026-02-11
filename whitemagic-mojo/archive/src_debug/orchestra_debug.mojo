
from sys.ffi import OwnedDLHandle

fn main() raises:
    print("--- FFI Debug Start ---")
    # Run from repo root (or set WHITEMAGIC_ROOT and build the Zig lib first).
    var zig_path = "whitemagic-zig/zig-out/lib/libwhitemagic.so"
    print("Loading Zig lib from:", zig_path)
    var zig_lib = OwnedDLHandle(zig_path)
    print("Zig lib loaded.")
    
    print("Calling wm_memory_init...")
    zig_lib.call["wm_memory_init", NoneType]()
    print("Zig memory initialized.")
    
    print("--- FFI Debug Complete ---")
