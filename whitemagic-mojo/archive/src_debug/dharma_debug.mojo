
from sys.ffi import OwnedDLHandle

fn main() raises:
    print("--- Dharma FFI Debug Start ---")
    # Run from repo root (and build the Dharma core first).
    var dharma_path = "dharma/target/release/libdharmacore.so"
    print("Loading Dharma lib from:", dharma_path)
    var dharma_lib = OwnedDLHandle(dharma_path)
    print("Dharma lib loaded.")
    
    # Try a simple call
    # verify_action_ffi(imp: f64, res: f64, harm: f64) -> i32
    print("Calling verify_action_ffi...")
    var res = dharma_lib.call["verify_action_ffi", Int32, Float64, Float64, Float64](0.5, 0.5, 0.5)
    print("Dharma call result:", res)
    
    print("--- Dharma FFI Debug Complete ---")
