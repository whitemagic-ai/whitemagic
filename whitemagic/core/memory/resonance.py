
import ctypes
from pathlib import Path

# Path to the shared library
LIB_PATH = Path("PROJECT_ROOT/staging/core_system/whitemagic-zig/zig-out/lib/libwhitemagic.so")

class ResonanceBridge:
    def __init__(self) -> None:
        self.lib: ctypes.CDLL | None = None
        self._load_lib()

    def _load_lib(self) -> None:
        if not LIB_PATH.exists():
            print(f"❌ Shared library not found at {LIB_PATH}")
            return

        try:
            lib = ctypes.CDLL(str(LIB_PATH))

            # wm_memory_dump_stats_json(path_ptr: [*]const u8, path_len: usize) bool
            lib.wm_memory_dump_stats_json.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
            lib.wm_memory_dump_stats_json.restype = ctypes.c_bool

            # Initialize Global Manager
            lib.wm_memory_init()
            self.lib = lib

            print("✅ Resonance Bridge loaded Zig Hypercore.")
        except Exception as e:
            print(f"❌ Failed to load Zig library: {e}")

    def dump_memory_state(self, output_path: str) -> bool:
        if not self.lib:
            return False

        try:
            path_bytes = output_path.encode("utf-8")
            success = self.lib.wm_memory_dump_stats_json(path_bytes, len(path_bytes))
            if success:
                print(f"✨ Resonance State dumped to {output_path}")
                return True
            else:
                print("⚠️ Failed to dump state (Zig return false)")
                return False
        except Exception as e:
            print(f"❌ Error dumping memory state: {e}")
            return False

if __name__ == "__main__":
    # Test run
    bridge = ResonanceBridge()
    bridge.dump_memory_state("resonance_state.json")
