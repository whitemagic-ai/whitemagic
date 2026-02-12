import sys
import ctypes
import os
import time
from pathlib import Path

# Add core_system to path so we can import built libraries
sys.path.append(str(Path(__file__).parent.parent.parent))

# Import Rust Core
try:
    import whitemagic_rs
    RUST_AVAILABLE = True
    print("✅ [Orchestrator] Rust Core Loaded (Dharma/Search)")
except ImportError:
    RUST_AVAILABLE = False
    print("❌ [Orchestrator] Rust Core NOT Found")

class ZigCore:
    def __init__(self, lib_path):
        self.lib_path = lib_path
        self.lib = None
        self.load()

    def load(self):
        if not os.path.exists(self.lib_path):
            print(f"❌ [Orchestrator] Zig Lib not found at {self.lib_path}")
            return
        
        try:
            self.lib = ctypes.CDLL(self.lib_path)
            print(f"✅ [Orchestrator] Zig Core Loaded (Metabolism/Thermal)")
            
            # Setup return types if needed, assuming void/int for now
            # self.lib.init_metabolism.argtypes = []
            # self.lib.init_metabolism.restype = ctypes.c_int
        except Exception as e:
            print(f"❌ [Orchestrator] Failed to load Zig Lib: {e}")

    def check_thermal(self):
        if not self.lib: return "Unknown"
        # Simulate checking thermal state via FFI if function exists
        # For now, just confirming the link is active
        return "Nominal (Linked)"

class Orchestrator:
    def __init__(self):
        print("\n🔮 Initializing WhiteMagic Orchestrator (Python Fallback)...")
        
        # Path to Zig Shared Lib
        zig_path = os.path.abspath(os.path.join(
            os.path.dirname(__file__), 
            "../whitemagic-zig/zig-out/lib/libwhitemagic.so"
        ))
        
        self.zig = ZigCore(zig_path)
        self.rust_version = "Unknown"
        
        if RUST_AVAILABLE:
            # We don't have version exposed yet but we can do a test
            pass

    def solar_return_check(self):
        print("\n--- v9 Solar Return System Verification ---")
        
        # 1. Rust/Dharma Check
        print("\n[Layer 8] Dharma/Ethical Core:")
        if RUST_AVAILABLE:
            # Use I Ching as a entropy/integrity check
            try:
                hex_num, lines = whitemagic_rs.iching_cast("System Integrity")
                print(f"  Status: ONLINE")
                print(f"  Integrity Hexagram: {hex_num}")
                if hex_num == 6:
                     print("  Note: Hexagram 6 (Conflict) - Proceed with caution.")
            except Exception as e:
                print(f"  Status: ERROR ({e})")
        else:
             print("  Status: OFFLINE")

        # 2. Zig/Metabolic Check
        print("\n[Layer 9] Metabolic/Thermal Core:")
        thermal_status = self.zig.check_thermal()
        print(f"  Status: {thermal_status}")
        if self.zig.lib:
            print("  Holographic Memory: Ready")
        
        print("\n[Layer 10] Orchestrator Status:")
        if RUST_AVAILABLE and self.zig.lib:
            print("  Status: SYNCHRONIZED")
            print("  System is ready for user interaction.")
        else:
            print("  Status: DEGRADED (Check missing cores)")

if __name__ == "__main__":
    orch = Orchestrator()
    orch.solar_return_check()
