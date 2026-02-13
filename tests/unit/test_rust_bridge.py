
import sys
import os

# Add project root to path
sys.path.append(os.getcwd())

print("🐍 Testing Rust Bridge...")

try:
    import whitemagic_rs
    print(f"✅ Rust Module Imported: {whitemagic_rs}")
    print(f"   Docstring: {whitemagic_rs.__doc__}")
    
    # Try a simple function if available
    if hasattr(whitemagic_rs, 'sum_as_string'):
        print(f"   Test Function: {whitemagic_rs.sum_as_string(2, 2)}")
        
except ImportError as e:
    print(f"❌ Failed to import whitemagic_rs: {e}")
    # Check if .so file exists
    import glob
    so_files = glob.glob("**/*.so", recursive=True)
    print(f"   Found .so files: {so_files}")

except Exception as e:
    print(f"❌ unexpected error: {e}")
