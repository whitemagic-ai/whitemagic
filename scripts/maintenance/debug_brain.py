import sys
import os

# Set path to staging/core_system
sys.path.insert(0, os.path.abspath("staging/core_system"))

try:
    import whitemagic.inference
    print(f"✅ Imported whitemagic.inference from: {whitemagic.inference.__file__}")
    print(f"Attributes: {dir(whitemagic.inference)}")
    
    if hasattr(whitemagic.inference, 'generate'):
        print("✅ generate() function found.")
    else:
        print("❌ generate() function MISSING.")

except ImportError as e:
    print(f"❌ ImportError: {e}")
    print(f"Sys Path: {sys.path}")
