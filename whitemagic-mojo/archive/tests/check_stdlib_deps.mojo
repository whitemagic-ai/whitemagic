from math import sqrt
from python import Python

fn main() raises:
    print("--- Mojo Stdlib & Python Interop Test ---")
    var f: Float32 = 16.0
    var s = sqrt(f)
    print("sqrt(16.0) =", s)
    
    try:
        var time_mod = Python.import_module("time")
        var t = time_mod.time()
        print("Time (Python interop):", t)
    except e:
        print("Failed to import Python time module:", e)
    
    print("--- Test Complete ---")
