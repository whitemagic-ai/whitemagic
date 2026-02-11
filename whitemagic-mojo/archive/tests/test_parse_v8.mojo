import sys
from python import Python

fn main() raises:
    var args = sys.argv()
    if len(args) < 2:
        print("Need 1 arg")
        return
    
    var py = Python.import_module("builtins")
    var arg1 = args[1]
    var py_f = py.float(arg1)
    var f64 = py_f.to_float64()
    print("Parsed:", f64)
