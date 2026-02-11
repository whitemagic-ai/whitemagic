from python import Python

fn main() raises:
    var py = Python.import_module("builtins")
    var s: String = "123.45"
    var py_float = py.float(s)
    # Try direct conversion or common alternatives
    var f = py_float.__float__().to_float64()
    print("F64:", f)
