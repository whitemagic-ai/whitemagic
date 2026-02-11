from python import Python

fn main() raises:
    var s: String = "123.45"
    var py = Python.import_module("builtins")
    var f = py.float(s)
    print("X:", f)
