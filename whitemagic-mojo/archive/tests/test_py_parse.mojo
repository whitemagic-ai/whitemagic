from python import Python

fn main() raises:
    var builtins = Python.import_module("builtins")
    var s: String = "123.45"
    var f = builtins.float(s)
    print("Parsed value:", f)
