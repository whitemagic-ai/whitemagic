from python import Python

fn main() raises:
    var builtins = Python.import_module("builtins")
    var s: String = "123.45"
    var f = builtins.float(s)
    print("Parsed via Python builtins:", f)
    
    var s2: String = "100"
    var i = builtins.int(s2)
    print("Parsed via Python builtins:", i)
