from python import Python

fn main() raises:
    var py = Python.import_module("builtins")
    var json = Python.import_module("json")
    
    var data = json.loads('{"val": 123.45, "int": 42}')
    var val = data["val"]
    var i = data["int"]
    
    print("Trying conversion via String...")
    # Convert to python string first
    var s_py = py.str(val)
    # Convert python string to Mojo String
    var s_mojo = String(s_py)
    print("String:", s_mojo)
    
    var f = Float64(s_mojo)
    print("Float64:", f)
    
    var s_i_py = py.str(i)
    var s_i_mojo = String(s_i_py)
    var integer = Int(s_i_mojo)
    print("Int:", integer)
