from python import Python
fn main() raises:
    var py = Python.import_module("builtins")
    var s: String = "123.45"
    var py_f = py.float(s)
    # The current way to get float from PythonObject in some nightlies is float(py_obj)
    # or py_obj.to_float64()
    try:
        var f = float(py_f)
        print("float(py_f) worked:", f)
    except:
        print("float(py_f) failed")
