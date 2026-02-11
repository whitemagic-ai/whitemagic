from python import Python

def main():
    try:
        var py = Python.import_module("builtins")
        var val = py.float(1.23)
        var s_py = py.str(val)
        var s_mojo = String(s_py)
        print("Converted to String:", s_mojo)
        var f = Float64(s_mojo)
        print("Converted back to Float64:", f)
    except:
        print("Error during conversion")
