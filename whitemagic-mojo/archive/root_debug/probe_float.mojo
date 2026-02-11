from python import Python

fn main() raises:
    var py = Python.import_module("builtins")
    var time = Python.import_module("time")
    
    var t_obj = time.time()
    print("Python time object:", t_obj)
    
    # Try explicit cast
    try:
        var f1 = float(t_obj)
        print("float(obj):", f1)
    except:
        print("float(obj) failed")
        
    # Try to_float64 if it exists (it might be __float__ or similar)
    try:
        # In newer Mojo, PythonObject might not have to_float64 method directly exposed?
        # But let's see.
        pass
    except:
        pass
