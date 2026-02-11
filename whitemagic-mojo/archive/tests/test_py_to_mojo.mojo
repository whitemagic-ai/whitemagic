from python import Python

fn main() raises:
    var py = Python.import_module("builtins")
    var s: String = "123.45"
    var py_float = py.float(s)
    var f: Float64 = py_float.to_float64()
    print("F64:", f)
    var f32: Float32 = f.cast[DType.float32]()
    print("F32:", f32)
