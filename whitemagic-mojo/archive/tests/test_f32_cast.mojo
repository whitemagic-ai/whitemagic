fn main():
    var f64: Float64 = 1.23
    var f32: Float32 = f64.cast[DType.float32]()
    print(f32)
