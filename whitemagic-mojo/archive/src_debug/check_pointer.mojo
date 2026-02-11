from memory import UnsafePointer, memset_zero

fn main():
    var ptr = UnsafePointer[Float32].alloc(10)
    memset_zero(ptr, 10)
    ptr[0] = 1.0
    print(ptr[0])
    ptr.free()
