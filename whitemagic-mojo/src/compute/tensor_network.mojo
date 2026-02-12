import math
import sys
from memory import memset_zero, memcpy, alloc
from sys.ffi import OwnedDLHandle
from algorithm import parallelize

# Define a simple generic Matrix struct
struct Matrix[rows: Int, cols: Int]:
    # Use type_of(alloc) to get the exact pointer type expected by the system
    # This bypasses the need to guess the exact UnsafePointer signature
    comptime PtrType = type_of(alloc[Float32](0))
    
    var data: Self.PtrType
    var handle_id: UInt64
    var is_unified: Bool
    comptime FnPtrType = type_of(alloc[UInt8](0))
    var free_fn_ptr: Self.FnPtrType
    var has_free_fn: Bool

    fn __init__(out self):
        self.data = alloc[Float32](Self.rows * Self.cols)
        memset_zero(self.data, Self.rows * Self.cols)
        self.handle_id = 0
        self.is_unified = False
        self.free_fn_ptr = alloc[UInt8](0)
        self.has_free_fn = False

    # Constructor for Unified Memory (Zig-backed) - Using 'ref' to borrow handle
    fn __init__(out self, ref zig_lib: OwnedDLHandle, id: UInt64):
        # wm_memory_alloc(id: u64, size: usize, alignment_val: u32) -> ?[*]u8
        # We need to bitcast the result to our PtrType
        
        # Capture UInt8 pointer type for the call
        comptime BytePtr = type_of(alloc[UInt8](0))
        
        var ptr_raw = zig_lib.call["wm_memory_alloc", BytePtr, UInt64, Int, UInt32](
            id, Self.rows * Self.cols * 4, 4
        )
        
        if not ptr_raw:
             print("Error: Unified memory allocation failed for ID", id)
             # Fallback to local alloc to prevent crash on access, though this masks the error
             self.data = alloc[Float32](Self.rows * Self.cols)
             self.is_unified = False # Treat as local so we free it
             self.free_fn_ptr = alloc[UInt8](0)
             self.has_free_fn = False
        else:
            # Bitcast to Float32 pointer
            self.data = ptr_raw.bitcast[Float32]()
            self.is_unified = True
            # Store pointer to Zig free function (as opaque pointer)
            self.free_fn_ptr = alloc[UInt8](0)
            self.has_free_fn = True
            
        self.handle_id = id
        memset_zero(self.data, Self.rows * Self.cols)

    fn __moveinit__(out self, deinit existing: Self):
        self.data = existing.data
        self.handle_id = existing.handle_id
        self.is_unified = existing.is_unified
        self.free_fn_ptr = existing.free_fn_ptr
        self.has_free_fn = existing.has_free_fn

    fn __del__(deinit self):
        if not self.is_unified:
            self.data.free()

    fn load_random(mut self):
        for i in range(Self.rows * Self.cols):
            var val = (Float32(i % 100) / 50.0) - 1.0 
            self.data[i] = val

    fn load_identity(mut self):
        memset_zero(self.data, Self.rows * Self.cols)
        for i in range(min(Self.rows, Self.cols)):
            self.data[i * Self.cols + i] = 1.0

    fn matmul[other_cols: Int](self, other: Matrix[Self.cols, other_cols]) -> Matrix[Self.rows, other_cols]:
        var res = Matrix[Self.rows, other_cols]()
        comptime nelts = sys.simd_width_of[DType.float32]()
        for i in range(Self.rows):
            for k in range(Self.cols):
                var val_a = self.data[i * Self.cols + k]
                # var vec_a = SIMD[DType.float32, nelts](val_a)
                for j in range(0, other_cols, nelts):
                    if j + nelts <= other_cols:
                        var vec_b = other.data.load[width=nelts](k * other_cols + j)
                        var vec_c = res.data.load[width=nelts](i * other_cols + j)
                        var vec_res = vec_c.fma(val_a, vec_b) # FMA with scalar val_a
                        res.data.store[width=nelts](i * other_cols + j, vec_res)
                    else:
                        for r in range(j, other_cols):
                             res.data[i * other_cols + r] += val_a * other.data[k * other_cols + r]
        return res^

    fn matmul_parallel[other_cols: Int](self, other: Matrix[Self.cols, other_cols]) -> Matrix[Self.rows, other_cols]:
        var res = Matrix[Self.rows, other_cols]()
        
        # Capture pointers to avoid copying large structs (though Matrix is small view)
        var ptr_a = self.data
        var ptr_b = other.data
        var ptr_res = res.data
        
        @parameter
        fn calc_row(i: Int):
            comptime nelts = sys.simd_width_of[DType.float32]()
            for k in range(Self.cols):
                var val_a = ptr_a[i * Self.cols + k]
                for j in range(0, other_cols, nelts):
                    if j + nelts <= other_cols:
                        var vec_b = ptr_b.load[width=nelts](k * other_cols + j)
                        var vec_c = ptr_res.load[width=nelts](i * other_cols + j)
                        var vec_res = vec_c.fma(val_a, vec_b)
                        ptr_res.store[width=nelts](i * other_cols + j, vec_res)
                    else:
                        for r in range(j, other_cols):
                             ptr_res[i * other_cols + r] += val_a * ptr_b[k * other_cols + r]
        
        parallelize[calc_row](Self.rows) # Use all threads
        return res^

    fn relu(mut self):
        for i in range(Self.rows * Self.cols):
            if self.data[i] < 0:
                self.data[i] = 0

# 1. Mojo Compute: Spectral Tensor (Optimized for 1D Signal Processing)
struct SpectralTensor[size: Int]:
    comptime PtrType = type_of(alloc[Float32](0))
    var data: Self.PtrType
    
    fn __init__(out self):
        self.data = alloc[Float32](Self.size)
        memset_zero(self.data, Self.size)
        
    fn __del__(deinit self):
        self.data.free()
        
    fn load_wave(mut self, frequency: Float32):
        for i in range(Self.size):
            # Simple sine wave generation
            var t = Float32(i) / Float32(Self.size)
            self.data[i] = math.sin(2.0 * 3.14159 * frequency * t)

    # Parallelized Transform (simulating FFT or heavy processing)
    fn transform_parallel(self, gain: Float32):
        var ptr = self.data
        
        @parameter
        fn process_chunk(idx: Int):
            # Process a single element (or chunk if stride handled differently)
            # Here parallelize iterates 0..size
            var val = ptr[idx]
            # Simulate heavy compute: signal modulation
            var mod = val * gain * math.cos(val) 
            ptr[idx] = mod
            
        parallelize[process_chunk](Self.size)

struct SystemClassifier:
    var W1: Matrix[4, 16]
    var W2: Matrix[16, 3]
    
    fn __init__(out self):
        self.W1 = Matrix[4, 16]()
        self.W2 = Matrix[16, 3]()
        self.W1.load_random()
        self.W2.load_random()

    # Unified Init - Using 'ref' to borrow handle
    fn __init__(out self, ref zig_lib: OwnedDLHandle):
        # Assign unique IDs for unified weights
        self.W1 = Matrix[4, 16](zig_lib, 1001)
        self.W2 = Matrix[16, 3](zig_lib, 1002)
        self.W1.load_random()
        self.W2.load_random()
        
    fn forward(self, input_metrics: Matrix[1, 4]) -> Matrix[1, 3]:
        var h1 = input_metrics.matmul(self.W1)
        h1.relu()
        var out = h1.matmul(self.W2)
        return out^
