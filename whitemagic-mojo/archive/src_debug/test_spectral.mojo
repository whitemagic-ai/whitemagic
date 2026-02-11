from compute.tensor_network import Matrix, SpectralTensor
from python import Python

fn main() raises:
    print("🌊 Testing Spectral Tensor & Parallel Compute...")
    
    # 1. Test Spectral Tensor
    var spectrum = SpectralTensor[1024]()
    spectrum.load_wave(440.0) # 440 Hz
    print("   Wave Loaded (1024 samples)")
    
    print("   Applying Parallel Transform...")
    spectrum.transform_parallel(1.5)
    print("   ✅ Parallel Transform Complete")
    
    # 2. Test Parallel MatMul
    print("\n⚡ Testing Parallel Matrix Multiplication...")
    var A = Matrix[128, 128]()
    var B = Matrix[128, 128]()
    A.load_random()
    B.load_identity()
    
    var time = Python.import_module("time")
    var start = time.perf_counter().to_float64()
    
    var C = A.matmul_parallel(B)
    
    var end = time.perf_counter().to_float64()
    var ms = (end - start) * 1000.0
    
    print("   MatMul (128x128) Time:", ms, "ms")
    print("   ✅ Parallel MatMul Complete")

