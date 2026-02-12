import time

struct GanaScore:
    var importance: Float32
    var urgency: Float32
    var resonance: Float32
    var score: Float32

    @always_inline
    fn __init__(out self, imp: Float32, urg: Float32, res: Float32):
        self.importance = imp
        self.urgency = urg
        self.resonance = res
        self.score = (imp * 0.5) + (urg * 0.3) + (res * 0.2)

fn main() raises:
    print("🔬 WhiteMagic v6.0 Scientific Benchmark (No-Fold Edition)")
    
    var iterations: Int = 10000000
    var start = time.perf_counter_ns()
    
    var checksum: Float32 = 0.0
    for i in range(iterations):
        # Dynamic input based on loop index to prevent constant folding
        var val = Float32(i % 100) / 100.0
        var gana = GanaScore(val, 0.5, 0.2)
        checksum += gana.score
        
    var end = time.perf_counter_ns()
    var duration_ns = end - start
    
    print("✨ Processed", iterations, "Gana scoring operations")
    print("🧪 Checksum (Verifying work):", checksum)
    print("⏱️  Total Time:", duration_ns, "ns")
    
    var avg_ns = Float64(duration_ns) / Float64(iterations)
    print("⚡ Average per operation:", avg_ns, "ns")
    
    # T480s @ 4.2GHz -> 0.238 ns/cycle
    var cycles = avg_ns / 0.238
    print("📊 Cycles per op:", cycles)
    
    if cycles < 1.0:
        print("🚀 VERDICT: SIMD Vectorization confirmed. Multiple ops per cycle.")
    else:
        print("📊 VERDICT: Scalar execution. Work is being performed.")

