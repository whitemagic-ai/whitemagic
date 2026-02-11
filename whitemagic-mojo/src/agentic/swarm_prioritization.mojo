import time

struct GanaScore:
    var importance: Float32
    var urgency: Float32
    var resonance: Float32
    var score: Float32

    fn __init__(out self, imp: Float32, urg: Float32, res: Float32):
        self.importance = imp
        self.urgency = urg
        self.resonance = res
        self.score = (imp * 0.5) + (urg * 0.3) + (res * 0.2)

fn main() raises:
    print("🔥 Mojo Swarm Prioritization Kernel Active")
    
    var iterations: Int = 1000000
    var start = time.perf_counter_ns()
    
    var total_score: Float32 = 0.0
    for i in range(iterations):
        var gana = GanaScore(0.8, 0.9, 0.5)
        total_score += gana.score
        
    var end = time.perf_counter_ns()
    var duration_ns = end - start
    
    print("✨ Processed", iterations, "Gana scoring operations")
    print("⏱️ Total Time:", duration_ns / 1000000, "ms")
    print("⚡ Average per operation:", Float64(duration_ns) / Float64(iterations), "ns")
