
import math
import sys

struct NeuroScorer:
    var decay_rate: Float64
    
    fn __init__(out self, decay_rate: Float64 = 0.05):
        self.decay_rate = decay_rate

    fn calculate_decay(self, current_score: Float64, days_since_access: Float64) -> Float64:
        # Apply exponential decay based on time.
        # Formula: score * e^(-decay_rate * time)
        var decay_factor = math.exp(-self.decay_rate * days_since_access)
        return current_score * decay_factor

    fn reinforce(self, current_score: Float64, importance: Float64) -> Float64:
        # Reinforce memory strength based on access importance.
        var boost = importance * 0.1
        var new_score = current_score + boost
        if new_score > 1.0:
            return 1.0
        return new_score

    fn novelty(self, access_count: Int, total_memories: Int) -> Float64:
        # Calculate novelty score.
        if access_count == 0:
            return 1.0
        var freq = Float64(access_count) / Float64(total_memories)
        return 1.0 - freq

fn main() raises:
    var args = sys.argv()
    if len(args) < 6:
        print("Usage: <binary> <current_score> <access_count> <total_memories> <days_since_access> <importance>")
        return

    var current_score = Float64(atof(args[1]))
    var access_count = Int(atoi(args[2]))
    var total_memories = Int(atoi(args[3]))
    var days_since_access = Float64(atof(args[4]))
    var importance = Float64(atof(args[5]))

    var scorer = NeuroScorer(0.05)
    
    # 1. Apply Decay
    var score = scorer.calculate_decay(current_score, days_since_access)
    
    # 2. Apply Reinforcement (if importance provided)
    if importance > 0:
        score = scorer.reinforce(score, importance)
        
    # 3. Incorporate Novelty
    var nov = scorer.novelty(access_count, total_memories)
    # Blend novelty into final score (20% weight as per Python WEIGHTS)
    var final_score = (score * 0.8) + (nov * 0.2)

    print("RESULT_SCORE: ", final_score)

# Helper functions for string to numeric conversion in Mojo nightly
from python import Python

fn atof(s: String) raises -> Float64:
    return Float64(s)

fn atoi(s: String) raises -> Int:
    return Int(s)
