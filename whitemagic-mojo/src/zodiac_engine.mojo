
import sys

struct ZodiacCore:
    var name: String
    var element: String
    var mode: String
    var frequency: Float64
    
    fn __init__(out self, name: String, element: String, mode: String):
        self.name = name
        self.element = element
        self.mode = mode
        
        # Calculate frequency directly in init to avoid calling self methods before full initialization
        var base_freq: Float64 = 0.0
        if element == "fire": base_freq = 440.0
        elif element == "earth": base_freq = 392.0
        elif element == "air": base_freq = 493.88
        elif element == "water": base_freq = 349.23
        
        var multiplier: Float64 = 1.0
        if mode == "cardinal": multiplier = 1.0
        elif mode == "fixed": multiplier = 1.25
        elif mode == "mutable": multiplier = 0.875
        
        self.frequency = base_freq * multiplier
        
    fn process(self, urgency: Float64) -> Float64:
        # Simulate core processing resonance
        var resonance = self.frequency
        if urgency > 0.8:
            resonance *= 1.1 # Urgent tasks resonate higher
        return resonance

fn main():
    # Simple CLI wrapper for testing
    var args = sys.argv()
    if len(args) < 4:
        print("Usage: zodiac_engine <name> <element> <mode>")
        return
        
    var core = ZodiacCore(args[1], args[2], args[3])
    print("Core Frequency: ", core.frequency)
