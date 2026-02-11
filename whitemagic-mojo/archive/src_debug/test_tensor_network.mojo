
from compute.tensor_network import Matrix, SystemClassifier

fn main():
    print("🧠 Testing True Tensor AI Network...")
    
    # 1. Initialize Network
    var nn = SystemClassifier()
    print("✅ Neural Network Initialized (Layer 1: 4x16, Layer 2: 16x3)")
    
    # 2. Create Input Vector (Simulated Metrics)
    # [Energy, Importance, Resonance, Harmony]
    var input = Matrix[1, 4]()
    input.data[0] = 0.8 # High Energy
    input.data[1] = 0.9 # High Importance
    input.data[2] = 0.5 # Mid Resonance
    input.data[3] = 0.2 # Low Harmony
    
    print("📊 Input Vector: [0.8, 0.9, 0.5, 0.2]")
    
    # 3. Forward Pass
    var output = nn.forward(input)
    
    print("🔮 Model Output (Logits):")
    print("   Healthy: ", output.data[0])
    print("   Warning: ", output.data[1])
    print("   Critical:", output.data[2])
    
    # Simple Argmax
    var max_idx = 0
    var max_val = output.data[0]
    
    if output.data[1] > max_val:
        max_idx = 1
        max_val = output.data[1]
    
    if output.data[2] > max_val:
        max_idx = 2
        max_val = output.data[2]
        
    var result = "UNKNOWN"
    if max_idx == 0: result = "HEALTHY"
    elif max_idx == 1: result = "WARNING"
    elif max_idx == 2: result = "CRITICAL"
    
    print("🤖 Classification Result:", result)
