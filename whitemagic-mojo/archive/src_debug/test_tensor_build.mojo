from compute.tensor_network import Matrix, SpectralTensor, SystemClassifier

fn main():
    print("Compiling Tensor Network modules...")
    var m = Matrix[10, 10]()
    m.load_identity()
    print("Matrix created and loaded.")
    
    var s = SpectralTensor[1024]()
    s.load_wave(440.0)
    print("SpectralTensor created.")
    
    var sys = SystemClassifier()
    print("SystemClassifier created.")
    print("✅ All modules compiled successfully.")
