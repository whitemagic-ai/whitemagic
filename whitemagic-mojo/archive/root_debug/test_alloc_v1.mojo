from memory import UnsafePointer

fn main():
    print("Testing pointer allocation...")
    
    # Try global alloc if available (older Mojo?)
    # var p = alloc[Float32](10) 
    
    # Try UnsafePointer static method (newer Mojo?)
    var p2 = UnsafePointer[Float32].alloc(10)
    p2[0] = 1.0
    print(p2[0])
    p2.free()
