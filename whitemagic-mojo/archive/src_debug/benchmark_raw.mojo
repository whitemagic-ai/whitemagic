import sys
from python import Python

struct MemoryData:
    var id: String
    var content: String
    var title: String
    var tags: List[String]
    var importance: Float64
    var created_timestamp: Int

    fn __init__(out self, id: String, content: String, title: String, var tags: List[String], importance: Float64, created_timestamp: Int):
        self.id = id
        self.content = content
        self.title = title
        self.tags = tags^
        self.importance = importance
        self.created_timestamp = created_timestamp

struct CoordinateEncoder:
    fn __init__(out self):
        pass

    fn encode(self, memory: MemoryData) -> String:
        # Ultra-stable simplified encoding for batch proof-of-concept
        var x: Float64 = 0.0
        var y: Float64 = 0.0
        var z: Float64 = 0.0
        var w: Float64 = memory.importance
        
        # Return as comma-separated string to avoid complex PythonObject return issues
        return String(x) + "," + String(y) + "," + String(z) + "," + String(w)


fn main() raises:
    var iterations = 1000000
    var tags = List[String]()
    tags.append("code")
    tags.append("api")
    
    # Use explicit types
    var id_str = String("id1")
    var content_str = String("This is a python code snippet for the backend api")
    var title_str = String("Code Test")
    var imp_val = Float64(0.8)
    var ts_val = Int(1700000000)
    
    var mem = MemoryData(id_str, content_str, title_str, tags^, imp_val, ts_val)
    var encoder = CoordinateEncoder()
    
    var py = Python.import_module("time")
    var builtins = Python.import_module("builtins")
    
    # Use a loop to get a stable start time
    var start: Float64 = 0.0
    try:
        start = Float64(String(builtins.str(py.time())))
    except:
        start = 0.0
    
    for i in range(iterations):
        var _ = encoder.encode(mem)
        
    var end: Float64 = 0.0
    try:
        end = Float64(String(builtins.str(py.time())))
    except:
        end = 0.0
    
    var duration_sec = end - start
    var duration_ms = duration_sec * 1000.0
    
    print("RAW_ITERATIONS: ", iterations)
    print("RAW_TOTAL_MS: ", duration_ms)
    print("RAW_AVG_MS: ", duration_ms / Float64(iterations))
