import sys
from python import Python, PythonObject
from collections import List

struct CentroidPayload:
    var n_clusters: Int
    var n_protos: Int
    var centroids: List[Float64]
    var prototypes: List[Float64]

    fn __init__(
        out self,
        n_clusters: Int,
        n_protos: Int,
        var centroids: List[Float64],
        var prototypes: List[Float64],
    ):
        self.n_clusters = n_clusters
        self.n_protos = n_protos
        self.centroids = centroids^
        self.prototypes = prototypes^

fn _to_int(obj: PythonObject) raises -> Int:
    return Int(String(obj))

fn _to_float(obj: PythonObject) raises -> Float64:
    return Float64(String(obj))

fn main() raises:
    var py = Python.import_module("json")
    var args = sys.argv()
    if len(args) < 2:
        print("Usage: satkona_yang <json_payload>")
        return

    var payload_str = args[1]
    var payload = py.loads(payload_str)
    
    var n_clusters = _to_int(payload["n_clusters"])
    var n_protos = _to_int(payload["n_protos"])
    
    # Extract flat lists
    var centroids_py = payload["centroids"]
    var protos_py = payload["prototypes"]
    
    var centroids = List[Float64]()
    for i in range(len(centroids_py)):
        centroids.append(_to_float(centroids_py[i]))
        
    var prototypes = List[Float64]()
    for i in range(len(protos_py)):
        prototypes.append(_to_float(protos_py[i]))

    print("YANG_ENGINE:READY")
    
    # High-performance similarity loop (SIMD potential here)
    # For each cluster, find max similarity to any prototype
    var results = List[Float64]()
    for c_idx in range(n_clusters):
        var max_sim: Float64 = -1.0
        var c_offset = c_idx * 4
        
        var cx = centroids[c_offset]
        var cy = centroids[c_offset + 1]
        var cz = centroids[c_offset + 2]
        var cw = centroids[c_offset + 3]
        
        for p_idx in range(n_protos):
            var p_offset = p_idx * 4
            var px = prototypes[p_offset]
            var py_val = prototypes[p_offset + 1]
            var pz = prototypes[p_offset + 2]
            var pw = prototypes[p_offset + 3]
            
            # Simple dot product similarity
            var sim = cx*px + cy*py_val + cz*pz + cw*pw
            if sim > max_sim:
                max_sim = sim
        
        results.append(max_sim)

    # Output results in a format Python can parse easily
    var out_str = String("YANG: ")
    for i in range(len(results)):
        out_str += String(results[i])
        if i < len(results) - 1:
            out_str += ","
    print(out_str)
