from math import sqrt
from memory import UnsafePointer, alloc
from collections import List
from random import random_float64
from python import Python

comptime nelts = 16

fn cosine_similarity_simd(a: UnsafePointer[Float32], b: UnsafePointer[Float32], size: Int) -> Float32:
    var dot_vec = SIMD[DType.float32, nelts](0)
    var norm_a_vec = SIMD[DType.float32, nelts](0)
    var norm_b_vec = SIMD[DType.float32, nelts](0)
    
    # Process vectorized part
    var main_loop_end = size - (size % nelts)
    for i in range(0, main_loop_end, nelts):
        var va = a.load[width=nelts](i)
        var vb = b.load[width=nelts](i)
        dot_vec += va * vb
        norm_a_vec += va * va
        norm_b_vec += vb * vb
        
    var dot = dot_vec.reduce_add()
    var norm_a = norm_a_vec.reduce_add()
    var norm_b = norm_b_vec.reduce_add()

    # Handle tail scalars
    for i in range(main_loop_end, size):
        var va = a[i]
        var vb = b[i]
        dot += va * vb
        norm_a += va * va
        norm_b += vb * vb
    
    var denom = sqrt(norm_a) * sqrt(norm_b)
    if denom == 0:
        return 0.0
        
    return dot / denom

struct SearchResult(Copyable, Movable):
    var index: Int
    var score: Float32

    fn __init__(out self, index: Int, score: Float32):
        self.index = index
        self.score = score

    fn __copyinit__(out self, other: Self):
        self.index = other.index
        self.score = other.score
        
    fn __moveinit__(out self, deinit other: Self):
        self.index = other.index
        self.score = other.score

    # Allow comparison for sorting if needed
    fn __lt__(self, other: Self) -> Bool:
        return self.score < other.score

fn vector_search(query: UnsafePointer[Float32], 
                 vectors: UnsafePointer[Float32], 
                 n_vectors: Int, 
                 vector_dim: Int, 
                 k: Int) -> List[SearchResult]:
    
    var top_k = List[SearchResult](capacity=k + 1)
    
    # Calculate all similarities
    for i in range(n_vectors):
        var offset = i * vector_dim
        # Use ptr + offset instead of .offset() as per warning
        var current_vec = vectors + offset
        var score = cosine_similarity_simd(query, current_vec, vector_dim)
        
        # Insertion logic
        var inserted = False
        for j in range(len(top_k)):
            # Explicit copy for comparison if needed, or direct access
            if score > top_k[j].score:
                top_k.insert(j, SearchResult(i, score))
                inserted = True
                break
        
        if not inserted and len(top_k) < k:
            top_k.append(SearchResult(i, score))
            
        # Trim to K
        if len(top_k) > k:
            _ = top_k.pop() # Remove lowest score (last element)
            
    return top_k^

fn main() raises:
    print("⚡ Mojo Semantic Search Engine v1.0")
    
    var n_vectors = 1000
    var dim = 1536
    var k = 5
    
    print("   Database Size:", n_vectors)
    print("   Dimensions:", dim)
    
    # Allocate memory
    var query = alloc[Float32](dim)
    var database = alloc[Float32](n_vectors * dim)
    
    # Fill with random data
    for i in range(dim):
        query.store(i, Float32(random_float64()))
        
    for i in range(n_vectors * dim):
        database.store(i, Float32(random_float64()))
        
    # Run Search
    var time = Python.import_module("time")
    # Keep as PythonObject to avoid conversion errors
    var start = time.perf_counter()
    
    var results = vector_search(query, database, n_vectors, dim, k)
    
    var end = time.perf_counter()
    var duration_sec = end - start
    var duration_ms = duration_sec * 1000.0
    
    print("\n🔍 Top", k, "Results:")
    for i in range(len(results)):
        # Access elements directly to avoid implicit copy issues
        print("   Rank", i+1, "| Index:", results[i].index, "| Score:", results[i].score)
        
    # Use Python builtin print to avoid Mojo conversion issues
    var py_print = Python.import_module("builtins").print
    py_print("\n⏱️  Search Time (ms):", duration_ms)
    
    # Cleanup
    query.free()
    database.free()
