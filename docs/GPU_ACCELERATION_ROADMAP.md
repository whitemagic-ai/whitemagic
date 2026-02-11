# GPU Acceleration Roadmap (v13.3.2)

## Current State — CPU-Only Performance

WhiteMagic's polyglot accelerators achieve microsecond-scale operations on CPU:

| Operation | Current (CPU) | Language | Notes |
|-----------|--------------|----------|-------|
| BM25 search (500 docs) | 2μs | Rust PyO3 | 500K queries/sec/core |
| Rate limiter check | 3μs | Rust atomic | 324K ops/sec |
| Holographic encode (single) | 50μs | Rust PyO3 | 5D XYZWV |
| MinHash (60 sets) | 1ms | Rust PyO3 | 128-hash LSH |
| Holographic encode (batch 100) | 3.3ms | Rust Rayon | Parallel |
| Distance matrix (100×128) | 10ms | Zig SIMD AVX2 | 13× vs Python |
| Keyword extraction | 35μs | Python regex | Fastest path for keywords |

## GPU Targets — Ordered by Impact

### Tier 1: High Impact (>100× projected speedup)

**1. Batch Holographic Encoding (110K memories)**
- **Current**: ~55s (Rust Rayon, batch-100 × 1,100 iterations)
- **GPU projected**: <1s
- **Why**: Each memory's 5D encoding is independent — embarrassingly parallel. 110K encodings fit trivially on any modern GPU.
- **Best language**: Mojo (already in polyglot, MLIR→GPU backend)
- **Fallback**: CUDA C++, Rust + wgpu compute shaders

**2. Pairwise Distance Matrix (N² cosine)**
- **Current**: 13s for 1000×384 (Zig SIMD)
- **GPU projected**: <100ms
- **Why**: N² pairwise = perfect GPU workload. Matrix multiply is what GPUs exist for.
- **Best language**: Mojo (MLIR), CUDA C++, or Triton (Python GPU kernels)

**3. Embedding Similarity (110K × 384)**
- **Current**: Minutes (Python numpy)
- **GPU projected**: <5s
- **Why**: Large matrix multiply. 110K × 384 × 384 FLOPs = ~16 GFLOP, trivial for any GPU.
- **Best language**: Mojo first, then CUDA if more control needed

### Tier 2: Medium Impact (10-100× projected speedup)

**4. Association Mining (N² Jaccard)**
- **Current**: Minutes for full 110K corpus
- **GPU projected**: <10s
- **Why**: Parallel set intersection. Each pair is independent.
- **Best language**: CUDA C++ (bitwise operations on GPU are fast)

**5. Constellation Detection (grid density + flood-fill)**
- **Current**: ~2s for 50K memories
- **GPU projected**: <200ms
- **Why**: Grid binning is a parallel histogram, flood-fill can use parallel BFS.
- **Best language**: CUDA or Mojo

### Tier 3: Enables New Capabilities

**6. Local Neural Inference**
- **Current**: Not available (no GPU)
- **GPU projected**: 20-100 tokens/sec (7B model)
- **Why**: On-device LLM for semantic understanding, embedding generation, query expansion
- **Implementation**: llama.cpp (C++), Ollama, or Mojo MLIR
- **Models**: Sentence-transformers (MiniLM-L6, 384 dims) for embeddings, Qwen/Llama for generation

## Language Selection Guide

| Language | Strength | Integration | When to Use |
|----------|----------|------------|-------------|
| **Mojo** | MLIR→GPU, Python-like syntax | Already in polyglot, subprocess bridge | First choice for GPU compute |
| **CUDA C++** | Maximum control, maximum perf | New bridge needed (ctypes or PyO3 wrapper) | When Mojo isn't enough |
| **Rust + wgpu** | Vulkan compute shaders | Existing PyO3 bridge | When you want Rust safety |
| **Triton** | Python-syntax GPU kernels | Native Python, compiles to PTX | Prototyping, lowest friction |

## What Stays CPU-Only

These operations are **latency-sensitive** (need answers in <10μs) and would be *slower* on GPU due to kernel launch overhead (~10-50μs):

- **BM25 search** — 2μs on CPU, GPU launch alone would be 10-50μs
- **Rate limiter** — 3μs on CPU, same kernel launch issue
- **Dharma rules** — 42μs on CPU, decision logic not parallelizable
- **Single holographic encode** — 50μs on CPU, GPU overhead > computation

**Rule of thumb**: GPU wins when you have >1000 independent operations to batch. Below that threshold, CPU (especially with Rust/Zig SIMD) wins on latency.

## Implementation Phases

### Phase 1: Mojo GPU Pilot (when GPU available)
1. Install Mojo GPU-capable runtime
2. Port `batch_encoder.mojo` to use GPU via MLIR
3. Benchmark 110K holographic batch encoding
4. Wire into Python via existing `mojo_bridge.py`

### Phase 2: Distance Matrix GPU
1. Implement N² cosine in Mojo GPU or Triton
2. Benchmark against Zig SIMD (current: 13× over Python)
3. Target: 100× over Zig, 1300× over Python

### Phase 3: Embedding Layer
1. Sentence-transformers for 384-dim embeddings
2. GPU-accelerated similarity search (FAISS or custom)
3. Replace Jaccard association mining with semantic similarity

### Phase 4: Local Inference
1. llama.cpp or Mojo inference for semantic understanding
2. Query expansion: "memory" → "memory recall holographic coordinate"
3. Automatic memory summarization during consolidation

## Hardware Requirements

| GPU | VRAM | Good For |
|-----|------|----------|
| RTX 3060 (12GB) | 12GB | Embedding similarity, batch encoding, 7B LLM (4-bit) |
| RTX 4070 (12GB) | 12GB | All Tier 1-2 targets, 13B LLM (4-bit) |
| RTX 4090 (24GB) | 24GB | Everything + 70B LLM (4-bit), real-time inference |

The current laptop has **no discrete GPU**. All GPU work is aspirational for when hardware becomes available.
