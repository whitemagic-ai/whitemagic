# Benchmark Comparison — WhiteMagic vs Comparable Tools

> Research on how WhiteMagic's performance compares to existing agent memory/tool systems.

## Comparable Tools

| Tool | Category | Key Metric | Notes |
|------|----------|-----------|-------|
| **mem0 (OpenMemory)** | Agent memory | Sub-50ms retrieval (claim) | Requires OpenAI API + Qdrant |
| **LangChain Memory** | Agent memory | Varies by backend | Wrapper, not native |
| **Zep** | Agent memory | ~100ms retrieval | Dedicated memory server |
| **ChromaDB** | Vector DB | ~10ms search (local) | Pure vector store, no tools |

## mem0 Published Benchmarks

Source: mem0 documentation + arXiv paper "Mem0: Building Production-Ready AI Agents with Scalable Long-Term Memory"

| Operation | mem0 Platform | mem0 Open Source |
|-----------|--------------|-----------------|
| **Memory add** | 200–500ms (async), 500–2000ms (sync) | 50–500ms (LLM-dependent) |
| **Memory search** | 50–200ms | 10–100ms (local vector) |
| **Keyword search** | +10ms overhead | N/A |
| **Reranking** | +150–200ms | N/A |
| **Memory filtering** | +200–300ms | N/A |
| **Combined search** | ~410ms | N/A |

**Key dependencies**: Requires OpenAI API key (LLM), Qdrant (vector DB), Docker (MCP server).

## WhiteMagic Performance Profile

### Memory Operations

| Operation | WhiteMagic | Notes |
|-----------|-----------|-------|
| **Memory store** | ~5–20ms | SQLite + FTS, no external API |
| **Memory search (FTS)** | ~2–10ms | SQLite FTS5, local |
| **Memory search (semantic)** | ~20–100ms | Depends on embedding model |
| **Hybrid recall (FTS + graph walk)** | ~50–200ms | v14 Living Graph |
| **Holographic encode (5D)** | ~0.05ms (Rust), ~5ms (Python) | 100× speedup with Rust |

### Polyglot Accelerators

| Operation | Speed | Language |
|-----------|-------|---------|
| **SIMD distance matrix** | 6.5–13× faster than Python | Zig (AVX2) |
| **BM25 query** | 0.002ms per query (500-doc) | Rust |
| **Rate limiter** | 324K ops/sec | Rust |
| **Holographic batch encode** | 3.3ms for 100 items | Rust |
| **MinHash similarity** | 1.0ms for 60 keyword sets | Rust |

### Head-to-Head: Memory Search

| Metric | WhiteMagic | mem0 (OSS) | mem0 (Platform) |
|--------|-----------|-----------|----------------|
| **Search latency** | 2–10ms (FTS) | 10–100ms | 50–200ms |
| **Hybrid search** | 50–200ms | N/A (OSS) | ~410ms |
| **External API required** | No | Yes (OpenAI) | Yes (API key) |
| **Works offline** | Yes | No | No |

### Head-to-Head: Memory Storage

| Metric | WhiteMagic | mem0 (OSS) | mem0 (Platform) |
|--------|-----------|-----------|----------------|
| **Store latency** | 5–20ms | 50–500ms | 200–2000ms |
| **Storage backend** | SQLite (single file) | Qdrant + SQLite | Cloud managed |
| **Requires Docker** | No | Yes (MCP server) | No |
| **Max memories tested** | 110K+ | Unknown | Unknown |

## v14.5 Benchmark Gauntlet v3 — Live Results (Feb 11, 2026)

Run on: Intel i5 (8 threads), 16GB RAM, Ubuntu 24.04, Python 3.12, Rust 1.84

### Tier 1: Core Baselines

| Operation | Median | P95 | Throughput |
|-----------|--------|-----|-----------|
| **Memory store** | 1.42ms | 2.52ms | 441 ops/s |
| **Memory search** | 2.35ms | 5.30ms | 367 ops/s |
| **Dispatch table lookup** | 268ns | 296ns | **3.61M ops/s** |
| **Harmony vector snapshot** | 252ns | 293ns | **3.66M ops/s** |

### Tier 2: Arrow IPC

| Operation | Median | P95 | Throughput |
|-----------|--------|-----|-----------|
| **Rust Arrow encode (100 docs)** | 288µs | 528µs | **3,100 ops/s** |
| **Rust Arrow decode (100 docs)** | 35.6µs | 69.5µs | **23,400 ops/s** |
| **Arrow IPC export (DB→bytes)** | 10.8ms | 14.6ms | 88 ops/s |
| **Arrow IPC import (bytes→DB)** | 42.5µs | 54.9µs | **23,000 ops/s** |

### Tier 3: Tokio Clone Army

| Clones | Median | P95 | Throughput |
|--------|--------|-----|-----------|
| **10 clones** | 711µs | 982µs | 1,300 ops/s |
| **100 clones** | 943µs | 1.17ms | 1,000 ops/s |
| **1,000 clones** | 3.53ms | 11.9ms | 217 ops/s |

---

*Updated: February 11, 2026 — WhiteMagic v14.5.0*