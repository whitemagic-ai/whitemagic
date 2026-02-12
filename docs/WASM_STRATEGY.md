# WASM Compilation Strategy

**Status**: Active Development (Seed Binary PoC)
**Target**: v15.1 (Seed), v16.0 (Full WASM Modules)

---

## Vision

Two-tier delivery: **WhiteMagic Lite** as a zero-dependency Rust binary (~2-5MB) for instant agent bootstrap, and **WhiteMagic Heavy** as the full 313-tool Python stack delivered via Docker or pip. Both speak MCP over stdio — an agent can start with Lite and upgrade to Heavy seamlessly.

## Delivery Tiers

### Tier 0: WhiteMagic Lite (Seed Binary)

A single native Rust executable. No Python, no pip, no Docker, no runtime deps.

```
wm-seed (single binary, ~2-5MB)
├── Embedded SQLite (via rusqlite bundled)
├── 16 quickstart memories (compiled into binary)
├── MCP stdio server (~30 core tools)
│   ├── Memory CRUD (create, read, search, list, delete)
│   ├── FTS5 full-text search with BM25
│   ├── Gnosis (system health)
│   ├── Capabilities discovery
│   ├── Dharma rules (basic YAML eval)
│   ├── Galaxy backup/restore
│   ├── Rate limiter + circuit breaker
│   └── 5D holographic spatial index
├── Karma ledger (append-only JSON)
└── State directory scaffolding
```

**Install**:
```bash
# Linux/macOS
curl -L https://github.com/whitemagic-ai/whitemagic/releases/download/v15.1/wm-seed-$(uname -s)-$(uname -m) -o wm
chmod +x wm
./wm init my-project    # Scaffolds state dir + seeds memories
./wm serve              # MCP stdio server, ready for AI clients
```

**Platforms**: linux-x64, linux-arm64, darwin-x64, darwin-arm64, windows-x64
**Install time**: <3 seconds. Zero dependencies.

### Tier 1: WhiteMagic Standard (`pip install whitemagic[mcp]`)

The full Python experience. 313 MCP tools across 28 Gana meta-tools. ~60s install.

### Tier 2: WhiteMagic Heavy (Docker)

Everything from Tier 1 plus all 11 polyglot accelerators pre-compiled:

```dockerfile
FROM python:3.12-slim

# Pre-built polyglot cores
COPY --from=rust-builder   /usr/local/lib/whitemagic_rs.so  /opt/wm/lib/
COPY --from=zig-builder    /usr/local/lib/libwhitemagic.so  /opt/wm/lib/
COPY --from=go-builder     /usr/local/bin/whitemagic-mesh   /opt/wm/bin/
COPY --from=haskell-builder /usr/local/bin/whitemagic-hs    /opt/wm/bin/
COPY --from=elixir-builder  /opt/wm/elixir/_build           /opt/wm/elixir/

# Python core + all extras
RUN pip install whitemagic[mcp,cli,embeddings,api,encrypt,tui]

# Pre-seeded memory, pre-built embedding index
COPY seed_memories.db /opt/wm/memory/whitemagic.db

ENV WM_STATE_ROOT=/opt/wm WM_MCP_PRAT=1 WM_SILENT_INIT=1
ENTRYPOINT ["python", "-m", "whitemagic.run_mcp"]
```

**Delivery**: `docker pull whitemagic-ai/whitemagic:v15`
**Size**: ~800MB (includes PyTorch for embeddings) or ~200MB slim (no embeddings)
**Install time**: ~30s pull on typical connection.

### Tier Comparison

| | Lite (Seed) | Standard (pip) | Heavy (Docker) |
|--|-------------|---------------|----------------|
| **Tools** | ~30 core | 313 full | 313 + polyglot accel |
| **Size** | 2-5MB | ~25MB source | 200-800MB image |
| **Install** | <3s curl | ~60s pip | ~30s docker pull |
| **Dependencies** | Zero | Python 3.10+ | Docker only |
| **Embeddings** | No | Optional | Pre-built |
| **Polyglot cores** | Rust only | Rust (optional) | All 11 pre-compiled |
| **Air-gapped** | ✅ | ✅ (with wheel) | ✅ (with image) |
| **Upgrade path** | → pip install | → Docker | Full |

### Agent Upgrade Flow

```
Agent starts → downloads wm-seed (3s)
  → has memory + search + basic tools immediately
  → decides it needs embeddings → pip install whitemagic[embeddings]
  → or: docker pull whitemagic-ai/whitemagic:v15
  → full 313-tool experience, same state directory
```

## What Would Be Compiled

The `whitemagic-rust/` crate already contains the performance-critical modules:

| Module | LOC | WASM Value |
|--------|-----|------------|
| `spatial_index_5d.rs` | ~800 | Holographic nearest-neighbor in <0.1ms |
| `minhash.rs` | ~250 | Near-duplicate detection |
| `search.rs` | ~400 | BM25 full-text search |
| `rate_limiter.rs` | ~200 | Atomic sliding-window rate limiting |
| `state_board.rs` | ~350 | Cross-process shared state |
| `event_ring.rs` | ~300 | Lock-free LMAX-style event bus |
| `holographic_encoder_5d.rs` | ~390 | 5D coordinate encoding |
| `arrow_bridge.rs` | ~200 | Arrow IPC for zero-copy data transfer |

**Total**: ~2,900 LOC → estimated 50-200KB `.wasm` per module

## Build Pipeline

```
whitemagic-rust/
├── Cargo.toml              # Add wasm32-wasi target
├── src/
│   ├── lib.rs              # PyO3 bindings (existing)
│   └── wasm_exports.rs     # WASM-specific exports (new)
└── wasm/
    ├── build.sh            # wasm-pack build script
    ├── whitemagic_core.wasm
    └── whitemagic_core.d.ts
```

### Build Commands

```bash
# Install wasm-pack
cargo install wasm-pack

# Build WASM module
wasm-pack build --target web --out-dir wasm/

# Or for Node.js/Deno runtimes
wasm-pack build --target nodejs --out-dir wasm-node/
```

## Distribution Channels

1. **npm**: `@whitemagic/core-wasm` — for JS/TS agent runtimes
2. **wasmtime registry**: For Rust/Go/Python agent runtimes via WASI
3. **Bundled in pip package**: `whitemagic/wasm/` directory with `.wasm` files
4. **CDN**: `cdn.whitemagic.dev/wasm/v15/whitemagic_core.wasm`

## Integration Points

### Python (via wasmtime-py)
```python
from wasmtime import Store, Module, Instance
store = Store()
module = Module.from_file(store.engine, "whitemagic_core.wasm")
instance = Instance(store, module, [])
# Call exported functions
result = instance.exports(store)["spatial_query_5d"](x, y, z, w, v, k)
```

### JavaScript/TypeScript
```typescript
import init, { spatial_query_5d } from '@whitemagic/core-wasm';
await init();
const results = spatial_query_5d(x, y, z, w, v, k);
```

### Static Binary MCP Server
The ultimate goal: a single Rust executable with embedded SQLite + WASM governance modules. Zero runtime deps. For air-gapped and distributed agents.

```
whitemagic-server (single binary, ~5MB)
├── Embedded SQLite (memory DB)
├── WASM governance modules (Dharma, Karma, Harmony)
├── MCP stdio transport
└── Optional HTTP transport
```

## Implementation Phases

### Phase 1: Core Math (v16.0)
- Compile `spatial_index_5d`, `minhash`, `holographic_encoder_5d`
- Publish to npm as `@whitemagic/core-wasm`
- Python fallback via wasmtime-py

### Phase 2: Search + Rate Limiting (v16.1)
- Compile `search`, `rate_limiter`
- Add WASI filesystem access for SQLite queries

### Phase 3: Static Binary (v17.0)
- Full MCP server as single Rust binary
- Embedded SQLite with SQLCipher encryption
- WASM governance modules hot-loadable

## Size Budget

| Component | Target Size |
|-----------|------------|
| Core math (Phase 1) | <100KB |
| Search + rate limit (Phase 2) | <150KB |
| Full static server (Phase 3) | <5MB |

## Install Speed Impact

Current: `pip install whitemagic[mcp]` takes ~60s (downloads 30+ Python deps)

With WASM:
- **npm install**: `npm i @whitemagic/core-wasm` — <5s, ~200KB
- **Direct download**: `curl -O https://cdn.whitemagic.dev/wasm/v16/whitemagic.wasm` — <1s
- **Static binary**: Single download, zero install — <3s

## Prerequisites

- `wasm-pack` installed
- Rust `wasm32-wasi` target: `rustup target add wasm32-wasi`
- Cargo.toml: `[package.metadata.wasm-pack.profile.release] wasm-opt = false` (already present)

## Contact

Questions or contributions: whitemagicdev@proton.me
