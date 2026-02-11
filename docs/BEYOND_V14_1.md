# Beyond v14.1 — Out of Scope & Future Improvements

This document captures objectives that were deliberately deferred from v14.1,
along with new improvement ideas surfaced during development. Organized by
strategic horizon.

---

## Horizon 1: Near-Term (v14.2 — weeks)

### MCP Bundle Packaging (.mcpb)
Package WhiteMagic as a single distributable MCP bundle that clients can
install without pip. The MCP spec is converging on a `.mcpb` format for
portable server distribution. This would make WhiteMagic installable from
MCP registries with zero Python knowledge.

**Effort**: 2-3 days
**Blocked by**: MCP bundle spec finalization (expected Q1 2026)

### MCP Elicitation for Governance
When a Dharma rule blocks an action (e.g., Violet profile blocks offensive
tools without engagement tokens), the MCP server should use the Elicitation
protocol to *ask the user* for permission rather than silently failing.
This requires MCP SDK 1.26+ `ElicitationResult` support.

**Effort**: 1-2 days
**Depends on**: MCP SDK elicitation support in target clients

### Galaxy Data Hygiene for Public Release
The default galaxy currently contains ~5,600 personal/development memories.
For public release, it should ship with only the 12 quickstart seed memories.
Steps:
1. Export current DB to a personal galaxy backup
2. Reset default galaxy
3. Run `scripts/seed_quickstart_memories.py`
4. Verify clean state with `wm doctor`

**Effort**: 1 hour (scripted)

### llms.txt / llms-full.txt Generation
Auto-generate the `llms.txt` and `llms-full.txt` files from the tool registry.
These are the emerging standard for LLM-readable API documentation. The
current `llms.txt` is hand-written and outdated.

**Effort**: 2 hours

### Workflow Templates as MCP Resources
Serve canonical tool-call sequences (e.g., "onboarding flow", "dream cycle +
serendipity", "security audit") as MCP resources that AI clients can read
and follow. This bridges the gap between having 208 tools and knowing
which ones to use together.

**Effort**: 1 day

---

## Horizon 2: Medium-Term (v15.0 — months)

### WASM Capability Modules
Compile performance-critical Rust tools to WebAssembly for portable
distribution. Agents could hot-swap KB-sized `.wasm` modules instead of
requiring full Python + Rust builds. Target modules:
- `SpatialIndex5D` (holographic queries)
- `BM25Search` (text search)
- `MinHash` (deduplication)
- `RateLimiter` (rate limiting)

**Effort**: 2-3 weeks
**Depends on**: Stable WASM Component Model, wasm32-wasi target for PyO3

### Static Binary MCP Server
Compile the lean MCP server to a single static binary (via Nuitka, PyInstaller,
or a Rust rewrite of the MCP transport layer). This eliminates the Python
dependency entirely for end users.

**Effort**: 1-2 weeks
**Risk**: Python dynamic imports make static compilation non-trivial

### Iceoryx2 IPC for Multi-Process Agents
Replace Redis pub/sub with Iceoryx2 zero-copy shared memory for local
multi-process agent communication. Target: <1µs latency for inter-agent
events, vs ~100µs for Redis.

**Effort**: 2 weeks
**Depends on**: iceoryx2-python bindings maturity

### Mojo GPU Acceleration
Port the embedding engine's batch encode/search to Mojo for GPU acceleration.
The current sentence-transformers path is CPU-bound and dominates latency
for bulk operations (ingestion, dream cycle, entity resolution).

**Effort**: 2-3 weeks
**Depends on**: Mojo GPU maturity (currently 0.26.x)

### Multi-Node Mesh (libp2p)
Activate the Go mesh module for distributed WhiteMagic instances. Agents
on different machines discover each other via mDNS, share memories via
protobuf, and coordinate via distributed voting.

**Effort**: 2-3 weeks (Go mesh exists, needs Python bridge + protocol design)

### Constellation-Based Recall
Use the detected memory constellations (from association graph community
detection) as a retrieval signal. When searching, boost results from the
same constellation as the query context. This would improve recall quality
for domain-specific queries.

**Effort**: 1 week

---

## Horizon 3: Long-Term (v16.0+ — quarters)

### Cryptographic Trust Layer
- **Signed manifests**: Every WhiteMagic release includes a signed manifest
  of all tool schemas and their SHA-256 fingerprints
- **Karma Transparency Log**: Anchor karma ledger entries to XRPL for
  verifiable ethical audit trails
- **Reproducible builds**: Deterministic compilation of Rust/Zig/Mojo
  accelerators so agents can verify binary integrity

### Autonomous Horizon
- **Bounded wallets**: Dharma-governed wallets with spending limits,
  category restrictions, and multi-sig approval for large transactions
- **PRAT Compute Marketplace**: Agents bid for compute based on
  Performance/Reliability/Availability/Trust scores
- **Self-evolving tools**: Agents can propose and vote on tool modifications
  via the voting system, with automatic code generation and testing

### MCP Apps Dashboard
A web-based dashboard (React/Next.js) for visualizing:
- Memory galaxies and their contents
- Association graph topology (interactive 3D)
- Harmony Vector time series
- Karma ledger audit trail
- Dream cycle history and serendipity insights
- Agent trust scores and maturity progression

The `nexus/` directory contains early Tauri-based prototypes.

### x402 / L402 Payment Integration
Enable agents to pay for WhiteMagic premium features (GPU compute, large
galaxy storage, priority dream cycles) via:
- x402 (Coinbase/Cloudflare HTTP 402 stablecoin micropayments)
- L402 (Bitcoin Lightning Network millisatoshi payments)
- XRPL Hooks (smart contract revenue splitting)

---

## Improvement Ideas (Unsorted)

### Performance
- **Association pruning**: The 27.8M association graph has significant bloat
  from bulk archive ingestion. Prune edges with strength <0.3 to reduce
  DB size and improve graph traversal speed.
- **Holographic re-encoding**: The Y/Z centroid is severely compressed
  (-0.58/-0.63). Re-calibrate the coordinate encoder for the 110K merged
  corpus to improve spatial query quality.
- **Disable Zig keyword path**: Python is 15× faster than Zig via ctypes
  for keyword extraction. The SIMD overhead doesn't justify the FFI cost
  for this workload.

### Developer Experience
- **`wm init`**: Interactive CLI wizard for first-time setup (choose galaxy
  name, seed quickstart memories, configure Ollama, run health check)
- **`wm doctor --fix`**: Auto-fix common issues (missing tables, stale
  indexes, orphaned associations)
- **MCP Inspector integration**: Pre-built MCP Inspector profiles for
  testing WhiteMagic tool calls

### Memory Quality
- **Tag cleanup**: 88K "archive" tags and 19K "scavenged" tags dominate
  and dilute semantic tags. Need a tag normalization pass.
- **Duplicate detection at ingest**: The surprise gate catches near-duplicates
  but doesn't handle exact duplicates from re-ingestion. Add a content
  hash check.
- **Memory importance calibration**: Many memories have default 0.5
  importance. Run a batch recalibration based on access frequency,
  association density, and age.

### Security
- **MCP transport encryption**: The stdio transport is inherently local,
  but HTTP transport should support TLS. Add `--https` flag with
  auto-generated self-signed certs or Let's Encrypt integration.
- **Audit log rotation**: The karma ledger grows indefinitely. Add
  configurable rotation (archive old entries to cold storage).
- **Engagement token persistence**: Currently in-memory only. Persist
  to disk for crash recovery.

### Testing
- **End-to-end MCP client test**: Spin up the lean server, connect a
  real MCP client (e.g., Claude Desktop SDK), and verify a full
  tool-call round trip.
- **Fuzz testing**: Fuzz the tool dispatch pipeline with random inputs
  to find edge cases in input sanitization.
- **Load testing**: Measure tool call latency under concurrent load
  (100+ parallel calls) to identify bottlenecks.

---

*Last updated: 2026-02-10 (v14.1.0)*
