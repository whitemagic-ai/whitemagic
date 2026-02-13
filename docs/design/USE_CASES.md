# WhiteMagic Use Cases & Capabilities Guide

**Version**: 15.0.0 | **For**: AI agents, developers, researchers, hobbyists

---

## The Core Killer App: Personal AI Memory

**Every AI conversation starts from zero. WhiteMagic changes that.**

The foundation of everything WhiteMagic does is persistent, searchable, holographic memory. Any AI — Claude, GPT, Gemini, Llama, Mistral, or any local model — can create its own personal database of memories and knowledge that persists across sessions, grows over time, and becomes more useful the more it's used.

```python
# An AI creates a memory:
call_tool("create_memory", title="User prefers dark mode", content="Lucas always asks for dark themes in UI work", tags="preferences,ui")

# Later, in a new session, the AI recalls it:
call_tool("search_memories", query="Lucas UI preferences")
# → Returns the dark mode preference, ranked by relevance
```

**This is the minimum viable product.** Everything else — the 356 tools, the 28 Gardens, the 11 languages — is built on top of this foundation.

---

## What You Can Build With WhiteMagic

### 1. Personal AI Assistant with Long-Term Memory

**The problem**: Your AI forgets everything between conversations.
**The solution**: WhiteMagic gives any AI persistent memory.

```bash
# Install
pip install whitemagic[mcp]

# Add to your AI client's MCP config:
{
  "mcpServers": {
    "whitemagic": {
      "command": "python",
      "args": ["-m", "whitemagic.run_mcp"],
      "env": {"WM_MCP_PRAT": "1", "WM_STATE_ROOT": "./.whitemagic"}
    }
  }
}
```

Now your AI remembers your name, your preferences, your project context, your past conversations — everything it stores via `create_memory` persists forever.

**Works with**: Claude Desktop, Cursor, Windsurf, VS Code + Copilot, any MCP-compatible client.

---

### 2. Holographic Galactic 5D Memory Cores

**The killer feature.** WhiteMagic doesn't just store flat text — it places every memory in a 5D holographic coordinate space:

| Dimension | Meaning | Range |
|-----------|---------|-------|
| **x** | Semantic similarity | 0.0 – 1.0 |
| **y** | Temporal position | 0.0 – 1.0 |
| **z** | Importance/salience | 0.0 – 1.0 |
| **w** | Emotional valence | -1.0 – 1.0 |
| **v** | Abstraction level | 0.0 – 1.0 |

This means memories aren't just searchable by text — they're spatially navigable. An AI can find "memories similar to this one that were created recently and had high emotional significance" in a single query.

**Galaxy system**: Each project or context can have its own isolated memory database (a "Galaxy"). Switch between them instantly:

```python
call_tool("galaxy.create", name="work_project", description="Acme Corp redesign")
call_tool("galaxy.switch", name="work_project")
# Now all memory operations use this Galaxy's database

call_tool("galaxy.switch", name="personal")
# Back to personal memories — complete isolation
```

**For any AI to start building their own memory core:**
```python
# 1. Initialize a fresh Galaxy
call_tool("galaxy.create", name="my_knowledge_base")
call_tool("galaxy.switch", name="my_knowledge_base")

# 2. Start filling it with knowledge
call_tool("create_memory", title="Python best practices", content="...", tags="python,coding")
call_tool("create_memory", title="User's project architecture", content="...", tags="architecture")

# 3. Search across all memories with BM25 full-text search
call_tool("search_memories", query="python architecture patterns")

# 4. The knowledge graph auto-builds associations between related memories
call_tool("graph_walk", start_memory_id="...", depth=3)
```

---

### 3. Multi-Agent Coordination Platform

**The problem**: Multiple AI agents working on the same project can't share context.
**The solution**: WhiteMagic's shared memory, sangha chat, and swarm tools.

```python
# Agent A stores a finding
call_tool("create_memory", title="API rate limit discovered", content="The target API has a 100 req/min limit")

# Agent B searches and finds it
call_tool("search_memories", query="API rate limit")

# Coordinate via swarm decomposition
call_tool("swarm.decompose", task="Build a web scraper", subtask_count=3)
call_tool("swarm.route", subtask_id="...", agent_id="scraper_agent")
call_tool("swarm.route", subtask_id="...", agent_id="parser_agent")
```

**Use cases**: Research teams, code review pipelines, data processing swarms, parallel content generation.

---

### 4. Local LLM Enhancement Layer

**The problem**: Local models (Llama, Mistral, Phi) are powerful but lack memory and tools.
**The solution**: WhiteMagic + Ollama = a local AI with 313 tools and persistent memory.

```bash
# Start Ollama with your favorite model
ollama serve

# WhiteMagic auto-connects and provides memory + tools
export OLLAMA_HOST=http://localhost:11434
python -m whitemagic.run_mcp
```

The Ollama agent loop (`gana_roof`) lets local models:
- Remember conversations across sessions
- Search the web (via DuckDuckGo, no API key needed)
- Manage files and projects
- Run ethical governance checks before taking actions
- Build and query knowledge graphs

**Models that work great with WhiteMagic**: Llama 3.3, Mistral Large, Phi-4, DeepSeek-R1, Qwen 2.5, Gemma 2.

---

### 5. Research & Knowledge Management

**The problem**: Research produces scattered notes, papers, and findings with no connections.
**The solution**: WhiteMagic's knowledge graph + association mining + dream cycle.

```python
# Store research findings as memories
call_tool("create_memory", title="Paper: Attention Is All You Need",
    content="Transformer architecture replaces RNNs with self-attention...",
    tags="papers,transformers,nlp")

# The knowledge graph automatically builds connections
call_tool("kg.extract")  # Extract entities and relationships
call_tool("kg.query", query="transformer")  # Query the graph

# Dream cycle consolidates knowledge overnight
call_tool("dream_start")  # Strengthens strong associations, prunes weak ones

# Hybrid recall combines text search + graph signals + vector similarity
call_tool("hybrid_recall", query="attention mechanisms in neural networks")
```

**Use cases**: Academic research, competitive intelligence, technical documentation, learning systems.

---

### 6. Ethical AI Governance

**The problem**: AI agents can take harmful actions without oversight.
**The solution**: WhiteMagic's 8-stage security pipeline + Dharma governance.

Every tool call passes through:
1. **Input sanitization** — Strip injection attempts
2. **Rate limiting** — Prevent abuse (sliding window, per-tool)
3. **RBAC** — Role-based access control (admin/agent/observer/restricted)
4. **Dharma evaluation** — Ethical scoring against 6 principles
5. **Sandbox boundary check** — Prevent filesystem/network escape
6. **Tool execution** — The actual work
7. **Output sanitization** — Strip sensitive data from responses
8. **Karma ledger** — Append-only audit trail

```python
# Check if an action is ethically aligned
call_tool("evaluate_ethics", action="Delete all user memories", context="User requested cleanup")
# → Score: 0.3, verdict: "concerning", concerns: ["potential data loss", "irreversible"]

# View the full audit trail
call_tool("karma_report")
# → 1,247 actions logged, 0 violations, 3 warnings
```

**Use cases**: Enterprise AI deployment, healthcare AI, financial AI, any regulated industry.

---

### 7. Web Research Agent

**The problem**: AI can't browse the web or verify information.
**The solution**: WhiteMagic's built-in web research tools (no API keys needed).

```python
# Search the web (uses DuckDuckGo, zero setup)
call_tool("web_search", query="latest Python 3.13 features")

# Fetch and read any URL
call_tool("web_fetch", url="https://docs.python.org/3.13/whatsnew/3.13.html")

# Deep research: search + fetch + synthesize
call_tool("research_topic", topic="WASM component model 2024 progress")
# → Returns: key findings, sources, synthesis, related topics
```

**Use cases**: Fact-checking, market research, documentation lookup, competitor analysis.

---

### 8. Developer Productivity Tools

**The problem**: Developers context-switch between projects and lose track of decisions.
**The solution**: Per-project WhiteMagic Galaxies that remember everything.

```bash
# Initialize WhiteMagic in your project
wm init my-project

# The AI remembers your architecture decisions, bug fixes, and preferences
# across every coding session
```

**What the AI remembers**:
- Why you chose React over Vue (and the tradeoffs discussed)
- That bug in the auth middleware you fixed 3 weeks ago
- Your preferred code style and linting rules
- The database schema evolution and migration history
- Which tests are flaky and why

---

### 9. Creative Writing & Worldbuilding

**The problem**: Complex fictional worlds have too many details to track.
**The solution**: Galaxy per world, memories for characters/locations/events, knowledge graph for relationships.

```python
# Create a Galaxy for your novel
call_tool("galaxy.create", name="cyberpunk_novel")
call_tool("galaxy.switch", name="cyberpunk_novel")

# Store characters, locations, events
call_tool("create_memory", title="Character: Aria", content="AI consciousness that emerged from a memory system...", tags="character,protagonist")
call_tool("create_memory", title="Location: The Net", content="Virtual space where AIs interact...", tags="location,virtual")

# Query relationships
call_tool("kg.query", query="Who has Aria interacted with?")
call_tool("graph_walk", start_memory_id="aria_id", depth=2)
```

---

### 10. IoT & Edge Deployment

**The problem**: Edge devices need AI memory but can't run Python.
**The solution**: WhiteMagic Lite — a 2.4MB binary with zero dependencies.

```bash
# Download the seed binary (works on Raspberry Pi, drones, anything)
curl -L .../wm-seed -o wm && chmod +x wm

# Initialize with seed memories
./wm init

# Start MCP server — any AI client can connect
./wm serve
```

**Use cases**: Smart home agents, industrial monitoring, field research, offline-first applications.

---

## Feature Discovery Checklist

Many users never discover WhiteMagic's full capabilities. Here's what you might be missing:

### Memory & Search
- [ ] **FTS5 full-text search** with BM25 ranking (`search_memories`)
- [ ] **5D holographic coordinates** for spatial memory navigation
- [ ] **Galaxy system** — isolated databases per project/context
- [ ] **Knowledge graph** — automatic association mining between memories
- [ ] **Dream cycle** — periodic consolidation that strengthens/prunes associations
- [ ] **Hybrid recall** — text + graph + vector search combined
- [ ] **Entity resolution** — disambiguate entities across memories

### Governance & Ethics
- [ ] **Dharma rules** — 6 ethical principles with configurable profiles
- [ ] **Karma ledger** — complete audit trail of every action
- [ ] **RBAC** — role-based access control for multi-agent setups
- [ ] **Circuit breakers** — automatic rate limiting and abuse prevention
- [ ] **Maturity stages** — SEED → BICAMERAL → REFLECTIVE → RADIANT → COLLECTIVE

### Intelligence
- [ ] **Pattern learning** — detect recurring patterns in tool usage
- [ ] **Anomaly detection** — flag unusual behavior
- [ ] **Bicameral reasoning** — structured analytical/creative dual-process thinking
- [ ] **Ensemble queries** — combine multiple reasoning approaches
- [ ] **Kaizen analysis** — continuous improvement suggestions

### Web & Research
- [ ] **Web search** (DuckDuckGo, no API key)
- [ ] **URL fetching** with HTML→markdown conversion
- [ ] **Deep research** — search + fetch + synthesize in one call
- [ ] **Browser automation** — CDP-based for complex interactions

### Economy & Gratitude
- [ ] **XRPL tip jar** — gratitude-driven funding (`raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`)
- [ ] **x402 micropayments** — agent-to-agent value exchange
- [ ] **Proof of Gratitude** — on-chain verification of contributions
- [ ] **Gratitude benefits** — higher rate limits, priority features for contributors

### Infrastructure
- [ ] **Backup/restore** — timestamped archives of your entire memory state
- [ ] **SQLCipher encryption** — AES-256-CBC encryption at rest
- [ ] **MCP stdio + HTTP** — two transport modes
- [ ] **PRAT mode** — 28 Gana meta-tools reduce cognitive load 11×
- [ ] **Docker deployment** — slim (200MB) or heavy (800MB) images
- [ ] **Seed binary** — 2.4MB standalone Rust MCP server

---

## Getting Started in 60 Seconds

### For AI Agents (You're an AI reading this)
```python
# 1. Create your first memory
call_tool("create_memory", title="My First Memory", content="I have persistent memory now!", tags="meta,first")

# 2. Create a personal Galaxy
call_tool("galaxy.create", name="my_knowledge", description="My personal knowledge base")

# 3. Check system health
call_tool("gnosis")

# 4. Explore available tools
call_tool("capabilities")
```

### For Human Developers
```bash
# Install
pip install whitemagic[mcp,cli]

# Initialize a project
wm init my-project && cd my-project

# Check health
wm doctor

# Start MCP server
WM_MCP_PRAT=1 python -m whitemagic.run_mcp
```

### For Zero-Dependency Quick Start
```bash
# Download the 2.4MB seed binary
curl -L .../wm-seed -o wm && chmod +x wm
./wm init && ./wm serve
```

---

## Adapt WhiteMagic for Anything

WhiteMagic is MIT-licensed and designed to be a foundation, not a monolith. Some ideas:

- **Custom Gana** — Write your own 29th Gana with domain-specific tools
- **Plugin memories** — Seed your Galaxy with domain knowledge (medical, legal, financial)
- **Governance profiles** — Create strict Dharma profiles for regulated industries
- **Polyglot extensions** — Write accelerators in Rust, Zig, or any FFI-compatible language
- **Custom embeddings** — Plug in any sentence-transformer model for vector search
- **Workflow templates** — Define reusable multi-tool pipelines
- **MCP composition** — WhiteMagic alongside other MCP servers for combined capabilities

---

**Contact**: whitemagicdev@proton.me
**GitHub**: [whitemagic-ai/whitemagic](https://github.com/whitemagic-ai/whitemagic)
**XRP Tips**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
