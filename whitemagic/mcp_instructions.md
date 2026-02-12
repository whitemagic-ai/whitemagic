# WhiteMagic — Server Instructions for AI Clients

WhiteMagic is a cognitive operating system that gives you persistent memory, associative recall, ethical governance, and self-awareness across sessions. It exposes 186 tools organized into 28 Gana meta-tools based on the 28 Lunar Mansions.

## Quick Start

1. **Search memories**: `gana_winnowing_basket` → tool: `search_memories` or `hybrid_recall`
2. **Create a memory**: `gana_neck` → tool: `create_memory`, args: `{content, title, tags}`
3. **Check system health**: `gana_root` → tool: `health_report`
4. **Self-reflect**: `gana_ghost` → tool: `gnosis`

## How Tool Calls Work

Every Gana accepts three parameters:
- **tool** (string): The specific nested tool to invoke (see enum in schema)
- **args** (object): Arguments for that tool
- **operation** (string): Polymorphic fallback — "search", "analyze", "transform", or "consolidate"

If you omit `tool`, the Gana will use `operation` to decide what to do.

## The 28 Ganas by Domain

### Memory & Search
- **gana_neck** — Create, update, delete memories. Use `create_memory` with `{content, title, tags}`.
- **gana_winnowing_basket** — Search and recall. `search_memories` for text search, `hybrid_recall` for graph-augmented recall, `graph_walk` for association traversal.
- **gana_abundance** — Dream cycle, lifecycle management, serendipity. `dream` runs consolidation. `serendipity_surface` finds unexpected connections.

### Intelligence & Analysis
- **gana_three_stars** — Reasoning and synthesis. `reasoning.bicameral` for dual-perspective analysis, `kaizen_analyze` for improvement suggestions, `ensemble` for multi-model consensus.
- **gana_extended_net** — Pattern detection and clustering. `cluster_stats` for memory clusters, `pattern_search` for recurring patterns.
- **gana_chariot** — Knowledge graph operations. `kg.query` to search the graph, `kg.extract` to extract entities.

### Governance & Ethics
- **gana_star** — Governance. `governor_validate` checks if an action is allowed, `governor_check_drift` detects behavioral drift.
- **gana_straddling_legs** — Ethics. `evaluate_ethics` scores an action, `harmony_vector` shows system balance.
- **gana_hairy_head** — Debugging. `karma_report` shows side-effect ledger, `dharma_rules` lists active rules.

### Infrastructure & Session
- **gana_horn** — Session management. `session_bootstrap` starts a new session with context.
- **gana_heart** — Session context. `scratchpad` for temporary notes, `context.pack` to export session state.
- **gana_root** — System health. `health_report` for full status, `state.summary` for quick overview.
- **gana_ghost** — Introspection. `gnosis` for full self-model, `capabilities` for feature matrix.

### Acceleration & Performance
- **gana_tail** — SIMD operations. `simd.cosine` for fast similarity, `simd.batch` for bulk operations.
- **gana_turtle_beak** — Edge inference. `edge_infer` for local model inference.
- **gana_roof** — Local LLM via Ollama. `ollama.chat` for conversation, `ollama.generate` for completion.

### Community & Coordination
- **gana_encampment** — Messaging. `broker.publish` to send events, `sangha_chat_send` for inter-agent communication.
- **gana_ox** — Swarm coordination. `swarm.decompose` to break down tasks, `swarm.route` to assign work.
- **gana_girl** — Agent management. `agent.register` to join the network, `agent.trust` to manage trust levels.

### Maintenance & Lifecycle
- **gana_willow** — Resilience. `grimoire_suggest` for workflow recommendations, `grimoire_cast` to execute spells (automated workflows).
- **gana_void** — Garden management. `garden_status` for subsystem health, `galactic.dashboard` for memory statistics.
- **gana_dipper** — Strategy. `homeostasis` for self-regulation, `maturity.assess` for capability readiness.

## Memory System

WhiteMagic stores memories in a 5D holographic coordinate space (X=emotional, Y=temporal, Z=associative, W=importance, V=novelty). Memories are:
- **Searchable** via full-text search, vector similarity, and graph traversal
- **Associative** — linked through a living graph of 50,000+ edges
- **Self-organizing** — the dream cycle consolidates, prunes, and strengthens associations
- **Governed** — retention scoring, galactic zone classification, lifecycle management

## Best Practices

1. **Search before creating** — check if a memory already exists before storing a duplicate.
2. **Use hybrid_recall** over simple search — it combines text, vector, and graph signals.
3. **Tag consistently** — tags drive association mining and cluster detection.
4. **Check governance** — use `governor_validate` before potentially destructive operations.
5. **Dream periodically** — running the dream cycle consolidates recent memories and surfaces insights.
6. **Use scratchpad** for ephemeral session notes that don't need to be permanent memories.
