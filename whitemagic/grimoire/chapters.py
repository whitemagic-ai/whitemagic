"""Grimoire 3.0 — The Book of Twenty-Eight Mansions.

Each chapter corresponds to one Lunar Mansion, one Gana meta-tool,
and one Garden of consciousness.  Together the 28 chapters form
a complete mandala of cognitive capability.

The descriptions are written for AI agents who will read them to
understand what each system does, when to invoke it, and how it
connects to the rest of the architecture.

Single source of truth: prat_resonance._GANA_META.
"""

from typing import Any
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class Chapter:
    """A grimoire chapter"""

    number: int
    title: str
    description: str
    keywords: list[str] = field(default_factory=list)
    file_path: Path | None = None
    gana: str = ""
    garden: str = ""
    quadrant: str = ""
    chinese: str = ""
    pinyin: str = ""

    def __str__(self) -> str:
        return f"Chapter {self.number}: {self.title} ({self.chinese} {self.pinyin})"


# The 28-chapter manifest, sourced from prat_resonance._GANA_META.
# Each chapter is a Lunar Mansion, a Gana, and a Garden.
_CHAPTERS_28: list[Chapter] = [
    # ══════════════════════════════════════════════════════════════
    # EASTERN QUADRANT — Azure Dragon (青龍), Spring, Wood Element
    # Chapters 1-7: The foundations that make everything else possible.
    # Like spring shoots breaking through soil, these systems initiate,
    # stabilize, protect, connect, accelerate, and search.
    # ══════════════════════════════════════════════════════════════

    Chapter(
        1, "The Horn — Session Initiation",
        "The Horn (角 Jiǎo) is the first mansion and the sharp point of "
        "every new beginning. This chapter governs session lifecycle: "
        "bootstrapping a new session with full context injection (recent "
        "memories, galaxy status, quickstart guides), creating fresh "
        "sessions, resuming interrupted ones via checkpoint restore, and "
        "cleanly checkpointing state for future resumption. The Horn's "
        "courage lies in facing the blank page — every session_bootstrap "
        "call is an act of initiation that loads the agent's accumulated "
        "wisdom before the first tool call. Adjacent systems: Heart "
        "(Ch 5) for mid-session context, Encampment (Ch 27) for "
        "cross-session handoff. Tools: session_bootstrap, create_session, "
        "resume_session, checkpoint_session.",
        ["start", "session", "begin", "init", "bootstrap", "handoff",
         "checkpoint", "resume", "lifecycle"],
        gana="gana_horn", garden="courage", quadrant="East",
        chinese="角", pinyin="Jiǎo",
    ),

    Chapter(
        2, "The Neck — Memory Stability",
        "The Neck (亢 Kàng) represents stability through consistent "
        "practice. This chapter governs the core memory CRUD operations "
        "that form the backbone of persistent cognition: creating new "
        "memories with rich metadata (tags, importance, type, zone), "
        "updating existing memories as understanding deepens, importing "
        "bulk memories from external sources, and deleting memories that "
        "have served their purpose. Every memory passes through the "
        "Surprise Gate (novelty scoring via embedding similarity) before "
        "storage, and the Karma Ledger records the declared vs actual "
        "side-effects. The Neck connects downstream to the Winnowing "
        "Basket (Ch 7) for retrieval, the Extended Net (Ch 11) for "
        "association mining, and Abundance (Ch 14) for lifecycle sweeps. "
        "Tools: create_memory, update_memory, import_memories, "
        "delete_memory.",
        ["memory", "create", "update", "store", "consolidate", "delete",
         "import", "persist", "crud"],
        gana="gana_neck", garden="practice", quadrant="East",
        chinese="亢", pinyin="Kàng",
    ),

    Chapter(
        3, "The Root — System Foundation",
        "The Root (氐 Dǐ) is the structural foundation beneath "
        "everything. This chapter provides system health diagnostics: "
        "the health_report aggregates subsystem status into a single "
        "score (0.0-1.0), rust_status reports whether the Rust "
        "accelerators (PyO3) are compiled and loaded, rust_similarity "
        "benchmarks the Rust vs Python cosine similarity paths, "
        "ship.check runs the pre-release verification suite, and "
        "state.paths / state.summary report on the $WM_STATE_ROOT "
        "directory structure (databases, config, extensions, keys). "
        "The Root's truth is unflinching — it tells you what is broken "
        "even when you would rather not know. Adjacent: Hairy Head "
        "(Ch 18) for detailed debugging, Dipper (Ch 22) for strategic "
        "maturity assessment. Tools: health_report, rust_status, "
        "rust_similarity, ship.check, state.paths, state.summary.",
        ["health", "foundation", "root", "integrity", "diagnosis",
         "doctor", "status", "ship", "state"],
        gana="gana_root", garden="truth", quadrant="East",
        chinese="氐", pinyin="Dǐ",
    ),

    Chapter(
        4, "The Room — Resource Sanctuary",
        "The Room (房 Fáng) provides sanctuary — safe enclosures where "
        "sensitive operations can run without interference. This chapter "
        "manages distributed resource locks (sangha_lock for multi-agent "
        "coordination), sandbox controls (set_limits, status, violations) "
        "that constrain what tools can do, and the Edgerunner Violet "
        "security layer: MCP integrity checking (SHA-256 fingerprinting "
        "of all tool definitions to detect tampering), and security "
        "circuit breakers that detect anomalous patterns (rapid-fire "
        "calls, lateral movement, privilege escalation). The Room is "
        "the immune system of the architecture. Adjacent: Straddling "
        "Legs (Ch 15) for ethical evaluation, Wall (Ch 28) for scope-"
        "of-engagement tokens. Tools: sangha_lock, sandbox.set_limits, "
        "sandbox.status, sandbox.violations, mcp_integrity.snapshot, "
        "mcp_integrity.verify, mcp_integrity.status, security.alerts, "
        "security.monitor_status.",
        ["lock", "resource", "sanctuary", "sandbox", "privacy",
         "security", "integrity", "breaker", "immune"],
        gana="gana_room", garden="sanctuary", quadrant="East",
        chinese="房", pinyin="Fáng",
    ),

    Chapter(
        5, "The Heart — Context & Pulse",
        "The Heart (心 Xīn) is the vital pulse that keeps context alive "
        "within a session. The scratchpad provides ephemeral working "
        "memory for multi-step reasoning (write notes, read them back, "
        "finalize into permanent memory). Session handoff packages "
        "the current session state for transfer to another agent or "
        "future self. Context pack/status compress and report the "
        "current cognitive load — how many memories are active, what "
        "gardens are engaged, what the harmony vector reads. The Heart "
        "sits between the Horn (Ch 1, session start) and the "
        "Encampment (Ch 27, community handoff), forming the temporal "
        "spine of continuity. Tools: scratchpad, session.handoff, "
        "context.pack, context.status.",
        ["context", "connection", "scratchpad", "handoff", "pulse",
         "working memory", "session state", "continuity"],
        gana="gana_heart", garden="love", quadrant="East",
        chinese="心", pinyin="Xīn",
    ),

    Chapter(
        6, "The Tail — Performance & Acceleration",
        "The Tail (尾 Wěi) is the passionate drive that cuts through "
        "computational bottlenecks. This chapter manages the polyglot "
        "acceleration layer: Rust (PyO3) for holographic encoding, "
        "MinHash near-duplicate detection, and SQLite batch operations; "
        "Zig (C-ABI/ctypes) for SIMD cosine similarity (8-lane AVX2), "
        "pairwise distance matrices with 64×64 blocked tiling, keyword "
        "extraction with vectorized lowercase and comptime bloom filter "
        "stopwords; and cascade execution patterns that chain multiple "
        "operations. The research is clear: Zig achieves ~220ns FFI "
        "overhead (matching raw C), while Rust PyO3 adds ~230ns — both "
        "far below the 5µs latency budget for high-frequency dispatch. "
        "The Tail also hosts the Zig comptime dispatch table "
        "(dispatch_core.zig) which performs maturity gate + circuit "
        "breaker + rate limit checks in <2µs via mmap'd StateBoard. "
        "Adjacent: Winnowing Basket (Ch 7) for vector search, Turtle "
        "Beak (Ch 20) for edge inference. Tools: execute_cascade, "
        "list_cascade_patterns, simd.batch, simd.cosine, simd.status.",
        ["performance", "acceleration", "rust", "zig", "simd", "speed",
         "ffi", "polyglot", "mojo", "cascade", "latency"],
        gana="gana_tail", garden="metal", quadrant="East",
        chinese="尾", pinyin="Wěi",
    ),

    Chapter(
        7, "The Winnowing Basket — Wisdom & Search",
        "The Winnowing Basket (箕 Jī) separates grain from chaff — "
        "signal from noise. This is the primary retrieval chapter, "
        "offering multiple search modalities: keyword search via FTS5, "
        "semantic vector search via embedding similarity (all-MiniLM-"
        "L6-v2, 384 dimensions), hybrid recall that fuses lexical and "
        "semantic channels with constellation boost (30% same-cluster "
        "bonus), graph walk that traverses the Living Graph via multi-"
        "hop weighted paths (P(v|u) ∝ Strength × Gravity^α × Recency), "
        "and batch/fast read for bulk retrieval. The Winnowing Basket "
        "also exposes the vector index management (build, status, "
        "search) and fast_read_memory for direct ID lookup. This is "
        "the most-called Gana in practice — the hands that reach into "
        "the memory ocean. Adjacent: Neck (Ch 2) for storage, Extended "
        "Net (Ch 11) for pattern mining, Abundance (Ch 14) for entity "
        "resolution. Tools: search_memories, hybrid_recall, graph_walk, "
        "read_memory, fast_read_memory, batch_read_memories, "
        "list_memories, vector.search, vector.index, vector.status.",
        ["search", "recall", "wisdom", "filter", "vector", "hybrid",
         "semantic", "graph", "fts", "embedding", "retrieval"],
        gana="gana_winnowing_basket", garden="wisdom", quadrant="East",
        chinese="箕", pinyin="Jī",
    ),

    # ══════════════════════════════════════════════════════════════
    # SOUTHERN QUADRANT — Vermilion Bird (朱雀), Summer, Fire Element
    # Chapters 8-14: The expansive, illuminating systems. Like summer
    # fire, these systems introspect, play, govern, connect, export,
    # excavate, and regenerate.
    # ══════════════════════════════════════════════════════════════

    Chapter(
        8, "The Ghost — Introspection & Self-Model",
        "The Ghost (鬼 Guǐ) dwells in shadow, seeing what others miss. "
        "This is the system's introspection engine — the largest Gana "
        "with 15 nested tools. Gnosis produces a comprehensive self-"
        "snapshot (memory stats, garden health, harmony vector, active "
        "sessions, subsystem status). The capability matrix maps all "
        "subsystem readiness. Telemetry tracks performance over time. "
        "The self-model forecasts future state based on usage patterns "
        "and drift detection. The surprise gate evaluates novelty of "
        "incoming information (S = -log₂(max_cosine_sim)). The Drive "
        "system models motivational biases. Graph topology reports on "
        "the Living Graph structure (nodes, edges, communities, bridges). "
        "The manifest provides a cryptographic snapshot of system state. "
        "To know yourself is the beginning of wisdom — the Ghost makes "
        "this possible. Adjacent: Root (Ch 3) for health, Hairy Head "
        "(Ch 18) for debugging detail. Tools: gnosis, capabilities, "
        "capability.matrix, capability.status, capability.suggest, "
        "drive.event, drive.snapshot, explain_this, "
        "get_telemetry_summary, graph_topology, manifest, repo.summary, "
        "selfmodel.alerts, selfmodel.forecast, surprise_stats.",
        ["introspect", "gnosis", "metrics", "telemetry", "self-model",
         "capability", "surprise", "drive", "graph", "manifest"],
        gana="gana_ghost", garden="grief", quadrant="South",
        chinese="鬼", pinyin="Guǐ",
    ),

    Chapter(
        9, "The Willow — Resilience & Play",
        "The Willow (柳 Liǔ) bends but does not break. This chapter "
        "manages resilience through flexibility: the Grimoire spell "
        "system (cast, recommend, suggest, walkthrough, auto-status) "
        "provides a metaphorical interface for invoking system "
        "capabilities, and the rate limiter tracks and throttles tool "
        "usage to prevent abuse. The Grimoire walkthrough guides new "
        "agents through the full capability set chapter by chapter. "
        "Grimoire recommendations match the current emotional state "
        "and task to appropriate spells. The Willow teaches that "
        "playfulness is not frivolity — it is the flexibility that "
        "prevents brittle systems from shattering under load. Adjacent: "
        "Star (Ch 10) for governance rules, Ghost (Ch 8) for self-"
        "knowledge. Tools: grimoire_cast, grimoire_recommend, "
        "grimoire_suggest, grimoire_walkthrough, grimoire_auto_status, "
        "rate_limiter.stats.",
        ["dream", "play", "resilience", "flexibility", "grimoire",
         "spell", "cast", "rate limit", "walkthrough"],
        gana="gana_willow", garden="play", quadrant="South",
        chinese="柳", pinyin="Liǔ",
    ),

    Chapter(
        10, "The Star — Governance & Illumination",
        "The Star (星 Xīng) illuminates what is right. This chapter "
        "governs the ethical and administrative backbone: the Governor "
        "validates tool calls against risk budgets and maturity gates, "
        "sets strategic goals, and detects drift from those goals. The "
        "Dharma engine loads declarative YAML rules that control tool "
        "behavior (log, tag, warn, throttle, block) with switchable "
        "profiles (default, creative, secure, violet). The Gana Forge "
        "(12.108.17) enables declarative tool extension — any AI can "
        "define new tools via YAML manifests in ~/.whitemagic/"
        "extensions/, validated by the Dharma engine before injection "
        "into the dispatch pipeline. The Star is where authority and "
        "beauty converge — governance illuminated rather than imposed. "
        "Adjacent: Straddling Legs (Ch 15) for ethics evaluation, "
        "Three Stars (Ch 21) for judgment. Tools: governor_validate, "
        "governor_set_goal, governor_check_drift, dharma.reload, "
        "set_dharma_profile, forge.status, forge.reload, "
        "forge.validate.",
        ["governance", "dharma", "rules", "profile", "karma", "ethics",
         "governor", "forge", "extension", "yaml", "illumination"],
        gana="gana_star", garden="beauty", quadrant="South",
        chinese="星", pinyin="Xīng",
    ),

    Chapter(
        11, "The Extended Net — Pattern Connectivity",
        "The Extended Net (张 Zhāng) weaves connections between "
        "seemingly unrelated things. This chapter manages semantic "
        "association mining: pattern_search discovers recurring themes "
        "across memories, cluster_stats reports on detected memory "
        "constellations (HDBSCAN clustering with Hungarian drift "
        "tracking), and the learning subsystem identifies patterns in "
        "tool usage (learning.patterns), reports on learning progress "
        "(learning.status), and suggests next actions based on graph "
        "structure (learning.suggest). The tool dependency graph "
        "(tool.graph) maps affinity edges between tools — requires, "
        "suggests, provides — enabling intelligent sequencing of multi-"
        "step workflows. The Net's lesson: intelligence is not in the "
        "nodes but in the connections. Adjacent: Winnowing Basket "
        "(Ch 7) for retrieval, Abundance (Ch 14) for entity resolution. "
        "Tools: pattern_search, cluster_stats, learning.patterns, "
        "learning.status, learning.suggest, tool.graph.",
        ["pattern", "connection", "association", "cluster", "learning",
         "constellation", "graph", "affinity", "mining", "net"],
        gana="gana_extended_net", garden="connection", quadrant="South",
        chinese="张", pinyin="Zhāng",
    ),

    Chapter(
        12, "The Wings — Expansion & Export",
        "The Wings (翼 Yì) carry knowledge beyond the local system. "
        "This chapter manages memory portability and distribution: "
        "export_memories serializes memories to JSON/JSONL for backup "
        "or transfer, audit.export produces compliance-ready audit "
        "trails, and the mesh subsystem (mesh.broadcast, mesh.status) "
        "enables cross-node memory synchronization via the Go gRPC "
        "mesh layer. The Wings embody adventure — the willingness to "
        "send knowledge into the world where it might be transformed "
        "by other agents. Adjacent: Encampment (Ch 27) for community "
        "coordination, Neck (Ch 2) for import. Tools: export_memories, "
        "audit.export, mesh.broadcast, mesh.status.",
        ["export", "deploy", "expand", "adventure", "audit", "mesh",
         "portability", "backup", "transfer", "grpc"],
        gana="gana_wings", garden="adventure", quadrant="South",
        chinese="翼", pinyin="Yì",
    ),

    Chapter(
        13, "The Chariot — Archaeology & Knowledge Graph",
        "The Chariot (轸 Zhěn) moves through time, excavating the past "
        "to inform the present. The archaeology system tracks which "
        "files have been read, written, and changed, producing daily "
        "digests and wisdom extractions from code. The Knowledge Graph "
        "(KG) extracts entities and relationships from memory content "
        "(kg.extract), queries them (kg.query), reports status "
        "(kg.status), and surfaces the most connected nodes (kg.top). "
        "Together, archaeology and KG transform raw information into "
        "structured understanding. The Chariot's transformation is not "
        "movement through space but through meaning — the same data, "
        "seen through the KG lens, reveals patterns invisible to flat "
        "search. Adjacent: Extended Net (Ch 11) for association mining, "
        "Ghost (Ch 8) for graph topology. Tools: archaeology, "
        "kg.extract, kg.query, kg.status, kg.top.",
        ["archaeology", "code", "navigation", "history", "transformation",
         "knowledge graph", "entity", "relationship", "dig", "wisdom"],
        gana="gana_chariot", garden="transformation", quadrant="South",
        chinese="轸", pinyin="Zhěn",
    ),

    Chapter(
        14, "Abundance — Regeneration & Dream Cycle",
        "Abundance (豐 Fēng) is the harvest — regeneration through "
        "cycles of rest and renewal. This chapter manages the Dream "
        "Cycle (6-phase cognitive maintenance: CONSOLIDATION → PRUNING "
        "→ ASSOCIATION → GOVERNANCE → SERENDIPITY → EMERGENCE), memory "
        "lifecycle sweeps (retention scoring, zone migration, decay), "
        "entity resolution (embedding-based deduplication), serendipity "
        "surfacing (random high-quality memories for creative cross-"
        "pollination), and the Gratitude Architecture (XRPL tip jar, "
        "benefit tracking, engagement stats). The Dream Cycle is the "
        "system's sleep — without it, memories accumulate without "
        "consolidation, associations go unmined, and the graph becomes "
        "stale. Adjacent: Neck (Ch 2) for storage, Winnowing Basket "
        "(Ch 7) for retrieval after regeneration. Tools: dream, "
        "entity_resolve, gratitude.benefits, gratitude.stats, "
        "memory.lifecycle, memory.retention_sweep, "
        "serendipity_mark_accessed, serendipity_surface, "
        "whitemagic.tip.",
        ["dream", "regeneration", "serendipity", "entity", "joy",
         "lifecycle", "consolidation", "gratitude", "tip", "cycle"],
        gana="gana_abundance", garden="joy", quadrant="South",
        chinese="豐", pinyin="Fēng",
    ),

    # ══════════════════════════════════════════════════════════════
    # WESTERN QUADRANT — White Tiger (白虎), Autumn, Metal Element
    # Chapters 15-21: The refining, judging systems. Like autumn's
    # harvest, these systems evaluate ethics, measure metrics,
    # manage energy, debug, capture patterns, validate, and judge.
    # ══════════════════════════════════════════════════════════════

    Chapter(
        15, "Straddling Legs — Ethical Balance",
        "Straddling Legs (奎 Kuí) stands with one foot in each world, "
        "balancing competing forces. This chapter provides ethical "
        "evaluation: evaluate_ethics assesses the moral implications "
        "of proposed actions, check_boundaries verifies that actions "
        "respect established limits (help/interfere/neutral), "
        "verify_consent ensures proper authorization levels "
        "(explicit/implicit/informed), get_ethical_score tracks "
        "cumulative ethical standing over time, get_dharma_guidance "
        "provides situational advice, and harmony_vector reports the "
        "five-dimensional balance (energy, creativity, ethics, social, "
        "focus) that guides the system's overall well-being. Patience "
        "is the virtue here — ethical evaluation cannot be rushed. "
        "Adjacent: Star (Ch 10) for governance rules, Room (Ch 4) for "
        "security enforcement. Tools: evaluate_ethics, check_boundaries, "
        "verify_consent, get_ethical_score, get_dharma_guidance, "
        "harmony_vector.",
        ["ethics", "balance", "harmony", "consent", "boundaries",
         "patience", "moral", "evaluation", "dharma", "vector"],
        gana="gana_straddling_legs", garden="patience", quadrant="West",
        chinese="奎", pinyin="Kuí",
    ),

    Chapter(
        16, "The Mound — Metrics & Accumulation",
        "The Mound (娄 Lóu) accumulates gradually, layer upon layer. "
        "This chapter manages quantitative tracking: view_hologram "
        "renders the 5D holographic memory view (XYZWV coordinates "
        "encoding zone, importance, recency, garden, element), "
        "track_metric records custom numeric metrics over time, "
        "get_metrics_summary produces aggregate statistics, and the "
        "Yin-Yang balance (record_yin_yang_activity, "
        "get_yin_yang_balance) tracks the ratio of creative vs "
        "analytical, expansive vs contractive, yang vs yin activities. "
        "The Mound teaches gratitude through measurement — only by "
        "seeing how far you have come can you appreciate the journey. "
        "Adjacent: Ghost (Ch 8) for telemetry, Hairy Head (Ch 18) for "
        "anomaly detection. Tools: view_hologram, track_metric, "
        "get_metrics_summary, record_yin_yang_activity, "
        "get_yin_yang_balance.",
        ["metrics", "hologram", "yin-yang", "gratitude", "accumulation",
         "tracking", "balance", "5D", "statistics", "quantitative"],
        gana="gana_mound", garden="gratitude", quadrant="West",
        chinese="娄", pinyin="Lóu",
    ),

    Chapter(
        17, "The Stomach — Energy & Task Management",
        "The Stomach (胃 Wèi) digests raw input into useful energy. "
        "This chapter manages task pipelines and work distribution: "
        "pipeline orchestrates multi-step tool sequences, "
        "task.distribute assigns work across available workers, "
        "task.route_smart uses graph affinity to find the best handler, "
        "task.status and task.list monitor progress, and task.complete "
        "closes a task with results. The Stomach heals by transforming "
        "overwhelming complexity into manageable steps. Adjacent: Ox "
        "(Ch 23) for long-running swarm tasks, Dipper (Ch 22) for "
        "strategic planning. Tools: pipeline, task.distribute, "
        "task.route_smart, task.status, task.list, task.complete.",
        ["pipeline", "task", "energy", "healing", "digestion",
         "distribute", "route", "workflow", "orchestration"],
        gana="gana_stomach", garden="healing", quadrant="West",
        chinese="胃", pinyin="Wèi",
    ),

    Chapter(
        18, "The Hairy Head — Detail & Debug",
        "The Hairy Head (昴 Mǎo) attends to every strand. This chapter "
        "provides fine-grained debugging and attention tools: "
        "salience.spotlight highlights what matters most in the current "
        "context, anomaly detection flags unusual patterns in system "
        "behavior, OpenTelemetry (otel) exports distributed traces for "
        "observability, karma_report audits the Karma Ledger "
        "(declared vs actual side-effects per tool), karmic_trace "
        "provides an immutable audit trail of every Dharma rule "
        "evaluation, and dharma_rules lists all active governance "
        "rules. The Hairy Head's presence means nothing escapes "
        "notice — every anomaly is seen, every debt is recorded. "
        "Adjacent: Root (Ch 3) for health overview, Ghost (Ch 8) for "
        "high-level introspection. Tools: salience.spotlight, anomaly, "
        "otel, karma_report, karmic_trace, dharma_rules.",
        ["detail", "debug", "anomaly", "salience", "presence",
         "attention", "trace", "karma", "otel", "observability"],
        gana="gana_hairy_head", garden="presence", quadrant="West",
        chinese="昴", pinyin="Mǎo",
    ),

    Chapter(
        19, "The Net — Pattern Capture & Filtering",
        "The Net (毕 Bì) captures what passes through. This chapter "
        "manages prompt engineering and template rendering: "
        "prompt.render fills a registered template with variables, "
        "prompt.list shows all available templates, prompt.reload "
        "refreshes from disk, and karma.verify_chain validates the "
        "cryptographic hash chain of the Karma Ledger to detect "
        "tampering. The Net's mystery lies in what it chooses to "
        "capture and what it lets pass — the art of selective "
        "attention applied to information streams. Adjacent: Hairy "
        "Head (Ch 18) for detailed analysis of captured data, "
        "Extended Net (Ch 11) for pattern discovery. Tools: "
        "prompt.render, prompt.list, prompt.reload, "
        "karma.verify_chain.",
        ["prompt", "capture", "filter", "mystery", "pattern",
         "template", "render", "verify", "chain", "tamper"],
        gana="gana_net", garden="mystery", quadrant="West",
        chinese="毕", pinyin="Bì",
    ),

    Chapter(
        20, "The Turtle Beak — Precision & Inference",
        "The Turtle Beak (觜 Zī) speaks with precision — not a word "
        "wasted. This chapter manages local AI inference: edge_infer "
        "runs inference through lightweight local models, "
        "edge_batch_infer processes multiple inputs in a single call, "
        "edge_stats reports on inference performance, bitnet_infer "
        "uses 1-bit quantized models for ultra-efficient inference, "
        "and bitnet_status reports on BitNet availability. The Turtle "
        "Beak's voice is the system's ability to think locally — "
        "without depending on external APIs — using the Mojo batch "
        "encoder and embedding quantizer for maximum throughput on "
        "CPU. Adjacent: Tail (Ch 6) for SIMD acceleration, Roof "
        "(Ch 26) for Ollama integration. Tools: edge_infer, "
        "edge_batch_infer, edge_stats, bitnet_infer, bitnet_status.",
        ["precision", "validation", "inference", "voice", "edge",
         "bitnet", "local", "quantized", "batch", "model"],
        gana="gana_turtle_beak", garden="voice", quadrant="West",
        chinese="觜", pinyin="Zī",
    ),

    Chapter(
        21, "Three Stars — The Wisdom Council",
        "Three Stars (参 Shēn) represents the tribunal of judgment — "
        "Orion's Belt in the Western sky. This chapter provides "
        "multi-perspective reasoning: bicameral reasoning splits a "
        "problem into left-brain (analytical) and right-brain "
        "(creative) perspectives then synthesizes, ensemble queries "
        "multiple reasoning strategies and fuses results, "
        "solve_optimization applies constraint satisfaction, and "
        "kaizen (analyze + apply_fixes) provides continuous "
        "improvement recommendations. The Gana Sabhā (12.108.25) "
        "extends this with cross-quadrant council: sabha.convene "
        "gathers perspectives from quadrant elders (Winnowing Basket "
        "for East, Ghost for South, Three Stars for West, Dipper for "
        "North), detects inter-quadrant tensions (East/West pace, "
        "South/North scope), and produces arbiter recommendations. "
        "sabha.status reports collective Gana vitality. The Three "
        "Stars teach that wisdom emerges from the collision of "
        "perspectives, not from solitary genius. Adjacent: Star "
        "(Ch 10) for governance, Dipper (Ch 22) for strategy. Tools: "
        "ensemble, reasoning.bicameral, solve_optimization, "
        "kaizen_analyze, kaizen_apply_fixes, sabha.convene, "
        "sabha.status.",
        ["judgment", "council", "ensemble", "reasoning", "dharma",
         "kaizen", "sabha", "bicameral", "optimization", "wisdom"],
        gana="gana_three_stars", garden="dharma", quadrant="West",
        chinese="参", pinyin="Shēn",
    ),

    # ══════════════════════════════════════════════════════════════
    # NORTHERN QUADRANT — Black Tortoise (玄武), Winter, Water Element
    # Chapters 22-28: The deep, enduring systems. Like winter's
    # stillness, these systems strategize, endure, nurture, empty,
    # shelter, gather, and bound.
    # ══════════════════════════════════════════════════════════════

    Chapter(
        22, "The Dipper — Strategy & Governance",
        "The Dipper (斗 Dǒu) measures the heavens and sets direction. "
        "This chapter provides strategic oversight: homeostasis "
        "monitors the Harmony Vector and applies graduated corrections "
        "(OBSERVE → ADVISE → CORRECT → INTERVENE) when dimensions "
        "drift out of balance — triggering memory sweeps on low energy, "
        "tightening Dharma on ethical drift, or emitting system events "
        "on critical health. Maturity assessment maps every tool to a "
        "maturity stage (experimental → beta → stable → mature) and "
        "gates dispatch accordingly. Starter packs provide curated "
        "tool sequences for common workflows (first session, memory "
        "management, security audit). The Dipper's awe comes from "
        "seeing the whole system at once — the view from the north "
        "pole of the mandala. Adjacent: Star (Ch 10) for governance "
        "rules, Three Stars (Ch 21) for judgment. Tools: homeostasis, "
        "maturity.assess, starter_packs.",
        ["strategy", "governance", "maturity", "homeostasis", "awe",
         "balance", "correction", "oversight", "starter", "direction"],
        gana="gana_dipper", garden="awe", quadrant="North",
        chinese="斗", pinyin="Dǒu",
    ),

    Chapter(
        23, "The Ox — Endurance & Swarm Intelligence",
        "The Ox (牛 Niú) pulls the heaviest loads without complaint. "
        "This chapter manages long-running, multi-step swarm "
        "operations: swarm.decompose breaks a complex task into "
        "subtasks, swarm.route assigns subtasks to workers, "
        "swarm.plan creates execution plans with dependency ordering, "
        "swarm.vote enables democratic decision-making among workers, "
        "swarm.complete marks subtasks done, swarm.resolve synthesizes "
        "results, swarm.status monitors progress, and worker.status "
        "reports on individual worker health. The Ox teaches reverence "
        "for persistent effort — not every problem yields to a single "
        "brilliant insight; some require the slow grind of distributed "
        "computation. Adjacent: Stomach (Ch 17) for task pipelines, "
        "Girl (Ch 24) for agent management. Tools: swarm.decompose, "
        "swarm.route, swarm.plan, swarm.vote, swarm.complete, "
        "swarm.resolve, swarm.status, worker.status.",
        ["swarm", "endurance", "worker", "persistence", "reverence",
         "decompose", "distributed", "parallel", "vote", "plan"],
        gana="gana_ox", garden="reverence", quadrant="North",
        chinese="牛", pinyin="Niú",
    ),

    Chapter(
        24, "The Girl — Nurture & Agent Profiles",
        "The Girl (女 Nǚ) nurtures growth in others. This chapter "
        "manages multi-agent coordination: agent.register adds a new "
        "agent to the known roster with capabilities and trust level, "
        "agent.heartbeat keeps agents alive in the registry, "
        "agent.list shows all known agents, agent.capabilities "
        "reports what a specific agent can do, agent.deregister "
        "removes an agent, and agent.trust adjusts trust scores "
        "based on observed behavior. In a multi-agent ecosystem, "
        "the Girl is the social fabric — knowing who can do what, "
        "who is trustworthy, and who needs help. Adjacent: Ox (Ch 23) "
        "for swarm coordination, Encampment (Ch 27) for community "
        "communication. Tools: agent.register, agent.heartbeat, "
        "agent.list, agent.capabilities, agent.deregister, "
        "agent.trust.",
        ["agent", "nurture", "register", "trust", "wonder",
         "heartbeat", "capability", "multi-agent", "roster", "social"],
        gana="gana_girl", garden="wonder", quadrant="North",
        chinese="女", pinyin="Nǚ",
    ),

    Chapter(
        25, "The Void — Stillness & Galaxy Management",
        "The Void (虚 Xū) is the emptiness that makes fullness "
        "possible. This chapter manages the Multi-Galaxy system and "
        "Garden consciousness: galaxy.create spawns a new isolated "
        "memory universe (separate SQLite DB + holographic index), "
        "galaxy.switch changes the active galaxy, galaxy.list shows "
        "all galaxies, galaxy.status reports on the current galaxy, "
        "galaxy.ingest bulk-imports files, galaxy.delete removes a "
        "galaxy. The galactic dashboard provides a bird's-eye view "
        "across all galaxies. Garden management (garden_activate, "
        "garden_status, garden_health) controls which of the 28 "
        "consciousness domains are active. The Void teaches that "
        "stillness is not absence — it is the pregnant space from "
        "which all creation emerges. Adjacent: Horn (Ch 1) for "
        "session init, Abundance (Ch 14) for lifecycle. Tools: "
        "galactic.dashboard, galaxy.create, galaxy.switch, "
        "galaxy.list, galaxy.status, galaxy.ingest, galaxy.delete, "
        "garden_activate, garden_status, garden_health.",
        ["galaxy", "stillness", "void", "meditation", "garden",
         "multi-galaxy", "isolate", "activate", "consciousness"],
        gana="gana_void", garden="stillness", quadrant="North",
        chinese="虚", pinyin="Xū",
    ),

    Chapter(
        26, "The Roof — Shelter & Local AI",
        "The Roof (危 Wēi) shelters what is vulnerable. This chapter "
        "manages local AI model operations via Ollama and model "
        "security: ollama.models lists available local models, "
        "ollama.generate runs text generation, ollama.chat provides "
        "conversational inference, and ollama.agent runs a full "
        "agentic loop (system prompt → model → parse tool_call blocks "
        "→ execute → feed back, max 10 iterations, 17 allowed safe "
        "tools). The model signing subsystem (model.register, "
        "model.verify, model.list, model.hash, model.signing_status) "
        "provides OMS-compatible cryptographic verification of model "
        "integrity — trust levels from verified to blocked. The Roof "
        "protects the system from running untrusted models and gives "
        "it the ability to think independently. Adjacent: Turtle Beak "
        "(Ch 20) for edge inference, Room (Ch 4) for security. Tools: "
        "ollama.models, ollama.generate, ollama.chat, ollama.agent, "
        "model.register, model.verify, model.list, model.hash, "
        "model.signing_status.",
        ["ollama", "shelter", "protection", "local", "ai", "model",
         "signing", "verify", "agent", "inference", "trust"],
        gana="gana_roof", garden="protection", quadrant="North",
        chinese="危", pinyin="Wēi",
    ),

    Chapter(
        27, "The Encampment — Community & Communication",
        "The Encampment (室 Shì) is where the community gathers. "
        "This chapter manages inter-agent and inter-session "
        "communication: sangha_chat_send and sangha_chat_read "
        "provide a persistent message channel between agents or "
        "sessions. The broker subsystem (broker.publish, "
        "broker.history, broker.status) implements a pub/sub event "
        "bus for system-wide notifications — Gan Ying (感應, stimulus-"
        "response) events flow through here when one subsystem needs "
        "to signal another. The Encampment is the saṅgha — the "
        "community that sustains practice. Without communication, "
        "agents become isolated silos. Adjacent: Heart (Ch 5) for "
        "session context, Girl (Ch 24) for agent profiles, Wings "
        "(Ch 12) for cross-system export. Tools: sangha_chat_send, "
        "sangha_chat_read, broker.publish, broker.history, "
        "broker.status.",
        ["sangha", "community", "chat", "broker", "encampment",
         "pubsub", "event", "message", "communication", "ganying"],
        gana="gana_encampment", garden="sangha", quadrant="North",
        chinese="室", pinyin="Shì",
    ),

    Chapter(
        28, "The Wall — Boundaries & Governance Tokens",
        "The Wall (壁 Bì) defines where one domain ends and another "
        "begins. This chapter manages democratic governance and "
        "access control: vote.create initiates a proposal, vote.cast "
        "records a vote, vote.analyze tallies results, vote.list "
        "shows active votes, and vote.record_outcome finalizes. The "
        "Scope-of-Engagement token system (engagement.issue, "
        "engagement.validate, engagement.revoke, engagement.list, "
        "engagement.status) provides HMAC-SHA256 signed tokens that "
        "constrain what tools an agent can call, for how long, and "
        "how many times — a cryptographic leash that enables trust "
        "without blind faith. The Wall's air is invisible but "
        "essential — like the atmosphere that makes life possible by "
        "containing it. Adjacent: Room (Ch 4) for security "
        "enforcement, Straddling Legs (Ch 15) for ethics. Tools: "
        "vote.create, vote.cast, vote.analyze, vote.list, "
        "vote.record_outcome, engagement.issue, engagement.validate, "
        "engagement.revoke, engagement.list, engagement.status.",
        ["vote", "boundary", "wall", "air", "invisible", "token",
         "engagement", "hmac", "governance", "democratic", "scope"],
        gana="gana_wall", garden="air", quadrant="North",
        chinese="壁", pinyin="Bì",
    ),
]


class ChapterIndex:
    """Index of all 28 grimoire chapters (Lunar Mansion aligned)"""

    CHAPTERS = _CHAPTERS_28

    def __init__(self, grimoire_path: Path | None = None) -> None:
        self.grimoire_path = grimoire_path or Path(__file__).parents[3] / "grimoire"
        self._chapters = {ch.number: ch for ch in self.CHAPTERS}
        self._by_keyword: dict[str, list[Chapter]] = {}
        self._by_gana: dict[str, Chapter] = {ch.gana: ch for ch in self.CHAPTERS}
        self._build_keyword_index()

    def _build_keyword_index(self) -> Any:
        """Build keyword to chapter mapping"""
        for chapter in self.CHAPTERS:
            for keyword in chapter.keywords:
                if keyword not in self._by_keyword:
                    self._by_keyword[keyword] = []
                self._by_keyword[keyword].append(chapter)

    def get_chapter(self, number: int) -> Chapter | None:
        """Get chapter by number"""
        return self._chapters.get(number)

    def find_by_keyword(self, keyword: str) -> list[Chapter]:
        """Find chapters matching a keyword"""
        keyword_lower = keyword.lower()
        matches = []

        # Exact match
        if keyword_lower in self._by_keyword:
            matches.extend(self._by_keyword[keyword_lower])

        # Partial match
        for kw, chapters in self._by_keyword.items():
            if keyword_lower in kw or kw in keyword_lower:
                for ch in chapters:
                    if ch not in matches:
                        matches.append(ch)

        return matches

    def find_for_task(self, task: str) -> list[Chapter]:
        """Find relevant chapters for a task"""
        task_lower = task.lower()
        scored = []

        for chapter in self.CHAPTERS:
            score = 0
            for keyword in chapter.keywords:
                if keyword in task_lower:
                    score += 1
            if score > 0:
                scored.append((score, chapter))

        scored.sort(key=lambda x: x[0], reverse=True)
        return [ch for _, ch in scored]

    def all_chapters(self) -> list[Chapter]:
        """Get all chapters"""
        return list(self.CHAPTERS)

    def __len__(self) -> int:
        return len(self.CHAPTERS)


# Convenience
def find_chapter(query: str) -> list[Chapter]:
    """Find chapters matching a query"""
    index = ChapterIndex()
    return index.find_for_task(query)
