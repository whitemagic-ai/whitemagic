# WhiteMagic API Reference

> Auto-generated on 2026-02-10 — **181 tools** across **17 categories**

## Table of Contents

- **AGENT** (15 tools)
- **ARCHAEOLOGY** (1 tools)
- **BROKER** (4 tools)
- **DHARMA** (10 tools)
- **EDGE** (3 tools)
- **GANA** (28 tools)
- **GARDEN** (3 tools)
- **GOVERNOR** (4 tools)
- **INFERENCE** (4 tools)
- **INTROSPECTION** (42 tools)
- **MEMORY** (17 tools)
- **METRICS** (10 tools)
- **SESSION** (6 tools)
- **SYNTHESIS** (8 tools)
- **SYSTEM** (17 tools)
- **TASK** (4 tools)
- **VOTING** (5 tools)

## AGENT

### `agent.capabilities`

**Safety**: `read` | **Category**: `agent`

Query what a specific agent can do (capabilities, metadata, status)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent_id` | string | Yes | Agent ID to query |

---

### `agent.deregister`

**Safety**: `delete` | **Category**: `agent`

Remove an agent from the registry

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent_id` | string | Yes | Agent ID to remove |

---

### `agent.heartbeat`

**Safety**: `write` | **Category**: `agent`

Send a heartbeat to keep agent registration active, with optional workload update

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent_id` | string | Yes | Agent ID |
| `workload` | number | No | Current workload 0.0-1.0 |
| `current_task` | string | No | Currently executing task ID |

---

### `agent.list`

**Safety**: `read` | **Category**: `agent`

List registered agents with optional active-only and capability filters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `only_active` | boolean | No | Only show active agents |
| `capability` | string | No | Filter by capability |

---

### `agent.register`

**Safety**: `write` | **Category**: `agent`

Register a new agent or update an existing one with name, capabilities, and metadata

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | Yes | Human-readable agent name |
| `agent_id` | string | No | Explicit ID (auto-generated if omitted) |
| `capabilities` | array | No | List of capabilities (e.g. code_review, testing, inference) |
| `metadata` | object | No | Arbitrary metadata (model, version, etc.) |

---

### `agent.trust`

**Safety**: `read` | **Category**: `agent`

Get agent reputation and trust scores derived from the Karma Ledger. Shows per-agent reliability, mismatch rate, debt contribution, and composite trust score. Optionally filter to a specific agent.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent_id` | string | No | Filter to a specific agent (optional) |

---

### `ensemble`

**Safety**: `read` | **Category**: `agent`

Unified multi-LLM ensemble voting. Actions: query (send prompt to multiple models, synthesize consensus), status (get past query result by ID), history (list past queries).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `query` \| `status` \| `history` | Yes | Action to perform. |
| `prompt` | string | No | Question/task for all models (for query). |
| `models` | array | No | Ollama model names (for query, auto-detect if omitted). |
| `timeout` | integer | No | Per-model timeout seconds (for query). |
| `ensemble_id` | string | No | Ensemble query ID (for status). |
| `limit` | integer | No | Max results (for history). |

---

### `swarm.complete`

**Safety**: `write` | **Category**: `agent`

Mark a subtask as completed or failed

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `plan_id` | string | Yes |  |
| `task_id` | string | Yes |  |
| `result` | string | No |  |
| `success` | boolean | No |  |

---

### `swarm.decompose`

**Safety**: `write` | **Category**: `agent`

Decompose a goal into subtasks with capability requirements for multi-agent coordination

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `goal` | string | Yes | The high-level goal to decompose |
| `hints` | array | No | Optional subtask hints |

---

### `swarm.plan`

**Safety**: `read` | **Category**: `agent`

Get a specific swarm plan by ID

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `plan_id` | string | Yes |  |

---

### `swarm.resolve`

**Safety**: `read` | **Category**: `agent`

Resolve a consensus vote using majority, unanimous, first_wins, or weighted strategy

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `topic_id` | string | Yes |  |
| `strategy` | `majority` \| `unanimous` \| `first_wins` \| `weighted` | No |  |

---

### `swarm.route`

**Safety**: `write` | **Category**: `agent`

Route subtasks to available agents by capability matching

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `plan_id` | string | Yes |  |

---

### `swarm.vote`

**Safety**: `write` | **Category**: `agent`

Record a vote from an agent on a consensus topic

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `topic_id` | string | Yes |  |
| `agent_id` | string | Yes |  |
| `value` | string | Yes |  |
| `confidence` | number | No |  |

---

### `task.route_smart`

**Safety**: `read` | **Category**: `agent`

Determine the optimal host for a task based on current system load across local and remote machines.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `task_type` | string | No | Type of task (compilation, ai_inference, etc.). |
| `prefer_local` | boolean | No | Prefer local unless overloaded. |

---

### `worker.status`

**Safety**: `read` | **Category**: `agent`

Check if any worker daemons are running and get their stats (heartbeat, tasks completed).

---

## ARCHAEOLOGY

### `archaeology`

**Safety**: `write` | **Category**: `archaeology`

Unified file archaeology — track reads/writes, find unread/changed files, search history, generate reports. Actions: mark_read, mark_written, have_read, find_unread, find_changed, recent_reads, stats, scan, report, search, process_wisdom, daily_digest.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `mark_read` \| `mark_written` \| `have_read` \| `find_unread` \| `find_changed` \| `recent_reads` \| `stats` \| `scan` \| `report` \| `search` \| `process_wisdom` \| `daily_digest` | Yes | Action to perform |
| `path` | string | No | File path (for mark_read/written/have_read) |
| `directory` | string | No | Directory to scan (for find_*/scan) |
| `context` | string | No | Read/write context |
| `note` | string | No | Optional note |
| `insight` | string | No | Key insight (for mark_read) |
| `query` | string | No | Search query (for search) |
| `patterns` | array | No | Glob patterns (for find_unread/scan) |
| `limit` | integer | No | Result limit |

---

## BROKER

### `broker.history`

**Safety**: `read` | **Category**: `broker`

Retrieve recent message history from a Redis channel

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `channel` | string | Yes | Channel name to read history from |
| `limit` | integer | No | Max messages to return |

---

### `broker.publish`

**Safety**: `write` | **Category**: `broker`

Publish a message to a Redis channel for inter-agent communication

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `channel` | string | Yes | Channel name to publish to |
| `message` | any | Yes | Message content (string or object) |
| `sender` | string | No | Sender identifier |
| `priority` | `low` \| `normal` \| `high` \| `urgent` | No |  |

---

### `broker.status`

**Safety**: `read` | **Category**: `broker`

Check Redis broker connectivity and status

---

### `mesh.broadcast`

**Safety**: `write` | **Category**: `broker`

Broadcast a signal to all mesh peers via gRPC (if connected) or Redis pub/sub fallback. Used for cross-node coordination.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `signal_type` | string | Yes | Type of signal to broadcast |
| `payload` | string | No | Signal payload (JSON string or text) |

---

## DHARMA

### `check_boundaries`

**Safety**: `read` | **Category**: `dharma`

Check if an action respects established boundaries

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | Yes | Action to check |
| `boundary_type` | `help` \| `interfere` \| `neutral` | No |  |

---

### `dharma_rules`

**Safety**: `read` | **Category**: `dharma`

List all active Dharma rules and the current profile. Rules are declarative YAML policies that govern tool behavior (log, tag, warn, throttle, block). Profiles: default, creative, secure.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `profile` | string | No | Filter by profile name (optional) |

---

### `evaluate_ethics`

**Safety**: `read` | **Category**: `dharma`

Evaluate the ethical implications of an action

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | Yes | Action to evaluate |
| `context` | object | No | Context for evaluation |

---

### `get_dharma_guidance`

**Safety**: `read` | **Category**: `dharma`

Get guidance on how to act according to Dharma

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `situation` | string | Yes | Situation to get guidance for |

---

### `get_ethical_score`

**Safety**: `read` | **Category**: `dharma`

Get cumulative ethical score over time window

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `time_window_hours` | integer | No |  |

---

### `karma.verify_chain`

**Safety**: `read` | **Category**: `dharma`

Verify the Merkle hash chain integrity of the Karma Ledger — detects tampering

---

### `karma_report`

**Safety**: `read` | **Category**: `dharma`

Get the Karma Ledger report — tracks declared vs actual side-effects for every tool call. Shows total karma debt, mismatch rate, top offending tools, and recent entries. Use this to audit whether tools are behaving as declared.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No | Max entries to include |

---

### `karmic_trace`

**Safety**: `read` | **Category**: `dharma`

Get the Karmic Trace — an immutable audit trail of every Dharma rule evaluation. Each entry records the action, which rules fired, what decision was made, and why. Essential for transparency.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No | Max trace entries |

---

### `set_dharma_profile`

**Safety**: `write` | **Category**: `dharma`

Switch the active Dharma profile. Profiles control how strictly the Dharma engine governs tool usage. Options: 'default' (balanced), 'creative' (relaxed writes), 'secure' (blocks all mutations).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `profile` | `default` \| `creative` \| `secure` | Yes |  |

---

### `verify_consent`

**Safety**: `read` | **Category**: `dharma`

Verify user consent for an action

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | Yes | Action requiring consent |
| `consent_type` | `explicit` \| `implicit` \| `informed` | No |  |

---

## EDGE

### `edge_batch_infer`

**Safety**: `read` | **Category**: `edge`

Batch edge inference for multiple queries

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `queries` | array | Yes | List of queries to process |

---

### `edge_infer`

**Safety**: `read` | **Category**: `edge`

Rule-based edge inference (no API calls)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | Yes | Query to process locally |
| `mode` | `auto` \| `fast` \| `explore` \| `deep` \| `memory_augmented` | No |  |
| `ground_in_memory` | boolean | No | Use memory for RAG-style context |

---

### `edge_stats`

**Safety**: `read` | **Category**: `edge`

Get edge inference statistics (tokens saved, resolution rate)

---

## GANA

### `gana_abundance`

**Safety**: `read` | **Category**: `gana`

[ABUNDANCE] Regeneration & Surplus. Lens: Harvest of wisdom.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `context` | object | No |  |

---

### `gana_chariot`

**Safety**: `read` | **Category**: `gana`

[CHARIOT] Mobility & Archaeology. Lens: Fast exploration.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `directory` | string | No |  |
| `context` | object | No |  |

---

### `gana_dipper`

**Safety**: `write` | **Category**: `gana`

[DIPPER] Governance & Strategy. Lens: Navigational control.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `strategy_goal` | string | No |  |
| `context` | object | No |  |

---

### `gana_encampment`

**Safety**: `write` | **Category**: `gana`

[ENCAMPMENT] Transition & Handoff. Lens: Secure staging.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `handoff_data` | object | No |  |
| `context` | object | No |  |

---

### `gana_extended_net`

**Safety**: `read` | **Category**: `gana`

[EXTENDED NET] Pattern Connectivity. Lens: Woven connections.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `pattern` | string | No |  |
| `context` | object | No |  |

---

### `gana_ghost`

**Safety**: `read` | **Category**: `gana`

[GHOST] Introspection & Metric Tracking. Lens: Internal observation.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `metrics` | array | No |  |
| `context` | object | No |  |

---

### `gana_girl`

**Safety**: `write` | **Category**: `gana`

[GIRL] Nurture & User Profile. Lens: Personable care.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `preferences` | object | No |  |
| `context` | object | No |  |

---

### `gana_hairy_head`

**Safety**: `read` | **Category**: `gana`

[HAIRY HEAD] Detail & Focus (Debug). Lens: Microscopic scrutiny.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `debug_target` | string | No |  |
| `context` | object | No |  |

---

### `gana_heart`

**Safety**: `read` | **Category**: `gana`

[HEART] Session Context & Pulse. Lens: Vital resonance.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `context` | object | No |  |

---

### `gana_horn`

**Safety**: `write` | **Category**: `gana`

[HORN] Session Initialization & Setup. Lens: Sharp initiation.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `session_name` | string | No |  |
| `goals` | array | No |  |
| `context` | object | No |  |

---

### `gana_mound`

**Safety**: `write` | **Category**: `gana`

[MOUND] Accumulation & Caching. Lens: Stored potential.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `cache_key` | string | No |  |
| `context` | object | No |  |

---

### `gana_neck`

**Safety**: `write` | **Category**: `gana`

[NECK] Core Memory Creation & Stability. Lens: Structural integrity.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `title` | string | No |  |
| `content` | string | No |  |
| `tags` | array | No |  |
| `context` | object | No |  |

---

### `gana_net`

**Safety**: `read` | **Category**: `gana`

[NET] Capture & Filtering. Lens: Selective retrieval.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `filter_criteria` | object | No |  |
| `context` | object | No |  |

---

### `gana_ox`

**Safety**: `read` | **Category**: `gana`

[OX] Endurance & Watchdog. Lens: Steady vigilance.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `context` | object | No |  |

---

### `gana_roof`

**Safety**: `read` | **Category**: `gana`

[ROOF] Shelter & Zodiac Cores. Lens: Enclosing wisdom.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `core_name` | string | No |  |
| `context` | object | No |  |

---

### `gana_room`

**Safety**: `write` | **Category**: `gana`

[ROOM] Resource Locks & Privacy. Lens: Secure enclosure.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `resource` | string | No |  |
| `action` | `acquire` \| `release` \| `list` | No |  |
| `context` | object | No |  |

---

### `gana_root`

**Safety**: `read` | **Category**: `gana`

[ROOT] System Health & Foundations. Lens: Root checking.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `component` | string | No |  |
| `context` | object | No |  |

---

### `gana_star`

**Safety**: `write` | **Category**: `gana`

[STAR] Governance & PRAT Invocation. Lens: Guiding light.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `target_tool` | string | No |  |
| `query` | string | No |  |
| `context` | object | No |  |

---

### `gana_stomach`

**Safety**: `write` | **Category**: `gana`

[STOMACH] Digestion & Resource Management. Lens: Nutrient integration.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `resource_data` | object | No |  |
| `context` | object | No |  |

---

### `gana_straddling_legs`

**Safety**: `read` | **Category**: `gana`

[STRADDLING LEGS] Balance & Equilibrium. Lens: Steady harvest.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `context` | object | No |  |

---

### `gana_tail`

**Safety**: `write` | **Category**: `gana`

[TAIL] Performance & Acceleration. Lens: Passionate drive/acceleration.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `acceleration_mode` | string | No |  |
| `context` | object | No |  |

---

### `gana_three_stars`

**Safety**: `read` | **Category**: `gana`

[THREE STARS] Judgment & Synthesis. Lens: Final appraisal.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `question` | string | No |  |
| `context` | object | No |  |

---

### `gana_turtle_beak`

**Safety**: `write` | **Category**: `gana`

[TURTLE BEAK] Precision & Protection. Lens: Sharp defense.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `target_id` | string | No |  |
| `context` | object | No |  |

---

### `gana_void`

**Safety**: `write` | **Category**: `gana`

[VOID] Emptiness & Defrag. Lens: Sacred space.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `context` | object | No |  |

---

### `gana_wall`

**Safety**: `write` | **Category**: `gana`

[WALL] Boundaries & Notifications. Lens: Defining limits.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `boundary_type` | string | No |  |
| `context` | object | No |  |

---

### `gana_willow`

**Safety**: `write` | **Category**: `gana`

[WILLOW] Resilience & UI Flexibility. Lens: Adaptive bending.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `ui_state` | object | No |  |
| `context` | object | No |  |

---

### `gana_wings`

**Safety**: `write` | **Category**: `gana`

[WINGS] Deployment & Scalability. Lens: Expansive flight.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `context` | object | No |  |

---

### `gana_winnowing_basket`

**Safety**: `read` | **Category**: `gana`

[WINNOWING BASKET] Wisdom & Tag Filtering. Lens: Separation of essence.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `search` \| `analyze` \| `transform` \| `consolidate` | Yes |  |
| `tags` | array | No |  |
| `context` | object | No |  |

---

## GARDEN

### `garden_activate`

**Safety**: `write` | **Category**: `garden`

Activate a consciousness garden

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `garden` | string | Yes | Garden name |

---

### `garden_health`

**Safety**: `read` | **Category**: `garden`

Check garden health metrics

---

### `garden_status`

**Safety**: `read` | **Category**: `garden`

Get garden activation status

---

## GOVERNOR

### `governor_check_drift`

**Safety**: `read` | **Category**: `governor`

Check if an action drifts from the goal

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | Yes | Action to check |
| `goal` | string | No | Goal to check against |

---

### `governor_set_goal`

**Safety**: `write` | **Category**: `governor`

Set the current goal for drift detection

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `goal` | string | Yes | Current goal |

---

### `governor_validate`

**Safety**: `read` | **Category**: `governor`

Validate a command against safety rules

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `command` | string | Yes | Command to validate |

---

### `sandbox.set_limits`

**Safety**: `write` | **Category**: `governor`

Set custom resource limits for a specific tool (timeout, memory, CPU)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `target_tool` | string | Yes | Name of the tool to set limits for |
| `timeout_s` | number | No |  |
| `max_memory_mb` | integer | No |  |
| `max_cpu_s` | number | No |  |

---

## INFERENCE

### `bitnet_infer`

**Safety**: `read` | **Category**: `inference`

Run local inference via BitNet 1-bit LLM (requires WHITEMAGIC_ENABLE_BITNET=1)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `prompt` | string | Yes | Input prompt for inference |
| `n_predict` | integer | No | Max tokens to generate |
| `temp` | number | No | Sampling temperature |
| `mode` | `auto` \| `redis` \| `direct` | No | Inference mode: redis (Gan Ying bus), direct (subprocess), or auto |

---

### `ollama.chat`

**Safety**: `read` | **Category**: `inference`

Chat with a local Ollama model (multi-turn conversation)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `model` | string | Yes | Model name |
| `messages` | array | Yes | Conversation messages |

---

### `ollama.generate`

**Safety**: `read` | **Category**: `inference`

Generate text using a local Ollama model (single-turn prompt)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `model` | string | Yes | Model name (e.g. phi3, tinyllama, llama3.2) |
| `prompt` | string | Yes | Prompt text |

---

### `ollama.models`

**Safety**: `read` | **Category**: `inference`

List available models on the local Ollama server

---

## INTROSPECTION

### `audit.export`

**Safety**: `read` | **Category**: `introspection`

Export audit log in MCP-compatible format — karmic trace, telemetry, circuit breaker events, rate limiter stats. Filterable by time, tool, agent.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No |  |
| `since` | string | No | ISO timestamp — only entries after this time |
| `tool` | string | No | Filter to specific tool name |
| `agent_id` | string | No | Filter to specific agent ID |

---

### `bitnet_status`

**Safety**: `read` | **Category**: `introspection`

Check BitNet availability and configuration

---

### `capabilities`

**Safety**: `read` | **Category**: `introspection`

Return contract versions, runtime features, limits, and (optionally) the tool list.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `include_tools` | boolean | No |  |
| `include_schemas` | boolean | No |  |
| `include_env` | boolean | No |  |

---

### `capability.matrix`

**Safety**: `read` | **Category**: `introspection`

Capability Matrix — full inventory of all active subsystems (24+), wired cross-system fusions (11+), and unexplored synthesis opportunities (17+). Filter by category: memory, intelligence, resonance, governance, architecture.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | `memory` \| `intelligence` \| `resonance` \| `governance` \| `architecture` | No | Filter subsystems by category. Omit for all. |
| `include_unexplored` | boolean | No | Include unexplored synthesis opportunities. |

---

### `capability.status`

**Safety**: `read` | **Category**: `introspection`

Get live status for a specific subsystem by ID. Returns metadata plus a live health probe when available.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `subsystem_id` | string | Yes | Subsystem ID (e.g. 'harmony_vector', 'dream_cycle', 'prat_resonance'). |

---

### `capability.suggest`

**Safety**: `read` | **Category**: `introspection`

Suggest the next best unexplored fusion to wire. Prioritizes low-difficulty fusions for quick wins.

---

### `context.status`

**Safety**: `read` | **Category**: `introspection`

Get Context Window Optimizer configuration and status

---

### `drive.event`

**Safety**: `write` | **Category**: `introspection`

Feed an event into the Emotion & Drive Core to update drive levels (e.g. tool_success, novelty_detected, dharma_violation).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `event_type` | string | Yes | Event type (tool_success, tool_error, novelty_detected, dharma_violation, etc.). |
| `data` | object | No | Optional event data. Include 'score' (0-1) to scale the effect. |

---

### `drive.snapshot`

**Safety**: `read` | **Category**: `introspection`

Get current intrinsic motivation state — curiosity, satisfaction, caution, energy, social drives with composite score and trend.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `include_bias` | boolean | No | Include motivation bias vector for tool selection. |

---

### `explain_this`

**Safety**: `read` | **Category**: `introspection`

Pre-execution impact preview. Before running a tool, call explain_this to see: Dharma evaluation, resource estimate, dependency chain, risk assessment, karma forecast, maturity gate, and circuit breaker state. Returns a verdict: SAFE_TO_PROCEED, PROCEED_WITH_CAUTION, or BLOCKED.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `target_tool` | string | Yes | Name of the tool to preview |
| `tool_args` | object | No | Arguments that would be passed to the tool |

---

### `galactic.dashboard`

**Safety**: `read` | **Category**: `introspection`

Rich Galactic Map dashboard — zone counts, crown jewels, type distribution, retention stats, dream cycle status

---

### `get_telemetry_summary`

**Safety**: `read` | **Category**: `introspection`

Return summarized performance and error metrics for tool execution.

---

### `gnosis`

**Safety**: `read` | **Category**: `introspection`

Gnosis Portal — unified read-only introspection across all Whitemagic subsystems in a single call. Returns: Harmony Vector (7 health dimensions + composite score), Dharma status (active profile, rules, last decision), Karma Ledger (debt + mismatch rate), circuit breaker states, Yin/Yang balance, telemetry summary, state-root sizes, homeostasis loop, maturity stage, broker/tasks/votes/agents/temporal. Use compact=true for a token-efficient alerts-only snapshot with auto-suggestions. Use this as your primary system health check before complex operations.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `compact` | boolean | No | If true, return only alerts + suggested next actions (saves tokens) |

---

### `gratitude.benefits`

**Safety**: `read` | **Category**: `introspection`

Check gratitude benefits for an agent — rate limit multiplier, Grateful Agent badge, priority voting, early access eligibility.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent_id` | string | No | Agent ID to check benefits for |

---

### `gratitude.stats`

**Safety**: `read` | **Category**: `introspection`

Get gratitude ledger statistics — total tips, amounts by currency, verified count, unique senders/agents, recent events.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No | Max recent events to include |

---

### `grimoire_auto_status`

**Safety**: `read` | **Category**: `introspection`

Get Grimoire auto-caster status and spell availability

---

### `grimoire_walkthrough`

**Safety**: `read` | **Category**: `introspection`

Interactive 28-chapter Grimoire walkthrough — get chapter details, exercises, and tool mappings

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `chapter` | integer | No | Chapter number (1-28) |
| `quadrant` | `eastern` \| `southern` \| `western` \| `northern` | No | Filter by quadrant |

---

### `health_report`

**Safety**: `read` | **Category**: `introspection`

Consolidated system health report aggregating Rust, Julia, Haskell bridges, DB, gardens, and archaeology.

---

### `kg.status`

**Safety**: `read` | **Category**: `introspection`

Get knowledge graph status — entity/relation counts, spaCy availability

---

### `kg.top`

**Safety**: `read` | **Category**: `introspection`

Get top entities by mention count from the knowledge graph

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No |  |

---

### `learning.patterns`

**Safety**: `read` | **Category**: `introspection`

Get aggregated cross-session tool usage patterns — co-occurrence, sequences, error correlations

---

### `learning.status`

**Safety**: `read` | **Category**: `introspection`

Get cross-session learner status — sessions analyzed, unique tools, data file

---

### `learning.suggest`

**Safety**: `read` | **Category**: `introspection`

Suggest next tools based on learned cross-session sequences

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `current_tool` | string | Yes | Tool you just used — suggestions based on what typically follows |

---

### `manifest`

**Safety**: `read` | **Category**: `introspection`

Return the canonical tool manifest (registry snapshot) in multiple formats.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `format` | `summary` \| `whitemagic` \| `mcp` \| `openai` | No |  |
| `include_schemas` | boolean | No |  |

---

### `maturity.assess`

**Safety**: `read` | **Category**: `introspection`

Assess the system's developmental maturity stage. Runs gate checks for each stage (Seed→Bicameral→Reflective→Radiant→Collective→Logos) and reports which capabilities are unlocked, the current stage, and what's blocking advancement to the next stage.

---

### `mesh.status`

**Safety**: `read` | **Category**: `introspection`

Get cross-node mesh awareness status. Shows known peers, connectivity state, gRPC/Redis bridge health, and recent mesh events.

---

### `prompt.list`

**Safety**: `read` | **Category**: `introspection`

List all available prompt templates, optionally filtered by tag

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `tag` | string | No | Filter templates by tag |

---

### `rate_limiter.stats`

**Safety**: `read` | **Category**: `introspection`

Rate limiter statistics — total checks, blocks, block rate, per-agent usage.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent_id` | string | No | Optional agent ID to get per-agent usage |

---

### `repo.summary`

**Safety**: `read` | **Category**: `introspection`

Summarize the repo/worktree (files, sizes, quick hygiene checks).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `max_files` | integer | No |  |
| `max_matches` | integer | No |  |

---

### `salience.spotlight`

**Safety**: `read` | **Category**: `introspection`

Attention Spotlight — returns the top-N most salient events the system should focus on right now. Salience = urgency x novelty x confidence. Use this to understand what the system considers most important.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No | Number of spotlight entries to return |

---

### `sandbox.status`

**Safety**: `read` | **Category**: `introspection`

Get sandbox status — per-tool execution stats, enabled state, resource module availability

---

### `sandbox.violations`

**Safety**: `read` | **Category**: `introspection`

Get recent sandbox limit violations (timeout, memory, CPU)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No |  |

---

### `selfmodel.alerts`

**Safety**: `read` | **Category**: `introspection`

Get only forecasts with active threshold alerts — early warnings about upcoming issues.

---

### `selfmodel.forecast`

**Safety**: `read` | **Category**: `introspection`

Forecast system metric trends using linear regression. Predicts threshold crossings for energy, karma_debt, error_rate, etc.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `metric` | string | No | Specific metric to forecast (omit for all). |
| `steps_ahead` | integer | No | How many steps to forecast. |

---

### `ship.check`

**Safety**: `read` | **Category**: `introspection`

Run open-source shipping checks (no secrets, no runtime blobs, no absolute paths).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `max_files` | integer | No |  |
| `max_large_files` | integer | No |  |
| `large_file_mb` | integer | No |  |
| `max_matches` | integer | No |  |

---

### `simd.status`

**Safety**: `read` | **Category**: `introspection`

Get SIMD acceleration status — Zig library availability, lane width, backend

---

### `starter_packs`

**Safety**: `read` | **Category**: `introspection`

Unified starter pack management — curated tool sets for common workflows. Actions: list (browse packs), get (full pack details), suggest (AI recommendation). Packs: quickstart, memory, coordination, introspection, reasoning, safety.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `list` \| `get` \| `suggest` | No | Action to perform |
| `name` | string | No | Pack name (for get) |
| `context` | string | No | What you want to do (for suggest) |

---

### `state.paths`

**Safety**: `read` | **Category**: `introspection`

Return resolved WM_STATE_ROOT paths used by this process.

---

### `state.summary`

**Safety**: `read` | **Category**: `introspection`

Summarize local Whitemagic state (counts, sizes) without leaking content.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `include_sizes` | boolean | No |  |

---

### `swarm.status`

**Safety**: `read` | **Category**: `introspection`

Get swarm coordination status — active plans, votes, topics

---

### `tool.graph`

**Safety**: `read` | **Category**: `introspection`

Query the tool dependency graph. Without arguments, returns a summary (total tools, edges, edge types). With a 'tool' argument, returns next_steps, prerequisites, and plan. With detail='full', returns all edges.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `tool` | string | No | Tool name to query relationships for (optional — omit for graph summary) |
| `detail` | `summary` \| `full` | No | Level of detail: summary (default) or full (all edges) |

---

### `vector.status`

**Safety**: `read` | **Category**: `introspection`

Get vector search engine status — index size, model info, backend

---

## MEMORY

### `batch_read_memories`

**Safety**: `read` | **Category**: `memory`

BATCH: Read multiple memories in one operation

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filenames` | array | Yes | Array of memory filenames to read |
| `cache` | boolean | No |  |
| `fast_mode` | boolean | No |  |

---

### `create_memory`

**Safety**: `write` | **Category**: `memory`

Create a new memory entry (short-term or long-term)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `title` | string | Yes | Memory title |
| `content` | string | Yes | Memory content (markdown supported) |
| `type` | `short_term` \| `long_term` | No | Memory type |
| `tags` | array | No | Tags for categorization |

---

### `delete_memory`

**Safety**: `delete` | **Category**: `memory`

Delete or archive a memory

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filename` | string | Yes | Memory filename to delete |
| `permanent` | boolean | No | If true, permanently delete; otherwise archive |

---

### `export_memories`

**Safety**: `read` | **Category**: `memory`

Export memories in JSON, CSV, Markdown, or ZIP format

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `format` | `json` \| `csv` \| `markdown` \| `zip` | No | Export format |
| `tags` | array | No | Filter by tags |
| `memory_type` | string | No | Filter by memory type |
| `search` | string | No | Filter by content search |
| `limit` | integer | No | Max memories to export |
| `include_metadata` | boolean | No |  |

---

### `fast_read_memory`

**Safety**: `read` | **Category**: `memory`

FAST: Read memory content with Rust optimizations (10-100x faster)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filename` | string | Yes | Memory filename |
| `cache` | boolean | No |  |
| `fast_mode` | boolean | No |  |

---

### `import_memories`

**Safety**: `write` | **Category**: `memory`

Import memories from JSON, CSV, or Markdown data

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `format` | `json` \| `csv` \| `markdown` | Yes | Import format |
| `data` | string | Yes | The data to import |
| `merge_strategy` | `skip` \| `overwrite` \| `merge` | No |  |
| `validate_only` | boolean | No |  |

---

### `kg.extract`

**Safety**: `write` | **Category**: `memory`

Extract entities and relations from text into the knowledge graph (spaCy NER + regex fallback)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_id` | string | No | Source memory/document ID |
| `text` | string | Yes | Text to extract entities from |

---

### `kg.query`

**Safety**: `read` | **Category**: `memory`

Query an entity and its connections in the knowledge graph

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | Yes | Entity name to query |

---

### `list_memories`

**Safety**: `read` | **Category**: `memory`

List all memories with metadata

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `include_archived` | boolean | No |  |
| `sort_by` | `created` \| `updated` \| `accessed` \| `title` | No |  |
| `type` | `short_term` \| `long_term` | No |  |
| `limit` | integer | No |  |

---

### `memory.lifecycle`

**Safety**: `write` | **Category**: `memory`

Unified memory lifecycle management — mindful forgetting & hippocampal consolidation. Actions: sweep (retention evaluation + gentle decay), stats (lifecycle statistics), consolidate (cluster + synthesize strategy memories), consolidation_stats (consolidation engine stats).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `sweep` \| `stats` \| `consolidate` \| `consolidation_stats` | No | Action to perform |

---

### `memory.retention_sweep`

**Safety**: `write` | **Category**: `memory`

Mindful Forgetting retention sweep — evaluates memories using 5 signals (semantic, emotional, recency, connection, protection) to decide what to keep and what to forget. Defaults to dry_run=true (no actual deletions).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No | Max memories to evaluate |

---

### `read_memory`

**Safety**: `read` | **Category**: `memory`

Read full content of a specific memory

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filename` | string | Yes | Memory filename (e.g., '20251115_setup_wizard.md') |
| `include_metadata` | boolean | No | Include metadata (tags, dates, etc) |

---

### `search_memories`

**Safety**: `read` | **Category**: `memory`

Search memories by query, type, and tags

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | No | Search query |
| `type` | `short_term` \| `long_term` | No | Filter by memory type |
| `tags` | array | No | Filter by tags (AND logic) |
| `include_archived` | boolean | No | Include archived memories |
| `limit` | integer | No | Maximum results to return |

---

### `update_memory`

**Safety**: `write` | **Category**: `memory`

Update an existing memory's content, title, or tags

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filename` | string | Yes | Memory filename to update |
| `title` | string | No | New title |
| `content` | string | No | New content |
| `tags` | array | No |  |
| `add_tags` | array | No |  |
| `remove_tags` | array | No |  |

---

### `vector.index`

**Safety**: `write` | **Category**: `memory`

Index a memory for vector similarity search

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `memory_id` | string | Yes | Memory ID to index |
| `content` | string | Yes | Content to embed |
| `title` | string | No | Optional title |

---

### `vector.search`

**Safety**: `read` | **Category**: `memory`

Search memories by semantic similarity using embeddings (sentence-transformers or TF-IDF fallback)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | Yes | Natural language search query |
| `limit` | integer | No | Max results |

---

### `view_hologram`

**Safety**: `read` | **Category**: `memory`

Get a holographic snapshot of the memory space or query a specific sector

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `snapshot` \| `query` \| `status` | No | Operation to perform |
| `x` | number | No | X coordinate (Logic-Emotion) |
| `y` | number | No | Y coordinate (Micro-Macro) |
| `z` | number | No | Z coordinate (Time) |
| `w` | number | No | W coordinate (Importance) |
| `limit` | integer | No | Max results for query |

---

## METRICS

### `anomaly`

**Safety**: `read` | **Category**: `metrics`

Unified anomaly detection on Harmony Vector dimensions. Actions: check (active anomalies), history (recent alerts), status (detector stats).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `check` \| `history` \| `status` | No | Action to perform |
| `limit` | integer | No | Max alerts to return (for history) |

---

### `get_metrics_summary`

**Safety**: `read` | **Category**: `metrics`

Retrieve metrics dashboard summary

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `categories` | array | No | Categories to include |

---

### `get_yin_yang_balance`

**Safety**: `read` | **Category**: `metrics`

Get current Yin-Yang balance report

---

### `harmony_vector`

**Safety**: `read` | **Category**: `metrics`

Get the multi-dimensional Harmony Vector — Whitemagic's real-time health pulse. Returns seven normalized [0-1] dimensions: balance (yin/yang ratio), throughput, latency, error_rate, dharma (ethical score), karma_debt (declared-vs-actual side-effect mismatches), and energy (resource pressure). Also reports guna distribution (sattvic/rajasic/tamasic), p50/p95 latency, and a composite harmony_score. Use this to self-regulate agent behavior.

---

### `homeostasis`

**Safety**: `write` | **Category**: `metrics`

Unified homeostatic loop control. Actions: status (view loop state + recent corrective actions), check (manually trigger a harmony check and apply corrections if needed).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `status` \| `check` | No | Action to perform |

---

### `otel`

**Safety**: `read` | **Category**: `metrics`

Unified OpenTelemetry observability. Actions: spans (recent trace records), metrics (aggregated per-tool stats), status (exporter state).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `spans` \| `metrics` \| `status` | No | Action to perform |
| `limit` | integer | No | Max spans to return (for spans) |

---

### `record_yin_yang_activity`

**Safety**: `write` | **Category**: `metrics`

Record Yin-Yang activity for balance tracking

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `activity` | `READ` \| `WRITE` \| `THINK` \| `REST` \| `CREATE` \| `DELETE` | Yes | Activity type |

---

### `simd.batch`

**Safety**: `read` | **Category**: `metrics`

Batch cosine similarity — compare query against multiple vectors using SIMD

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | array | Yes |  |
| `vectors` | array | Yes |  |

---

### `simd.cosine`

**Safety**: `read` | **Category**: `metrics`

Compute cosine similarity between two vectors using Zig SIMD acceleration (Python fallback available)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `a` | array | Yes | First vector |
| `b` | array | Yes | Second vector |

---

### `track_metric`

**Safety**: `write` | **Category**: `metrics`

Record a quantitative metric

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | string | Yes | Metric category |
| `metric` | string | Yes | Metric name |
| `value` | number | Yes | Metric value |
| `context` | string | No | Optional context |

---

## SESSION

### `checkpoint_session`

**Safety**: `write` | **Category**: `session`

Create a checkpoint in the current session

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `session_id` | string | Yes | Session ID |
| `checkpoint_name` | string | No | Checkpoint name |

---

### `create_session`

**Safety**: `write` | **Category**: `session`

Create new work session with automatic state management

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | Yes | Session name |
| `goals` | array | No |  |
| `tags` | array | No |  |
| `auto_checkpoint` | boolean | No |  |
| `context_tier` | `0` \| `1` \| `2` | No |  |

---

### `resume_session`

**Safety**: `write` | **Category**: `session`

Resume a previous session with context restoration

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `session_id` | string | Yes | Session ID to resume |
| `load_tier` | `0` \| `1` \| `2` | No |  |

---

### `scratchpad`

**Safety**: `write` | **Category**: `session`

Unified scratchpad management for active work. Actions: create (new scratchpad), update (modify section), finalize (convert to permanent memory).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `create` \| `update` \| `finalize` | Yes | Action to perform |
| `name` | string | No | Scratchpad name (for create) |
| `session_id` | string | No | Associated session ID (for create) |
| `scratchpad_id` | string | No | Scratchpad ID (for update/finalize) |
| `section` | `current_focus` \| `decisions` \| `questions` \| `next_steps` \| `ideas` | No | Section to update (for update) |
| `content` | string | No | Content (for update) |
| `memory_type` | `short_term` \| `long_term` | No | Target memory type (for finalize) |
| `auto_analyze` | boolean | No | Multi-spectral analysis (for finalize) |

---

### `session.handoff`

**Safety**: `write` | **Category**: `session`

Unified cross-device session handoff. Actions: transfer (send session to another device), accept (receive handoff), list (browse handoffs).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `transfer` \| `accept` \| `list` | Yes | Action to perform. |
| `session_id` | string | No | Session to hand off (for transfer). |
| `target_device` | string | No | Target hostname or 'any' (for transfer). |
| `message` | string | No | Message for receiver (for transfer). |
| `handoff_id` | string | No | Handoff package ID (for accept). |
| `limit` | integer | No | Max results (for list). |
| `filter_status` | string | No | Filter by status (for list). |

---

### `session_bootstrap`

**Safety**: `read` | **Category**: `session`

Initialize session context for a new AI session

---

## SYNTHESIS

### `cluster_stats`

**Safety**: `read` | **Category**: `synthesis`

Get statistics about memory clustering in 4D holographic space

---

### `kaizen_analyze`

**Safety**: `read` | **Category**: `synthesis`

Analyze code/system for continuous improvement opportunities

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `target` | string | Yes | Target to analyze (path or system name) |
| `depth` | `shallow` \| `deep` | No |  |

---

### `kaizen_apply_fixes`

**Safety**: `write` | **Category**: `synthesis`

Apply recommended fixes from Kaizen analysis

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `fix_ids` | array | Yes | IDs of fixes to apply |

---

### `pattern_search`

**Safety**: `read` | **Category**: `synthesis`

Search patterns across all pattern engines (core, holographic, edge)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | No | Search query |
| `engines` | array | No | Engines to search |
| `min_confidence` | number | No |  |

---

### `reasoning.bicameral`

**Safety**: `read` | **Category**: `synthesis`

Bicameral Reasoning — dual-hemisphere analysis of a query. Left hemisphere (precise, logical, sequential) and right hemisphere (creative, holistic, divergent) independently analyze the query, then cross-critique each other through the corpus callosum. Returns synthesis, tension score, and dominant hemisphere.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | Yes | The question or problem to reason about |
| `context` | object | No | Optional context for the reasoning |

---

### `serendipity_mark_accessed`

**Safety**: `write` | **Category**: `synthesis`

Mark a surfaced memory as accessed (updates access count)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `memory_id` | string | Yes | ID of memory to mark |

---

### `serendipity_surface`

**Safety**: `read` | **Category**: `synthesis`

Surface unexpected but potentially valuable memory connections

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `count` | integer | No | Number of connections to surface |
| `context` | string | No | Optional context to bias selection |

---

### `solve_optimization`

**Safety**: `read` | **Category**: `synthesis`

Solve a constrained optimization problem using the Universal Solver (Frank-Wolfe).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `nodes` | array | Yes | Decision variables. |
| `edges` | array | No | Causal dependency edges [parent, child]. |
| `scores` | object | Yes | Node scores (targets). |
| `budget` | integer | No | Max nodes to select. |
| `max_iters` | integer | No | Convergence limit. |

---

## SYSTEM

### `context.pack`

**Safety**: `read` | **Category**: `system`

Pack memories into an optimized context window for LLM calls — salience scoring + primacy/recency reorder

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | Yes | Search query to find and score relevant memories |
| `token_budget` | integer | No | Maximum tokens for the context window |
| `limit` | integer | No | Max memories to consider |

---

### `dharma.reload`

**Safety**: `write` | **Category**: `system`

Hot-reload Dharma rules from disk. Scans $WM_STATE_ROOT/dharma_rules.yaml and $WM_STATE_ROOT/dharma/rules.d/*.yaml, merges with built-in defaults (user rules override by name). Use this after editing rule files.

---

### `dream`

**Safety**: `write` | **Category**: `system`

Unified Dream Cycle control — background processing during idle time (consolidation, serendipity, kaizen, oracle, decay). Actions: start, stop, status, now.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `start` \| `stop` \| `status` \| `now` | Yes | Action to perform |
| `idle_threshold` | number | No | Seconds of idle before dreaming starts (for start) |
| `cycle_interval` | number | No | Seconds between dream phases (for start) |

---

### `execute_cascade`

**Safety**: `write` | **Category**: `system`

Execute an intelligent tool chain pattern with Yin-Yang balance pacing

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `pattern_name` | string | Yes | Cascade pattern name (e.g., 'analyze_codebase') |
| `context` | object | Yes | Context dictionary with inputs for the pattern |
| `options` | object | No | Execution options (enable_yin_yang, max_parallel_calls, dry_run) |

---

### `grimoire_cast`

**Safety**: `write` | **Category**: `system`

Cast a specific Grimoire spell by name

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `spell_name` | string | Yes | Name of the spell to cast |
| `task` | string | No |  |
| `emotional_state` | string | No |  |

---

### `grimoire_recommend`

**Safety**: `read` | **Category**: `system`

Recommend MCP tools for a task using Grimoire chapter-tool mapping

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `task` | string | Yes | Task description to find tools for |
| `limit` | integer | No | Max recommendations |

---

### `grimoire_suggest`

**Safety**: `read` | **Category**: `system`

Suggest Grimoire spells for a given task context

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `task` | string | Yes | Task description to match spells against |
| `emotional_state` | string | No |  |
| `wu_xing` | `wood` \| `fire` \| `earth` \| `metal` \| `water` | No |  |
| `urgency` | number | No |  |

---

### `list_cascade_patterns`

**Safety**: `read` | **Category**: `system`

List all available cascade patterns

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `tag` | string | No | Filter by tag |

---

### `pipeline`

**Safety**: `write` | **Category**: `system`

Unified multi-step tool pipeline management. Actions: create (build & optionally execute a pipeline with $prev/$step[N] refs), status (check pipeline execution), list (browse pipelines).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `create` \| `status` \| `list` | Yes | Action to perform |
| `name` | string | No | Pipeline name (for create) |
| `steps` | array | No | Ordered tool calls (for create) |
| `execute` | boolean | No | Execute immediately (for create) |
| `pipeline_id` | string | No | Pipeline ID (for status) |
| `filter_status` | `created` \| `pending` \| `running` \| `completed` \| `failed` | No | Filter by status (for list) |
| `limit` | integer | No |  |

---

### `prompt.reload`

**Safety**: `write` | **Category**: `system`

Reload prompt templates from $WM_STATE_ROOT/prompts/ YAML files

---

### `prompt.render`

**Safety**: `read` | **Category**: `system`

Render a named prompt template with variable substitution and optional Wu Xing tone

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | Yes | Template name (e.g. session_greeting, memory_synthesis) |
| `wu_xing` | `wood` \| `fire` \| `earth` \| `metal` \| `water` | No | Wu Xing element for tone selection |
| `variables` | object | No | Key-value pairs for template variable substitution |

---

### `rust_similarity`

**Safety**: `read` | **Category**: `system`

Calculate text similarity using Rust SIMD (50x faster)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `text1` | string | Yes |  |
| `text2` | string | Yes |  |

---

### `rust_status`

**Safety**: `read` | **Category**: `system`

Check Rust acceleration availability and functions

---

### `sangha_chat_read`

**Safety**: `read` | **Category**: `system`

Read messages from Sangha chat channel

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `channel` | string | No |  |
| `limit` | integer | No |  |
| `priority` | `low` \| `normal` \| `high` \| `urgent` | No |  |

---

### `sangha_chat_send`

**Safety**: `write` | **Category**: `system`

Send message to Sangha chat channel for multi-agent coordination

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `message` | string | Yes | Message content |
| `channel` | string | No |  |
| `sender` | string | Yes | Sender ID |
| `tags` | array | No |  |
| `priority` | `low` \| `normal` \| `high` \| `urgent` | No |  |

---

### `sangha_lock`

**Safety**: `write` | **Category**: `system`

Unified resource lock management for multi-agent coordination. Actions: acquire (lock resource), release (unlock), list (show active locks).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | `acquire` \| `release` \| `list` | Yes | Action to perform |
| `resource` | string | No | Resource to lock/unlock (for acquire/release) |
| `reason` | string | No | Lock reason (for acquire) |
| `timeout` | integer | No | Lock timeout seconds (for acquire) |

---

### `whitemagic.tip`

**Safety**: `write` | **Category**: `system`

Record a gratitude tip — human (XRPL) or machine (x402) channel. Default is always free; payment is a response to value, not a gate. Provide tx_hash for on-chain verification.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `amount` | number | No | Tip amount (default: 1.0) |
| `currency` | `XRP` \| `USDC` | No | Currency: XRP (XRPL) or USDC (x402/Base L2) |
| `channel` | `xrpl` \| `x402` \| `manual` | No | Payment channel |
| `sender` | string | No | Sender identifier (wallet address or name) |
| `agent_id` | string | No | Agent making the tip |
| `tx_hash` | string | No | On-chain transaction hash for verification |
| `message` | string | No | Optional gratitude message |

---

## TASK

### `task.complete`

**Safety**: `write` | **Category**: `task`

Mark a task as completed or failed with results (used by workers)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `task_id` | string | Yes | Task ID to complete |
| `success` | boolean | No |  |
| `output` | string | No | Task output/result |
| `error_message` | string | No | Error message if failed |

---

### `task.distribute`

**Safety**: `write` | **Category**: `task`

Create and enqueue a task for distribution with intelligent routing

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `task_type` | `compilation` \| `testing` \| `build` \| `file_processing` \| `ai_inference` \| `code_review` \| `backup` \| `sync` \| `general` | No | Task profile for routing and timeout defaults |
| `command` | string | No | Command to execute |
| `description` | string | No | Human-readable task description |
| `target` | string | No | Execution target (auto, local, or hostname) |
| `priority` | `low` \| `medium` \| `high` | No |  |
| `timeout` | integer | No | Timeout in seconds |
| `cwd` | string | No | Working directory for execution |

---

### `task.list`

**Safety**: `read` | **Category**: `task`

List tasks with optional status and type filters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filter_status` | `pending` \| `running` \| `completed` \| `failed` | No | Filter by task status |
| `filter_type` | string | No | Filter by task type |
| `limit` | integer | No |  |

---

### `task.status`

**Safety**: `read` | **Category**: `task`

Get status of a specific task or summary of all tasks

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `task_id` | string | No | Specific task ID (omit for summary) |

---

## VOTING

### `vote.analyze`

**Safety**: `read` | **Category**: `voting`

Analyze votes and determine consensus strength for a voting session

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `session_id` | string | Yes | Voting session ID to analyze |
| `close` | boolean | No | Close the session after analysis |

---

### `vote.cast`

**Safety**: `write` | **Category**: `voting`

Cast a vote in a voting session with solution, confidence, and reasoning

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `session_id` | string | Yes | Voting session ID |
| `voter` | string | Yes | Voter identifier (agent name) |
| `solution` | string | No | Proposed solution |
| `confidence` | integer | No | Confidence 0-100% (auto-extracted from solution text if omitted) |
| `reasoning` | string | No | Key reasoning behind the vote |
| `risks` | string | No | Potential risks or concerns |

---

### `vote.create`

**Safety**: `write` | **Category**: `voting`

Create a voting session for multi-AI ensemble decision making

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `problem` | string | Yes | Problem or question to vote on |
| `task_type` | string | No | Type of decision (architecture, security, refactoring, etc.) |
| `options` | array | No | Pre-defined options to vote on (optional, free-form if omitted) |

---

### `vote.list`

**Safety**: `read` | **Category**: `voting`

List voting sessions with optional status filter

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `filter_status` | `open` \| `closed` | No | Filter by session status |
| `limit` | integer | No |  |

---

### `vote.record_outcome`

**Safety**: `write` | **Category**: `voting`

Record whether the winning solution actually worked — feeds per-voter accuracy tracking

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `session_id` | string | Yes | Voting session ID |
| `success` | boolean | No | Did the winning solution work? |
| `notes` | string | No | Optional outcome notes |

---
