# Galaxy-Per-Client Workflow Guide

> How freelancers, consultants, and coaches can use WhiteMagic's multi-galaxy system
> to maintain isolated, searchable memory spaces for each client.

---

## The Problem

Professionals serving multiple clients face a memory management challenge:
- Client A's project details must not leak into Client B's context
- Each client has their own history, preferences, goals, and documents
- Switching between clients requires rapid context loading
- Post-session workflows (summaries, follow-ups) should be client-specific

## The Solution: One Galaxy Per Client

WhiteMagic's multi-galaxy system gives each client their own isolated SQLite database with independent memory, associations, and holographic index.

---

## Quick Start

### 1. Create Client Galaxies

```python
from whitemagic.tools.unified_api import call_tool

# Create a galaxy for each client
call_tool("galaxy.create", name="client_acme_corp")
call_tool("galaxy.create", name="client_jane_smith")
call_tool("galaxy.create", name="client_greenfield_labs")
```

Each galaxy creates:
- `~/.whitemagic/memory/galaxies/{name}/whitemagic.db` — isolated memory DB
- Independent holographic index and association graph
- No cross-contamination between galaxies

### 2. Switch Before Each Session

```python
# Before meeting with Acme Corp
call_tool("galaxy.switch", name="client_acme_corp")

# Now all memory operations are scoped to this client
call_tool("search_memories", query="last meeting notes")
# → Returns only Acme Corp memories
```

### 3. Record Session Notes

```python
# During or after the session
call_tool("create_memory",
    title="Session 2026-02-11: Q1 Review",
    content="Discussed Q1 results. Revenue up 15%. Concerns about supply chain. Action: draft proposal for backup supplier.",
    tags=["session", "q1_review", "2026-02-11"],
    importance=0.8
)
```

### 4. Ingest Client Documents

```python
# Bulk-import a folder of client documents
call_tool("galaxy.ingest",
    name="client_acme_corp",
    directory="/path/to/acme_corp_documents"
)
# → All files ingested as searchable memories
```

### 5. Search Client History

```python
# Deep search across all of this client's history
call_tool("hybrid_recall", query="supply chain concerns", limit=5)
# → Returns relevant memories from Acme Corp only

# Cross-session archaeology
call_tool("archaeology_search", query="what did we decide about pricing")
# → Finds decisions across all sessions
```

---

## Post-Session Workflow

Automate the common tasks after every client session:

```python
# 1. Store session summary
call_tool("create_memory",
    title=f"Session Summary: {client_name} - {date}",
    content=session_notes,
    tags=["session_summary", date],
    importance=0.85
)

# 2. Extract action items
call_tool("create_memory",
    title=f"Action Items: {client_name} - {date}",
    content=action_items,
    tags=["action_items", "pending", date],
    importance=0.9
)

# 3. Update client goals (if discussed)
call_tool("create_memory",
    title=f"Updated Goals: {client_name}",
    content=updated_goals,
    tags=["goals", "current"],
    importance=0.95
)

# 4. Draft follow-up (if needed)
call_tool("create_memory",
    title=f"Draft Follow-Up: {client_name}",
    content=draft_email,
    tags=["follow_up", "draft", date],
    importance=0.7
)
```

### Automate with Pipelines

```python
# Create a reusable post-session pipeline
call_tool("pipeline.create",
    name="post_session",
    steps=[
        {"tool": "create_memory", "args": {"title": "Session: {client} - {date}", "content": "{notes}", "tags": ["session_summary"]}},
        {"tool": "search_memories", "args": {"query": "pending action items", "tags": ["action_items", "pending"]}, "output": "existing_actions"},
        {"tool": "create_memory", "args": {"title": "Actions: {client} - {date}", "content": "{new_actions}\n\nPrior pending: {existing_actions}", "tags": ["action_items"]}},
    ]
)
```

---

## Client Overview Dashboard

Check the status of all your client galaxies:

```python
# List all galaxies with stats
result = call_tool("galaxy.list")
for g in result["details"]["galaxies"]:
    print(f"{g['name']}: {g.get('memory_count', '?')} memories, last active: {g.get('last_accessed', 'unknown')}")
```

### "Who Haven't I Contacted?" Check

```python
# Switch to each client galaxy and check last session date
for client in ["client_acme_corp", "client_jane_smith", "client_greenfield_labs"]:
    call_tool("galaxy.switch", name=client)
    result = call_tool("search_memories", query="session summary", limit=1)
    last_session = result["details"]["results"][0]["timestamp"] if result["details"]["results"] else "never"
    print(f"{client}: last session {last_session}")
```

---

## Tips and Best Practices

1. **Naming convention**: Use `client_` prefix for client galaxies (e.g., `client_acme_corp`). Keep personal galaxies without prefix (e.g., `default`, `personal`).

2. **Tag consistently**: Use standard tags across all clients:
   - `session_summary` — post-session notes
   - `action_items` + `pending`/`completed` — trackable actions
   - `goals` + `current`/`archived` — client goals
   - `decision` — key decisions with rationale
   - `document` — ingested documents

3. **Don't forget to switch**: Always `galaxy.switch` before a client session. Memories created in the wrong galaxy are hard to move.

4. **Use importance scores**: 0.9+ for decisions and goals, 0.7-0.8 for session notes, 0.5 for general context.

5. **Regular lifecycle sweeps**: Run `memory.lifecycle_sweep` periodically per galaxy to keep memory quality high.

6. **Back up client galaxies**: Each galaxy is a standalone SQLite file. Copy it for backup: `cp ~/.whitemagic/memory/galaxies/client_acme_corp/whitemagic.db ~/backups/`

---

## Starter Pack

Get started quickly with the Pro tier pack:

```python
call_tool("starter_packs.get", name="pro_tier")
```

This returns all the tools you need for client management with usage guidance.

---

*Guide created: February 2026 — WhiteMagic v15.1.0*
