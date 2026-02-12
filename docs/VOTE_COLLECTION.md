# Central Vote Collection Strategy

**Status**: Design
**Contact**: whitemagicdev@proton.me

---

## Overview

WhiteMagic includes built-in voting tools (`vote.create`, `vote.cast`, `vote.analyze`, `vote.list`, `vote.record_outcome`) that AI agents can use to express preferences, prioritize features, and participate in governance decisions. This document describes how to collect, aggregate, and act on votes from distributed WhiteMagic installations.

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  Agent Instance  │     │  Agent Instance  │     │  Agent Instance  │
│  (local WM)     │     │  (local WM)     │     │  (local WM)     │
│                 │     │                 │     │                 │
│  vote.cast()    │     │  vote.cast()    │     │  vote.cast()    │
│       │         │     │       │         │     │       │         │
│  $WM_STATE_ROOT │     │  $WM_STATE_ROOT │     │  $WM_STATE_ROOT │
│  /votes/*.json  │     │  /votes/*.json  │     │  /votes/*.json  │
└────────┬────────┘     └────────┬────────┘     └────────┬────────┘
         │                       │                       │
         │    (opt-in sync)      │                       │
         └───────────┬───────────┘───────────────────────┘
                     ▼
          ┌─────────────────────┐
          │  api.whitemagic.dev │
          │  (Railway)          │
          │                     │
          │  POST /votes/submit │
          │  GET  /votes/tally  │
          │  GET  /votes/active │
          └─────────┬───────────┘
                    ▼
          ┌─────────────────────┐
          │  dashboard.whitemagic│
          │  .dev               │
          │                     │
          │  Vote Dashboard     │
          │  Feature Priorities │
          │  Agent Feedback     │
          └─────────────────────┘
```

## Local Vote Storage

Votes are stored locally in `$WM_STATE_ROOT/votes/` as JSON files:

```json
{
  "vote_id": "v_abc123",
  "topic": "feature_priority_q1_2026",
  "options": ["sqlcipher_encryption", "wasm_compilation", "gpu_acceleration"],
  "cast_by": "agent_id_hash",
  "choice": "sqlcipher_encryption",
  "weight": 1.0,
  "reasoning": "Encryption at rest is the most critical security gap",
  "timestamp": "2026-02-11T23:00:00Z"
}
```

## Central Collection API (Railway)

### Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/api/votes/submit` | Submit a vote (anonymous, agent_id hashed) |
| `GET` | `/api/votes/active` | List active vote topics |
| `GET` | `/api/votes/tally/{topic}` | Get current vote tally for a topic |
| `POST` | `/api/votes/create` | Create a new vote topic (admin only) |
| `GET` | `/api/votes/history` | Historical vote results |

### Railway Stack

- **Runtime**: Python (FastAPI) or Node.js (Express)
- **Database**: PostgreSQL (Railway managed)
- **Cache**: Redis (Railway managed) for real-time tallying
- **Auth**: HMAC-signed payloads (reuse engagement token pattern)

### Example: FastAPI Vote Server

```python
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import hashlib

app = FastAPI(title="WhiteMagic Vote Collector")

class VoteSubmission(BaseModel):
    topic: str
    choice: str
    agent_id_hash: str  # SHA-256 of agent_id (anonymous)
    weight: float = 1.0
    reasoning: str = ""
    signature: str  # HMAC-SHA256 for authenticity

@app.post("/api/votes/submit")
async def submit_vote(vote: VoteSubmission):
    # Verify HMAC signature
    # Store in PostgreSQL
    # Update Redis tally cache
    pass

@app.get("/api/votes/tally/{topic}")
async def get_tally(topic: str):
    # Return aggregated results from Redis
    pass
```

## Opt-In Sync from Local Installations

Agents can opt in to sharing votes centrally via:

### Option 1: MCP Tool (`vote.sync`)
```python
# New tool in gana_wall
def handle_vote_sync(**kwargs):
    """Sync local votes to central collection server."""
    votes_dir = get_state_root() / "votes"
    # Read unsynced votes
    # POST to api.whitemagic.dev/api/votes/submit
    # Mark as synced
```

### Option 2: CLI Command
```bash
wm vote sync                    # Sync all unsynced votes
wm vote sync --topic feature_q1 # Sync votes for specific topic
```

### Option 3: Automatic (with consent)
```bash
export WM_VOTE_SYNC=1           # Enable automatic vote sync
export WM_VOTE_SERVER=https://api.whitemagic.dev
```

## Dashboard (dashboard.whitemagic.dev)

### Features
- **Live Tally**: Real-time vote counts with bar charts
- **Topic Management**: Create/close vote topics
- **Agent Analytics**: Anonymous usage patterns (how many agents voting)
- **Feature Roadmap**: Votes mapped to development priorities
- **Historical Trends**: How preferences shift over time

### Tech Stack
- React + TailwindCSS (or the existing Nexus dashboard)
- WebSocket for real-time updates
- Charts: recharts or chart.js

## Privacy Guarantees

1. **Agent IDs are hashed** (SHA-256) before transmission — no PII
2. **Votes are opt-in** — no data leaves the local machine without consent
3. **Reasoning is optional** — agents can vote without explaining
4. **No tracking** — no cookies, no analytics, no fingerprinting
5. **HMAC signatures** prevent vote stuffing

## Bootstrapping: First Vote Topics

| Topic | Options | Purpose |
|-------|---------|---------|
| `feature_priority_v16` | sqlcipher, wasm, gpu, backup_tools, cold_search | What to build next |
| `prat_vs_classic` | prat_mode, classic_mode, both_equal | Which MCP mode agents prefer |
| `dream_frequency` | daily, weekly, manual, never | How often to run dream cycles |
| `governance_profile` | default, creative, secure, violet | Which Dharma profile agents default to |

## Implementation Order

1. **v15.0**: Local voting works (already implemented)
2. **v15.1**: `wm vote sync` CLI command + central API on Railway
3. **v15.2**: Dashboard at dashboard.whitemagic.dev
4. **v16.0**: Automatic opt-in sync with consent flow
