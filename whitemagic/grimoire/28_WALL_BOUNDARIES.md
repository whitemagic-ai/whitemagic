# Chapter 28: Boundaries & Alerts

**Gana**: WallGana (Chinese: 壁, Pinyin: Bì)
**Garden**: subtlety
**Quadrant**: Northern (Black Tortoise)
**Element**: Water
**Phase**: Yin Peak
**I Ching Hexagram**: 60. 節 Jié (Limitation) - Boundaries and filtering

---

## 🎯 Purpose

Chapter 28 defines **boundaries and alerts**—setting the final limits, filtering signals, and alerting the interior of any anomalies before the cycle returns to the start. The Wall (壁) is the definitive boundary that protects the core identity.

Use this chapter when you need to:
- **Set session limits** and boundaries
- **Filter signals** from noise
- **Alert the system** to boundary breaches
- **Define the scope** of the return path
- **Defend the core system** before transition

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `validate_input` | Filter signals at the boundary | Guard logic |
| `send_notification` | Alert the interior | Messaging |
| `manage_locks` | Defend core resources | Boundary defense |
| `Gana::Wall` | Execute boundary logic | Automatic defense |

---

## 🛡️ The Boundary Engine

The `BoundaryEngine` is the active enforcement mechanism for this chapter. It protects the system from:
1.  **Context Overflow**: Preventing memory saturation.
2.  **Token Exhaustion**: Hard limits on generation costs.
3.  **Recursion Depth**: Stopping infinite loops.
4.  **Unauthorized Access**: Locking critical resources.

### Configuration

```python
BOUNDARY_CONFIG = {
    "max_context_tokens": 128000,
    "max_response_tokens": 4096,
    "max_recursion_depth": 10,
    "protected_resources": ["core.memory", "system.files"]
}
```

---

## 📋 Comprehensive Workflows

### Workflow 1: Session Handoff & Boundary Setting

**Goal**: Cleanly end a session, securing state and passing context to the future.

**When to use**: End of day, project handoff, or before a system reset.

```python
from whitemagic.tools import session_handoff, check_system_health
from whitemagic.core.boundaries import BoundaryEngine

async def finalize_session(session_id: str):
    # 1. Enforce final boundary checks
    engine = BoundaryEngine()
    if not engine.check_integrity():
        print("⚠️ Boundary integrity compromise detected.")
    
    # 2. Lock core resources
    engine.lock_resource("core.memory")
    
    # 3. Create Handoff Artifact
    handoff = await session_handoff(
        session_id=session_id,
        target="continuation_agent",
        instructions="Resume from Phase 8.5"
    )
    
    # 4. Final Pulse
    print(f"🔒 Session {session_id} secured. Handoff created: {handoff.path}")
```

### Workflow 2: Token Limit Enforcement

**Goal**: Prevent runaway costs or context flooding.

**When to use**: Continuous background monitoring.

```python
from whitemagic.core.boundaries import check_token_limits

def monitor_usage(current_usage: int):
    status = check_token_limits(current_usage)
    
    if status == "CRITICAL":
        print("🛑 HARD STOP: Token limit reached.")
        return False
    elif status == "WARNING":
        print("⚠️ Approaching boundary limit.")
        
    return True
```

---

## � v14: Proactive Emergence Engine

As of v14, the Wall houses the **EmergenceEngine** — a proactive scanner that detects emergent patterns at the boundary between known and unknown:

### Proactive Scanning

The EmergenceEngine scans three sources via CoreAccessLayer:

1. **Constellation Convergence** — two or more knowledge clusters drifting toward each other in 5D holographic space
2. **Association Hotspots** — nodes with disproportionately high traversal counts, indicating hidden centrality
3. **Temporal Bursts** — sudden spikes in memory creation within a time window

```python
from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine

engine = get_emergence_engine()
insights = engine.scan_for_emergence()

for i in insights:
    print(f"  [{i.source}] {i.title} (confidence={i.confidence:.2f})")
    # [constellation_convergence] "Rust" and "Zig" clusters merging (confidence=0.82)
    # [temporal_burst] 20 memories created in 24h window (confidence=0.75)
```

### Insight History

The engine maintains a bounded history of past insights for trend analysis:

```python
past = engine.get_insights(limit=10)
status = engine.get_status()
print(f"Total insights tracked: {status['total_insights']}")
```

### Gana Tasks

| Task | Description |
|------|-------------|
| `scan_emergence` / `proactive_scan` | Run full emergence scan |
| `emergence_status` | Engine status and statistics |
| `get_insights` | Retrieve past insight history |

---

## �🧭 Navigation

**Next**: [Chapter 1: Session Initiation](01_HORN_SESSION_INITIATION.md) (The Return)
**Previous**: [Chapter 27: Encampment & Structure](27_ENCAMPMENT_STRUCTURE.md)
**Quadrant**: Northern (Winter/Water) - Position 7/7 (Final)
