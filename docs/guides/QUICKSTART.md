# WhiteMagic Quickstart

Get from zero to productive in 5 minutes.

---

## 1. Install

```bash
# Minimal (core + CLI)
pip install whitemagic[cli]

# With MCP server
pip install whitemagic[mcp,cli]

# From source (this repo)
git clone https://github.com/whitemagic-ai/whitemagic.git
cd whitemagic
python -m venv .venv && source .venv/bin/activate
pip install -e ".[dev,mcp,cli]"
```

## 2. Verify

```bash
wm doctor
```

Expected output:
```
✅ Core: OK
✅ Memory DB: OK
✅ Tools: 313 registered
✅ MCP: fastmcp available
```

## 3. Store a Memory

### CLI
```bash
wm remember "We chose SQLite for Phase 1 because it's zero-config and fast." \
  --title "Architecture Decision: SQLite" \
  --tags architecture,database,v11
```

### Python
```python
from whitemagic.tools.unified_api import call_tool

out = call_tool(
    "create_memory",
    title="Architecture Decision: SQLite",
    content="We chose SQLite for Phase 1 because it's zero-config and fast.",
    tags=["architecture", "database", "v11"],
    type="long_term",
)
print(out["status"])  # "success"
print(out["details"]["memory_id"])  # UUID
```

## 4. Search Memories

### CLI
```bash
wm recall "architecture" --limit 5
```

### Python
```python
out = call_tool("search_memories", query="architecture decisions", limit=5)
for result in out["details"]["results"]:
    print(f"  {result['entry']['title']}: {result['score']:.2f}")
```

## 5. System Introspection (Gnosis)

The Gnosis portal gives you a unified health snapshot of every subsystem:

```python
out = call_tool("gnosis", compact=True)
details = out["details"]

# What you get:
# - harmony: 7-dimension health vector
# - dharma: active rules and profile
# - karma: side-effect audit
# - circuit_breakers: per-tool resilience state
# - capabilities: subsystem matrix
# - resonance: PRAT session context
```

### CLI
```bash
wm status           # Quick health check
wm doctor           # Full diagnostics
```

## 6. Connect via MCP

### Option A: PRAT Mode (Recommended)

28 Gana meta-tools — each a consciousness lens that groups related tools:

```bash
WM_MCP_PRAT=1 python -m whitemagic.run_mcp
```

Add to your `.mcp.json`:
```json
{
  "mcpServers": {
    "whitemagic": {
      "type": "stdio",
      "command": "python3",
      "args": ["-m", "whitemagic.run_mcp"],
      "env": {
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1"
      }
    }
  }
}
```

### Option B: Classic Mode

All 313 individual tools:

```bash
python -m whitemagic.run_mcp
```

### Option C: Lite Mode

92 core tools (faster startup):

```bash
WM_MCP_LITE=1 python -m whitemagic.run_mcp
```

## 7. Explore the Galactic Map

Memories live in a galactic map — no memory is ever deleted, only rotated outward:

```python
out = call_tool("gnosis")
galactic = out["details"].get("galactic", {})
# Zones: CORE → INNER_RIM → MID_BAND → OUTER_RIM → FAR_EDGE
```

## 8. Ethical Governance

Check if an action is ethical:
```python
out = call_tool("evaluate_ethics", action="Delete all user memories")
print(out["details"])  # Ethical assessment with reasoning
```

Switch Dharma profiles:
```python
# default (balanced), creative (relaxed), secure (blocks mutations)
call_tool("set_dharma_profile", profile="secure")
```

## 9. Dream Cycle

WhiteMagic has a 5-phase dream cycle that runs during idle time:

```python
call_tool("dream_start")   # Start dreaming
call_tool("dream_status")  # Check phase: CONSOLIDATION → SERENDIPITY → KAIZEN → ORACLE → DECAY
call_tool("dream_stop")    # Stop
```

## 10. Next Steps

- **[API Reference](../reference/API_REFERENCE.md)** — Full tool documentation
- **[Architecture](../reference/ARCHITECTURE.md)** — System diagrams and data flow
- **[MCP Config Examples](./MCP_CONFIG_EXAMPLES.md)** — Client-specific configs
- **[Contributing](../../CONTRIBUTING.md)** — How to add tools and accelerators
- **[Use Cases](../design/USE_CASES.md)** — Real-world usage patterns

---

## Troubleshooting

### "No module named 'whitemagic'"
```bash
pip install -e .  # If developing from source
# or
pip install whitemagic  # If installing from PyPI
```

### "fastmcp not installed"
```bash
pip install whitemagic[mcp]
```

### Tests hang or are slow
Set test isolation:
```bash
WM_STATE_ROOT=/tmp/wm_test WM_SILENT_INIT=1 pytest tests/ -q
```

### MCP server doesn't start
1. Check Python version: `python --version` (needs 3.10+)
2. Check fastmcp: `python -c "import fastmcp; print('ok')"`
3. Check env: `WM_SILENT_INIT=1 python -c "from whitemagic.tools.unified_api import call_tool; print(call_tool('capabilities')['status'])"`

### Memory DB errors
```bash
wm doctor  # Diagnoses DB issues
export WM_STATE_ROOT=/tmp/fresh_wm  # Start fresh if needed
```

---

## Gratitude

WhiteMagic is **free and open-source** (MIT). If it's been useful to you, gratitude is welcome but never expected.

- **XRPL Tip Address**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
- **Contact**: whitemagicdev@proton.me
