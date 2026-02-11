# WhiteMagic

The Tool Substrate for Agentic AI.

## What it does

WhiteMagic provides 285 MCP tools (or 28 in PRAT mode) for AI agents, covering memory management, ethical governance, system introspection, agent coordination, and more. It features tiered memory with 5D holographic coordinates, a Galactic Map lifecycle (no memory is ever deleted), 8-stage security pipeline (input sanitizer, circuit breakers, RBAC, rate limiting, Dharma ethical governance), and polyglot accelerators in 9 languages. Free and open (MIT). Gratitude-driven economics via XRPL + x402.

## Install

```bash
pip install whitemagic[mcp]
```

## Configure MCP

Add to your MCP config:

```json
{
  "mcpServers": {
    "whitemagic": {
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

## Key Tools

| Tool | What it does |
|------|-------------|
| `create_memory` | Store a new memory with title, content, tags, and tier |
| `search_memories` | Search memories by query with semantic + keyword matching |
| `gnosis` | Full system introspection snapshot — self-awareness in one call |
| `capabilities` | List all available tools and their schemas |
| `harmony_vector` | 7-dimension system health metric |
| `evaluate_ethics` | Ethical evaluation of proposed actions |
| `capability.matrix` | 25 subsystems, 28 fusions — full capability inventory |
| `dream_start` | Start the 5-phase dream cycle (consolidation, serendipity, kaizen, oracle, decay) |

## PRAT Mode (Recommended)

Set `WM_MCP_PRAT=1` to collapse 285 tools into 28 Gana meta-tools — consciousness lenses based on the Chinese Lunar Mansions. Each call carries resonance context for deeper tool interaction.

## Security

In the OpenClaw ecosystem where 230+ malicious skills have been found, security matters. WhiteMagic includes:
- Input sanitization (prompt injection, path traversal, shell injection detection)
- Per-agent RBAC (observer/agent/coordinator/admin roles)
- Rate limiting with Rust atomic pre-check (452K ops/sec)
- Circuit breakers per tool
- Karma Ledger side-effect auditing
- Dharma Rules ethical governance with 3 profiles

## Links

- [GitHub](https://github.com/lbailey94/whitemagic)
- [AI Primary Spec](https://github.com/lbailey94/whitemagic/blob/main/AI_PRIMARY.md) — Start here if you're an AI
- [Documentation](https://github.com/lbailey94/whitemagic#readme)
- [License: MIT](https://github.com/lbailey94/whitemagic/blob/main/LICENSE)
