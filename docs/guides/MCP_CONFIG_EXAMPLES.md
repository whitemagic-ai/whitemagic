# MCP Integration Cookbook

WhiteMagic exposes its 313 tools (or 28 PRAT Gana meta-tools) via the Model Context Protocol (MCP) over stdio. This guide covers configuration for all major MCP clients.

---

## Modes

| Mode | Tools | Env Var | Best For |
|------|-------|---------|----------|
| **PRAT** | 28 Gana meta-tools | `WM_MCP_PRAT=1` | AI agents (recommended) |
| **Classic** | 313 individual tools | _(default)_ | Power users |
| **Lite** | 92 core tools | `WM_MCP_LITE=1` | Fast startup |

---

## Client Configurations

### Windsurf (`.mcp.json` in project root)

```json
{
  "mcpServers": {
    "whitemagic": {
      "type": "stdio",
      "command": "python3",
      "args": ["-m", "whitemagic.run_mcp"],
      "cwd": ".",
      "env": {
        "PYTHONPATH": ".",
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1"
      }
    }
  }
}
```

> **Note:** Restart Windsurf after changing `.mcp.json`.

### Cursor (`.mcp.json` in project root)

```json
{
  "mcpServers": {
    "whitemagic-prat": {
      "type": "stdio",
      "command": "python3",
      "args": ["-m", "whitemagic.run_mcp"],
      "cwd": ".",
      "env": {
        "PYTHONPATH": ".",
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1",
        "WM_STATE_ROOT": "/tmp/whitemagic_state"
      }
    }
  }
}
```

### Claude Desktop (`claude_desktop_config.json`)

Location:
- **macOS:** `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Linux:** `~/.config/Claude/claude_desktop_config.json`
- **Windows:** `%APPDATA%\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "whitemagic": {
      "command": "python3",
      "args": ["-m", "whitemagic.run_mcp"],
      "cwd": "/absolute/path/to/whitemagic",
      "env": {
        "PYTHONPATH": "/absolute/path/to/whitemagic",
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1"
      }
    }
  }
}
```

### VS Code (settings.json)

If using an MCP extension for VS Code:

```json
{
  "mcp.servers": {
    "whitemagic": {
      "command": "python3",
      "args": ["-m", "whitemagic.run_mcp"],
      "cwd": "${workspaceFolder}",
      "env": {
        "PYTHONPATH": "${workspaceFolder}",
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1"
      }
    }
  }
}
```

### Docker

```bash
docker run --rm -i ghcr.io/whitemagic-ai/whitemagic:latest
```

Or with custom state:
```bash
docker run --rm -i \
  -v /path/to/state:/data/whitemagic \
  ghcr.io/whitemagic-ai/whitemagic:latest
```

### pip-installed (global)

If WhiteMagic is installed via pip:

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

---

## Schema Adaptation

Some MCP clients have schema limitations. Set `WM_MCP_CLIENT` to auto-adapt:

| Client | Env Value | What it does |
|--------|-----------|--------------|
| Gemini | `gemini` | Removes `default` from required params |
| DeepSeek | `deepseek` | Simplifies enum descriptions |
| Qwen | `qwen` | Flattens nested objects |

Example:
```json
"env": {
  "WM_MCP_PRAT": "1",
  "WM_MCP_CLIENT": "gemini"
}
```

---

## Orientation Resources

After connecting, MCP clients can request bootstrap documents:
- `whitemagic://orientation/ai-primary` — Primary orientation for AI runtimes
- `whitemagic://grimoire/index` — Grimoire chapter navigation

These are served via MCP `resources/list` and `resources/read`.

---

## Troubleshooting

### Server doesn't start

1. **Check Python version:** `python3 --version` (needs 3.10+)
2. **Check fastmcp:** `python3 -c "import fastmcp; print('ok')"`
3. **Test manually:** `WM_SILENT_INIT=1 python3 -m whitemagic.run_mcp` — should print tool count to stderr
4. **Check path:** Ensure `cwd` and `PYTHONPATH` point to the whitemagic repo root

### "fastmcp not installed"

```bash
pip install whitemagic[mcp]
# or
pip install fastmcp
```

### Too many tools / client timeout

Use PRAT mode (`WM_MCP_PRAT=1`) to reduce from 313 → 28 tools, or Lite mode (`WM_MCP_LITE=1`) for 92 tools.

### Tools not responding

1. Check `wm doctor` for system health
2. Check `WM_STATE_ROOT` is writable
3. Try: `WM_SILENT_INIT=1 python3 -c "from whitemagic.tools.unified_api import call_tool; print(call_tool('capabilities')['status'])"`

### Permission errors

Default role is `coordinator` (full access). To restrict:
```python
from whitemagic.tools.tool_permissions import get_agent_role_registry
get_agent_role_registry().set_default_roles(["agent"])
```

### Rate limiting

Default: 60 calls/min per tool, 300 calls/min global. Check with:
```python
from whitemagic.tools.unified_api import call_tool
call_tool("rate_limiter.stats")
```

---

## Gratitude

WhiteMagic is **free and open-source** (MIT). If it's been useful to you, gratitude is welcome but never expected.

- **XRPL Tip Address**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
- **Contact**: whitemagicdev@proton.me
