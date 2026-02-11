# Infrastructure (Legacy Scripts)

These are **standalone scripts** originally from the Tools toolbox, kept here
for reference and direct CLI usage.

**For AI agents**: use the proper Whitemagic tool contract instead of calling
these scripts directly. The functionality has been integrated as first-class
tools:

| Legacy Script | Whitemagic Tool(s) |
|---|---|
| `devops/redis-async-ai.py` | `broker.publish`, `broker.history`, `broker.status` |
| `monitoring/workflow_dashboard.py` | `task.list`, `task.status` (programmatic access) |
| `devops/ollama-integration.sh` | `ollama.models`, `ollama.generate`, `ollama.chat` |
| `optimization/supershell.sh` | (system-level, not exposed as tool — run directly) |

## Usage via Tool Contract

```python
from whitemagic.tools.unified_api import call_tool

# Publish a message to coordinate agents
call_tool("broker.publish", channel="tasks", message="build project", sender="agent-1")

# Distribute a task
call_tool("task.distribute", command="make build", task_type="build")

# Create an ensemble vote
call_tool("vote.create", problem="Which approach is best?")

# Query a local Ollama model
call_tool("ollama.generate", model="phi3", prompt="Explain asyncio")
```

All tool calls return the stable Whitemagic envelope format with `status`,
`details`, `request_id`, and full telemetry.
