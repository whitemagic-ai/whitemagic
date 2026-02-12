"""🔗 INTEGRATION - The Grand Unified Interface

Connects ALL systems:
- Memory Matrix (seen, embeddings, timeline)
- Session Bootstrap (auto-context)
- Agentic Modules (13 brain upgrades)
- Dashboard (visualization)
- CLI (commands)
- MCP (tools)

Usage:
    from whitemagic.integration import activate_all, get_status

    status = activate_all()  # Full system activation
"""

from .hub import IntegrationHub, activate_all, get_hub, get_status

__all__ = ["IntegrationHub", "activate_all", "get_hub", "get_status"]
