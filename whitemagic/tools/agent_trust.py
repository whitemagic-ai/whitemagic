"""Agent Trust Scores — Reputation Derived from Karma Ledger.
============================================================
Each agent that interacts with WhiteMagic accumulates a trust profile
based on how accurately their tool calls match declared side-effects.

Trust dimensions:
  - **reliability** — fraction of calls with zero mismatch
  - **mismatch_rate** — fraction of calls with declared vs actual mismatch
  - **debt_contribution** — total karma debt this agent has accrued
  - **call_volume** — total tool calls by this agent
  - **composite_trust** — weighted score [0.0–1.0] where 1.0 = perfect

The composite trust score can be used by:
  - Rate limiter (lower trust → stricter limits)
  - Maturity gates (require minimum trust for advanced tools)
  - Pipeline validation (warn on low-trust agent pipelines)
  - Homeostatic loop (factor trust into health assessments)

MCP tool: ``agent.trust``
"""

from __future__ import annotations

import logging
from typing import Any

logger = logging.getLogger(__name__)


def get_agent_trust_scores(agent_id: str | None = None) -> dict[str, Any]:
    """Compute trust scores for agents from Karma Ledger data.

    If agent_id is provided, returns scores for that agent only.
    Otherwise returns scores for all known agents.
    """
    try:
        from whitemagic.dharma.karma_ledger import get_karma_ledger
        ledger = get_karma_ledger()
        report = ledger.report()
    except Exception as e:
        return {"status": "error", "error": f"Karma Ledger unavailable: {e}"}

    entries = report.get("entries", [])
    if not entries:
        return {
            "status": "success",
            "agents": {},
            "note": "No karma ledger entries found — trust scores unavailable",
        }

    # Aggregate per-agent stats
    agent_stats: dict[str, _AgentStats] = {}
    for entry in entries:
        # Entries may have an agent_id field, or fall back to "default"
        aid = entry.get("agent_id", "default")
        if aid not in agent_stats:
            agent_stats[aid] = _AgentStats()
        stats = agent_stats[aid]
        stats.total_calls += 1
        stats.total_debt += entry.get("debt_delta", 0.0)
        if entry.get("mismatch", False):
            stats.mismatches += 1

    # Compute trust scores
    result_agents: dict[str, dict[str, Any]] = {}
    for aid, stats in agent_stats.items():
        if agent_id and aid != agent_id:
            continue
        mismatch_rate = stats.mismatches / max(stats.total_calls, 1)
        reliability = 1.0 - mismatch_rate
        # Composite: weighted blend of reliability (70%) and debt penalty (30%)
        # Debt penalty: 1.0 if zero debt, decays as debt grows
        debt_penalty = 1.0 / (1.0 + stats.total_debt * 0.1)
        composite = 0.7 * reliability + 0.3 * debt_penalty

        result_agents[aid] = {
            "reliability": round(reliability, 4),
            "mismatch_rate": round(mismatch_rate, 4),
            "debt_contribution": round(stats.total_debt, 4),
            "call_volume": stats.total_calls,
            "mismatches": stats.mismatches,
            "composite_trust": round(composite, 4),
            "tier": _trust_tier(composite),
        }

    return {
        "status": "success",
        "agents": result_agents,
        "total_agents": len(result_agents),
    }


class _AgentStats:
    """Accumulator for per-agent karma stats."""

    __slots__ = ("total_calls", "mismatches", "total_debt")

    def __init__(self) -> None:
        self.total_calls: int = 0
        self.mismatches: int = 0
        self.total_debt: float = 0.0


def _trust_tier(composite: float) -> str:
    """Map composite trust score to a human-readable tier."""
    if composite >= 0.95:
        return "EXEMPLARY"
    elif composite >= 0.85:
        return "TRUSTED"
    elif composite >= 0.70:
        return "STANDARD"
    elif composite >= 0.50:
        return "PROBATIONARY"
    else:
        return "RESTRICTED"
