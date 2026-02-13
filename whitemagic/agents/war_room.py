"""War Room — Strategic Command & Control for Shadow Clone Armies
================================================================
The central command facility where Generals direct combined-arms operations
using the Imperial Doctrine, Art of War principles, and Wu Xing force timing.

Command Hierarchy (from the Gemini conversation):
  Emperor (User) → General (Primary Agent) → Lieutenant (Sub-Agent)
  → Officer (Swarm Orchestrator) → Soldier (Clone)

The War Room orchestrates:
  1. Campaign planning via Art of War terrain assessment
  2. Force composition via Imperial Doctrine
  3. Phase timing via Wu Xing elemental cycles
  4. Combined-arms execution via named tactics
  5. Ralph Wiggum fallback when smart approaches fail
  6. After-action reporting and knowledge harvesting

"The general who wins the battle makes many calculations in his temple
 before the battle is fought." — Sun Tzu, Ch. I

Usage:
    from whitemagic.agents.war_room import get_war_room

    war_room = get_war_room()

    # Plan a campaign
    campaign = war_room.plan_campaign(
        objective="Audit all 848 Python files for SQL injection vectors",
        emperor_intent="Find and fix security vulnerabilities",
    )

    # Execute a named tactic
    result = await war_room.execute_tactic(
        tactic_name="Cast a Brick to Attract Jade",
        objective="Find the bug in consolidation.py",
    )

    # Deploy the Empty Fort (last resort)
    result = await war_room.empty_fort(
        objective="Fix this impossible legacy bug",
        error_log="TypeError that makes no sense",
    )
"""

from __future__ import annotations

import logging
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Command Hierarchy
# ---------------------------------------------------------------------------

class Rank(str, Enum):
    """Command ranks in the Imperial Army."""

    EMPEROR = "emperor"          # User — The Mandate of Heaven
    GENERAL = "general"          # Primary Agent — Full context, decides strategy
    LIEUTENANT = "lieutenant"    # Sub-Agent — Slices plans into tasks
    OFFICER = "officer"          # Swarm Orchestrator — Routes and coordinates
    SOLDIER = "soldier"          # Clone — Executes and reports


@dataclass
class Commander:
    """A commander in the hierarchy."""

    rank: Rank
    name: str
    capabilities: list[str] = field(default_factory=list)
    subordinates: list[str] = field(default_factory=list)
    active_campaigns: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "rank": self.rank.value,
            "name": self.name,
            "capabilities": self.capabilities,
            "subordinates": self.subordinates,
            "active_campaigns": self.active_campaigns,
        }


# ---------------------------------------------------------------------------
# Campaign Status
# ---------------------------------------------------------------------------

class CampaignStatus(str, Enum):
    """Campaign lifecycle states."""

    PLANNING = "planning"
    RECONNAISSANCE = "reconnaissance"
    ENGAGEMENT = "engagement"
    CONSOLIDATION = "consolidation"
    VERIFICATION = "verification"
    COMPLETE = "complete"
    DEFEATED = "defeated"
    RETREATED = "retreated"


@dataclass
class CampaignRecord:
    """A record of a campaign execution."""

    campaign_id: str
    objective: str
    emperor_intent: str
    status: CampaignStatus
    tactic_used: str
    wu_xing_phase: str
    forces_deployed: list[dict[str, Any]] = field(default_factory=list)
    phases_completed: list[dict[str, Any]] = field(default_factory=list)
    total_clones_spawned: int = 0
    total_duration_ms: float = 0.0
    findings: list[str] = field(default_factory=list)
    created_at: str = ""
    completed_at: str = ""

    def __post_init__(self) -> None:
        if not self.created_at:
            self.created_at = datetime.now().isoformat()

    def to_dict(self) -> dict[str, Any]:
        return {
            "campaign_id": self.campaign_id,
            "objective": self.objective,
            "status": self.status.value,
            "tactic_used": self.tactic_used,
            "wu_xing_phase": self.wu_xing_phase,
            "total_clones_spawned": self.total_clones_spawned,
            "total_duration_ms": round(self.total_duration_ms, 2),
            "phases_completed": len(self.phases_completed),
            "findings_count": len(self.findings),
            "created_at": self.created_at,
            "completed_at": self.completed_at,
        }


# ---------------------------------------------------------------------------
# War Room
# ---------------------------------------------------------------------------

class WarRoom:
    """Strategic Command & Control for Shadow Clone Armies.

    The War Room is where campaigns are planned, forces are marshaled,
    and tactics are executed. It integrates all subsystems:
      - Imperial Doctrine (force composition + stratagems)
      - Art of War Engine (terrain assessment + campaign planning)
      - Wu Xing System (phase timing)
      - FoolGuard (Ralph Wiggum fallback)
      - Swarm Orchestrator (decomposition + routing)
      - Tokio Clone Army (mass consensus)
      - Python Async Executor (precision strikes)
    """

    def __init__(self) -> None:
        self._campaigns: list[CampaignRecord] = []
        self._commanders: dict[str, Commander] = {}
        self._max_campaigns = 100

        # Register default commanders
        self._register_default_hierarchy()

    def _register_default_hierarchy(self) -> None:
        """Set up the default command hierarchy."""
        self._commanders = {
            "emperor": Commander(
                Rank.EMPEROR, "Emperor",
                capabilities=["mandate", "vision", "objective_setting"],
            ),
            "general": Commander(
                Rank.GENERAL, "First General",
                capabilities=["strategy", "doctrine", "campaign_planning", "force_composition"],
                subordinates=["lieutenant_recon", "lieutenant_strike", "lieutenant_support"],
            ),
            "lieutenant_recon": Commander(
                Rank.LIEUTENANT, "Recon Lieutenant",
                capabilities=["reconnaissance", "scouting", "intelligence"],
                subordinates=["officer_tokio"],
            ),
            "lieutenant_strike": Commander(
                Rank.LIEUTENANT, "Strike Lieutenant",
                capabilities=["execution", "precision_strike", "bug_fixing"],
                subordinates=["officer_cavalry", "officer_ralph"],
            ),
            "lieutenant_support": Commander(
                Rank.LIEUTENANT, "Support Lieutenant",
                capabilities=["consolidation", "testing", "verification"],
                subordinates=["officer_siege"],
            ),
            "officer_tokio": Commander(
                Rank.OFFICER, "Tokio Infantry Commander",
                capabilities=["mass_scan", "consensus", "voting"],
            ),
            "officer_cavalry": Commander(
                Rank.OFFICER, "Python Cavalry Commander",
                capabilities=["mcp_tools", "code_edit", "analysis"],
            ),
            "officer_ralph": Commander(
                Rank.OFFICER, "Dare-to-Die Commander",
                capabilities=["stateless_fix", "brute_force", "chaos"],
            ),
            "officer_siege": Commander(
                Rank.OFFICER, "Siege Engineer Commander",
                capabilities=["testing", "deployment", "heavy_tools"],
            ),
        }

    # ------------------------------------------------------------------
    # Campaign Planning
    # ------------------------------------------------------------------

    def plan_campaign(
        self,
        objective: str,
        emperor_intent: str = "",
        constraints: dict[str, Any] | None = None,
    ) -> CampaignRecord:
        """Plan a full campaign for an objective.

        Integrates:
          - Art of War terrain assessment
          - Imperial Doctrine force composition
          - Wu Xing phase detection
          - Stratagem selection

        Args:
            objective: What needs to be accomplished
            emperor_intent: High-level intent from the user
            constraints: Optional constraints (max_clones, time_limit, etc.)

        Returns:
            CampaignRecord ready for execution
        """
        campaign_id = f"campaign_{uuid.uuid4().hex[:8]}"

        # Get doctrine recommendations
        try:
            from whitemagic.agents.doctrine import get_doctrine
            doctrine = get_doctrine()
            summary = doctrine.campaign_summary(objective)
            tactic_name = summary["recommended_tactic"]["name"]
            wu_xing_phase = summary["wu_xing_phase"]
            forces = summary["force_composition"]
            total_clones = summary["total_clones"]
        except Exception as e:
            logger.debug(f"Doctrine unavailable, using defaults: {e}")
            tactic_name = "Chain of Stratagems"
            wu_xing_phase = "earth"
            forces = [{"type": "light_infantry", "count": 1000}]
            total_clones = 1000

        # Get Art of War terrain assessment
        try:
            from whitemagic.core.intelligence.wisdom.art_of_war import get_war_engine
            engine = get_war_engine()
            plan = engine.plan_campaign(objective)
            terrain_desc = plan.terrain.description
        except Exception:
            terrain_desc = "unknown terrain"

        campaign = CampaignRecord(
            campaign_id=campaign_id,
            objective=objective,
            emperor_intent=emperor_intent or objective,
            status=CampaignStatus.PLANNING,
            tactic_used=tactic_name,
            wu_xing_phase=wu_xing_phase,
            forces_deployed=forces,
            total_clones_spawned=total_clones,
            findings=[f"Terrain: {terrain_desc}"],
        )

        self._campaigns.append(campaign)
        if len(self._campaigns) > self._max_campaigns:
            self._campaigns = self._campaigns[-self._max_campaigns:]

        logger.info(
            f"Campaign {campaign_id} planned: '{objective}' "
            f"(tactic={tactic_name}, phase={wu_xing_phase}, clones={total_clones})"
        )

        return campaign

    # ------------------------------------------------------------------
    # Tactic Execution
    # ------------------------------------------------------------------

    async def execute_tactic(
        self,
        tactic_name: str,
        objective: str,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Execute a named tactic from the Imperial Doctrine.

        Available tactics:
          - "Cast a Brick to Attract Jade" — Two-phase filtering
          - "Besiege Wei to Rescue Zhao" — Decomposition
          - "The Empty Fort" — Chaos monkey fallback
          - "The Wave of Ignorance" — Ralph Wiggum brute force
          - "Chain of Stratagems" — Full pipeline

        Args:
            tactic_name: Name of the tactic to execute
            objective: What to accomplish
            context: Optional additional context

        Returns:
            Dict with execution results
        """
        t0 = time.time()
        tactic_lower = tactic_name.lower()
        results: dict[str, Any] = {
            "tactic": tactic_name,
            "objective": objective,
            "phases": [],
        }

        if "brick" in tactic_lower or "jade" in tactic_lower:
            results = await self._execute_cast_brick(objective, context)
        elif "wei" in tactic_lower or "zhao" in tactic_lower:
            results = await self._execute_besiege_wei(objective, context)
        elif "empty fort" in tactic_lower:
            results = await self._execute_empty_fort(objective, context)
        elif "wave" in tactic_lower or "ignorance" in tactic_lower:
            results = await self._execute_wave_of_ignorance(objective, context)
        elif "chain" in tactic_lower:
            results = await self._execute_chain_stratagems(objective, context)
        else:
            # Default: Chain of Stratagems
            results = await self._execute_chain_stratagems(objective, context)

        results["total_duration_ms"] = (time.time() - t0) * 1000
        return results

    async def _execute_cast_brick(
        self, objective: str, context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """Cast a Brick to Attract Jade (抛砖引玉).

        Phase 1: Deploy Tokio infantry for rough consensus (the Brick)
        Phase 2: Send Python cavalry to precise targets (the Jade)
        """
        phases = []

        # Phase 1: The Skirmish — Tokio infantry scouts
        phase1 = await self._deploy_tokio_scouts(objective, num_clones=5000)
        phases.append({"name": "The Skirmish", "type": "tokio_infantry", **phase1})

        # Phase 2: The Charge — Python cavalry on narrowed targets
        targets = phase1.get("top_strategies", [])
        phase2 = await self._deploy_cavalry_strike(objective, targets)
        phases.append({"name": "The Charge", "type": "python_cavalry", **phase2})

        return {
            "tactic": "Cast a Brick to Attract Jade",
            "chinese": "抛砖引玉",
            "objective": objective,
            "phases": phases,
            "stratagems": [17, 13],
        }

    async def _execute_besiege_wei(
        self, objective: str, context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """Besiege Wei to Rescue Zhao (围魏救赵).

        Phase 1: Officers decompose into lanes
        Phase 2: Route each lane to optimal force
        Phase 3: Execute all lanes in parallel
        """
        phases = []

        # Phase 1: The Council — Swarm decomposition
        try:
            from whitemagic.agents.swarm import get_swarm
            swarm = get_swarm()
            plan = swarm.decompose(objective)
            subtasks = [t.description for t in plan.subtasks]
            phases.append({
                "name": "The Council",
                "type": "officers",
                "plan_id": plan.id,
                "subtasks": subtasks,
            })
        except Exception as e:
            subtasks = [objective]
            phases.append({"name": "The Council", "type": "officers", "error": str(e)})

        # Phase 2+3: Route and execute each lane
        lane_results = []
        for i, task in enumerate(subtasks[:5]):  # Cap at 5 lanes
            result = await self._deploy_tokio_scouts(task, num_clones=1000)
            lane_results.append({"lane": i, "task": task[:80], **result})

        phases.append({"name": "The Pincer", "type": "combined", "lanes": lane_results})

        return {
            "tactic": "Besiege Wei to Rescue Zhao",
            "chinese": "围魏救赵",
            "objective": objective,
            "phases": phases,
            "stratagems": [2, 3, 35],
        }

    async def _execute_empty_fort(
        self, objective: str, context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """The Empty Fort Strategy (空城计).

        When smart agents are stuck, flood with chaos.
        Phase 1: Ralph Wiggum Dare-to-Die Corps
        Phase 2: Tokio flood with creative strategies
        Phase 3: Consensus from the chaos
        """
        phases = []

        # Phase 1: Dare-to-Die Corps
        try:
            from whitemagic.core.intelligence.agentic.fool_guard import get_fool_guard
            guard = get_fool_guard()
            error_log = (context or {}).get("error_log")
            target_file = (context or {}).get("target_file")
            dtd_result = await guard.deploy_dare_to_die(
                mission=objective,
                target_file=target_file,
                error_log=error_log,
                max_attempts=10,
            )
            phases.append({
                "name": "The Bluff",
                "type": "dare_to_die",
                **dtd_result.to_dict(),
            })
        except Exception as e:
            phases.append({"name": "The Bluff", "type": "dare_to_die", "error": str(e)})

        # Phase 2: Tokio flood
        phase2 = await self._deploy_tokio_scouts(
            objective, num_clones=10000,
            strategies=["creative", "synthesis", "adversarial"],
        )
        phases.append({"name": "The Haze", "type": "tokio_flood", **phase2})

        return {
            "tactic": "The Empty Fort",
            "chinese": "空城计",
            "objective": objective,
            "phases": phases,
            "stratagems": [32, 20, 27],
        }

    async def _execute_wave_of_ignorance(
        self, objective: str, context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """The Wave of Ignorance (敢死队冲锋).

        Pure Ralph Wiggum doctrine. Stateless clones, fresh context, brute force.
        """
        phases = []

        try:
            from whitemagic.core.intelligence.agentic.fool_guard import get_fool_guard
            guard = get_fool_guard()
            error_log = (context or {}).get("error_log")
            target_file = (context or {}).get("target_file")

            # Deploy with higher attempt count for brute force
            result = await guard.deploy_dare_to_die(
                mission=objective,
                target_file=target_file,
                error_log=error_log,
                max_attempts=20,
            )
            phases.append({
                "name": "The Charge",
                "type": "dare_to_die",
                **result.to_dict(),
            })
        except Exception as e:
            phases.append({"name": "The Charge", "error": str(e)})

        return {
            "tactic": "The Wave of Ignorance",
            "chinese": "敢死队冲锋",
            "objective": objective,
            "phases": phases,
            "stratagems": [27, 34, 15],
        }

    async def _execute_chain_stratagems(
        self, objective: str, context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """Chain of Stratagems (连环计).

        Full pipeline: Scout → Filter → Strike → Consolidate → Verify.
        """
        phases = []

        # Scout
        scout = await self._deploy_tokio_scouts(objective, num_clones=5000)
        phases.append({"name": "Scout", "type": "tokio_infantry", **scout})

        # Filter (Officers analyze)
        try:
            from whitemagic.agents.swarm import get_swarm
            swarm = get_swarm()
            plan = swarm.decompose(objective)
            filter_result = {
                "plan_id": plan.id,
                "subtasks": len(plan.subtasks),
            }
        except Exception:
            filter_result = {"subtasks": 1}
        phases.append({"name": "Filter", "type": "officers", **filter_result})

        # Strike (Python cavalry)
        strike = await self._deploy_cavalry_strike(objective, [])
        phases.append({"name": "Strike", "type": "python_cavalry", **strike})

        # Consolidate (Siege engineers — WorkerDaemon)
        phases.append({
            "name": "Consolidate",
            "type": "siege_engineers",
            "status": "queued_for_worker_daemon",
        })

        # Verify (Ralph clones — fresh eyes)
        try:
            from whitemagic.core.intelligence.agentic.fool_guard import get_fool_guard
            guard = get_fool_guard()
            verify = await guard.deploy_dare_to_die(
                mission=f"Verify: {objective}", max_attempts=5,
            )
            phases.append({"name": "Verify", "type": "dare_to_die", **verify.to_dict()})
        except Exception as e:
            phases.append({"name": "Verify", "error": str(e)})

        return {
            "tactic": "Chain of Stratagems",
            "chinese": "连环计",
            "objective": objective,
            "phases": phases,
            "stratagems": [35, 17, 18, 22],
        }

    # ------------------------------------------------------------------
    # Force Deployment Helpers
    # ------------------------------------------------------------------

    async def _deploy_tokio_scouts(
        self,
        objective: str,
        num_clones: int = 5000,
        strategies: list[str] | None = None,
    ) -> dict[str, Any]:
        """Deploy Tokio Light Infantry for mass reconnaissance."""
        try:
            from whitemagic.optimization.rust_accelerators import (
                tokio_clones_available,
                tokio_deploy_clones,
            )
            if tokio_clones_available():
                result = tokio_deploy_clones(
                    objective, num_clones,
                    strategies or ["direct", "analytical", "adversarial", "creative", "synthesis"],
                )
                if result:
                    return {
                        "clones_deployed": result.get("total_clones", 0),
                        "winner_strategy": result.get("winner", {}).get("strategy", "?"),
                        "avg_confidence": result.get("avg_confidence", 0),
                        "elapsed_ms": result.get("elapsed_ms", 0),
                        "top_strategies": list(result.get("strategy_votes", {}).keys()),
                        "backend": "rust_tokio",
                    }
        except Exception as e:
            logger.debug(f"Tokio deployment failed: {e}")

        # Fallback: Python async simulation
        return {
            "clones_deployed": 0,
            "winner_strategy": "direct",
            "avg_confidence": 0.5,
            "elapsed_ms": 0,
            "top_strategies": ["direct"],
            "backend": "python_fallback",
            "note": "Rust Tokio unavailable",
        }

    async def _deploy_cavalry_strike(
        self,
        objective: str,
        targets: list[str],
    ) -> dict[str, Any]:
        """Deploy Python Heavy Cavalry for precision strikes.

        In production, this would invoke AdaptiveParallelExecutor with
        actual MCP tool calls. Currently returns a deployment record.
        """
        return {
            "clones_deployed": min(50, max(1, len(targets))),
            "targets": targets[:10],
            "strategy": "chain_of_thought",
            "backend": "python_async",
            "status": "ready_for_execution",
        }

    # ------------------------------------------------------------------
    # Quick-Access Shortcuts
    # ------------------------------------------------------------------

    async def empty_fort(
        self,
        objective: str,
        error_log: str | None = None,
        target_file: str | None = None,
    ) -> dict[str, Any]:
        """Quick shortcut: Execute The Empty Fort Strategy.

        Use when smart agents are stuck and you need chaos.
        """
        return await self.execute_tactic(
            "The Empty Fort",
            objective,
            context={"error_log": error_log, "target_file": target_file},
        )

    async def cast_brick(self, objective: str) -> dict[str, Any]:
        """Quick shortcut: Cast a Brick to Attract Jade.

        Two-phase: mass scouting then precision strike.
        """
        return await self.execute_tactic("Cast a Brick to Attract Jade", objective)

    async def wave_of_ignorance(
        self,
        objective: str,
        error_log: str | None = None,
        target_file: str | None = None,
    ) -> dict[str, Any]:
        """Quick shortcut: The Wave of Ignorance (pure Ralph Wiggum)."""
        return await self.execute_tactic(
            "The Wave of Ignorance",
            objective,
            context={"error_log": error_log, "target_file": target_file},
        )

    # ------------------------------------------------------------------
    # Wu Xing Integration
    # ------------------------------------------------------------------

    def detect_optimal_phase(self) -> dict[str, Any]:
        """Detect the optimal Wu Xing phase for current conditions.

        Considers time of day and system harmony state.
        """
        result: dict[str, Any] = {}

        # Time-based phase (from wu_xing.py)
        try:
            from whitemagic.core.intelligence.wisdom.wu_xing import get_wu_xing
            wx = get_wu_xing()
            phase = wx.detect_current_phase()
            suggestion = wx.suggest_optimization(phase)
            balance = wx.check_balance()
            result.update({
                "time_phase": phase.value,
                "suggestion": suggestion,
                "balance": balance,
            })
        except Exception:
            hour = datetime.now().hour
            phases = {
                range(6, 10): "wood",
                range(10, 14): "fire",
                range(14, 18): "earth",
                range(18, 22): "metal",
            }
            detected = "water"
            for rng, p in phases.items():
                if hour in rng:
                    detected = p
                    break
            result["time_phase"] = detected

        # Harmony-based adjustment
        try:
            from whitemagic.harmony.vector import read_harmony_fast
            snap = read_harmony_fast()
            result["harmony_score"] = snap.harmony_score
            result["energy"] = snap.energy
            # Low energy → suggest Water (reflection) over Fire (action)
            if snap.energy < 0.4:
                result["energy_override"] = "water"
                result["recommendation"] = "Low energy detected. Favor reflection over action."
        except Exception:
            pass

        return result

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_hierarchy(self) -> dict[str, Any]:
        """Get the full command hierarchy."""
        return {name: cmd.to_dict() for name, cmd in self._commanders.items()}

    def get_campaign_history(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get recent campaign records."""
        return [c.to_dict() for c in self._campaigns[-limit:]]

    def get_status(self) -> dict[str, Any]:
        """Get War Room status summary."""
        active = sum(1 for c in self._campaigns if c.status not in
                     (CampaignStatus.COMPLETE, CampaignStatus.DEFEATED, CampaignStatus.RETREATED))
        total_clones = sum(c.total_clones_spawned for c in self._campaigns)

        return {
            "total_campaigns": len(self._campaigns),
            "active_campaigns": active,
            "total_clones_spawned": total_clones,
            "commanders": len(self._commanders),
            "available_tactics": [
                "Cast a Brick to Attract Jade",
                "Besiege Wei to Rescue Zhao",
                "The Empty Fort",
                "The Wave of Ignorance",
                "Chain of Stratagems",
            ],
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_war_room: WarRoom | None = None


def get_war_room() -> WarRoom:
    """Get the global War Room instance."""
    global _war_room
    if _war_room is None:
        _war_room = WarRoom()
        logger.info("War Room initialized — 9 Commanders, 5 Tactics, 36 Stratagems ready")
    return _war_room
