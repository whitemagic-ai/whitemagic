"""Maturity Gates - Gated Developmental Milestones.
================================================
Inspired by CyberBrains' Jaynes x Bailey developmental progression:
  Seed → Bicameral → Self-Conscious → Radiant → Group-Conscious → Logos

Each milestone unlocks new capabilities only after safety gates pass.
This prevents the system from using advanced features before the
foundational layers are verified and stable.

The maturity model applies to a running Whitemagic instance:
  - Stage 1 (Seed):        Core memory + tool registry operational
  - Stage 2 (Bicameral):   Governor + event bus verified
  - Stage 3 (Reflective):  Consciousness + retention engine online
  - Stage 4 (Radiant):     Surplus sharing, multi-agent coordination
  - Stage 5 (Collective):  Mesh/guild contracts, swarm intelligence
  - Stage 6 (Logos):       Long-horizon foresight, planetary design

Usage:
    from whitemagic.core.governance.maturity_gates import (
        get_maturity_engine, MaturityStage
    )

    engine = get_maturity_engine()
    report = engine.assess()
    print(report.current_stage)       # MaturityStage.REFLECTIVE
    print(report.unlocked_capabilities)
    print(report.next_gate)
"""

import logging
import threading
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import IntEnum
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Maturity Stages
# ---------------------------------------------------------------------------

class MaturityStage(IntEnum):
    """Developmental milestones, ordered by increasing capability."""

    SEED = 1          # Bare minimum: memory + tools
    BICAMERAL = 2     # Governor + event bus verified
    REFLECTIVE = 3    # Consciousness + retention engine
    RADIANT = 4       # Surplus sharing + coordination
    COLLECTIVE = 5    # Mesh / guild contracts
    LOGOS = 6         # Long-horizon foresight


# Capabilities unlocked at each stage
_STAGE_CAPABILITIES: dict[MaturityStage, set[str]] = {
    MaturityStage.SEED: {
        "memory.create",
        "memory.read",
        "memory.search",
        "memory.list",
        "tools.registry",
        "tools.dispatch",
        "cli.basic",
    },
    MaturityStage.BICAMERAL: {
        "governor.validate",
        "governor.budget",
        "event_bus.emit",
        "event_bus.listen",
        "temporal_scheduler.fast",
        "temporal_scheduler.medium",
        "mcp.serve",
    },
    MaturityStage.REFLECTIVE: {
        "consciousness.self_reflect",
        "consciousness.narrative",
        "retention.evaluate",
        "retention.sweep",
        "salience.score",
        "salience.spotlight",
        "temporal_scheduler.slow",
        "memory.consolidate",
        "patterns.detect",
    },
    MaturityStage.RADIANT: {
        "surplus.share",
        "surplus.donate_flops",
        "coordination.multi_agent",
        "gardens.full_suite",
        "api.publish",
    },
    MaturityStage.COLLECTIVE: {
        "mesh.connect",
        "mesh.guild_contract",
        "mesh.swarm_reasoning",
        "governance.council_vote",
        "governance.proposal",
    },
    MaturityStage.LOGOS: {
        "foresight.long_horizon",
        "foresight.scenario_map",
        "foresight.dream_query",
        "design.planetary",
    },
}


# ---------------------------------------------------------------------------
# Safety Gate Checks
# ---------------------------------------------------------------------------

@dataclass
class GateResult:
    """Result of a single gate check."""

    gate_name: str
    passed: bool
    reason: str
    details: dict[str, Any] = field(default_factory=dict)


def _check_seed() -> list[GateResult]:
    """Stage 1: Can we store and retrieve memories? Is the tool registry loaded?"""
    results = []

    # Memory system
    try:
        from whitemagic.core.memory.unified import get_unified_memory
        get_unified_memory()
        results.append(GateResult("memory_system", True, "UnifiedMemory operational"))
    except Exception as exc:
        results.append(GateResult("memory_system", False, f"UnifiedMemory failed: {exc}"))

    # Tool registry
    try:
        from whitemagic.tools.registry import TOOL_REGISTRY
        count = len(TOOL_REGISTRY)
        results.append(GateResult("tool_registry", count > 0, f"{count} tools registered"))
    except Exception as exc:
        results.append(GateResult("tool_registry", False, f"Registry failed: {exc}"))

    return results


def _check_bicameral() -> list[GateResult]:
    """Stage 2: Governor + event bus operational."""
    results = []

    # Governor
    try:
        from whitemagic.core.governor import get_governor
        get_governor()
        results.append(GateResult("governor", True, "Governor active"))
    except Exception as exc:
        results.append(GateResult("governor", False, f"Governor failed: {exc}"))

    # Event bus
    try:
        from whitemagic.core.resonance.gan_ying_enhanced import get_bus
        get_bus()
        results.append(GateResult("event_bus", True, "Gan Ying Bus active"))
    except Exception as exc:
        results.append(GateResult("event_bus", False, f"Event bus failed: {exc}"))

    return results


def _check_reflective() -> list[GateResult]:
    """Stage 3: Consciousness + retention engine + salience arbiter online."""
    results = []

    # Retention engine
    try:
        from whitemagic.core.memory.mindful_forgetting import get_retention_engine
        get_retention_engine()
        results.append(GateResult("retention_engine", True, "RetentionEngine operational"))
    except Exception as exc:
        results.append(GateResult("retention_engine", False, f"RetentionEngine failed: {exc}"))

    # Salience arbiter
    try:
        from whitemagic.core.resonance.salience_arbiter import get_salience_arbiter
        get_salience_arbiter()
        results.append(GateResult("salience_arbiter", True, "SalienceArbiter operational"))
    except Exception as exc:
        results.append(GateResult("salience_arbiter", False, f"SalienceArbiter failed: {exc}"))

    # Temporal scheduler
    try:
        from whitemagic.core.resonance.temporal_scheduler import get_temporal_scheduler
        get_temporal_scheduler()
        results.append(GateResult("temporal_scheduler", True, "TemporalScheduler operational"))
    except Exception as exc:
        results.append(GateResult("temporal_scheduler", False, f"TemporalScheduler failed: {exc}"))

    return results


def _check_radiant() -> list[GateResult]:
    """Stage 4: Multi-agent + surplus sharing capabilities.

    Requires actual usage history — not just capability presence.
    A fresh install should NOT reach RADIANT until the system has been
    meaningfully used (memories created, sessions run, etc.).
    """
    results = []

    # Usage history gate: require at least 5 memories and 1 session
    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        count = um.backend.count() if hasattr(um.backend, "count") else 0
        has_usage = count >= 5
        results.append(GateResult(
            "usage_history", has_usage,
            f"{count} memories stored (need ≥5)",
            details={"memory_count": count},
        ))
    except Exception as exc:
        results.append(GateResult("usage_history", False, f"Could not check usage: {exc}"))

    # Check homeostasis (system must be self-regulating)
    try:
        from whitemagic.core.homeostasis import HomeostasisSystem
        h = HomeostasisSystem()
        metrics = h.assess_health()
        healthy = metrics.coherence >= 0.5
        results.append(GateResult(
            "homeostasis", healthy,
            f"Coherence={metrics.coherence:.2f}",
            details={"coherence": metrics.coherence},
        ))
    except Exception as exc:
        results.append(GateResult("homeostasis", False, f"Homeostasis check failed: {exc}"))

    return results


def _check_collective() -> list[GateResult]:
    """Stage 5: Mesh connectivity + guild contracts."""
    results = []

    # Check mesh availability (Go mesh binary or gRPC proto)
    try:
        from pathlib import Path
        mesh_proto = Path(__file__).resolve().parents[3] / "mesh" / "proto" / "mesh.proto"
        results.append(GateResult(
            "mesh_proto", mesh_proto.exists(),
            "Mesh proto available" if mesh_proto.exists() else "Mesh proto not found",
        ))
    except Exception as exc:
        results.append(GateResult("mesh_proto", False, f"Mesh check failed: {exc}"))

    # Zodiac Council
    try:
        from whitemagic.core.governance.zodiac_council import get_council
        get_council()
        results.append(GateResult("zodiac_council", True, "ZodiacCouncil available"))
    except Exception as exc:
        results.append(GateResult("zodiac_council", False, f"Council unavailable: {exc}"))

    return results


def _check_logos() -> list[GateResult]:
    """Stage 6: Long-horizon foresight (aspirational — not yet implemented)."""
    results = []
    # This is forward-looking. For now, always returns not-passed
    # until a foresight engine is implemented.
    results.append(GateResult(
        "foresight_engine", False,
        "Logos-grade foresight engine not yet implemented (aspirational)",
    ))
    return results


_GATE_CHECKERS: dict[MaturityStage, Callable] = {
    MaturityStage.SEED: _check_seed,
    MaturityStage.BICAMERAL: _check_bicameral,
    MaturityStage.REFLECTIVE: _check_reflective,
    MaturityStage.RADIANT: _check_radiant,
    MaturityStage.COLLECTIVE: _check_collective,
    MaturityStage.LOGOS: _check_logos,
}


# ---------------------------------------------------------------------------
# Assessment Report
# ---------------------------------------------------------------------------

@dataclass
class MaturityReport:
    """Full assessment of the system's developmental maturity."""

    current_stage: MaturityStage
    all_gates: dict[str, list[GateResult]]  # stage_name -> gate results
    unlocked_capabilities: set[str]
    next_gate: str | None                # Name of next stage to unlock
    next_gate_blockers: list[GateResult]    # What's preventing advancement
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        caps = sorted(self.unlocked_capabilities)
        return {
            "current_stage": self.current_stage.name,
            "current_stage_value": self.current_stage.value,
            "unlocked_capabilities": caps,
            "capability_count": len(caps),
            "next_gate": self.next_gate,
            "next_gate_blockers": [
                {"gate": g.gate_name, "reason": g.reason}
                for g in self.next_gate_blockers
            ],
            "timestamp": self.timestamp.isoformat(),
        }


# ---------------------------------------------------------------------------
# Maturity Engine
# ---------------------------------------------------------------------------

class MaturityEngine:
    """Evaluates the system's developmental maturity by running gate checks
    for each stage in order. Stops at the first stage where any gate fails.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._last_report: MaturityReport | None = None
        self._assessment_count: int = 0

    def assess(self) -> MaturityReport:
        """Run all gate checks and determine current maturity stage."""
        all_gates: dict[str, list[GateResult]] = {}
        current_stage = MaturityStage.SEED  # Minimum
        unlocked: set[str] = set()
        next_gate: str | None = None
        next_blockers: list[GateResult] = []

        for stage in MaturityStage:
            checker = _GATE_CHECKERS.get(stage)
            if checker is None:
                continue

            try:
                results = checker()
            except Exception as exc:
                results = [GateResult(f"{stage.name}_error", False, str(exc))]

            all_gates[stage.name] = results
            all_passed = all(r.passed for r in results)

            if all_passed:
                current_stage = stage
                unlocked |= _STAGE_CAPABILITIES.get(stage, set())
            else:
                # This is the next gate we need to pass
                next_gate = stage.name
                next_blockers = [r for r in results if not r.passed]
                break

        report = MaturityReport(
            current_stage=current_stage,
            all_gates=all_gates,
            unlocked_capabilities=unlocked,
            next_gate=next_gate,
            next_gate_blockers=next_blockers,
        )

        with self._lock:
            self._last_report = report
            self._assessment_count += 1

        logger.info(
            f"🌱 Maturity Assessment #{self._assessment_count}: "
            f"Stage={current_stage.name} ({current_stage.value}/6), "
            f"Capabilities={len(unlocked)}"
            + (f", Next gate: {next_gate}" if next_gate else " — FULLY MATURE"),
        )

        return report

    def is_capable(self, capability: str) -> bool:
        """Check if a specific capability is unlocked at the current maturity."""
        if self._last_report is None:
            self.assess()
        return capability in (self._last_report.unlocked_capabilities if self._last_report else set())

    def require_stage(self, minimum: MaturityStage) -> bool:
        """Check if the system has reached at least the given stage."""
        if self._last_report is None:
            self.assess()
        return (self._last_report.current_stage if self._last_report else MaturityStage.SEED) >= minimum

    def get_stats(self) -> dict[str, Any]:
        return {
            "assessment_count": self._assessment_count,
            "current_stage": self._last_report.current_stage.name if self._last_report else "NOT_ASSESSED",
            "capabilities_unlocked": len(self._last_report.unlocked_capabilities) if self._last_report else 0,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_engine_instance: MaturityEngine | None = None
_engine_lock = threading.Lock()


def get_maturity_engine() -> MaturityEngine:
    """Get or create the global MaturityEngine singleton."""
    global _engine_instance
    with _engine_lock:
        if _engine_instance is None:
            _engine_instance = MaturityEngine()
        return _engine_instance
