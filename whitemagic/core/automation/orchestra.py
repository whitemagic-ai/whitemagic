"""Automation Orchestra - System Integration Hub.

Coordinates all automated systems to work together harmoniously:
- Immune System (detect & heal threats)
- Consolidation Engine (memory management)
- Trigger System (when to act)
- Metrics Tracking (performance monitoring)

Like an orchestra conductor, this ensures all systems play together
in harmony, creating emergent capabilities greater than the sum of parts.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from whitemagic.core import MemoryManager  # type: ignore[attr-defined]
from whitemagic.core.automation.consolidation import ConsolidationEngine
from whitemagic.core.automation.triggers import TriggerManager
from whitemagic.core.immune import (
    AntibodyLibrary,
    ImmuneMemory,
    ImmuneResponse,
    ThreatDetector,
    ThreatLevel,
)
from whitemagic.metrics.collector import MetricsCollector

logger = logging.getLogger(__name__)


@dataclass
class OrchestrationEvent:
    """An event that triggers coordinated system actions."""

    event_type: str  # "immune_threat", "memory_overflow", "health_check"
    source_system: str  # Which system detected the event
    severity: str  # "low", "medium", "high", "critical"
    data: dict
    timestamp: str


class AutomationOrchestra:
    """Coordinates all automated systems.

    Enables cross-system intelligence:
    - Immune threat → Trigger consolidation
    - Memory overflow → Trigger immune scan
    - Health degradation → Trigger full system check
    """

    def __init__(self, project_root: Path | None = None):
        self.project_root = project_root or Path.cwd()

        # Initialize all systems
        self.memory_manager = MemoryManager(base_dir=str(self.project_root))
        self.consolidation = ConsolidationEngine(self.memory_manager)
        self.trigger_manager = TriggerManager(self.memory_manager)
        self.threat_detector = ThreatDetector(self.project_root)
        self.antibody_library = AntibodyLibrary(self.project_root)
        self.immune_memory = ImmuneMemory()
        self.immune_response = ImmuneResponse(self.antibody_library, self.immune_memory)
        self.metrics = MetricsCollector()

        # Event queue for cross-system coordination
        self.event_queue: list[OrchestrationEvent] = []

    def perform_health_check(self, auto_heal: bool = False) -> dict:
        """Comprehensive health check across all systems.

        Returns unified health report with coordinated actions.
        """
        logger.info("🎼 Automation Orchestra - Full Health Check")
        logger.info("=" * 60)

        health_report: dict[str, Any] = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "overall_health": "unknown",
            "systems": {},
            "coordinated_actions": [],
            "recommendations": [],
        }

        # 1. Immune System Scan
        logger.info("\n🔬 Scanning immune system...")
        threats = self.threat_detector.scan_system()
        immune_health = self.threat_detector.generate_health_report()

        health_report["systems"]["immune"] = {
            "health_score": immune_health["health_score"],
            "threats_found": len(threats),
            "critical_threats": sum(1 for t in threats if t.level == ThreatLevel.CRITICAL),
        }

        # 2. Memory System Health
        logger.info("🧠 Checking memory system...")
        try:
            stats = self.memory_manager.stats()
            short_term_count = stats.get("short_term", 0)
            memory_health_score = 100 - min(50, (short_term_count / 40) * 50)  # Degrade as we approach 40

            health_report["systems"]["memory"] = {
                "health_score": memory_health_score,
                "short_term_count": short_term_count,
                "threshold": 40,
                "status": "healthy" if short_term_count < 30 else "needs_consolidation",
            }
        except Exception as e:
            health_report["systems"]["memory"] = {
                "health_score": 50,
                "error": str(e),
            }

        # 3. Calculate Overall Health
        system_scores = [s.get("health_score", 50) for s in health_report["systems"].values()]
        overall_score = sum(system_scores) / len(system_scores) if system_scores else 50
        health_report["overall_health"] = self._score_to_status(overall_score)
        health_report["overall_score"] = overall_score

        # 4. Coordinate Actions (the magic happens here!)
        coordinated_actions = self._coordinate_actions(health_report, threats, auto_heal)
        health_report["coordinated_actions"] = coordinated_actions

        # 5. Generate Recommendations
        health_report["recommendations"] = self._generate_recommendations(health_report)

        return health_report

    def _coordinate_actions(self, health_report: dict, threats: list, auto_heal: bool) -> list[dict]:
        """Coordinate actions across systems based on health status.

        This is where system integration creates emergent intelligence!
        """
        actions = []

        # Rule 1: Critical immune threats → Trigger immediate healing
        critical_threats = [t for t in threats if t.level == ThreatLevel.CRITICAL]
        if critical_threats and auto_heal:
            logger.info("\n⚠️  CRITICAL THREATS DETECTED - Triggering immune response")
            outcomes = self.immune_response.respond_to_threats(critical_threats, auto_heal=True)
            actions.append({
                "system": "immune",
                "action": "auto_heal_critical",
                "threats_addressed": len(critical_threats),
                "success_rate": sum(1 for o in outcomes if o.success) / len(outcomes) if outcomes else 0,
            })

        # Rule 2: Memory overflow + immune threats → Consolidate first, then heal
        memory_status = health_report["systems"].get("memory", {})
        if memory_status.get("status") == "needs_consolidation" and len(threats) > 10:
            logger.info("\n🔄 Memory overflow + many threats → Consolidating first")
            # Consolidation might fix some threats (e.g., memory leak)
            consolidation_results = self.consolidation.auto_consolidate(dry_run=not auto_heal)
            actions.append({
                "system": "consolidation",
                "action": "auto_consolidate",
                "triggered_by": "memory_overflow_with_threats",
                "results": consolidation_results.get("metrics", {}),
            })

        # Rule 3: Low overall health → Trigger full maintenance cycle
        if health_report.get("overall_score", 100) < 60:
            logger.info("\n🏥 Low overall health → Full maintenance cycle")
            actions.append({
                "system": "orchestra",
                "action": "full_maintenance",
                "triggered_by": "low_health_score",
                "recommendation": "Run all automated systems in sequence",
            })

        # Rule 4: High threat count → Check if consolidation would help
        if len(threats) > 15:
            # Many threats might indicate systemic issues
            actions.append({
                "system": "analysis",
                "action": "threat_pattern_analysis",
                "threat_count": len(threats),
                "recommendation": "Analyze threat patterns for systemic issues",
            })

        return actions

    def _score_to_status(self, score: float) -> str:
        """Convert numeric score to status string."""
        if score >= 90:
            return "excellent"
        elif score >= 75:
            return "good"
        elif score >= 60:
            return "fair"
        elif score >= 40:
            return "poor"
        else:
            return "critical"

    def _generate_recommendations(self, health_report: dict) -> list[str]:
        """Generate actionable recommendations based on health report."""
        recommendations = []

        overall_score = health_report.get("overall_score", 50)

        if overall_score < 60:
            recommendations.append("🔧 Run 'whitemagic.systems.immune heal --no-dry-run' to fix threats")

        memory_system = health_report["systems"].get("memory", {})
        if memory_system.get("status") == "needs_consolidation":
            recommendations.append("📦 Run 'whitemagic consolidate --no-dry-run' to clean memory")

        immune_system = health_report["systems"].get("immune", {})
        if immune_system.get("critical_threats", 0) > 0:
            recommendations.append("⚠️  Address critical threats immediately")

        if overall_score >= 90:
            recommendations.append("✅ System is healthy - continue monitoring")

        return recommendations

    def trigger_maintenance_cycle(self, dry_run: bool = True) -> dict:
        """Run a full automated maintenance cycle:
        1. Immune scan
        2. Heal critical threats
        3. Consolidate memories
        4. Update metrics.
        """
        logger.info("🎼 Automation Orchestra - Maintenance Cycle")
        logger.info("=" * 60)

        results: dict[str, Any] = {
            "started_at": datetime.now(timezone.utc).isoformat(),
            "dry_run": dry_run,
            "steps": [],
        }

        # Step 1: Immune Scan
        logger.info("\n1️⃣  Immune System Scan...")
        threats = self.threat_detector.scan_system()
        results["steps"].append({
            "step": "immune_scan",
            "threats_found": len(threats),
            "critical": sum(1 for t in threats if t.level == ThreatLevel.CRITICAL),
        })

        # Step 2: Heal Critical Threats
        critical_threats = [t for t in threats if t.level == ThreatLevel.CRITICAL]
        if critical_threats:
            logger.info(f"\n2️⃣  Healing {len(critical_threats)} critical threats...")
            outcomes = self.immune_response.respond_to_threats(critical_threats, auto_heal=not dry_run)
            results["steps"].append({
                "step": "heal_critical",
                "addressed": len(outcomes),
                "successful": sum(1 for o in outcomes if o.success),
            })
        else:
            logger.info("\n2️⃣  No critical threats - skipping")
            results["steps"].append({"step": "heal_critical", "skipped": True})

        # Step 3: Consolidate Memories
        logger.info("\n3️⃣  Memory Consolidation...")
        consolidation_results = self.consolidation.auto_consolidate(dry_run=dry_run)
        results["steps"].append({
            "step": "consolidation",
            "archived": len(consolidation_results.get("archived", [])),
            "promoted": len(consolidation_results.get("promoted", [])),
            "merged": len(consolidation_results.get("merged", [])),
            "scratchpads_cleaned": len(consolidation_results.get("scratchpads_cleaned", [])),
        })

        # Step 4: Final Health Check
        logger.info("\n4️⃣  Final Health Check...")
        final_health = self.threat_detector.generate_health_report()
        results["final_health"] = final_health

        results["completed_at"] = datetime.now(timezone.utc).isoformat()

        logger.info("\n" + "=" * 60)
        logger.info("✅ Maintenance cycle complete")
        logger.info(f"   Final Health Score: {final_health['health_score']}/100")

        return results

    def emergency_response(self) -> dict:
        """Emergency response for critical system issues.

        Runs immediately with auto-heal enabled.
        """
        logger.info("🚨 EMERGENCY RESPONSE ACTIVATED")
        logger.info("=" * 60)

        # Run health check with auto-heal
        health_report = self.perform_health_check(auto_heal=True)

        # If still critical, try maintenance cycle
        if health_report.get("overall_score", 100) < 40:
            logger.info("\n⚡ Running emergency maintenance...")
            maintenance_results = self.trigger_maintenance_cycle(dry_run=False)
            health_report["emergency_maintenance"] = maintenance_results

        return health_report
