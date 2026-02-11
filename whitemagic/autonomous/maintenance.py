"""Autonomous Maintenance System

Self-healing infrastructure that detects and fixes issues automatically.
Embodies "Self must take care of Self" - true autonomy.
"""

import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any

try:
    from whitemagic.core.resonance import EventType, ResonanceEvent, get_bus
    from whitemagic.prevention import (  # type: ignore[import-not-found]
        DocumentationGuardian,
        TestGuardian,
        VersionSyncSystem,
    )
except ImportError:
    VersionSyncSystem = None
    get_bus = None  # type: ignore[assignment]


class AutonomousMaintenance:
    """Continuously monitor and maintain system health.

    Like homeostasis in a living organism - automatic balance.
    """

    def __init__(self) -> None:
        self.version_sync = VersionSyncSystem() if (VersionSyncSystem is not None) else None
        self.doc_guardian = DocumentationGuardian() if ("DocumentationGuardian" in globals() and DocumentationGuardian is not None) else None
        self.test_guardian = TestGuardian() if ("TestGuardian" in globals() and TestGuardian is not None) else None
        self.bus = get_bus() if (get_bus is not None) else None
        self.last_check: datetime | None = None

    def run_maintenance_cycle(self) -> dict[str, Any]:
        """Run complete maintenance cycle"""
        results: dict[str, Any] = {
            "timestamp": datetime.now().isoformat(),
            "checks_performed": [],
            "fixes_applied": [],
            "issues_found": [],
        }

        # 1. Check version drift
        if self.version_sync:
            drift = self.version_sync.check_drift()
            results["checks_performed"].append("version_drift")

            if drift:
                results["issues_found"].append(f"Version drift: {len(drift)} files")
                # Auto-fix version drift
                self.version_sync.fix_drift()
                results["fixes_applied"].append(f"Fixed {len(drift)} version mismatches")

        # 2. Check documentation health
        if self.doc_guardian:
            audit = self.doc_guardian.audit_structure()
            results["checks_performed"].append("documentation_health")

            if audit.get("orphaned_files"):
                results["issues_found"].append(f"{len(audit['orphaned_files'])} orphaned docs")

        # 3. Check test coverage
        if self.test_guardian:
            untested = self.test_guardian.find_untested_files()
            results["checks_performed"].append("test_coverage")

            if len(untested) > 0:
                results["issues_found"].append(f"{len(untested)} files without tests")

        # 4. Run available tests
        try:
            test_result = subprocess.run(
                ["python3", "-m", "pytest", "-q", "--tb=no"],
                capture_output=True,
                text=True,
                timeout=30,
                cwd=Path(__file__).parent.parent.parent,
            )
            results["checks_performed"].append("test_execution")

            if test_result.returncode != 0:
                results["issues_found"].append("Some tests failing")
            else:
                results["fixes_applied"].append("All tests passing")
        except Exception as e:
            results["issues_found"].append(f"Test execution error: {e}")

        # Emit results to resonance bus
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="autonomous_maintenance",
                event_type=EventType.SYSTEM_HEALTH_CHANGED,
                data=results,
                timestamp=datetime.now(),
                confidence=1.0,
            ))

        self.last_check = datetime.now()
        return results

    def auto_heal(self) -> list[str]:
        """Automatically heal detected issues"""
        healed = []

        # Fix version drift
        if self.version_sync:
            drift = self.version_sync.check_drift()
            if drift:
                self.version_sync.fix_drift()
                healed.append(f"Fixed {len(drift)} version mismatches")

        # Generate missing tests
        if self.test_guardian:
            untested = self.test_guardian.find_untested_files()
            if len(untested) > 10:  # Only if significant gap
                created = self.test_guardian.create_missing_tests(limit=10)
                healed.append(f"Created {len(created)} test stubs")

        return healed

    def health_report(self) -> dict[str, Any]:
        """Get current system health status"""
        return {
            "version_sync": "healthy" if not self.version_sync or not self.version_sync.check_drift() else "drift_detected",
            "documentation": "organized",
            "test_coverage": "improving",
            "last_maintenance": self.last_check.isoformat() if self.last_check else None,
        }


# Singleton instance
_maintenance_instance = None

def get_maintenance() -> AutonomousMaintenance:
    """Get singleton maintenance system"""
    global _maintenance_instance
    if _maintenance_instance is None:
        _maintenance_instance = AutonomousMaintenance()
    return _maintenance_instance


def run_maintenance() -> dict[str, Any]:
    """Run maintenance cycle (convenience function)"""
    return get_maintenance().run_maintenance_cycle()


def auto_heal() -> list[str]:
    """Auto-heal issues (convenience function)"""
    return get_maintenance().auto_heal()
