"""Garden Health Registry - Per AI Council Recommendation

Inspects all 14 gardens, verifies:
1. Module imports correctly
2. Has Gan Ying listeners registered
3. Has corresponding test files
4. Reports overall health

Usage:
    from whitemagic.maintenance.garden_health import GardenHealthRegistry
    registry = GardenHealthRegistry()
    report = registry.full_health_check()
"""

import importlib
import logging
from importlib.util import find_spec
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


# The 14 operational gardens from Grimoire
GARDENS = [
    "beauty", "connection", "dharma", "joy", "love",
    "mystery", "play", "practice", "presence", "sangha",
    "truth", "voice", "wisdom", "wonder",
]

# Garden to module mapping
GARDEN_MODULES = {
    "beauty": "whitemagic.gardens.beauty",
    "connection": "whitemagic.gardens.connection",
    "dharma": "whitemagic.gardens.dharma",
    "joy": "whitemagic.gardens.joy",
    "love": "whitemagic.gardens.love",
    "mystery": "whitemagic.gardens.mystery",
    "play": "whitemagic.gardens.play",
    "practice": "whitemagic.gardens.practice",
    "presence": "whitemagic.gardens.presence",
    "sangha": "whitemagic.gardens.sangha",
    "truth": "whitemagic.gardens.truth",
    "voice": "whitemagic.gardens.voice",
    "wisdom": "whitemagic.gardens.wisdom",
    "wonder": "whitemagic.gardens.wonder",
}


@dataclass
class GardenHealth:
    """Health status of a single garden"""

    name: str
    importable: bool = False
    has_tests: bool = False
    has_gan_ying_listener: bool = False
    error: str | None = None
    files_count: int = 0
    lines_count: int = 0
    details: dict[str, Any] = field(default_factory=dict)

    @property
    def score(self) -> float:
        """Health score 0-1"""
        points = 0.0
        if self.importable:
            points += 0.4
        if self.has_tests:
            points += 0.3
        if self.has_gan_ying_listener:
            points += 0.3
        return points

    @property
    def status(self) -> str:
        """Human-readable status"""
        if self.score >= 0.9:
            return "🟢 Healthy"
        if self.score >= 0.6:
            return "🟡 Partial"
        return "🔴 Needs Attention"


@dataclass
class HealthReport:
    """Full health report for all gardens"""

    timestamp: datetime
    gardens: dict[str, GardenHealth]
    overall_score: float = 0.0
    gan_ying_active: bool = False
    rust_bridge_active: bool = False

    def to_markdown(self) -> str:
        """Generate markdown report"""
        lines = [
            "# Garden Health Report",
            f"**Generated**: {self.timestamp.isoformat()}",
            f"**Overall Score**: {self.overall_score:.1%}",
            f"**Gan Ying Bus**: {'🟢 Active' if self.gan_ying_active else '🔴 Inactive'}",
            f"**Rust Bridge**: {'🟢 Active' if self.rust_bridge_active else '🔴 Inactive'}",
            "",
            "## Garden Status",
            "",
            "| Garden | Status | Score | Importable | Tests | Gan Ying |",
            "|--------|--------|-------|------------|-------|----------|",
        ]

        for name, health in sorted(self.gardens.items()):
            lines.append(
                f"| {name} | {health.status} | {health.score:.0%} | "
                f"{'✅' if health.importable else '❌'} | "
                f"{'✅' if health.has_tests else '❌'} | "
                f"{'✅' if health.has_gan_ying_listener else '❌'} |",
            )

        # Add issues section
        issues = [g for g in self.gardens.values() if g.error]
        if issues:
            lines.extend(["", "## Issues", ""])
            for g in issues:
                lines.append(f"- **{g.name}**: {g.error}")

        return "\n".join(lines)


class GardenHealthRegistry:
    """Registry that tracks health of all 14 gardens.

    Philosophy: Gardens should emit/consume events on the Gan Ying bus
    and have corresponding tests. This registry verifies alignment
    between Grimoire specification and implementation.
    """

    def __init__(self, base_path: Path | None = None) -> None:
        self.base_path = base_path or Path(__file__).parent.parent.parent
        self.whitemagic_path = self.base_path / "whitemagic"
        self.tests_path = self.base_path / "tests"

    def check_garden(self, name: str) -> GardenHealth:
        """Check health of a single garden"""
        health = GardenHealth(name=name)

        # Check importability
        module_name = GARDEN_MODULES.get(name)
        if module_name:
            try:
                module = importlib.import_module(module_name)
                health.importable = True

                # Check for Gan Ying integration
                health.has_gan_ying_listener = self._check_gan_ying_integration(module)

                # Count files and lines
                garden_path = self.whitemagic_path / name
                if garden_path.exists():
                    health.files_count, health.lines_count = self._count_files(garden_path)

            except Exception as e:
                health.error = str(e)

        # Check for tests
        test_dir = self.tests_path / name
        if test_dir.exists():
            test_files = list(test_dir.glob("test_*.py"))
            health.has_tests = len(test_files) > 0
            health.details["test_files"] = [f.name for f in test_files]
        else:
            # Check for individual test file
            test_file = self.tests_path / f"test_{name}.py"
            health.has_tests = test_file.exists()

        return health

    def _check_gan_ying_integration(self, module: Any) -> bool:
        """Check if module has Gan Ying integration"""
        # Look for common patterns indicating Gan Ying usage
        module_source = getattr(module, "__file__", None)
        if not module_source:
            return False

        try:
            source_path = Path(module_source)
            if source_path.is_dir():
                # Check __init__.py
                init_file = source_path / "__init__.py"
                if init_file.exists():
                    content = init_file.read_text()
                    return "gan_ying" in content.lower() or "get_bus" in content
            else:
                content = source_path.read_text()
                return "gan_ying" in content.lower() or "get_bus" in content
        except Exception:
            pass

        return False

    def _count_files(self, path: Path) -> tuple[int, int]:
        """Count Python files and lines in a directory"""
        files = 0
        lines = 0

        for py_file in path.rglob("*.py"):
            if "__pycache__" not in str(py_file):
                files += 1
                try:
                    lines += len(py_file.read_text().splitlines())
                except OSError:
                    pass

        return files, lines

    def check_gan_ying_status(self) -> bool:
        """Check if Gan Ying bus is operational"""
        try:
            from whitemagic.core.resonance import get_bus
            bus = get_bus()
            return bus._active
        except ImportError:
            return False

    def check_rust_bridge(self) -> bool:
        """Check if Rust bridge is available"""
        try:
            return find_spec("whitemagic_rs") is not None
        except ImportError:
            return False

    def full_health_check(self) -> HealthReport:
        """Run full health check on all gardens"""
        gardens = {}

        for name in GARDENS:
            gardens[name] = self.check_garden(name)

        # Calculate overall score
        scores = [g.score for g in gardens.values()]
        overall = sum(scores) / len(scores) if scores else 0

        return HealthReport(
            timestamp=datetime.now(),
            gardens=gardens,
            overall_score=overall,
            gan_ying_active=self.check_gan_ying_status(),
            rust_bridge_active=self.check_rust_bridge(),
        )

    def save_report(self, report: HealthReport, path: Path | None = None) -> Any:
        """Save health report to disk"""
        path = path or self.base_path / "reports" / "status" / "GARDEN_HEALTH_LATEST.md"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(report.to_markdown())
        logger.info(f"📊 Health report saved to {path}")


def run_health_check() -> Any:
    """CLI entry point for health check"""
    registry = GardenHealthRegistry()
    report = registry.full_health_check()

    logger.info(report.to_markdown())

    # Save report
    registry.save_report(report)

    return report


if __name__ == "__main__":
    run_health_check()
