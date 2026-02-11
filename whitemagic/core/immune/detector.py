"""Threat Detection System.

Inspired by biological immune systems' ability to recognize pathogens,
this module detects threats to system health:
- Version drift
- Import errors
- Missing dependencies
- Inconsistent state
- Configuration drift
"""

from __future__ import annotations

from typing import Any
import re
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path


class ThreatLevel(Enum):
    """Threat severity levels (inspired by immune response intensity)."""

    LOW = "low"              # Minor issue, monitor only
    MEDIUM = "medium"        # Should fix soon
    HIGH = "high"            # Fix immediately
    CRITICAL = "critical"    # System-breaking, emergency response


class ThreatType(Enum):
    """Types of threats the immune system recognizes."""

    VERSION_DRIFT = "version_drift"
    IMPORT_ERROR = "import_error"
    MISSING_DEPENDENCY = "missing_dependency"
    STATE_INCONSISTENCY = "state_inconsistency"
    CONFIGURATION_DRIFT = "configuration_drift"
    MEMORY_LEAK = "memory_leak"
    QUALITY_DEGRADATION = "quality_degradation"

    # NEW: From November 19-20 learnings
    TEST_COVERAGE_LOW = "test_coverage_low"  # <80% test coverage
    DOC_FRAGMENTATION = "doc_fragmentation"  # Too many scattered docs
    MODULE_ISOLATION = "module_isolation"  # Built but never integrated
    RESONANCE_BLOCKED = "resonance_blocked"  # Gan Ying not flowing
    ZODIAC_INCOMPLETE = "zodiac_incomplete"  # Missing core connections


@dataclass
class Threat:
    """Detected threat to system health."""

    threat_type: ThreatType
    level: ThreatLevel
    description: str
    location: str  # File, module, or system component
    detected_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    metadata: dict = field(default_factory=dict)

    # Biological metaphor fields
    antigen: str = ""  # The specific pattern that triggered detection
    suggested_antibody: str = ""  # Which antibody pattern to apply


class ThreatDetector:
    """Immune System Threat Detector.

    Scans the system for potential threats, similar to how
    immune cells patrol the body looking for foreign antigens.
    """

    def __init__(self, project_root: Path | None = None) -> None:
        self.project_root = project_root or Path.cwd()
        self.detected_threats: list[Threat] = []
        self.scan_history: list[dict] = []

    def scan_system(self) -> list[Threat]:
        """Perform full system scan for threats.

        Returns:
            List of detected threats

        """
        threats = []

        # Scan 1: Version consistency
        threats.extend(self._detect_version_drift())

        # Scan 2: Import integrity
        threats.extend(self._detect_import_issues())

        # Scan 3: Configuration consistency
        threats.extend(self._detect_config_drift())

        # Scan 4: Memory health
        threats.extend(self._detect_memory_issues())

        self.detected_threats = threats
        self._record_scan()

        return threats

    def _detect_version_drift(self) -> list[Threat]:
        """Detect version inconsistencies across the system."""
        threats = []

        # Read canonical version
        version_file = self.project_root / "VERSION"
        if not version_file.exists():
            threats.append(Threat(
                threat_type=ThreatType.VERSION_DRIFT,
                level=ThreatLevel.CRITICAL,
                description="Missing VERSION file - no source of truth",
                location=str(version_file),
                antigen="missing_version_file",
                suggested_antibody="version_sync",
            ))
            return threats

        canonical_version = version_file.read_text().strip()

        # Check version in key files
        version_locations = {
            "pyproject.toml": r'version\s*=\s*"([^"]+)"',
            "setup.py": r'version\s*=\s*"([^"]+)"',
            "README.md": r"v(\d+\.\d+\.\d+)",
            "whitemagic/__init__.py": r'__version__\s*=\s*"([^"]+)"',
        }

        for file_path, pattern in version_locations.items():
            full_path = self.project_root / file_path
            if not full_path.exists():
                continue

            content = full_path.read_text()
            matches = re.findall(pattern, content)

            for match in matches:
                if match != canonical_version:
                    threats.append(Threat(
                        threat_type=ThreatType.VERSION_DRIFT,
                        level=ThreatLevel.HIGH,
                        description=f"Version mismatch: {file_path} has '{match}' but VERSION has '{canonical_version}'",
                        location=str(full_path),
                        antigen=f"version_mismatch:{match}",
                        suggested_antibody="version_sync",
                        metadata={
                            "found_version": match,
                            "expected_version": canonical_version,
                            "file": file_path,
                        },
                    ))

        return threats

    def _detect_import_issues(self) -> list[Threat]:
        """Detect import errors and missing dependencies."""
        threats = []

        # Scan Python files for import statements
        py_files = list(self.project_root.glob("**/*.py"))

        for py_file in py_files:
            if ".venv" in str(py_file) or "node_modules" in str(py_file):
                continue

            try:
                content = py_file.read_text()

                # Find all import statements
                import_pattern = r"^(?:from\s+(\S+)\s+import|import\s+(\S+))"
                imports = re.findall(import_pattern, content, re.MULTILINE)

                # Check for common problematic imports
                for from_import, direct_import in imports:
                    module = from_import or direct_import

                    # Check if it's a whitemagic internal import
                    if module.startswith("whitemagic"):
                        # Verify the module exists
                        module_parts = module.split(".")
                        if len(module_parts) > 1:
                            module_path = self.project_root / "/".join(module_parts)
                            if not (module_path.with_suffix(".py").exists() or
                                    (module_path / "__init__.py").exists()):
                                threats.append(Threat(
                                    threat_type=ThreatType.IMPORT_ERROR,
                                    level=ThreatLevel.MEDIUM,
                                    description=f"Import '{module}' in {py_file.name} may not exist",
                                    location=str(py_file),
                                    antigen=f"missing_module:{module}",
                                    suggested_antibody="import_fix",
                                ))

            except Exception:
                # If we can't read the file, log but don't fail
                pass

        return threats

    def _detect_config_drift(self) -> list[Threat]:
        """Detect configuration inconsistencies."""
        threats = []

        # Check for example configs without actual configs.
        #
        # NOTE: Runtime config lives under WM_STATE_ROOT; the repo should not
        # contain `.whitemagic/*` state by default.
        config_pairs = [
            (".env", ".env.example"),
        ]

        for actual_config, example_config in config_pairs:
            actual_path = self.project_root / actual_config
            example_path = self.project_root / example_config

            if example_path.exists() and not actual_path.exists():
                threats.append(Threat(
                    threat_type=ThreatType.CONFIGURATION_DRIFT,
                    level=ThreatLevel.LOW,
                    description=f"Example config {example_config} exists but actual {actual_config} missing",
                    location=str(actual_path),
                    antigen="missing_config",
                    suggested_antibody="config_init",
                ))

        return threats

    def _detect_memory_issues(self) -> list[Threat]:
        """Detect memory system health issues."""
        threats = []

        from whitemagic.config.paths import MEMORY_DIR

        memory_dir = MEMORY_DIR
        if not memory_dir.exists():
            threats.append(Threat(
                threat_type=ThreatType.STATE_INCONSISTENCY,
                level=ThreatLevel.MEDIUM,
                description="Memory directory does not exist",
                location=str(memory_dir),
                antigen="missing_memory_dir",
                suggested_antibody="memory_init",
            ))
            return threats

        # Check for excessive short-term memories
        short_term_dir = memory_dir / "short_term"
        if short_term_dir.exists():
            short_term_count = len(list(short_term_dir.glob("*.md")))
            if short_term_count > 50:
                threats.append(Threat(
                    threat_type=ThreatType.MEMORY_LEAK,
                    level=ThreatLevel.MEDIUM,
                    description=f"Excessive short-term memories: {short_term_count} (threshold: 50)",
                    location=str(short_term_dir),
                    antigen="memory_leak",
                    suggested_antibody="consolidate",
                    metadata={"count": short_term_count, "threshold": 50},
                ))

        return threats

    def _record_scan(self) -> Any:
        """Record scan results for historical tracking."""
        scan_record = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "threats_found": len(self.detected_threats),
            "threat_levels": {
                "critical": sum(1 for t in self.detected_threats if t.level == ThreatLevel.CRITICAL),
                "high": sum(1 for t in self.detected_threats if t.level == ThreatLevel.HIGH),
                "medium": sum(1 for t in self.detected_threats if t.level == ThreatLevel.MEDIUM),
                "low": sum(1 for t in self.detected_threats if t.level == ThreatLevel.LOW),
            },
        }
        self.scan_history.append(scan_record)

    def get_critical_threats(self) -> list[Threat]:
        """Get only critical and high-priority threats."""
        return [
            t for t in self.detected_threats
            if t.level in [ThreatLevel.CRITICAL, ThreatLevel.HIGH]
        ]

    def generate_health_report(self) -> dict:
        """Generate system health report."""
        total_threats = len(self.detected_threats)
        critical_count = sum(1 for t in self.detected_threats if t.level == ThreatLevel.CRITICAL)

        if total_threats == 0:
            health_status = "HEALTHY"
            health_score = 100
        elif critical_count > 0:
            health_status = "CRITICAL"
            health_score = max(0, 50 - (critical_count * 10))
        elif total_threats < 5:
            health_status = "GOOD"
            health_score = max(70, 100 - (total_threats * 5))
        else:
            health_status = "FAIR"
            health_score = max(50, 100 - (total_threats * 3))

        return {
            "health_status": health_status,
            "health_score": health_score,
            "total_threats": total_threats,
            "threats_by_level": {
                "critical": sum(1 for t in self.detected_threats if t.level == ThreatLevel.CRITICAL),
                "high": sum(1 for t in self.detected_threats if t.level == ThreatLevel.HIGH),
                "medium": sum(1 for t in self.detected_threats if t.level == ThreatLevel.MEDIUM),
                "low": sum(1 for t in self.detected_threats if t.level == ThreatLevel.LOW),
            },
            "threats_by_type": self._group_by_type(),
            "scan_timestamp": datetime.now(timezone.utc).isoformat(),
        }

    def _group_by_type(self) -> dict[str, int]:
        """Group threats by type for reporting."""
        type_counts: dict[str, int] = {}
        for threat in self.detected_threats:
            type_name = threat.threat_type.value
            type_counts[type_name] = type_counts.get(type_name, 0) + 1
        return type_counts
