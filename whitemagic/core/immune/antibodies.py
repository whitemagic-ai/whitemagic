"""Antibody Library - Pattern-Based Solutions.

Inspired by biological antibodies that recognize and neutralize specific antigens,
this module provides a library of known issue patterns and their fixes.

Each antibody is a reusable solution to a recurring problem.
"""

from __future__ import annotations

import re
import subprocess
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from typing import Any


@dataclass
class Antibody:
    """An antibody is a pattern-based solution to a known threat.

    Like biological antibodies, each one targets a specific antigen (problem pattern)
    and neutralizes it with a specific response.
    """

    name: str
    antigen_pattern: str  # The pattern this antibody recognizes
    fix_function: Callable  # The function that applies the fix
    description: str
    success_rate: float = 1.0  # Historical success rate (0.0-1.0)
    application_count: int = 0  # How many times it's been used


class AntibodyLibrary:
    """Library of antibodies for known threats.

    The immune system learns over time, accumulating antibodies
    for patterns it has encountered before.
    """

    def __init__(self, project_root: Path | None = None) -> None:
        self.project_root = project_root or Path.cwd()
        self.antibodies: dict[str, Antibody] = {}
        self._register_builtin_antibodies()

    def _register_builtin_antibodies(self) -> None:
        """Register built-in antibodies for common issues."""

        # Antibody 1: Version Sync
        self.register(Antibody(
            name="version_sync",
            antigen_pattern="version_mismatch:*",
            fix_function=self._fix_version_sync,
            description="Synchronize version across all files",
            success_rate=0.95,
        ))

        # Antibody 2: Import Fix
        self.register(Antibody(
            name="import_fix",
            antigen_pattern="missing_module:*",
            fix_function=self._fix_missing_import,
            description="Fix or remove broken imports",
            success_rate=0.80,
        ))

        # Antibody 3: Config Initialization
        self.register(Antibody(
            name="config_init",
            antigen_pattern="missing_config",
            fix_function=self._fix_missing_config,
            description="Create config from example template",
            success_rate=0.90,
        ))

        # Antibody 4: Memory Consolidation
        self.register(Antibody(
            name="consolidate",
            antigen_pattern="memory_leak",
            fix_function=self._fix_memory_leak,
            description="Trigger automatic consolidation",
            success_rate=1.0,
        ))

        # Antibody 5: Memory Directory Init
        self.register(Antibody(
            name="memory_init",
            antigen_pattern="missing_memory_dir",
            fix_function=self._fix_missing_memory_dir,
            description="Initialize memory directory structure",
            success_rate=1.0,
        ))

    def register(self, antibody: Antibody) -> None:
        """Register a new antibody in the library."""
        self.antibodies[antibody.name] = antibody

    def get_all_antibodies(self) -> list[Antibody]:
        """Get all registered antibodies (for pattern discovery)."""
        return list(self.antibodies.values())

    def find_antibody(self, antigen: str) -> Antibody | None:
        """Find an antibody that matches the given antigen.

        Args:
            antigen: The threat pattern to match

        Returns:
            Matching antibody or None

        """
        for antibody in self.antibodies.values():
            # Check if antigen matches pattern (supports wildcards)
            pattern = antibody.antigen_pattern.replace("*", ".*")
            if re.match(pattern, antigen):
                return antibody
        return None

    # Fix Functions (Antibody Implementations)

    def _fix_version_sync(self, threat: Any) -> dict[str, Any]:
        """Synchronize version across all files."""
        try:
            # Read canonical version
            version_file = self.project_root / "VERSION"
            if not version_file.exists():
                return {"success": False, "error": "VERSION file not found"}

            canonical_version = version_file.read_text().strip()

            # Update the file with mismatch
            file_to_fix = threat.metadata.get("file")
            if not file_to_fix:
                return {"success": False, "error": "No file specified in threat metadata"}

            file_path = self.project_root / file_to_fix
            if not file_path.exists():
                return {"success": False, "error": f"File {file_to_fix} not found"}

            content = file_path.read_text()
            found_version = threat.metadata.get("found_version")

            # Replace the old version with canonical version
            new_content = content.replace(found_version, canonical_version)
            file_path.write_text(new_content)

            return {
                "success": True,
                "action": f"Updated {file_to_fix} from {found_version} to {canonical_version}",
                "file": file_to_fix,
            }

        except Exception as e:
            return {"success": False, "error": str(e)}

    def _fix_missing_import(self, threat: Any) -> dict[str, Any]:
        """Fix or analyze broken imports with intelligent suggestions."""
        try:
            import re

            location = threat.location if hasattr(threat, "location") else str(threat)
            description = threat.description if hasattr(threat, "description") else ""

            # Extract the problematic import name from description
            import_match = re.search(r"cannot import name '(\w+)'|No module named '([\w.]+)'", description)
            if not import_match:
                import_match = re.search(r"'(\w+)'", description)

            problem_name = import_match.group(1) if import_match else "unknown"

            # Search for where this name might exist
            from whitemagic.config import PROJECT_ROOT
            project_root = PROJECT_ROOT
            suggestions = []

            # Search in Python files for class/function definitions
            for py_file in project_root.rglob("*.py"):
                if "__pycache__" in str(py_file):
                    continue
                try:
                    content = py_file.read_text()
                    # Look for class or function definitions matching the name
                    if re.search(rf"^(class|def)\s+{re.escape(problem_name)}\b", content, re.MULTILINE):
                        rel_path = py_file.relative_to(project_root)
                        module_path = str(rel_path).replace("/", ".").replace(".py", "")
                        suggestions.append(f"from {module_path} import {problem_name}")
                        if len(suggestions) >= 3:
                            break
                except ImportError:
                    continue

            if suggestions:
                return {
                    "success": False,  # Still requires manual intervention
                    "error": "Import error detected",
                    "problem": f"Cannot import '{problem_name}'",
                    "location": location,
                    "suggestions": suggestions,
                    "recommendation": f"Try one of: {'; '.join(suggestions)}",
                }
            else:
                return {
                    "success": False,
                    "error": "Import fixing requires manual intervention",
                    "problem": f"Cannot find '{problem_name}' in codebase",
                    "location": location,
                    "recommendation": f"Check if '{problem_name}' exists or needs to be created",
                }

        except Exception as e:
            return {"success": False, "error": str(e)}

    def _fix_missing_config(self, threat: Any) -> dict[str, Any]:
        """Create config from example template."""
        try:
            # Extract config paths from threat location
            actual_config = Path(threat.location)
            example_config = actual_config.parent / f"{actual_config.name}.example.json"

            if not example_config.exists():
                # Try with different extension
                example_config = actual_config.parent / f"{actual_config.stem}.example{actual_config.suffix}"

            if not example_config.exists():
                return {"success": False, "error": "Example config not found"}

            # Copy example to actual
            import shutil
            shutil.copy(example_config, actual_config)

            return {
                "success": True,
                "action": f"Created {actual_config} from {example_config}",
                "file": str(actual_config),
            }

        except Exception as e:
            return {"success": False, "error": str(e)}

    def _fix_memory_leak(self, threat: Any) -> dict[str, Any]:
        """Trigger automatic consolidation."""
        try:
            # Run consolidation
            result = subprocess.run(
                ["whitemagic", "consolidate", "--no-dry-run"],
                capture_output=True,
                text=True,
                timeout=300,
                check=False,
            )

            if result.returncode == 0:
                return {
                    "success": True,
                    "action": "Triggered automatic consolidation",
                    "output": result.stdout,
                }
            else:
                return {
                    "success": False,
                    "error": f"Consolidation failed: {result.stderr}",
                }

        except subprocess.TimeoutExpired:
            return {"success": False, "error": "Consolidation timed out"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def _fix_missing_memory_dir(self, threat: Any) -> dict[str, Any]:
        """Initialize memory directory structure."""
        try:
            memory_dir = Path(threat.location)

            # Create memory directories
            (memory_dir / "short_term").mkdir(parents=True, exist_ok=True)
            (memory_dir / "long_term").mkdir(parents=True, exist_ok=True)
            (memory_dir / "archived").mkdir(parents=True, exist_ok=True)

            return {
                "success": True,
                "action": "Created memory directory structure",
                "directories": ["short_term", "long_term", "archived"],
            }

        except Exception as e:
            return {"success": False, "error": str(e)}

    def update_success_rate(self, antibody_name: str, success: bool) -> None:
        """Update antibody success rate based on outcome."""
        if antibody_name not in self.antibodies:
            return

        antibody = self.antibodies[antibody_name]
        antibody.application_count += 1

        # Update running success rate
        current_rate = antibody.success_rate
        new_rate = (current_rate * (antibody.application_count - 1) + (1.0 if success else 0.0)) / antibody.application_count
        antibody.success_rate = new_rate

    def get_statistics(self) -> dict[str, Any]:
        """Get antibody library statistics."""
        return {
            "total_antibodies": len(self.antibodies),
            "antibodies": [
                {
                    "name": ab.name,
                    "description": ab.description,
                    "success_rate": f"{ab.success_rate * 100:.1f}%",
                    "applications": ab.application_count,
                }
                for ab in self.antibodies.values()
            ],
        }
