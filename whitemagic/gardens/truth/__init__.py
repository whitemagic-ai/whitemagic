"""Truth Garden — System Health & Structural Integrity.

Mansion: #3 Root (氐 Di)
Quadrant: Eastern (Azure Dragon)
PRAT Gana: gana_root — 6 tools (health_report, rust_status, rust_similarity,
    ship.check, state.paths, state.summary)

The Root Gana digs deep. The Truth Garden provides the substrate for
system health verification: checking accelerator availability, validating
structural integrity, and reporting honest system state.

Holographic Integration:
- Logical/objective (X-axis -0.5) — truth is verifiable
- Universal principle (Y-axis +0.3) — health is non-negotiable
- Timeless (Z-axis 0.0) — structural truth doesn't change
- High importance (W-axis +0.35) — foundations matter
"""

from __future__ import annotations

import logging
import os
import threading
from collections import deque
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

logger = logging.getLogger(__name__)


class TruthGarden(BaseGarden, GanYingMixin):
    """Garden of Truth — System health engine for the Root Gana.

    Serves the Root Gana's health tools by providing:
    - Accelerator availability checks (Rust, Zig, Haskell, etc.)
    - State path validation and reporting
    - Ship-readiness checks for deployment
    - Health history for trend analysis
    """

    name = "truth"
    category = "health"
    resonance_partners = ["courage", "wisdom", "dharma"]
    mansion_number = 3
    gana_name = "gana_root"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.truths_discovered: list[dict[str, Any]] = []
        self.health_history: deque[dict[str, Any]] = deque(maxlen=100)
        self._accelerator_cache: dict[str, bool] = {}
        self._cache_time: float = 0.0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Truth", "mansion": 3})

    def get_name(self) -> str:
        return "truth"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=-0.5, y=0.3, z=0.0, w=0.35)

    # ------------------------------------------------------------------
    # Accelerator status — serving rust_status, rust_similarity tools
    # ------------------------------------------------------------------

    def check_accelerators(self, force: bool = False) -> dict[str, Any]:
        """Check availability of all polyglot accelerators.

        Results are cached for 60 seconds unless forced.
        """
        import time
        now = time.time()
        if not force and self._accelerator_cache and (now - self._cache_time) < 60:
            return dict(self._accelerator_cache)

        status: dict[str, Any] = {}

        # Rust (PyO3)
        try:
            import whitemagic_rs  # noqa: F401
            status["rust"] = True
        except ImportError:
            status["rust"] = False

        # Zig (ctypes .so)
        try:
            from whitemagic.optimization import zig_accelerators
            status["zig"] = hasattr(zig_accelerators, '_lib') or True
        except ImportError:
            status["zig"] = False

        # Haskell
        try:
            from whitemagic.haskell import haskell_bridge  # type: ignore[import-not-found]  # noqa: F401
            status["haskell"] = True
        except ImportError:
            status["haskell"] = False

        # Elixir
        try:
            from whitemagic.elixir import elixir_bridge  # type: ignore[import-not-found]  # noqa: F401
            status["elixir"] = True
        except ImportError:
            status["elixir"] = False

        with self._lock:
            self._accelerator_cache = status
            self._cache_time = now

        return status

    # ------------------------------------------------------------------
    # State paths — serving state.paths, state.summary tools
    # ------------------------------------------------------------------

    def get_state_paths(self) -> dict[str, Any]:
        """Report all WhiteMagic state paths and their status."""
        try:
            from whitemagic.config import paths as cfg_paths
            state_root = getattr(cfg_paths, 'STATE_ROOT', Path.home() / '.whitemagic')
        except ImportError:
            state_root = Path.home() / '.whitemagic'

        paths: dict[str, Any] = {}
        key_dirs = ['memory', 'dharma', 'cache', 'logs', 'embeddings']
        for d in key_dirs:
            p = state_root / d
            paths[d] = {
                "path": str(p),
                "exists": p.exists(),
                "size_mb": round(sum(f.stat().st_size for f in p.rglob('*') if f.is_file()) / 1048576, 2) if p.exists() else 0,
            }

        # Hot DB
        hot_db = state_root / 'memory' / 'whitemagic.db'
        paths["hot_db"] = {
            "path": str(hot_db),
            "exists": hot_db.exists(),
            "size_mb": round(hot_db.stat().st_size / 1048576, 2) if hot_db.exists() else 0,
        }

        # Cold DB
        cold_db = state_root / 'memory' / 'whitemagic_cold.db'
        paths["cold_db"] = {
            "path": str(cold_db),
            "exists": cold_db.exists(),
            "size_mb": round(cold_db.stat().st_size / 1048576, 2) if cold_db.exists() else 0,
        }

        return {"state_root": str(state_root), "paths": paths}

    # ------------------------------------------------------------------
    # Health report — serving health_report tool
    # ------------------------------------------------------------------

    def health_report(self, deep: bool = False) -> dict[str, Any]:
        """Generate a comprehensive health report."""
        report: dict[str, Any] = {
            "timestamp": datetime.now().isoformat(),
            "garden": "truth",
            "gana": "root",
        }

        report["accelerators"] = self.check_accelerators(force=deep)
        report["state"] = self.get_state_paths()

        # Python environment
        import sys
        report["python"] = {
            "version": sys.version.split()[0],
            "venv": os.environ.get("VIRTUAL_ENV", "none"),
        }

        # Memory DB quick check
        try:
            from whitemagic.core.memory.sqlite_backend import get_pool  # type: ignore[attr-defined]
            pool = get_pool()
            with pool.connection() as conn:
                count = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
                report["memory_count"] = count
        except Exception as exc:
            report["memory_count_error"] = str(exc)

        with self._lock:
            self.health_history.append(report)

        self.emit(EventType.GARDEN_ACTIVITY, {"action": "health_report", "deep": deep})  # type: ignore[attr-defined]
        return report

    # ------------------------------------------------------------------
    # Ship check — serving ship.check tool
    # ------------------------------------------------------------------

    def ship_check(self) -> dict[str, Any]:
        """Check if the system is ready to ship / deploy."""
        issues: list[str] = []
        warnings: list[str] = []

        # Check critical paths
        paths = self.get_state_paths()
        if not paths["paths"].get("hot_db", {}).get("exists"):
            issues.append("Hot database not found")

        # Check accelerators
        accel = self.check_accelerators()
        if not accel.get("rust"):
            warnings.append("Rust accelerators not available (performance impact)")

        # Check version file
        version_path = Path(__file__).parents[3] / "VERSION"
        if version_path.exists():
            version = version_path.read_text().strip()
        else:
            version = "unknown"
            issues.append("VERSION file not found")

        return {
            "ready": len(issues) == 0,
            "version": version,
            "issues": issues,
            "warnings": warnings,
            "accelerators": accel,
        }

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def discover_truth(self, what: str, evidence: Any = None) -> dict[str, Any]:
        truth = {"what": what, "evidence": evidence}
        self.truths_discovered.append(truth)
        self.emit(EventType.TRUTH_SPOKEN, truth)
        return truth

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        self.emit(EventType.TRUTH_SPOKEN, {"source": "wisdom", "note": "Wisdom illuminates truth"})

    @listen_for(EventType.PATTERN_DETECTED)
    def on_pattern(self, event: Any) -> None:
        self.discover_truth(f"Pattern reveals truth: {event.data}")

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "truths_discovered": len(self.truths_discovered),
            "health_reports_taken": len(self.health_history),
            "accelerators": self.check_accelerators(),
        })
        return base


_instance = None
def get_truth_garden() -> TruthGarden:
    global _instance
    if _instance is None:
        _instance = TruthGarden()
    return _instance
