"""Julia Scientific Bridge - Python to Julia via subprocess.
==========================================================
Bridges to Julia scientific computing modules for statistical memory
analysis and time-series forecasting. Julia processes are called via
subprocess with JSON stdin/stdout protocol.

Julia modules:
- memory_stats.jl - Statistical memory analysis (distributions, outliers, zones)
- self_model_forecast.jl - Holt-Winters forecasting with confidence intervals

Falls back to pure Python when Julia is not available.

Usage:
    from whitemagic.core.acceleration.julia_bridge import (
        julia_importance_distribution, julia_forecast_metric,
        julia_batch_forecast, julia_bridge_status
    )
"""
from __future__ import annotations

import json
import logging
import os
import shutil
import subprocess
import threading
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

_julia_bin: str | None = None
_julia_lock = threading.Lock()
_HAS_JULIA = False
_JULIA_DIR: Path | None = None


def _find_julia() -> tuple:
    """Locate Julia binary and source directory."""
    base = Path(__file__).resolve().parent.parent.parent.parent
    julia_dir = base / "whitemagic-julia" / "src"

    candidates = [
        os.environ.get("JULIA_PATH", ""),
        shutil.which("julia") or "",
        "/usr/local/bin/julia",
        str(base / ".pixi" / "envs" / "default" / "bin" / "julia"),
    ]
    for path in candidates:
        if path and os.path.isfile(path) and os.access(path, os.X_OK):
            if julia_dir.is_dir():
                return path, julia_dir
            return path, None
    return None, julia_dir if julia_dir.is_dir() else None


def _init_julia() -> Any:
    """Initialize Julia bridge (lazy, thread-safe)."""
    global _julia_bin, _HAS_JULIA, _JULIA_DIR
    if _julia_bin is not None or _HAS_JULIA:
        return
    with _julia_lock:
        if _julia_bin is not None or _HAS_JULIA:
            return
        _julia_bin, _JULIA_DIR = _find_julia()
        if _julia_bin and _JULIA_DIR:
            _HAS_JULIA = True
            logger.info("Julia bridge initialized: bin=%s, dir=%s", _julia_bin, _JULIA_DIR)
        else:
            logger.debug("Julia not found - using Python fallback")


def _call_julia(module_file: str, request: dict[str, Any]) -> dict[str, Any] | None:
    """Call a Julia module via subprocess with JSON protocol."""
    _init_julia()
    if not _julia_bin or not _JULIA_DIR:
        return None

    src_path = _JULIA_DIR / module_file
    if not src_path.exists():
        logger.debug("Julia source not found: %s", src_path)
        return None

    # Build a Julia script that includes the module and processes the request
    script = f'''
using JSON3
include("{src_path}")
request = JSON3.read(readline(stdin), Dict{{String, Any}})
result = {Path(module_file).stem.title().replace("_", "")}.handle_request(request)
println(JSON3.write(result))
'''

    try:
        proc = subprocess.run(
            [_julia_bin, "-e", script],
            input=json.dumps(request),
            capture_output=True,
            text=True,
            timeout=60,
        )
        if proc.returncode == 0 and proc.stdout.strip():
            parsed = json.loads(proc.stdout.strip())
            if isinstance(parsed, dict):
                return parsed
        elif proc.stderr:
            logger.debug("Julia %s stderr: %s", module_file, proc.stderr[:300])
    except subprocess.TimeoutExpired:
        logger.warning("Julia %s timed out after 60s", module_file)
    except Exception as e:
        logger.debug("Julia %s call failed: %s", module_file, e)

    return None


# ---------------------------------------------------------------------------
# MemoryStats - Statistical memory analysis
# ---------------------------------------------------------------------------

def julia_importance_distribution(
    scores: list[float],
) -> dict[str, Any] | None:
    """Analyze memory importance score distribution using Julia.

    Returns comprehensive statistics: moments, percentiles, outlier detection.
    """
    return _call_julia("memory_stats.jl", {
        "command": "importance_distribution",
        "scores": scores,
    })


def julia_zone_transitions(
    before: list[float],
    after: list[float],
) -> dict[str, Any] | None:
    """Compute galactic zone transition probabilities (Markov model).

    Args:
        before: Galactic distances before a sweep.
        after: Galactic distances after a sweep.

    """
    return _call_julia("memory_stats.jl", {
        "command": "zone_transitions",
        "before": before,
        "after": after,
    })


def julia_detect_outliers(
    values: list[float],
    threshold: float = 3.0,
) -> dict[str, Any] | None:
    """Detect outliers using modified z-score (MAD-based)."""
    return _call_julia("memory_stats.jl", {
        "command": "detect_outliers",
        "values": values,
        "threshold": threshold,
    })


def julia_cluster_significance(
    cluster_sizes: list[int],
    total_points: int,
    volume: float,
) -> dict[str, Any] | None:
    """Test cluster significance against uniform null model."""
    return _call_julia("memory_stats.jl", {
        "command": "cluster_significance",
        "cluster_sizes": cluster_sizes,
        "total_points": total_points,
        "volume": volume,
    })


def julia_full_memory_analysis(
    importance: list[float],
    distances: list[float],
) -> dict[str, Any] | None:
    """Run complete statistical analysis on memory corpus."""
    return _call_julia("memory_stats.jl", {
        "command": "full_analysis",
        "importance": importance,
        "distances": distances,
    })


# ---------------------------------------------------------------------------
# SelfModelForecast - Time-series forecasting
# ---------------------------------------------------------------------------

def julia_forecast_metric(
    values: list[float],
    steps: int = 5,
    alpha: float = 0.3,
    beta: float = 0.1,
) -> dict[str, Any] | None:
    """Forecast a Self-Model metric using Holt-Winters exponential smoothing.

    Returns forecasts with 80% and 95% confidence intervals.
    """
    return _call_julia("self_model_forecast.jl", {
        "command": "forecast",
        "values": values,
        "steps": steps,
        "alpha": alpha,
        "beta": beta,
    })


def julia_detect_anomalies(
    values: list[float],
    threshold: float = 2.5,
) -> dict[str, Any] | None:
    """Detect anomalies in a metric time series via residual z-scores."""
    return _call_julia("self_model_forecast.jl", {
        "command": "detect_anomalies",
        "values": values,
        "threshold": threshold,
    })


def julia_metric_correlations(
    metrics: dict[str, list[float]],
) -> dict[str, Any] | None:
    """Compute pairwise Pearson correlations between Self-Model metrics."""
    return _call_julia("self_model_forecast.jl", {
        "command": "correlations",
        "metrics": metrics,
    })


def julia_batch_forecast(
    metrics: dict[str, list[float]],
    steps: int = 5,
) -> dict[str, Any] | None:
    """Forecast all Self-Model metrics in one call with alert generation."""
    return _call_julia("self_model_forecast.jl", {
        "command": "batch_forecast",
        "metrics": metrics,
        "steps": steps,
    })


# ---------------------------------------------------------------------------
# Status
# ---------------------------------------------------------------------------

def julia_bridge_status() -> dict[str, Any]:
    """Get Julia bridge status."""
    _init_julia()
    modules = {}
    if _JULIA_DIR:
        for f in ["memory_stats.jl", "self_model_forecast.jl",
                   "causal_resonance.jl", "constellations.jl", "gan_ying.jl"]:
            modules[f.replace(".jl", "")] = (_JULIA_DIR / f).exists()

    return {
        "has_julia": _HAS_JULIA,
        "julia_bin": _julia_bin or "not found",
        "julia_dir": str(_JULIA_DIR) if _JULIA_DIR else "not found",
        "modules": modules,
        "backend": "julia_scientific" if _HAS_JULIA else "python_fallback",
    }
