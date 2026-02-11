"""Introspection tool handlers — core system introspection + health report."""
import logging
from pathlib import Path
from typing import Any, cast

from whitemagic.tools import introspection as _core

logger = logging.getLogger(__name__)

def handle_capabilities(**kwargs: Any) -> dict[str, Any]:
    return cast(
        "dict[str, Any]",
        _core.capabilities(
        include_tools=bool(kwargs.get("include_tools", True)),
        include_schemas=bool(kwargs.get("include_schemas", False)),
        include_env=bool(kwargs.get("include_env", True)),
        ),
    )

def handle_manifest(**kwargs: Any) -> dict[str, Any]:
    return cast(
        "dict[str, Any]",
        _core.manifest(
        format=str(kwargs.get("format", "summary")),
        include_schemas=bool(kwargs.get("include_schemas", False)),
        ),
    )

def handle_state_paths(**kwargs: Any) -> dict[str, Any]:
    return cast("dict[str, Any]", _core.state_paths())

def handle_state_summary(**kwargs: Any) -> dict[str, Any]:
    return cast("dict[str, Any]", _core.state_summary(include_sizes=bool(kwargs.get("include_sizes", True))))

def handle_repo_summary(**kwargs: Any) -> dict[str, Any]:
    return cast(
        "dict[str, Any]",
        _core.repo_summary(
        max_files=int(kwargs.get("max_files", 2500)),
        max_matches=int(kwargs.get("max_matches", 25)),
        ),
    )

def handle_ship_check(**kwargs: Any) -> dict[str, Any]:
    return cast(
        "dict[str, Any]",
        _core.ship_check(
        max_files=int(kwargs.get("max_files", 4000)),
        max_large_files=int(kwargs.get("max_large_files", 25)),
        large_file_mb=int(kwargs.get("large_file_mb", 10)),
        max_matches=int(kwargs.get("max_matches", 50)),
        ),
    )

def handle_get_telemetry_summary(**kwargs: Any) -> dict[str, Any]:
    return cast("dict[str, Any]", _core.telemetry_summary())


def handle_gnosis(**kwargs: Any) -> dict[str, Any]:
    """Gnosis Portal — unified introspection across all Whitemagic subsystems."""
    from whitemagic.tools.gnosis import gnosis_snapshot
    compact = kwargs.get("compact", False)
    snap = gnosis_snapshot(compact=compact)
    return {"status": "success", "gnosis": snap}


def handle_health_report(**kwargs: Any) -> dict[str, Any]:
    """Consolidated system health report aggregating multiple subsystems."""
    report: dict[str, Any] = {"status": "success"}

    # 1. Capabilities / version info
    try:
        caps = _core.capabilities(include_tools=True, include_schemas=False, include_env=True)
        report["version"] = caps.get("package_version", caps.get("version", "unknown"))
        runtime = caps.get("runtime", {})
        report["python_version"] = runtime.get("python_version", "unknown")
        report["tool_count"] = len(caps.get("tools", []))
    except Exception as e:
        report["capabilities_error"] = str(e)

    # 2. State summary
    try:
        state = _core.state_summary(include_sizes=True)
        sizes = state.get("sizes_bytes", {})
        total_bytes = sum(sizes.values()) if isinstance(sizes, dict) else 0
        report["state"] = {
            "root": state.get("wm_state_root", ""),
            "exists": state.get("exists", False),
            "total_size_mb": round(total_bytes / (1024 * 1024), 1),
        }
    except Exception as e:
        report["state_error"] = str(e)

    # 3. Rust bridge
    try:
        from whitemagic.tools.handlers.rust_bridge import handle_rust_status
        rust = handle_rust_status()
        report["rust"] = {
            "available": rust.get("available", False),
            "version": rust.get("version", "unknown"),
        }
    except Exception as e:
        report["rust"] = {"available": False, "error": str(e)}

    # 4. Garden health
    try:
        from whitemagic.tools.handlers.garden import handle_garden_health
        gardens = handle_garden_health()
        report["gardens"] = gardens.get("health", {})
    except Exception as e:
        report["gardens_error"] = str(e)

    # 5. Archaeology stats
    try:
        from whitemagic.tools.handlers.archaeology import handle_archaeology_stats
        arch = handle_archaeology_stats()
        report["archaeology"] = {
            "files_tracked": arch.get("total_files", 0),
            "total_reads": arch.get("total_reads", 0),
        }
    except Exception as e:
        report["archaeology_error"] = str(e)

    # 6. Yin-Yang balance
    try:
        from whitemagic.tools.handlers.balance import handle_get_yin_yang_balance
        balance = handle_get_yin_yang_balance()
        report["yin_yang"] = balance.get("balance", {})
    except Exception as e:
        report["yin_yang_error"] = str(e)

    # 7. DB stats
    try:
        from whitemagic.config.paths import DB_PATH
        db = Path(DB_PATH)
        if db.exists():
            import sqlite3
            conn = sqlite3.connect(str(db))
            cursor = conn.execute("SELECT COUNT(*) FROM memories")
            count = cursor.fetchone()[0]
            conn.close()
            report["db"] = {
                "path": str(db),
                "size_mb": round(db.stat().st_size / (1024 * 1024), 1),
                "memory_count": count,
            }
        else:
            report["db"] = {"path": str(db), "exists": False}
    except Exception as e:
        report["db_error"] = str(e)

    # 8. Julia bridge
    try:
        import shutil
        julia_path = shutil.which("julia") or "/snap/bin/julia"
        report["julia"] = {"available": Path(julia_path).exists(), "path": julia_path}
    except Exception:
        report["julia"] = {"available": False}

    # 9. Haskell bridge
    try:
        import shutil
        ghc_path = shutil.which("ghc") or str(Path.home() / ".ghcup/bin/ghc")
        report["haskell"] = {"available": Path(ghc_path).exists(), "path": ghc_path}
    except Exception:
        report["haskell"] = {"available": False}

    # Compute overall health score
    checks = []
    checks.append(report.get("rust", {}).get("available", False))
    checks.append(report.get("db", {}).get("memory_count", 0) > 0)
    checks.append(report.get("julia", {}).get("available", False))
    checks.append("state" in report)
    checks.append("gardens" in report)
    health_score = sum(1 for c in checks if c) / max(len(checks), 1)
    report["health_score"] = round(health_score, 2)
    report["health_status"] = "healthy" if health_score >= 0.8 else "degraded" if health_score >= 0.5 else "critical"

    return report


def handle_capability_matrix(**kwargs: Any) -> dict[str, Any]:
    """Return the full capability matrix: subsystems, fusions, unexplored opportunities."""
    from whitemagic.tools.capability_matrix import get_capability_matrix
    return cast(
        "dict[str, Any]",
        get_capability_matrix(
        category=kwargs.get("category"),
        include_unexplored=bool(kwargs.get("include_unexplored", True)),
        ),
    )


def handle_capability_status(**kwargs: Any) -> dict[str, Any]:
    """Get live status for a specific subsystem."""
    from whitemagic.tools.capability_matrix import get_subsystem_status
    subsystem_id = kwargs.get("subsystem_id", "")
    if not subsystem_id:
        return {"status": "error", "error": "subsystem_id is required"}
    return cast("dict[str, Any]", get_subsystem_status(subsystem_id))


def handle_capability_suggest(**kwargs: Any) -> dict[str, Any]:
    """Suggest the next best fusion to wire."""
    from whitemagic.tools.capability_matrix import suggest_next_fusion
    return cast("dict[str, Any]", suggest_next_fusion())
