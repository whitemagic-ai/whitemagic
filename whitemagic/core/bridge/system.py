
from typing import Any


def system_initialize_all(verbose: bool = False, **kwargs: Any) -> dict[str, Any]:
    """Initialize all WhiteMagic systems."""
    from whitemagic.core.orchestration.consciousness import initialize_all

    result = initialize_all(verbose=verbose)
    return result if isinstance(result, dict) else {"status": str(result)}


def system_get_status(**kwargs: Any) -> dict[str, Any]:
    """Get system status."""
    from whitemagic.core.orchestration.consciousness import get_status

    status = get_status()
    return status if isinstance(status, dict) else {"status": str(status)}


def check_system_health(
    component: str = "all",
    operation: str | None = None,
    deep_scan: bool = False,
    quick_check: bool = True,
    scan_disk: bool = False,
    **kwargs: Any,
) -> dict[str, Any]:
    """Check overall system health."""
    from whitemagic.core.health_monitor import HealthMonitor

    monitor = HealthMonitor()
    result = monitor.check_system_health(deep_scan=deep_scan)

    # If scan_disk is requested, add disk stats via archaeology
    if scan_disk:
        from whitemagic.archaeology import get_archaeologist
        disk_stats = get_archaeologist().stats(scan_disk=True)
        result["disk_usage"] = {
            "total_mb": disk_stats.get("disk_usage_mb", 0),
            "artifacts": disk_stats.get("artifacts", {}),
            "large_files": disk_stats.get("large_files", []),
        }

    return {
        "component": "system",
        "status": result.get("status", "unknown"),
        "details": result.get("details", {}),
        "issues": result.get("issues", []),
        "deep_scan": deep_scan,
        "timestamp": result.get("timestamp", "unknown"),
        "rust_acceleration": result.get("rust_acceleration"),
        "accelerators": result.get("accelerators"),
        "disk_usage": result.get("disk_usage"),
    }

def check_memory_health(component: str = "memory", **kwargs: Any) -> dict[str, Any]:
    """Check memory system health."""
    from whitemagic.core.health_monitor import HealthMonitor

    monitor = HealthMonitor()
    result = monitor.check_memory_health()

    return {
        "component": "memory",
        "status": result.get("status", "unknown"),
        "details": result.get("details", {}),
        "issues": result.get("issues", []),
        "timestamp": result.get("timestamp", "unknown"),
    }

def check_resonance_health(component: str = "resonance", duration_seconds: int = 60, **kwargs: Any) -> dict[str, Any]:
    """Check resonance (Gan Ying bus) health."""
    from whitemagic.core.health_monitor import HealthMonitor

    monitor = HealthMonitor()
    result = monitor.check_resonance_health(duration=duration_seconds)

    return {
        "component": "resonance",
        "status": result.get("status", "unknown"),
        "details": result.get("details", {}),
        "issues": result.get("issues", []),
        "duration_seconds": duration_seconds,
        "timestamp": result.get("timestamp", "unknown"),
    }

def check_integrations_health(component: str = "integrations", quick_check: bool = True, **kwargs: Any) -> dict[str, Any]:
    """Check integrations health."""
    from whitemagic.core.health_monitor import HealthMonitor

    monitor = HealthMonitor()
    result = monitor.check_integrations_health(quick_check=quick_check)

    return {
        "component": "integrations",
        "status": result.get("status", "unknown"),
        "details": result.get("details", {}),
        "issues": result.get("issues", []),
        "quick_check": quick_check,
        "timestamp": result.get("timestamp", "unknown"),
    }

def debug_system(operation: str = "inspect_state", component: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Debug system components."""
    return {"operation": operation, "component": component, "status": "healthy"}
