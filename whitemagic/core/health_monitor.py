"""Health Monitor System
Checks the health of various system components including memory, resonance, and integrations.
"""

import importlib
from datetime import datetime
from typing import Any


class HealthMonitor:
    """Monitors and checks system health."""

    def __init__(self) -> None:
        self.timestamp = datetime.now().isoformat()

    def check_system_health(self, deep_scan: bool = False) -> dict[str, Any]:
        """Check overall system health."""
        # Check accelerators
        accelerators_health = self.check_accelerators()

        # Check core components
        memory_health = self.check_memory_health()
        resonance_health = self.check_resonance_health(duration=1)
        integrations_health = self.check_integrations_health(quick_check=not deep_scan)

        issues = []
        issues.extend(accelerators_health.get("issues", []))
        issues.extend(memory_health.get("issues", []))
        issues.extend(resonance_health.get("issues", []))
        issues.extend(integrations_health.get("issues", []))

        status = "healthy"
        if issues:
            status = "degraded"

        return {
            "status": status,
            "timestamp": self.timestamp,
            "accelerators": accelerators_health,
            "details": {
                "memory": memory_health,
                "resonance": resonance_health,
                "integrations": integrations_health,
            },
            "issues": issues,
        }

    def check_accelerators(self) -> dict[str, Any]:
        """Check status of optional accelerators (Rust, Zig, FastMCP)."""
        status = "healthy"
        issues = []
        details = {}

        # 1. Rust Extension (whitemagic_rs)
        try:
            import whitemagic_rs
            start = datetime.now()
            # Perform a tiny SIMD operation
            whitemagic_rs.rust_similarity("health", "check")
            latency = (datetime.now() - start).total_seconds() * 1000
            details["rust"] = {
                "status": "active",
                "version": getattr(whitemagic_rs, "__version__", "unknown"),
                "latency_ms": latency,
            }
        except ImportError:
            details["rust"] = {"status": "inactive", "reason": "not_installed"}
        except Exception as e:
            details["rust"] = {"status": "error", "reason": str(e)}
            issues.append(f"Rust accelerator error: {str(e)}")

        # 2. FastMCP
        try:
            import fastmcp
            details["fastmcp"] = {"status": "active", "version": getattr(fastmcp, "__version__", "unknown")}
        except ImportError:
            details["fastmcp"] = {"status": "missing", "reason": "install with .[mcp]"}
            issues.append("FastMCP missing (MCP server will not run)")

        # 3. Zig (via Rust bridge mostly, but check libs)
        # We can check for the shared library file presence as a heuristic
        # FUTURE: Add direct Zig check if we expose it deeper

        return {
            "status": status,
            "details": details,
            "issues": issues,
        }

    def check_memory_health(self) -> dict[str, Any]:
        """Check memory system health."""
        status = "healthy"
        issues: list[str] = []
        details: dict[str, Any] = {}

        try:
            from whitemagic.core.memory.manager import MemoryManager
            # Just try to instantiate, don't necessarily need to operate
            manager = MemoryManager()
            details["initialized"] = True

            # Simple check if we can access the DB path (abstracted)
            details["db_path"] = str(manager.db_path) if hasattr(manager, "db_path") else "unknown"

        except Exception as e:
            status = "unhealthy"
            issues.append(f"MemoryManager initialization failed: {str(e)}")
            details["error"] = str(e)

        return {
            "status": status,
            "details": details,
            "issues": issues,
            "timestamp": datetime.now().isoformat(),
        }

    def check_resonance_health(self, duration: int = 1) -> dict[str, Any]:
        """Check resonance (Gan Ying bus) health."""
        status = "healthy"
        issues: list[str] = []
        details: dict[str, Any] = {}

        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            bus = get_bus()
            details["connected"] = True
            details["queue_size"] = bus.queue.qsize() if hasattr(bus, "queue") else 0

        except Exception as e:
            status = "unhealthy"
            issues.append(f"Gan Ying Bus connection failed: {str(e)}")
            details["error"] = str(e)

        return {
            "status": status,
            "details": details,
            "issues": issues,
            "timestamp": datetime.now().isoformat(),
        }

    def check_integrations_health(self, quick_check: bool = True) -> dict[str, Any]:
        """Check integrations health."""
        status = "healthy"
        issues = []
        details = {}

        modules_to_check = [
            "whitemagic.core.memory.manager",
            "whitemagic.core.resonance.gan_ying",
            "whitemagic.zodiac.zodiac_cores",
            "whitemagic.gardens",
            "whitemagic.archaeology",
        ]

        for module in modules_to_check:
            try:
                importlib.import_module(module)
                details[module] = "available"
            except ImportError as e:
                issues.append(f"Module {module} missing: {str(e)}")
                details[module] = "missing"
            except Exception as e:
                issues.append(f"Module {module} error: {str(e)}")
                details[module] = "error"

        if issues:
            status = "degraded"

        return {
            "status": status,
            "details": details,
            "issues": issues,
            "timestamp": datetime.now().isoformat(),
        }
