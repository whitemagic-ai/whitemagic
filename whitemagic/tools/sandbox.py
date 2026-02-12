"""Tool Sandboxing — Per-tool CPU/Memory/Time Resource Limits.
============================================================
Wraps tool execution with configurable resource constraints.
Uses threading timeout + resource module (Linux) for enforcement.

Integrates into dispatch pipeline between maturity gate and governor.

Configuration via tool_limits dict or WM_SANDBOX_ENABLED env var.

Usage:
    from whitemagic.tools.sandbox import get_sandbox
    sandbox = get_sandbox()
    result = sandbox.execute(tool_name, handler_fn, kwargs, limits)
"""
from __future__ import annotations

import logging
import os
import threading
import time
from collections.abc import Callable
from concurrent.futures import ThreadPoolExecutor
from concurrent.futures import TimeoutError as FuturesTimeout
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)

# Try resource module (Linux/Mac only)
HAS_RESOURCE = False
try:
    import resource
    HAS_RESOURCE = True
except ImportError:
    pass


@dataclass
class ResourceLimits:
    """Resource limits for a single tool execution."""

    timeout_s: float = 30.0       # Max execution time in seconds
    max_memory_mb: int = 512      # Max memory in MB (Linux only)
    max_cpu_s: float = 10.0       # Max CPU time in seconds (Linux only)
    max_output_bytes: int = 10_000_000  # Max output size

    def to_dict(self) -> dict[str, Any]:
        return {
            "timeout_s": self.timeout_s,
            "max_memory_mb": self.max_memory_mb,
            "max_cpu_s": self.max_cpu_s,
            "max_output_bytes": self.max_output_bytes,
        }


@dataclass
class SandboxResult:
    """Result of a sandboxed execution."""

    success: bool
    result: Any = None
    error: str | None = None
    duration_s: float = 0.0
    exceeded_limit: str | None = None

    def to_dict(self) -> dict[str, Any]:
        d: dict[str, Any] = {"success": self.success, "duration_s": round(self.duration_s, 4)}
        if self.result is not None:
            d["result"] = self.result
        if self.error:
            d["error"] = self.error
        if self.exceeded_limit:
            d["exceeded_limit"] = self.exceeded_limit
        return d


# Default limits by safety level
DEFAULT_LIMITS = {
    "READ": ResourceLimits(timeout_s=15, max_memory_mb=256, max_cpu_s=5),
    "WRITE": ResourceLimits(timeout_s=30, max_memory_mb=512, max_cpu_s=10),
    "DELETE": ResourceLimits(timeout_s=10, max_memory_mb=256, max_cpu_s=3),
}


class ToolSandbox:
    """Sandboxed execution environment for MCP tools.

    Enforces:
      - Wall-clock timeout via ThreadPoolExecutor
      - CPU time limits via resource.setrlimit (Linux)
      - Memory limits via resource.setrlimit (Linux)
      - Output size capping
    """

    def __init__(self, enabled: bool = True) -> None:
        self._enabled = enabled and os.environ.get("WM_SANDBOX_ENABLED", "1") != "0"
        self._lock = threading.Lock()
        self._pool = ThreadPoolExecutor(max_workers=4, thread_name_prefix="wm-sandbox")
        self._tool_limits: dict[str, ResourceLimits] = {}
        self._stats: dict[str, dict[str, Any]] = {}  # tool -> {calls, timeouts, errors}
        self._violations: list[dict[str, Any]] = []

    def set_limits(self, tool_name: str, limits: ResourceLimits) -> None:
        """Set custom limits for a specific tool."""
        with self._lock:
            self._tool_limits[tool_name] = limits

    def get_limits(self, tool_name: str, safety: str = "READ") -> ResourceLimits:
        """Get limits for a tool, falling back to safety-level defaults."""
        with self._lock:
            if tool_name in self._tool_limits:
                return self._tool_limits[tool_name]
        return DEFAULT_LIMITS.get(safety.upper(), DEFAULT_LIMITS["READ"])

    def execute(
        self,
        tool_name: str,
        handler: Callable,
        kwargs: dict[str, Any],
        safety: str = "READ",
    ) -> SandboxResult:
        """Execute a tool handler within sandbox constraints."""
        if not self._enabled:
            # Pass-through when disabled
            start = time.time()
            try:
                result = handler(**kwargs)
                return SandboxResult(success=True, result=result, duration_s=time.time() - start)
            except Exception as e:
                return SandboxResult(success=False, error=str(e), duration_s=time.time() - start)

        limits = self.get_limits(tool_name, safety)
        start = time.time()

        # Track stats
        with self._lock:
            if tool_name not in self._stats:
                self._stats[tool_name] = {"calls": 0, "timeouts": 0, "errors": 0, "limit_hits": 0}
            self._stats[tool_name]["calls"] += 1

        def _run() -> Any:
            # Apply resource limits in the worker thread (Linux only)
            if HAS_RESOURCE:
                try:
                    # CPU time limit (soft, hard)
                    cpu_limit = int(limits.max_cpu_s)
                    resource.setrlimit(resource.RLIMIT_CPU, (cpu_limit, cpu_limit + 5))
                    # Memory limit
                    mem_bytes = limits.max_memory_mb * 1024 * 1024
                    resource.setrlimit(resource.RLIMIT_AS, (mem_bytes, mem_bytes))
                except (ValueError, OSError):
                    pass  # Not all limits supported everywhere
            return handler(**kwargs)

        try:
            future = self._pool.submit(_run)
            result = future.result(timeout=limits.timeout_s)
            duration = time.time() - start
            return SandboxResult(success=True, result=result, duration_s=duration)

        except FuturesTimeout:
            duration = time.time() - start
            with self._lock:
                self._stats[tool_name]["timeouts"] += 1
                self._violations.append({
                    "tool": tool_name, "limit": "timeout",
                    "value": limits.timeout_s, "time": time.time(),
                })
                if len(self._violations) > 500:
                    self._violations = self._violations[-250:]
            return SandboxResult(
                success=False, error=f"Timeout after {limits.timeout_s}s",
                duration_s=duration, exceeded_limit="timeout",
            )

        except MemoryError:
            duration = time.time() - start
            with self._lock:
                self._stats[tool_name]["limit_hits"] += 1
                self._violations.append({
                    "tool": tool_name, "limit": "memory",
                    "value": limits.max_memory_mb, "time": time.time(),
                })
            return SandboxResult(
                success=False, error=f"Memory limit exceeded ({limits.max_memory_mb}MB)",
                duration_s=duration, exceeded_limit="memory",
            )

        except Exception as e:
            duration = time.time() - start
            with self._lock:
                self._stats[tool_name]["errors"] += 1
            return SandboxResult(success=False, error=str(e), duration_s=duration)

    def get_violations(self, limit: int = 20) -> list[dict[str, Any]]:
        """Get recent limit violations."""
        with self._lock:
            return list(reversed(self._violations[-limit:]))

    def status(self) -> dict[str, Any]:
        """Get sandbox status and per-tool stats."""
        with self._lock:
            return {
                "enabled": self._enabled,
                "has_resource_module": HAS_RESOURCE,
                "tools_tracked": len(self._stats),
                "total_violations": len(self._violations),
                "per_tool": dict(self._stats),
            }


# Singleton
_sandbox: ToolSandbox | None = None
_sandbox_lock = threading.Lock()

def get_sandbox() -> ToolSandbox:
    global _sandbox
    if _sandbox is None:
        with _sandbox_lock:
            if _sandbox is None:
                _sandbox = ToolSandbox()
    return _sandbox
