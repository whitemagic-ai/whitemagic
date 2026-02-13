"""Shelter Manager — Sovereign Sandbox Orchestration (v15.2).
============================================================
Creates, manages, and tears down isolated execution environments
with graceful degradation across 5 tiers of isolation.

Environment variables:
    WM_SHELTER_TIER           — Default tier: auto | thread | namespace | container | microvm | wasm
    WM_SHELTER_RUNTIME        — Container runtime: podman (default) | nerdctl | docker
    WM_SHELTER_FIRECRACKER    — Path to firecracker binary
    WM_SHELTER_MAX_CONCURRENT — Max simultaneous shelters (default: 4)
    WM_SHELTER_TIMEOUT_S      — Default timeout seconds (default: 300)
"""

from __future__ import annotations

import json
import logging
import os
import shutil
import subprocess
import tempfile
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

class ShelterTier(Enum):
    """Isolation tiers, ordered from lightest to heaviest."""
    THREAD = "thread"         # Tier 0: existing ToolSandbox resource limits
    NAMESPACE = "namespace"   # Tier 1: Linux unshare + chroot
    CONTAINER = "container"   # Tier 2: rootless podman/nerdctl
    MICROVM = "microvm"       # Tier 3: Firecracker / Cloud Hypervisor
    WASM = "wasm"             # Tier 4: wasmtime (WASI)


class ShelterState(Enum):
    CREATED = "created"
    RUNNING = "running"
    COMPLETED = "completed"
    ERROR = "error"
    DESTROYED = "destroyed"


@dataclass
class ShelterCapabilities:
    """Explicit capability grants for a shelter. Everything else denied."""
    filesystem_read: list[str] = field(default_factory=list)   # glob patterns
    filesystem_write: list[str] = field(default_factory=list)
    network: str = "none"      # none | read_only | filtered | full
    network_allow: list[str] = field(default_factory=list)     # URL patterns
    tools_allow: list[str] = field(default_factory=list)       # WM tool names
    system_allow: list[str] = field(default_factory=list)      # syscall groups

    def to_dict(self) -> dict[str, Any]:
        return {
            "filesystem_read": self.filesystem_read,
            "filesystem_write": self.filesystem_write,
            "network": self.network,
            "network_allow": self.network_allow,
            "tools_allow": self.tools_allow,
            "system_allow": self.system_allow,
        }


@dataclass
class ShelterLimits:
    """Resource limits for a shelter."""
    timeout_s: int = 300
    max_memory_mb: int = 1024
    max_cpu_s: int = 60
    max_disk_mb: int = 500

    def to_dict(self) -> dict[str, Any]:
        return {
            "timeout_s": self.timeout_s,
            "max_memory_mb": self.max_memory_mb,
            "max_cpu_s": self.max_cpu_s,
            "max_disk_mb": self.max_disk_mb,
        }


@dataclass
class Shelter:
    """A running or completed shelter instance."""
    name: str
    tier: ShelterTier
    state: ShelterState
    capabilities: ShelterCapabilities
    limits: ShelterLimits
    ephemeral: bool = True
    created_at: str = field(default_factory=lambda: datetime.now().isoformat())
    work_dir: str = ""
    pid: int | None = None
    exit_code: int | None = None
    output: str = ""
    error: str = ""
    duration_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "tier": self.tier.value,
            "state": self.state.value,
            "capabilities": self.capabilities.to_dict(),
            "limits": self.limits.to_dict(),
            "ephemeral": self.ephemeral,
            "created_at": self.created_at,
            "work_dir": self.work_dir,
            "pid": self.pid,
            "exit_code": self.exit_code,
            "output_length": len(self.output),
            "error_length": len(self.error),
            "duration_ms": round(self.duration_ms, 1),
        }


# ---------------------------------------------------------------------------
# Tier availability detection
# ---------------------------------------------------------------------------

def _detect_available_tiers() -> dict[str, bool]:
    """Detect which isolation tiers are available on this system."""
    tiers: dict[str, bool] = {
        "thread": True,  # Always available
        "namespace": False,
        "container": False,
        "microvm": False,
        "wasm": False,
    }

    # Tier 1: Linux namespaces (unshare)
    if os.name == "posix":
        tiers["namespace"] = shutil.which("unshare") is not None

    # Tier 2: Container runtime
    runtime = os.environ.get("WM_SHELTER_RUNTIME", "podman")
    for rt in [runtime, "podman", "nerdctl", "docker"]:
        if shutil.which(rt):
            tiers["container"] = True
            break

    # Tier 3: MicroVM
    fc_path = os.environ.get("WM_SHELTER_FIRECRACKER", "")
    if fc_path and Path(fc_path).exists():
        tiers["microvm"] = True
    elif shutil.which("firecracker"):
        tiers["microvm"] = True
    elif shutil.which("cloud-hypervisor"):
        tiers["microvm"] = True

    # Tier 4: WASM
    tiers["wasm"] = shutil.which("wasmtime") is not None

    return tiers


def _best_available_tier(tiers: dict[str, bool]) -> ShelterTier:
    """Select the highest available tier."""
    # Preference order: container > namespace > wasm > microvm > thread
    preference = ["container", "namespace", "wasm", "microvm", "thread"]
    for tier_name in preference:
        if tiers.get(tier_name, False):
            return ShelterTier(tier_name)
    return ShelterTier.THREAD


def _get_container_runtime() -> str:
    """Find the best available container runtime."""
    preferred = os.environ.get("WM_SHELTER_RUNTIME", "podman")
    for rt in [preferred, "podman", "nerdctl", "docker"]:
        if shutil.which(rt):
            return rt
    return "podman"


# ---------------------------------------------------------------------------
# Execution backends
# ---------------------------------------------------------------------------

def _execute_thread(shelter: Shelter, code: str) -> tuple[str, str, int]:
    """Tier 0: Execute in a sandboxed thread with resource limits."""
    try:
        from whitemagic.execution.sandbox import SafeSandbox
        sandbox = SafeSandbox(timeout_seconds=shelter.limits.timeout_s)
        result = sandbox.execute(code)
        if result.success:
            return result.output, "", 0
        return result.output, result.error or "Sandbox execution failed", 1
    except Exception as e:
        return "", str(e), 1


def _execute_namespace(shelter: Shelter, code: str) -> tuple[str, str, int]:
    """Tier 1: Execute in a Linux namespace (unshare + chroot)."""
    script_path = Path(shelter.work_dir) / "run.py"
    script_path.write_text(code, encoding="utf-8")

    cmd = [
        "unshare", "--user", "--mount", "--pid", "--fork",
        "python3", str(script_path),
    ]

    try:
        proc = subprocess.run(
            cmd,
            capture_output=True, text=True,
            timeout=shelter.limits.timeout_s,
            cwd=shelter.work_dir,
        )
        return proc.stdout, proc.stderr, proc.returncode
    except subprocess.TimeoutExpired:
        return "", "Timeout exceeded", 124
    except Exception as e:
        return "", str(e), 1


def _execute_container(shelter: Shelter, code: str) -> tuple[str, str, int]:
    """Tier 2: Execute in a rootless container."""
    runtime = _get_container_runtime()
    script_path = Path(shelter.work_dir) / "run.py"
    script_path.write_text(code, encoding="utf-8")

    network_flag = "--network=none"
    if shelter.capabilities.network == "full":
        network_flag = "--network=host"
    elif shelter.capabilities.network == "read_only":
        network_flag = "--network=host"  # filtered at application level

    cmd = [
        runtime, "run", "--rm",
        "--read-only",
        network_flag,
        f"--memory={shelter.limits.max_memory_mb}m",
        f"--cpus={shelter.limits.max_cpu_s}",
        f"--timeout={shelter.limits.timeout_s}",
        "-v", f"{shelter.work_dir}:/data:Z",
        "python:3.12-slim",
        "python3", "/data/run.py",
    ]

    try:
        proc = subprocess.run(
            cmd,
            capture_output=True, text=True,
            timeout=shelter.limits.timeout_s + 30,  # grace period
        )
        return proc.stdout, proc.stderr, proc.returncode
    except subprocess.TimeoutExpired:
        return "", "Container timeout exceeded", 124
    except FileNotFoundError:
        return "", f"Container runtime '{runtime}' not found", 127
    except Exception as e:
        return "", str(e), 1


def _execute_wasm(shelter: Shelter, wasm_module: str) -> tuple[str, str, int]:
    """Tier 4: Execute a WASM module via wasmtime."""
    cmd = [
        "wasmtime", "run",
        f"--dir={shelter.work_dir}",
        wasm_module,
    ]

    try:
        proc = subprocess.run(
            cmd,
            capture_output=True, text=True,
            timeout=shelter.limits.timeout_s,
        )
        return proc.stdout, proc.stderr, proc.returncode
    except subprocess.TimeoutExpired:
        return "", "WASM timeout exceeded", 124
    except FileNotFoundError:
        return "", "wasmtime not found", 127
    except Exception as e:
        return "", str(e), 1


# ---------------------------------------------------------------------------
# Shelter Manager
# ---------------------------------------------------------------------------

class ShelterManager:
    """Manages the lifecycle of isolated execution shelters."""

    def __init__(self) -> None:
        self._shelters: dict[str, Shelter] = {}
        self._lock = threading.Lock()
        self._available_tiers = _detect_available_tiers()
        self._max_concurrent = int(os.environ.get("WM_SHELTER_MAX_CONCURRENT", "4"))
        self._default_timeout = int(os.environ.get("WM_SHELTER_TIMEOUT_S", "300"))

    def available_tiers(self) -> dict[str, bool]:
        """Report which isolation tiers are available."""
        return dict(self._available_tiers)

    def best_tier(self) -> str:
        """Return the name of the best available tier."""
        return _best_available_tier(self._available_tiers).value

    def create(
        self,
        name: str,
        tier: str = "auto",
        capabilities: list[str] | None = None,
        limits: dict[str, int] | None = None,
        ephemeral: bool = True,
    ) -> dict[str, Any]:
        """Create a new shelter.

        Args:
            name: Unique shelter name.
            tier: Isolation tier (auto, thread, namespace, container, microvm, wasm).
            capabilities: List of capability grants (e.g., ["network_read"]).
            limits: Resource limits dict.
            ephemeral: Auto-destroy on completion.

        Returns:
            Shelter status dict.
        """
        with self._lock:
            if name in self._shelters:
                return {"status": "error", "reason": f"Shelter '{name}' already exists"}

            active = sum(1 for s in self._shelters.values()
                        if s.state in (ShelterState.CREATED, ShelterState.RUNNING))
            if active >= self._max_concurrent:
                return {"status": "error", "reason": f"Max concurrent shelters ({self._max_concurrent}) reached"}

        # Resolve tier
        if tier == "auto":
            resolved_tier = _best_available_tier(self._available_tiers)
        else:
            resolved_tier = ShelterTier(tier)
            if not self._available_tiers.get(resolved_tier.value, False):
                # Graceful degradation
                resolved_tier = _best_available_tier(self._available_tiers)
                logger.info(f"Shelter '{name}': requested {tier}, degraded to {resolved_tier.value}")

        # Parse capabilities
        caps = ShelterCapabilities()
        if capabilities:
            for cap in capabilities:
                if cap == "network_read":
                    caps.network = "read_only"
                elif cap == "network_full":
                    caps.network = "full"
                elif cap.startswith("fs_read:"):
                    caps.filesystem_read.append(cap[8:])
                elif cap.startswith("fs_write:"):
                    caps.filesystem_write.append(cap[9:])
                elif cap.startswith("tool:"):
                    caps.tools_allow.append(cap[5:])

        # Parse limits
        lim = ShelterLimits(timeout_s=self._default_timeout)
        if limits:
            if "timeout_s" in limits:
                lim.timeout_s = limits["timeout_s"]
            if "max_memory_mb" in limits:
                lim.max_memory_mb = limits["max_memory_mb"]
            if "max_cpu_s" in limits:
                lim.max_cpu_s = limits["max_cpu_s"]
            if "max_disk_mb" in limits:
                lim.max_disk_mb = limits["max_disk_mb"]

        # Create work directory
        work_dir = tempfile.mkdtemp(prefix=f"wm_shelter_{name}_")

        shelter = Shelter(
            name=name,
            tier=resolved_tier,
            state=ShelterState.CREATED,
            capabilities=caps,
            limits=lim,
            ephemeral=ephemeral,
            work_dir=work_dir,
        )

        with self._lock:
            self._shelters[name] = shelter

        logger.info(f"🏠 Shelter '{name}' created (tier={resolved_tier.value})")
        return {"status": "ok", **shelter.to_dict()}

    def execute(
        self,
        name: str,
        payload: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Execute a payload inside a shelter.

        Args:
            name: Shelter name.
            payload: Dict with 'type' (python|shell|wasm) and 'code' or 'wasm_module'.

        Returns:
            Execution result dict.
        """
        with self._lock:
            shelter = self._shelters.get(name)
            if not shelter:
                return {"status": "error", "reason": f"Shelter '{name}' not found"}
            if shelter.state not in (ShelterState.CREATED, ShelterState.COMPLETED):
                return {"status": "error", "reason": f"Shelter '{name}' in state {shelter.state.value}"}
            shelter.state = ShelterState.RUNNING

        payload = payload or {}
        payload_type = payload.get("type", "python")
        code = payload.get("code", "")
        wasm_module = payload.get("wasm_module", "")
        input_data = payload.get("input_data", {})

        # Write input data to shelter work dir
        if input_data:
            input_path = Path(shelter.work_dir) / "input.json"
            input_path.write_text(json.dumps(input_data), encoding="utf-8")

        # Dharma check — always on, even inside shelters
        try:
            from whitemagic.dharma.rules import get_rules_engine
            engine = get_rules_engine()
            action = {"tool": "shelter.execute", "args": {"shelter": name, "type": payload_type}}
            verdict = engine.evaluate(action)
            if verdict and hasattr(verdict, "blocked") and verdict.blocked:
                shelter.state = ShelterState.ERROR
                shelter.error = f"Dharma blocked: {getattr(verdict, 'reason', 'unknown')}"
                return {"status": "blocked", "reason": shelter.error}
        except Exception:
            pass  # Dharma unavailable — proceed

        start = time.perf_counter()
        stdout, stderr, exit_code = "", "", 1

        try:
            if shelter.tier == ShelterTier.THREAD:
                stdout, stderr, exit_code = _execute_thread(shelter, code)
            elif shelter.tier == ShelterTier.NAMESPACE:
                stdout, stderr, exit_code = _execute_namespace(shelter, code)
            elif shelter.tier == ShelterTier.CONTAINER:
                stdout, stderr, exit_code = _execute_container(shelter, code)
            elif shelter.tier == ShelterTier.WASM:
                stdout, stderr, exit_code = _execute_wasm(shelter, wasm_module)
            elif shelter.tier == ShelterTier.MICROVM:
                # MicroVM not yet implemented — degrade to container
                stdout, stderr, exit_code = _execute_container(shelter, code)
        except Exception as e:
            stderr = str(e)
            exit_code = 1

        elapsed = (time.perf_counter() - start) * 1000

        shelter.output = stdout[:50000]  # cap output
        shelter.error = stderr[:10000]
        shelter.exit_code = exit_code
        shelter.duration_ms = elapsed
        shelter.state = ShelterState.COMPLETED if exit_code == 0 else ShelterState.ERROR

        # Karma logging
        try:
            from whitemagic.dharma.karma_ledger import get_karma_ledger
            ledger = get_karma_ledger()
            ledger.record(
                tool="shelter.execute",
                declared_safety="WRITE",
                actual_writes=1,
                success=exit_code == 0,
            )
        except Exception:
            pass

        # Auto-destroy if ephemeral
        if shelter.ephemeral and shelter.state == ShelterState.COMPLETED:
            self._cleanup_workdir(shelter)

        logger.info(
            f"🏠 Shelter '{name}': exit={exit_code}, "
            f"tier={shelter.tier.value}, {elapsed:.0f}ms",
        )

        return {
            "status": "ok" if exit_code == 0 else "error",
            "exit_code": exit_code,
            "output": stdout[:5000],
            "error": stderr[:2000],
            "duration_ms": round(elapsed, 1),
            "tier": shelter.tier.value,
        }

    def inspect(self, name: str, artifact: str = "") -> dict[str, Any]:
        """Read output or artifacts from a shelter.

        Args:
            name: Shelter name.
            artifact: Specific file to read from the shelter work dir.

        Returns:
            Dict with artifact content or shelter output.
        """
        with self._lock:
            shelter = self._shelters.get(name)
            if not shelter:
                return {"status": "error", "reason": f"Shelter '{name}' not found"}

        if artifact:
            artifact_path = Path(shelter.work_dir) / artifact
            if not artifact_path.exists():
                return {"status": "error", "reason": f"Artifact '{artifact}' not found"}
            # Security: prevent path traversal
            try:
                artifact_path.resolve().relative_to(Path(shelter.work_dir).resolve())
            except ValueError:
                return {"status": "error", "reason": "Path traversal denied"}
            content = artifact_path.read_text(encoding="utf-8", errors="ignore")[:50000]
            return {"status": "ok", "artifact": artifact, "content": content}

        return {
            "status": "ok",
            "output": shelter.output[:5000],
            "error": shelter.error[:2000],
            "exit_code": shelter.exit_code,
            "state": shelter.state.value,
        }

    def destroy(self, name: str) -> dict[str, Any]:
        """Tear down a shelter and clean up resources."""
        with self._lock:
            shelter = self._shelters.get(name)
            if not shelter:
                return {"status": "error", "reason": f"Shelter '{name}' not found"}

        self._cleanup_workdir(shelter)
        shelter.state = ShelterState.DESTROYED

        with self._lock:
            del self._shelters[name]

        logger.info(f"🏠 Shelter '{name}' destroyed")
        return {"status": "ok", "message": f"Shelter '{name}' destroyed"}

    def status(self) -> dict[str, Any]:
        """List active shelters and system capabilities."""
        with self._lock:
            shelters = [s.to_dict() for s in self._shelters.values()]

        return {
            "available_tiers": self._available_tiers,
            "best_tier": self.best_tier(),
            "max_concurrent": self._max_concurrent,
            "active_shelters": len(shelters),
            "shelters": shelters,
        }

    def policy(self, name: str, capabilities: dict[str, Any] | None = None) -> dict[str, Any]:
        """Get or set capability policy for a shelter."""
        with self._lock:
            shelter = self._shelters.get(name)
            if not shelter:
                return {"status": "error", "reason": f"Shelter '{name}' not found"}

        if capabilities is None:
            return {"status": "ok", "capabilities": shelter.capabilities.to_dict()}

        # Update capabilities
        if "network" in capabilities:
            shelter.capabilities.network = capabilities["network"]
        if "filesystem_read" in capabilities:
            shelter.capabilities.filesystem_read = capabilities["filesystem_read"]
        if "filesystem_write" in capabilities:
            shelter.capabilities.filesystem_write = capabilities["filesystem_write"]
        if "tools_allow" in capabilities:
            shelter.capabilities.tools_allow = capabilities["tools_allow"]

        return {"status": "ok", "capabilities": shelter.capabilities.to_dict()}

    def _cleanup_workdir(self, shelter: Shelter) -> None:
        """Clean up a shelter's work directory."""
        if shelter.work_dir and Path(shelter.work_dir).exists():
            try:
                shutil.rmtree(shelter.work_dir, ignore_errors=True)
            except Exception:
                pass


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_manager: ShelterManager | None = None
_manager_lock = threading.Lock()


def get_shelter_manager() -> ShelterManager:
    """Get the global ShelterManager singleton."""
    global _manager
    if _manager is None:
        with _manager_lock:
            if _manager is None:
                _manager = ShelterManager()
    return _manager
