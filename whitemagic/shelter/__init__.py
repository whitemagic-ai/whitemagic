"""Sovereign Sandbox — Qubes-Style Isolated Execution (v15.2).
=============================================================
Provides 5-tier isolation for untrusted workloads:

  Tier 0: Thread    — existing ToolSandbox resource limits (default)
  Tier 1: Namespace — Linux unshare + chroot
  Tier 2: Container — rootless podman/nerdctl
  Tier 3: MicroVM   — Firecracker / Cloud Hypervisor (KVM)
  Tier 4: WASM      — wasmtime (WASI), capability-based

Each shelter declares explicit capability grants. Everything else
is denied by default. Dharma governance applies inside shelters.

Usage:
    from whitemagic.shelter import get_shelter_manager
    mgr = get_shelter_manager()

    shelter = mgr.create("research_task", tier="container",
                         capabilities=["network_read"], timeout_s=300)
    result = mgr.execute(shelter.name, payload={"type": "python", "code": "..."})
    artifacts = mgr.inspect(shelter.name, artifact="output.json")
    mgr.destroy(shelter.name)
"""

from whitemagic.shelter.manager import (
    ShelterManager,
    get_shelter_manager,
)

__all__ = ["ShelterManager", "get_shelter_manager"]
