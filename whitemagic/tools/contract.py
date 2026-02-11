"""Whitemagic Tool Contract (AI-Primary).
====================================

This module defines the canonical tool contract surface for Whitemagic:
- Envelope schema versioning (stable across tools)
- Error code conventions
- Environment variable interface expectations

The MCP server is the canonical interface; CLI/REST are adapters.
"""

from __future__ import annotations

from dataclasses import dataclass

# Bump when the *shape* of tool responses or required behaviors change.
ENVELOPE_VERSION = "1.0"

# Bump when tool names, input schemas, or semantics change in a way that
# callers should treat as a contract update. This is independent of package
# versioning because the tool surface may change without a release bump (or vice
# versa).
TOOL_CONTRACT_VERSION = "11.0.0"


@dataclass(frozen=True)
class EnvVar:
    name: str
    purpose: str
    example: str | None = None


ENV_VARS: list[EnvVar] = [
    EnvVar(
        name="WM_STATE_ROOT",
        purpose="Canonical root directory for all Whitemagic runtime state (memory, sessions, logs, artifacts).",
        example="/tmp/whitemagic_state",
    ),
    EnvVar(
        name="WM_CONFIG_ROOT",
        purpose="Legacy alias for WM_STATE_ROOT (still supported).",
        example="~/.whitemagic",
    ),
    EnvVar(
        name="WM_SILENT_INIT",
        purpose="If set, suppress noisy initialization logs (useful for AI callers).",
        example="1",
    ),
    EnvVar(
        name="WHITEMAGIC_ALLOWED_PATHS",
        purpose="Colon-separated allowlist of additional filesystem roots for *read-only* tools.",
        example="/repo:/docs",
    ),
    EnvVar(
        name="WHITEMAGIC_ALLOW_RESTRICTED_TOOLS",
        purpose="If true, allow restricted tools in cloud mode (advanced / dangerous).",
        example="true",
    ),
    EnvVar(
        name="WHITEMAGIC_ALLOW_DANGEROUS_TOOLS",
        purpose="If true, allow dangerous tools in cloud mode (advanced / dangerous).",
        example="true",
    ),
    EnvVar(
        name="WHITEMAGIC_ALLOW_CWD_PATH",
        purpose="If true in cloud mode, include current working directory in the path allowlist.",
        example="true",
    ),
]

