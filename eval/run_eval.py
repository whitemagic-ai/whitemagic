#!/usr/bin/env python3
"""
Whitemagic Eval Harness (AI-Primary)
===================================

This script runs a small, deterministic set of tool calls to validate:
- stable envelope shape
- deterministic `now` override behavior
- idempotency replay behavior for write tools
- repo ship hygiene via `ship.check`

It is intentionally lightweight and has no external dependencies beyond the
Whitemagic core install.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import time
from pathlib import Path
from uuid import uuid4

# Support both:
# - `python -m eval.run_eval` (module mode)
# - `python eval/run_eval.py` (script mode from repo root)
REPO_ROOT = Path(__file__).resolve().parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))


_ENVELOPE_KEYS = {
    "status",
    "tool",
    "request_id",
    "idempotency_key",
    "message",
    "error_code",
    "details",
    "retryable",
    "writes",
    "artifacts",
    "metrics",
    "side_effects",
    "warnings",
    "timestamp",
    "envelope_version",
    "tool_contract_version",
}


def _assert_envelope(out: dict) -> tuple[bool, str]:
    missing = _ENVELOPE_KEYS.difference(out.keys())
    if missing:
        return False, f"missing envelope keys: {sorted(missing)}"
    try:
        json.dumps(out)
    except Exception as exc:
        return False, f"not JSON-serializable: {exc}"
    return True, "ok"


def _parse_args(argv: list[str]) -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--state-root", help="Override WM_STATE_ROOT for this run.")
    p.add_argument("--silent-init", action="store_true", help="Set WM_SILENT_INIT=1.")
    p.add_argument("--now", help="ISO timestamp override for deterministic runs.")
    p.add_argument("--out", help="Write report.json into this directory (default: $WM_STATE_ROOT/artifacts/eval/<ts>/).")
    return p.parse_args(argv)


def main(argv: list[str]) -> int:
    args = _parse_args(argv)

    if args.state_root:
        os.environ["WM_STATE_ROOT"] = args.state_root
    if args.silent_init:
        os.environ["WM_SILENT_INIT"] = "1"

    # Import after env bootstrap.
    from whitemagic.config.paths import ARTIFACTS_DIR, WM_ROOT
    from whitemagic.tools.unified_api import call_tool

    now = args.now

    report: dict[str, object] = {
        "ok": True,
        "wm_state_root": str(WM_ROOT),
        "timestamp": now,
        "scenarios": [],
    }

    def run(name: str, fn) -> None:
        start = time.perf_counter()
        entry: dict[str, object] = {"name": name}
        try:
            out = fn()
            extras = None
            if isinstance(out, tuple) and len(out) == 2:
                out, extras = out
                entry["extras"] = extras
            entry["output"] = out
            env_ok, env_reason = _assert_envelope(out if isinstance(out, dict) else {})
            entry["envelope_ok"] = env_ok
            entry["envelope_reason"] = env_reason
            entry["ok"] = env_ok and out.get("status") in {"success", "error"}  # type: ignore[union-attr]
        except Exception as exc:
            entry["ok"] = False
            entry["exception"] = repr(exc)
        finally:
            entry["duration_ms"] = int((time.perf_counter() - start) * 1000)

        if not entry.get("ok"):
            report["ok"] = False
        report["scenarios"].append(entry)

    def _capabilities_scenario() -> dict:
        out = call_tool("capabilities", include_tools=False, include_env=False, now=now)
        if out.get("status") != "success":
            raise RuntimeError(f"capabilities failed: {out.get('message')}")
        return out

    run("capabilities", _capabilities_scenario)

    def _invalid_params_manifest_scenario() -> dict:
        out = call_tool("manifest", format="not-a-real-format", now=now)
        if out.get("status") != "error":
            raise RuntimeError("manifest(invalid) expected status=error")
        if out.get("error_code") != "invalid_params":
            raise RuntimeError(f"manifest(invalid) expected error_code=invalid_params got {out.get('error_code')}")
        return out

    run("invalid_params_manifest", _invalid_params_manifest_scenario)

    def _idempotency_scenario() -> tuple[dict, dict]:
        key = f"eval-{uuid4()}"
        first = call_tool(
            "create_memory",
            title="eval idempotency",
            content="hello",
            type="short_term",
            tags=["eval"],
            idempotency_key=key,
            now=now,
        )
        second = call_tool(
            "create_memory",
            title="eval idempotency",
            content="hello",
            type="short_term",
            tags=["eval"],
            idempotency_key=key,
            now=now,
        )

        if first.get("status") != "success":
            raise RuntimeError(f"create_memory failed: {first.get('message')}")
        if second.get("status") != "success":
            raise RuntimeError(f"create_memory replay failed: {second.get('message')}")

        mem_id_1 = (first.get("details") or {}).get("memory_id")
        mem_id_2 = (second.get("details") or {}).get("memory_id")
        if mem_id_1 != mem_id_2:
            raise RuntimeError(f"idempotency mismatch: {mem_id_1} != {mem_id_2}")
        if (second.get("side_effects") or {}).get("idempotency_replay") is not True:
            raise RuntimeError("missing side_effects.idempotency_replay == true on replay")

        return second, {"first": first, "second": second}

    run("idempotency_create_memory", _idempotency_scenario)

    def _ship_check_scenario() -> dict:
        out = call_tool("ship.check", now=now)
        if out.get("status") != "success":
            raise RuntimeError(f"ship.check failed: {out.get('message')}")
        # ship.check findings are advisory — absolute paths in changelogs and
        # test fixtures are expected.  Only fail on secrets.
        if not (out.get("details") or {}).get("ok"):
            issues = (out.get("details") or {}).get("issues") or []
            secret_issues = [i for i in issues if i.get("kind") == "potential_secrets"]
            if secret_issues:
                raise RuntimeError(f"ship.check found secrets: {secret_issues}")
        return out

    run("ship_check", _ship_check_scenario)

    # Deterministic timestamp check (verbatim).
    def _now_scenario() -> dict:
        ts = now or "2026-01-01T00:00:00Z"
        out = call_tool("capabilities", include_tools=False, include_env=False, now=ts)
        if out.get("timestamp") != ts:
            raise RuntimeError(f"timestamp mismatch: expected {ts} got {out.get('timestamp')}")
        return out

    run("now_override_timestamp", _now_scenario)

    # Write report.
    if args.out:
        out_dir = Path(args.out).expanduser().resolve()
    else:
        out_dir = ARTIFACTS_DIR / "eval" / (now or time.strftime("%Y%m%dT%H%M%S"))
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "report.json"
    out_path.write_text(json.dumps(report, indent=2, sort_keys=True, ensure_ascii=True) + "\n", encoding="utf-8")

    print(str(out_path))
    return 0 if report.get("ok") else 2


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
