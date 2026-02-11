#!/usr/bin/env python3
"""
Tool Smoke Test for Whitemagic
=============================

This is a *non-exhaustive* runtime verification that:
- the canonical TOOL_REGISTRY entries are callable via `whitemagic.tools.unified_api.call_tool`
- tool calls return structured results (dict) without crashing the process

It intentionally uses an isolated WM_STATE_ROOT so it does not touch a user's real state.
"""

from __future__ import annotations

import concurrent.futures
import json
import os
import sys
import time
import traceback
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

# Per-tool timeout in seconds — prevents individual tools from hanging the smoke run
TOOL_TIMEOUT_S = 20


@dataclass
class Context:
    wm_state_root: Path
    sample_file: Path
    session_id: Optional[str] = None
    scratchpad_id: Optional[str] = None
    memory_id_1: Optional[str] = None
    memory_filename_1: Optional[str] = None
    memory_id_2: Optional[str] = None
    memory_filename_2: Optional[str] = None


def _default_value(prop: Dict[str, Any]) -> Any:
    if "default" in prop:
        return prop["default"]
    if "enum" in prop and isinstance(prop["enum"], list) and prop["enum"]:
        return prop["enum"][0]
    t = prop.get("type")
    if t == "string":
        return "test"
    if t == "integer":
        return 1
    if t == "number":
        return 0.5
    if t == "boolean":
        return False
    if t == "array":
        return []
    if t == "object":
        return {}
    return None


def _build_args(tool_name: str, schema: Dict[str, Any], ctx: Context) -> Dict[str, Any]:
    props = schema.get("properties", {}) or {}
    required: List[str] = list(schema.get("required", []) or [])

    args: Dict[str, Any] = {}
    for key in required:
        prop = props.get(key, {})
        args[key] = _default_value(prop)

    # Targeted overrides for safer / more meaningful smoke execution.
    if tool_name in {"create_session"}:
        args["name"] = "smoke_session"

    if tool_name in {"checkpoint_session", "resume_session"}:
        args["session_id"] = ctx.session_id or "missing_session"

    if tool_name in {"scratchpad_create"}:
        args["name"] = "smoke_scratchpad"
        if "session_id" in props:
            args["session_id"] = ctx.session_id

    if tool_name in {"scratchpad_update", "scratchpad_finalize"}:
        args["scratchpad_id"] = ctx.scratchpad_id or "missing_scratchpad"
        # For update, required includes content; section may be required by schema.
        if tool_name == "scratchpad_update":
            args.setdefault("section", "current_focus")
            args.setdefault("content", "smoke update")

    if tool_name in {"create_memory"}:
        args["title"] = "Smoke Memory"
        args["content"] = "smoke test content"
        args.setdefault("type", "short_term")
        args.setdefault("tags", ["smoke"])

    if tool_name in {"read_memory", "fast_read_memory"}:
        args["filename"] = ctx.memory_filename_1 or "missing.md"

    if tool_name in {"batch_read_memories"}:
        args["filenames"] = [ctx.memory_filename_1 or "missing.md"]

    if tool_name in {"search_memories"}:
        args["query"] = "smoke"
        args.setdefault("limit", 5)

    if tool_name in {"parallel_search"}:
        args["queries"] = ["smoke"]

    if tool_name in {"list_memories"}:
        args.setdefault("limit", 5)

    if tool_name in {"update_memory"}:
        args["filename"] = ctx.memory_filename_2 or (ctx.memory_filename_1 or "missing.md")
        args.setdefault("title", "Smoke Updated Title")

    if tool_name in {"delete_memory"}:
        args["filename"] = ctx.memory_filename_2 or (ctx.memory_filename_1 or "missing.md")
        args.setdefault("permanent", False)

    if tool_name in {"edge_infer"}:
        args["query"] = "smoke: what is whitemagic?"

    if tool_name in {"edge_batch_infer"}:
        args["queries"] = ["smoke: batch 1", "smoke: batch 2"]

    if tool_name in {"track_metric"}:
        args["category"] = "smoke"
        args["metric"] = "count"
        args["value"] = 1

    if tool_name in {"record_yin_yang_activity"}:
        args["activity"] = "READ"

    if tool_name in {"garden_activate"}:
        args["garden"] = "wisdom"

    if tool_name.startswith("archaeology_"):
        args.setdefault("path", str(ctx.sample_file))
        args.setdefault("directory", str(ctx.wm_state_root))

    if tool_name in {"governor_validate"}:
        args["command"] = "echo smoke"

    if tool_name in {"governor_set_goal"}:
        args["goal"] = "smoke test"

    if tool_name in {"governor_check_drift"}:
        args["action"] = "run smoke tools"
        args.setdefault("goal", "smoke test")

    if tool_name in {"sangha_chat_send"}:
        args["message"] = "smoke hello"
        args["sender"] = "smoke"

    if tool_name in {"sangha_lock_acquire", "sangha_lock_release"}:
        args["resource"] = "smoke_resource"

    if tool_name in {"execute_cascade"}:
        args["pattern_name"] = "smoke"
        args["context"] = {"note": "smoke"}

    if tool_name in {"rust_similarity"}:
        args["text1"] = "a"
        args["text2"] = "a"

    if tool_name in {"kaizen_analyze"}:
        args["target"] = str(ctx.wm_state_root)

    if tool_name in {"kaizen_apply_fixes"}:
        args["fix_ids"] = []
        args.setdefault("dry_run", True)

    if tool_name in {"serendipity_mark_accessed"}:
        args["memory_id"] = ctx.memory_id_1 or "missing_memory"

    if tool_name in {"evaluate_ethics", "check_boundaries", "verify_consent"}:
        # These tools expect "action" as a string per schema, but unified_api
        # wraps it into an object for the dharma bridge. Keep it simple.
        args["action"] = "smoke: do a safe read-only check"

    if tool_name in {"get_dharma_guidance"}:
        args["situation"] = "smoke: what should I do next?"

    if tool_name.startswith("gana_"):
        args.setdefault("operation", "analyze")
        args.setdefault("context", {})

    # Remove None values (call_tool often treats absence differently than None).
    args = {k: v for k, v in args.items() if v is not None}
    return args


def main() -> int:
    # Ensure isolated state root (can be overridden via env).
    os.environ.setdefault("WM_STATE_ROOT", str(Path("/tmp") / "wm_tool_smoke"))
    os.environ.setdefault("WM_SILENT_INIT", "1")
    os.environ.setdefault("WM_ALLOW_MODEL_DOWNLOAD", "0")
    os.environ.setdefault("WHITEMAGIC_OLLAMA_TIMEOUT_S", "20")

    # Allow running from repo without requiring installation.
    repo_root = Path(__file__).resolve().parent.parent
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))

    from whitemagic.tools.registry import TOOL_REGISTRY
    from whitemagic.tools.unified_api import call_tool

    wm_state_root = Path(os.environ["WM_STATE_ROOT"]).expanduser().resolve()
    wm_state_root.mkdir(parents=True, exist_ok=True)

    sample_file = wm_state_root / "sample.txt"
    sample_file.write_text("smoke file", encoding="utf-8")

    ctx = Context(wm_state_root=wm_state_root, sample_file=sample_file)

    # Seed minimal state for tools that require IDs.
    try:
        sess = call_tool("create_session", name="smoke_session")
        if isinstance(sess, dict):
            ctx.session_id = ((sess.get("details") or {}).get("session") or {}).get("id")
    except Exception:
        ctx.session_id = None

    try:
        mem1 = call_tool("create_memory", title="Smoke1", content="smoke one", tags=["smoke"], type="short_term")
        details = mem1.get("details") if isinstance(mem1, dict) else {}
        ctx.memory_id_1 = (details or {}).get("memory_id")
        ctx.memory_filename_1 = (details or {}).get("filename") or (f"{ctx.memory_id_1}.md" if ctx.memory_id_1 else None)

        mem2 = call_tool("create_memory", title="Smoke2", content="smoke two", tags=["smoke"], type="short_term")
        details = mem2.get("details") if isinstance(mem2, dict) else {}
        ctx.memory_id_2 = (details or {}).get("memory_id")
        ctx.memory_filename_2 = (details or {}).get("filename") or (f"{ctx.memory_id_2}.md" if ctx.memory_id_2 else None)
    except Exception:
        pass

    try:
        sp = call_tool("scratchpad_create", name="smoke_scratchpad", session_id=ctx.session_id)
        if isinstance(sp, dict):
            ctx.scratchpad_id = ((sp.get("details") or {}).get("scratchpad") or {}).get("id")
    except Exception:
        ctx.scratchpad_id = None

    results: List[Dict[str, Any]] = []
    failures = 0

    executor = concurrent.futures.ThreadPoolExecutor(max_workers=1)

    for tool_def in TOOL_REGISTRY:
        name = tool_def.name
        schema = tool_def.input_schema

        args = _build_args(name, schema, ctx)
        start = time.time()
        try:
            future = executor.submit(call_tool, name, **args)
            out = future.result(timeout=TOOL_TIMEOUT_S)
            duration_ms = int((time.time() - start) * 1000)
            ok = (
                isinstance(out, dict)
                and out.get("tool") == name
                and isinstance(out.get("status"), str)
                and "details" in out
            )
            if not ok:
                failures += 1
            results.append(
                {
                    "tool": name,
                    "ok": ok,
                    "duration_ms": duration_ms,
                    "args": args,
                    "result_type": type(out).__name__,
                    "status": out.get("status") if isinstance(out, dict) else None,
                    "message": out.get("message") if isinstance(out, dict) else None,
                    "error_code": out.get("error_code") if isinstance(out, dict) else None,
                }
            )
        except concurrent.futures.TimeoutError:
            failures += 1
            duration_ms = int((time.time() - start) * 1000)
            results.append(
                {
                    "tool": name,
                    "ok": False,
                    "duration_ms": duration_ms,
                    "args": args,
                    "result_type": "timeout",
                    "exception": f"Tool exceeded {TOOL_TIMEOUT_S}s timeout",
                }
            )
            # Critical: timed-out futures can leave the single worker thread wedged.
            # Recreate executor so later tools still get a fresh worker.
            executor.shutdown(wait=False, cancel_futures=True)
            executor = concurrent.futures.ThreadPoolExecutor(max_workers=1)
        except Exception as exc:
            failures += 1
            duration_ms = int((time.time() - start) * 1000)
            results.append(
                {
                    "tool": name,
                    "ok": False,
                    "duration_ms": duration_ms,
                    "args": args,
                    "result_type": "exception",
                    "exception": str(exc),
                    "traceback": traceback.format_exc(),
                }
            )

    executor.shutdown(wait=False, cancel_futures=True)

    out_dir = Path(os.environ["WM_STATE_ROOT"]) / "reports"
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "tool_smoke.json"
    out_path.write_text(json.dumps(results, indent=2, sort_keys=True), encoding="utf-8")

    print(f"Wrote {out_path} ({len(results)} tools). Failures: {failures}")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    # Some tools may spawn background loops/threads that keep Python alive
    # after smoke execution is complete. Force process exit to keep this gate bounded.
    rc = main()
    try:
        sys.stdout.flush()
        sys.stderr.flush()
    finally:
        os._exit(rc)
