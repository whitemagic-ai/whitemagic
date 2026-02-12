"""Unified Tool API - Bridge between MCP and Python Tools
Expanded to support all 44 MCP tools.
"""

import asyncio
import json
import logging
import os
import time
import traceback
from collections.abc import Coroutine
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any, TypeVar, cast
from uuid import uuid4

# Tool contract helpers (AI-first)
from whitemagic.config.paths import WM_ROOT, ensure_paths
from whitemagic.tools.envelope import err, normalize_raw
from whitemagic.tools.errors import ErrorCode
from whitemagic.tools.idempotency import get_record, put_record
from whitemagic.tools.registry import ToolSafety, get_tool
from whitemagic.tools.schema import validate_params
from whitemagic.utils.time import now_iso, override_now

logger = logging.getLogger(__name__)
T = TypeVar("T")

def _nervous_system_check(tool_name: str) -> tuple[bool, str]:
    """Pre-dispatch check via Nervous System (StateBoard + DispatchBridge).

    Checks circuit breakers and rate limits via the Zig/Python dispatch bridge.
    Returns (allowed, reason).
    """
    try:
        from whitemagic.core.acceleration.dispatch_bridge import DispatchResult, get_dispatch
        bridge = get_dispatch()
        # Map tool name to a generic tool_id (0-27) via hash
        tool_id = hash(tool_name) % 28
        result = bridge.check(tool_id)
        if result == DispatchResult.CIRCUIT_OPEN:
            return False, f"Circuit breaker OPEN for {tool_name}"
        if result == DispatchResult.RATE_LIMITED:
            return False, f"Rate limited: {tool_name}"
        if result == DispatchResult.IMMATURE:
            return False, f"Tool maturity gate blocked: {tool_name}"
    except Exception:
        pass  # Nervous system is advisory — never block on failure
    return True, ""


def _nervous_system_post(tool_name: str, duration: float, success: bool) -> None:
    """Post-dispatch: sync Harmony Vector to StateBoard and publish to EventRing."""
    # Sync Harmony Vector → StateBoard mmap
    try:
        from whitemagic.harmony.vector import get_harmony_vector
        from whitemagic.core.acceleration.state_board_bridge import get_state_board
        hv = get_harmony_vector()
        snap = hv.snapshot()
        board = get_state_board()
        board.write_harmony(
            balance=snap.balance,
            throughput=snap.throughput,
            latency=snap.latency,
            error_rate=snap.error_rate,
            dharma=snap.dharma,
            karma_debt=snap.karma_debt,
            energy=snap.energy,
        )
    except Exception:
        pass
    # Publish tool completion to EventRing
    try:
        from whitemagic.core.acceleration.event_ring_bridge import get_event_ring
        event_type = "tool_completed" if success else "error_occurred"
        get_event_ring().publish(
            event_type=event_type,
            source=tool_name,
            confidence=1.0,
            data=f"{duration:.3f}s".encode()[:80],
        )
    except Exception:
        pass


def _emit_gan_ying(event_type_name: str, data: dict[str, Any], source: str = "mcp") -> None:
    """Emit Gan Ying events without breaking tool flows."""
    try:
        # Use the public wrapper (handles unknown string event types safely).
        from whitemagic.core.resonance import emit_event

        emit_event(event_type_name, data, source=source, confidence=1.0)
    except Exception as exc:
        logger.info(f"Gan Ying event ({event_type_name}) failed: {exc}")

def _load_rust() -> tuple[object | None, str | None]:
    """Load the Rust bridge if available."""
    try:
        import whitemagic_rs  # type: ignore
        return whitemagic_rs, None
    except Exception as exc:  # pragma: no cover - best-effort availability
        return None, str(exc)


def _resolve_base_path(kwargs: dict[str, Any]) -> Path:
    """Resolve base path with security validation."""
    base_path = kwargs.get("base_path") or os.environ.get("WM_BASE_PATH")
    if not base_path:
        return cast("Path", WM_ROOT)

    resolved = Path(base_path).expanduser().resolve()

    # Strong default: state lives under WM_STATE_ROOT. Allow external state roots
    # only via explicit opt-in.
    allow_external = os.getenv("WHITEMAGIC_ALLOW_EXTERNAL_STATE_ROOT", "false").lower() == "true"
    if not allow_external:
        try:
            resolved.relative_to(WM_ROOT)
        except ValueError:
            return cast("Path", WM_ROOT)

    # Security: validate base_path is allowed (read/write allowlist).
    from whitemagic.security.tool_gating import get_tool_gate
    gate = get_tool_gate()
    allowed, _reason = gate.path_validator.is_path_allowed(str(resolved))
    if not allowed:
        return cast("Path", WM_ROOT)

    resolved.mkdir(parents=True, exist_ok=True)
    return resolved


def _session_dir(base_path: Path) -> Path:
    session_dir = base_path / "sessions"
    session_dir.mkdir(parents=True, exist_ok=True)
    return session_dir


def _session_path(base_path: Path, session_id: str) -> Path:
    return _session_dir(base_path) / f"{session_id}.json"


def _load_session(base_path: Path, session_id: str) -> dict[str, Any]:
    path = _session_path(base_path, session_id)
    if not path.exists():
        raise FileNotFoundError(f"Session not found: {session_id}")
    return cast("dict[str, Any]", json.loads(path.read_text(encoding="utf-8")))


def _save_session(base_path: Path, session: dict[str, Any]) -> None:
    path = _session_path(base_path, session["id"])
    path.write_text(json.dumps(session, indent=2), encoding="utf-8")


def _run_async(coro: Coroutine[Any, Any, T]) -> T:
    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(coro)

    with ThreadPoolExecutor(max_workers=1) as executor:
        return executor.submit(asyncio.run, coro).result()


def _local_models_archived() -> dict[str, Any]:
    return {
        "status": "error",
        "message": "Local model execution is archived/disabled in this build. "
        "Use an external model via MCP/REST to call Whitemagic tools.",
        "archived": True,
    }


def record_yin_yang_activity(activity: str) -> dict[str, Any]:
    """Record Yin-Yang activity from MCP."""
    try:
        from whitemagic.harmony.yin_yang_tracker import get_tracker

        tracker = get_tracker()
        metrics = tracker.record_activity(activity)

        return {
            "success": True,
            "balance_score": metrics.balance_score,
            "burnout_risk": metrics.burnout_risk,
            "recommendation": metrics.recommendation,
            "yang_ratio": metrics.yang_ratio,
            "yin_ratio": metrics.yin_ratio,
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


def get_yin_yang_balance() -> dict[str, Any]:
    """Get current Yin-Yang balance report."""
    try:
        from whitemagic.harmony.yin_yang_tracker import get_tracker

        tracker = get_tracker()
        return cast("dict[str, Any]", tracker.get_report())
    except Exception as e:
        return {"error": str(e)}


def _dispatch_tool(tool_name: str, **kwargs: Any) -> Any:
    """Dispatch tool calls via the dispatch table (v11 refactor).

    The dispatch table lives in whitemagic.tools.dispatch_table and maps
    tool names to handler functions grouped by category.  Governor
    interception, gana prefix routing, and bridge fallback are handled
    inside ``dispatch()``.
    """
    from whitemagic.tools.dispatch_table import dispatch as _table_dispatch

    return _table_dispatch(tool_name, **kwargs)


# Dead code removed: the 1400-line if/elif dispatcher was replaced by
# whitemagic.tools.dispatch_table (Phase 2 refactor, v11 hardening).
# Original handlers live in whitemagic/tools/handlers/*.py


_DEAD_CODE_REMOVED = True  # Marker for grep-ability



_TOOL_ALIASES: dict[str, str] = {
    # Legacy names -> canonical v11 names
    "manifest_read": "manifest",
    "manifest_summary": "manifest",
    "state_paths": "state.paths",
    "state_summary": "state.summary",
    "repo_summary": "repo.summary",
    "ship_check": "ship.check",
    # Underscore aliases for dot-notation tools
    "broker_publish": "broker.publish",
    "broker_history": "broker.history",
    "broker_status": "broker.status",
    "task_distribute": "task.distribute",
    "task_status": "task.status",
    "task_list": "task.list",
    "task_complete": "task.complete",
    "vote_create": "vote.create",
    "vote_cast": "vote.cast",
    "vote_analyze": "vote.analyze",
    "vote_list": "vote.list",
    "vote_record_outcome": "vote.record_outcome",
    "ollama_models": "ollama.models",
    "ollama_generate": "ollama.generate",
    "ollama_chat": "ollama.chat",
    "agent_register": "agent.register",
    "agent_heartbeat": "agent.heartbeat",
    "agent_list": "agent.list",
    "agent_capabilities": "agent.capabilities",
    "agent_deregister": "agent.deregister",
    "pipeline_create": "pipeline.create",
    "pipeline_status": "pipeline.status",
    "pipeline_list": "pipeline.list",
    "homeostasis_status": "homeostasis.status",
    "homeostasis_check": "homeostasis.check",
    "maturity_assess": "maturity.assess",
    "tool_graph": "tool.graph",
    "tool_graph_full": "tool.graph_full",
    "dharma_reload": "dharma.reload",
    "salience_spotlight": "salience.spotlight",
    "reasoning_bicameral": "reasoning.bicameral",
    "memory_retention_sweep": "memory.retention_sweep",
    "starter_packs_list": "starter_packs.list",
    "starter_packs_get": "starter_packs.get",
    "starter_packs_suggest": "starter_packs.suggest",
    "capability_matrix": "capability.matrix",
    "capability_status": "capability.status",
    "capability_suggest": "capability.suggest",
    "rate_limiter_stats": "rate_limiter.stats",
    "audit_export": "audit.export",
    "agent_trust": "agent.trust",
}


def _canonical_tool_name(tool_name: str) -> str:
    name = tool_name.strip()
    return _TOOL_ALIASES.get(name, name)


def _tool_writes_hint(tool_name: str) -> list[dict[str, Any]]:
    # Best-effort: most writes are within WM_STATE_ROOT.
    return [{"kind": "wm_state_root", "path": str(WM_ROOT)}]


def call_tool(tool_name: str, **kwargs: Any) -> dict[str, Any]:
    """Canonical tool entrypoint (AI-first contract).

    Responsibilities:
    - Ensure WM_STATE_ROOT directories exist
    - Validate params (best-effort) against TOOL_REGISTRY schema when available
    - Apply ToolGate policy checks
    - Provide idempotency for write tools via `idempotency_key`
    - Normalize all outputs into the stable envelope format
    """
    from whitemagic.security.tool_gating import check_tool_execution

    ensure_paths()

    # Common fields (present in every tool schema)
    request_id = str(kwargs.pop("request_id", "") or uuid4())
    idempotency_key = kwargs.pop("idempotency_key", None)
    dry_run = bool(kwargs.pop("dry_run", False))
    now_override = kwargs.pop("now", None)

    canonical = _canonical_tool_name(tool_name)
    ts = now_override or now_iso()
    call_started_at = time.time()

    # Touch dream cycle idle timer on every tool call
    try:
        from whitemagic.core.dreaming import get_dream_cycle
        get_dream_cycle().touch()
    except Exception:
        pass

    # Cross-session learning — record tool usage
    try:
        from whitemagic.core.learning import get_session_learner
        get_session_learner().record_tool_use(canonical)
    except Exception:
        pass

    def _record_telemetry(out: dict[str, Any]) -> None:
        duration = time.time() - call_started_at
        status_value = str(out.get("status", "")).lower()
        telemetry_status = "success" if status_value in {"success", "ok"} else "error"
        telemetry_error = out.get("error_code") if telemetry_status == "error" else None
        try:
            from whitemagic.core.monitoring.telemetry import get_telemetry
            get_telemetry().record_call(canonical, duration, telemetry_status, telemetry_error)
        except Exception:
            pass
        # OpenTelemetry span export
        try:
            from whitemagic.core.monitoring.otel_export import record_tool_span
            record_tool_span(canonical, duration, telemetry_status)
        except Exception:
            pass
        # Feed the Harmony Vector (MandalaOS integration)
        declared_safety = "READ"
        if tool_def is not None:
            declared_safety = tool_def.safety.value.upper()
        actual_writes = len(out.get("writes", []) or [])
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            snap = hv.record_call(
                tool_name=canonical,
                duration_s=duration,
                success=(telemetry_status == "success"),
                declared_safety=declared_safety,
                actual_writes=actual_writes,
            )
            # Inject harmony score into envelope metrics
            metrics = out.get("metrics")
            if isinstance(metrics, dict):
                metrics["harmony_score"] = snap.harmony_score
                metrics["guna"] = snap.guna_rajasic_pct  # dominant guna hint
        except Exception:
            pass
        # Feed the Karma Ledger (declared vs actual side-effects)
        try:
            from whitemagic.dharma.karma_ledger import get_karma_ledger
            get_karma_ledger().record(
                tool=canonical,
                declared_safety=declared_safety,
                actual_writes=actual_writes,
                success=(telemetry_status == "success"),
            )
        except Exception:
            pass

    def _finish(out: dict[str, Any]) -> dict[str, Any]:
        _record_telemetry(out)
        return out

    with override_now(now_override):
        tool_def = get_tool(canonical)

        # Best-effort schema validation (only for registry tools).
        if tool_def is not None:
            valid, reason, sanitized = validate_params(tool_def.input_schema, kwargs)
            if not valid:
                return _finish(err(
                    tool=canonical,
                    request_id=request_id,
                    idempotency_key=idempotency_key,
                    timestamp=ts,
                    error_code=ErrorCode.INVALID_PARAMS,
                    message=reason,
                    details={"tool": canonical},
                ))
            kwargs = sanitized

        # ToolGate policy checks + param sanitation (applies to all tools).
        allowed, reason, sanitized_params = check_tool_execution(canonical, kwargs)
        if not allowed:
            return _finish(err(
                tool=canonical,
                request_id=request_id,
                idempotency_key=idempotency_key,
                timestamp=ts,
                error_code=ErrorCode.POLICY_BLOCKED,
                message=reason,
                details={"tool": canonical},
                retryable=False,
            ))
        kwargs = sanitized_params

        # Idempotency replay (write/delete tools only)
        if idempotency_key and tool_def is not None and tool_def.safety != ToolSafety.READ:
            record = get_record(canonical, str(idempotency_key))
            if record is not None:
                replay = dict(record.response)
                replay["request_id"] = request_id
                replay["timestamp"] = ts
                side_effects_obj = replay.get("side_effects")
                side_effects: dict[str, Any] = dict(side_effects_obj) if isinstance(side_effects_obj, dict) else {}
                side_effects.update(
                    {
                        "idempotency_replay": True,
                        "idempotency_stored_at": record.stored_at,
                    },
                )
                replay["side_effects"] = side_effects
                return _finish(replay)

        # Nervous System pre-dispatch check (circuit breakers, rate limits)
        ns_allowed, ns_reason = _nervous_system_check(canonical)
        if not ns_allowed:
            return _finish(err(
                tool=canonical,
                request_id=request_id,
                idempotency_key=idempotency_key,
                timestamp=ts,
                error_code=ErrorCode.POLICY_BLOCKED,
                message=ns_reason,
                details={"tool": canonical, "source": "nervous_system"},
                retryable=True,
            ))

        # Dispatch to handler.
        try:
            dispatch_kwargs = dict(kwargs)
            if dry_run:
                dispatch_kwargs["dry_run"] = True
            # Zig/StateBoard already validated circuit breaker, rate limit, maturity
            # — tell the middleware pipeline to skip redundant Python checks
            if ns_allowed:
                dispatch_kwargs["_zig_prevalidated"] = True
            raw = _dispatch_tool(canonical, **dispatch_kwargs)
        except ImportError as exc:
            out = err(
                tool=canonical,
                request_id=request_id,
                idempotency_key=idempotency_key,
                timestamp=ts,
                error_code=ErrorCode.MISSING_DEPENDENCY,
                message=str(exc),
                details={"tool": canonical},
                retryable=False,
            )
        except Exception as exc:
            out = err(
                tool=canonical,
                request_id=request_id,
                idempotency_key=idempotency_key,
                timestamp=ts,
                error_code=ErrorCode.INTERNAL_ERROR,
                message=str(exc),
                details={"tool": canonical, "traceback": traceback.format_exc() if os.getenv("WM_DEBUG") else None},
                retryable=False,
            )
        else:
            # Normalize into the stable envelope.
            out = normalize_raw(
                tool=canonical,
                request_id=request_id,
                idempotency_key=str(idempotency_key) if idempotency_key else None,
                timestamp=ts,
                raw=raw,
            )

        # Ensure write tools include an explicit writes hint.
        if tool_def is not None and tool_def.safety != ToolSafety.READ and not out.get("writes"):
            out["writes"] = _tool_writes_hint(canonical)

        # Persist idempotency result on success (write/delete only)
        if (
            idempotency_key
            and not dry_run
            and tool_def is not None
            and tool_def.safety != ToolSafety.READ
            and out.get("status") == "success"
        ):
            try:
                put_record(canonical, str(idempotency_key), out)
            except Exception:
                # Never fail a tool call due to idempotency persistence.
                pass

        # Nervous System post-dispatch sync
        _nervous_system_post(
            canonical,
            time.time() - call_started_at,
            out.get("status") in ("success", "ok"),
        )

        return _finish(out)

def smart_infer(query: str, mode: str = "auto", ground_in_memory: bool = False) -> dict:
    """Unified local inference with automatic tier selection.

    Wu Wei principle: Query finds its own path (fast/explore/deep).

    Args:
        query: Query string
        mode: auto/fast/explore/deep/memory_augmented
        ground_in_memory: Use memory for RAG-style context

    Returns:
        dict with answer, tier, confidence, latency_ms, tokens_saved

    """
    return {
        "status": "error",
        "error": "Local inference (edge_infer) has been archived. Please use an external model via MCP.",
    }



def inference_stats() -> dict:
    """Get unified inference statistics."""
    return {
        "status": "error",
        "error": "Local inference (edge_stats) has been archived.",
    }
