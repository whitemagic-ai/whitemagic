"""Pipeline tool handlers — declarative tool chaining.

Allows agents to define multi-step tool chains that execute sequentially,
with each step able to reference the output of previous steps via ``$prev``.
All state is stored under WM_STATE_ROOT/pipelines/.
"""
import json
import os
import re
from datetime import datetime
from pathlib import Path
from typing import Any, cast
from uuid import uuid4


def handle_pipeline(**kwargs: Any) -> dict[str, Any]:
    """Unified pipeline handler — routes by action parameter."""
    action = kwargs.get("action", "list")
    dispatch = {
        "create": handle_pipeline_create,
        "status": handle_pipeline_status,
        "list": handle_pipeline_list,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def _emit(event_type_name: str, data: dict[str, Any]) -> None:
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type_name, data, source="pipeline")
    except Exception:
        pass


def _pipelines_dir() -> Path:
    from whitemagic.config.paths import WM_ROOT
    d = WM_ROOT / "pipelines"
    d.mkdir(parents=True, exist_ok=True)
    return Path(d)


def _pipeline_path(pipeline_id: str) -> Path:
    return _pipelines_dir() / f"{pipeline_id}.json"


def _load_pipeline(pipeline_id: str) -> dict[str, Any] | None:
    p = _pipeline_path(pipeline_id)
    if not p.exists():
        return None
    try:
        data = json.loads(p.read_text(encoding="utf-8"))
        if isinstance(data, dict):
            return cast("dict[str, Any]", data)
        return None
    except (json.JSONDecodeError, OSError):
        return None


def _save_pipeline(pipeline: dict[str, Any]) -> None:
    p = _pipeline_path(pipeline["id"])
    p.write_text(json.dumps(pipeline, indent=2), encoding="utf-8")


# ---------------------------------------------------------------------------
# Variable substitution engine
# ---------------------------------------------------------------------------

def _resolve_vars(value: Any, results: list[dict[str, Any]]) -> Any:
    """Resolve $prev and $step[N] references in args.

    - ``$prev`` → the details dict of the immediately preceding step
    - ``$prev.key`` → a specific key from prev details
    - ``$step[0]`` → details of step 0
    - ``$step[0].key`` → specific key from step 0 details
    """
    if isinstance(value, str):
        # Full replacement: if the entire string is a reference, return the object
        m = re.fullmatch(r"\$prev(?:\.(\w+))?", value)
        if m:
            if not results:
                return value
            prev = results[-1].get("details", {})
            return prev.get(m.group(1), prev) if m.group(1) else prev

        m = re.fullmatch(r"\$step\[(\d+)\](?:\.(\w+))?", value)
        if m:
            idx = int(m.group(1))
            if idx < len(results):
                step_details = results[idx].get("details", {})
                return step_details.get(m.group(2), step_details) if m.group(2) else step_details
            return value

        # Inline substitution within a larger string
        def _sub(match: re.Match[str]) -> str:
            ref = match.group(0)
            m2 = re.match(r"\$prev\.(\w+)", ref)
            if m2 and results:
                return str(results[-1].get("details", {}).get(m2.group(1), ref))
            m2 = re.match(r"\$step\[(\d+)\]\.(\w+)", ref)
            if m2:
                idx = int(m2.group(1))
                if idx < len(results):
                    return str(results[idx].get("details", {}).get(m2.group(2), ref))
            return ref

        return re.sub(r"\$(?:prev\.\w+|step\[\d+\]\.\w+)", _sub, value)

    if isinstance(value, dict):
        return {k: _resolve_vars(v, results) for k, v in value.items()}
    if isinstance(value, list):
        return [_resolve_vars(v, results) for v in value]
    return value


# ---------------------------------------------------------------------------
# Dependency Graph ↔ Pipeline validation (A2 Synthesis)
# ---------------------------------------------------------------------------

def _validate_pipeline_deps(steps: list[dict[str, Any]]) -> dict[str, Any]:
    """Validate pipeline step ordering against the Tool Dependency Graph.
    Returns warnings for ordering violations and missing prerequisites.
    """
    try:
        from whitemagic.tools.dependency_graph import get_tool_graph
        graph = get_tool_graph()
    except Exception:
        return {"valid": True, "note": "Dependency graph unavailable — skipping validation"}

    tool_names = [s.get("tool", "") for s in steps]
    tool_positions = {name: i for i, name in enumerate(tool_names)}
    warnings: list[str] = []
    suggestions: list[str] = []

    for i, tool_name in enumerate(tool_names):
        if not tool_name:
            continue

        # Check hard prerequisites ("requires" edges)
        prereqs = graph.prerequisites(tool_name)
        if prereqs:
            for edge in prereqs:
                if edge.get("type") == "requires":
                    prereq_tool = edge.get("source", "")
                    if prereq_tool not in tool_positions:
                        warnings.append(
                            f"Step {i} ({tool_name}) requires '{prereq_tool}' "
                            f"which is not in the pipeline",
                        )
                    elif tool_positions[prereq_tool] >= i:
                        warnings.append(
                            f"Step {i} ({tool_name}) requires '{prereq_tool}' "
                            f"but it appears at step {tool_positions[prereq_tool]} (after)",
                        )

    # Check if the last tool has strong suggested follow-ups not in pipeline
    if tool_names:
        last_tool = tool_names[-1]
        next_steps = graph.next_steps(last_tool)
        if next_steps:
            for edge in next_steps:
                et = edge.get("type", "")
                w = edge.get("weight", 0)
                target = edge.get("target", "")
                if et == "suggests" and w >= 0.7 and target not in tool_positions:
                    suggestions.append(
                        f"After '{last_tool}', consider adding '{target}' "
                        f"(affinity: {w:.0%})",
                    )

    return {
        "valid": len(warnings) == 0,
        "warnings": warnings,
        "suggestions": suggestions[:3],
    }


# ---------------------------------------------------------------------------
# Handler: pipeline.create
# ---------------------------------------------------------------------------

def handle_pipeline_create(**kwargs: Any) -> dict[str, Any]:
    """Create and optionally execute a tool pipeline."""
    steps = kwargs.get("steps")
    if not steps or not isinstance(steps, list):
        return {"status": "error", "error": "steps is required (array of {tool, args})"}

    name = kwargs.get("name", "")
    execute = kwargs.get("execute", True)

    pipeline_id = f"pipe-{uuid4().hex[:8]}"
    now = datetime.now().isoformat()

    pipeline = {
        "id": pipeline_id,
        "name": name or pipeline_id,
        "steps": steps,
        "status": "created",
        "created_at": now,
        "created_by": os.uname().nodename,
        "results": [],
        "current_step": 0,
    }

    # --- Dependency Graph validation (A2 synthesis) ---
    validation = _validate_pipeline_deps(steps)

    if not execute:
        pipeline["status"] = "pending"
        pipeline["validation"] = validation
        _save_pipeline(pipeline)
        return {
            "status": "success",
            "message": f"Pipeline {pipeline_id} created (not executed)",
            "pipeline_id": pipeline_id,
            "step_count": len(steps),
            "validation": validation,
        }

    # Execute the pipeline
    result = _execute_pipeline(pipeline)
    result["validation"] = validation
    return result


def _execute_pipeline(pipeline: dict[str, Any]) -> dict[str, Any]:
    """Execute a pipeline step by step."""
    from whitemagic.tools.unified_api import call_tool

    pipeline["status"] = "running"
    pipeline["started_at"] = datetime.now().isoformat()
    _save_pipeline(pipeline)

    steps = pipeline["steps"]
    results = pipeline.get("results", [])
    start_step = pipeline.get("current_step", 0)

    for i in range(start_step, len(steps)):
        step = steps[i]
        tool_name = step.get("tool")
        if not tool_name:
            pipeline["status"] = "failed"
            pipeline["error"] = f"Step {i}: missing 'tool' field"
            _save_pipeline(pipeline)
            return {
                "status": "error",
                "error": f"Step {i} missing 'tool' field",
                "pipeline_id": pipeline["id"],
                "completed_steps": i,
            }

        # Resolve variable references
        raw_args = step.get("args", {})
        resolved_args = _resolve_vars(raw_args, results)
        if not isinstance(resolved_args, dict):
            resolved_args = {}

        # Execute the tool
        try:
            result = call_tool(tool_name, **resolved_args)
        except Exception as exc:
            result = {"status": "error", "error": str(exc)}

        results.append(result)
        pipeline["results"] = results
        pipeline["current_step"] = i + 1
        _save_pipeline(pipeline)

        # Check for failure — stop pipeline on error unless step says continue_on_error
        if result.get("status") == "error" and not step.get("continue_on_error", False):
            pipeline["status"] = "failed"
            pipeline["failed_at_step"] = i
            pipeline["completed_at"] = datetime.now().isoformat()
            _save_pipeline(pipeline)
            return {
                "status": "error",
                "error": f"Pipeline failed at step {i} ({tool_name}): {result.get('message', '')}",
                "pipeline_id": pipeline["id"],
                "completed_steps": i,
                "total_steps": len(steps),
                "results": results,
            }

    pipeline["status"] = "completed"
    pipeline["completed_at"] = datetime.now().isoformat()
    _save_pipeline(pipeline)

    return {
        "status": "success",
        "message": f"Pipeline {pipeline['id']} completed ({len(steps)} steps)",
        "pipeline_id": pipeline["id"],
        "completed_steps": len(steps),
        "total_steps": len(steps),
        "results": results,
    }


# ---------------------------------------------------------------------------
# Handler: pipeline.status
# ---------------------------------------------------------------------------

def handle_pipeline_status(**kwargs: Any) -> dict[str, Any]:
    """Get the status of a pipeline."""
    pipeline_id = kwargs.get("pipeline_id")
    if not pipeline_id:
        return {"status": "error", "error": "pipeline_id is required"}

    pipeline = _load_pipeline(pipeline_id)
    if not pipeline:
        return {"status": "error", "error": f"Pipeline {pipeline_id} not found", "error_code": "not_found"}

    return {
        "status": "success",
        "pipeline": {
            "id": pipeline["id"],
            "name": pipeline.get("name"),
            "pipeline_status": pipeline.get("status"),
            "current_step": pipeline.get("current_step", 0),
            "total_steps": len(pipeline.get("steps", [])),
            "created_at": pipeline.get("created_at"),
            "completed_at": pipeline.get("completed_at"),
        },
        "results": pipeline.get("results", []),
    }


# ---------------------------------------------------------------------------
# Handler: pipeline.list
# ---------------------------------------------------------------------------

def handle_pipeline_list(**kwargs: Any) -> dict[str, Any]:
    """List pipelines."""
    limit = kwargs.get("limit", 20)
    status_filter = kwargs.get("filter_status")

    pdir = _pipelines_dir()
    pipelines = []
    for f in sorted(pdir.glob("*.json"), reverse=True):
        try:
            p = json.loads(f.read_text(encoding="utf-8"))
            if status_filter and p.get("status") != status_filter:
                continue
            pipelines.append({
                "id": p["id"],
                "name": p.get("name", ""),
                "status": p.get("status"),
                "steps": len(p.get("steps", [])),
                "current_step": p.get("current_step", 0),
                "created_at": p.get("created_at"),
            })
        except (json.JSONDecodeError, OSError, KeyError):
            continue
        if len(pipelines) >= limit:
            break

    return {
        "status": "success",
        "count": len(pipelines),
        "pipelines": pipelines,
    }
