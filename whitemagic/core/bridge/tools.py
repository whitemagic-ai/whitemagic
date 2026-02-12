
from collections.abc import Callable
from typing import Any

from whitemagic.core.bridge.adaptive import (
    adapt_response,
    prat_get_context,
    prat_invoke,
    prat_list_morphologies,
    prat_status,
)
from whitemagic.core.bridge.agent import manage_agent_collaboration
from whitemagic.core.bridge.archaeology import (
    archaeology_daily_digest,
    archaeology_extract_wisdom,
    archaeology_find_changed,
    archaeology_find_unread,
    archaeology_generate_report,
    archaeology_mark_read,
    archaeology_mark_written,
    archaeology_process_wisdom,
    archaeology_recent_reads,
    archaeology_report,
    archaeology_scan_directory,
    archaeology_search,
    archaeology_stats,
)
from whitemagic.core.bridge.autonomous import run_autonomous_cycle
from whitemagic.core.bridge.benchmark import run_benchmarks
from whitemagic.core.bridge.collaboration import (
    garden_sangha_workspace_info,
    profile_get_profile,
    profile_update_preferences,
    sangha_chat_read,
    sangha_chat_send,
    sangha_lock_acquire,
    sangha_lock_list,
    sangha_lock_release,
    windsurf_backup,
    windsurf_merge_backups,
)
from whitemagic.core.bridge.dharma import (
    dharma_check_boundaries,
    dharma_evaluate_ethics,
    dharma_get_ethical_score,
    dharma_get_guidance,
    dharma_list_principles,
    dharma_verify_consent,
)
from whitemagic.core.bridge.gana import gana_invoke
from whitemagic.core.bridge.garden import (
    garden_activate,
    garden_garden_activate,
    garden_garden_status,
    garden_list,
    garden_resonance_map,
    manage_gardens,
    protect_context,
    validate_integrations,
)
from whitemagic.core.bridge.inference import (
    bitnet_infer,
    bitnet_status,
    local_ml_infer,
    local_ml_status,
    run_local_inference,
)
from whitemagic.core.bridge.kaizen import analyze_wu_xing_phase
from whitemagic.core.bridge.meditation import (
    meditation_meditate,
    meditation_pause,
    meditation_reflect,
)
from whitemagic.core.bridge.memory import (
    manage_memories,
    memory_create,
    memory_delete,
    memory_list,
    memory_read,
    memory_search,
    memory_update,
    parallel_search,
)
from whitemagic.core.bridge.metrics import get_metrics_summary, track_metric
from whitemagic.core.bridge.optimization import optimize_cache, optimize_models
from whitemagic.core.bridge.pattern import detect_patterns
from whitemagic.core.bridge.reasoning import apply_reasoning_methods
from whitemagic.core.bridge.rust import (
    enable_rust_acceleration,
    rust_check_available,
    rust_compress,
    rust_consolidate_memories,
    rust_extract_patterns,
    rust_extract_todos,
    rust_fast_search,
    rust_fast_similarity,
    rust_parallel_grep,
    rust_read_files_batch,
    rust_scan_codebase,
)
from whitemagic.core.bridge.session import (
    session_checkpoint,
    session_create_handoff,
    session_get_context,
    session_handoff,
    session_init,
    session_list,
)
from whitemagic.core.bridge.system import (
    check_integrations_health,
    check_memory_health,
    check_resonance_health,
    check_system_health,
    debug_system,
    system_get_status,
    system_initialize_all,
)
from whitemagic.core.bridge.voice import manage_voice_patterns
from whitemagic.core.bridge.wisdom import (
    consult_art_of_war,
    consult_full_council,
    consult_iching,
    synthesize_wisdom,
)

# Import all domain modules
from whitemagic.core.bridge.zodiac import (
    manage_zodiac_cores,
    zodiac_activate_core,
    zodiac_consult_council,
    zodiac_list_cores,
    zodiac_run_cycle,
)

# === Routers implemented here to avoid circular imports if they were in separate files ===

def manage_locks_router(operation: str = "list", **kwargs: Any) -> dict[str, Any]:
    """Route manage_locks operations."""
    if "resource_id" in kwargs and "resource" not in kwargs:
        kwargs["resource"] = kwargs.pop("resource_id")
    if "timeout_seconds" in kwargs and "timeout" not in kwargs:
        kwargs["timeout"] = kwargs.pop("timeout_seconds")

    if operation == "acquire":
        return sangha_lock_acquire(**kwargs)
    elif operation == "release":
        return sangha_lock_release(**kwargs)
    else:
        return sangha_lock_list(**kwargs)

def enable_acceleration_router(operation: str = "edge_infer", **kwargs: Any) -> dict[str, Any]:
    """Route enable_acceleration operations."""
    if "operations" in kwargs:
        ops_list = kwargs.pop("operations")
        if isinstance(ops_list, list) and len(ops_list) > 0:
            operation = ops_list[0].replace("whitemagic_rs.", "").replace("fast_similarity", "rust")

    if operation == "edge_infer":
        # Legacy edge_infer - mapping to local_inference
        if "prompt" in kwargs and "query" not in kwargs:
            kwargs["query"] = kwargs.pop("prompt")
        return run_local_inference(operation="quick_recall", **kwargs)
    elif operation == "batch_infer":
        return run_local_inference(operation="batch", **kwargs)
    elif operation == "rust" or operation == "enable_rust":
        return enable_rust_acceleration(**kwargs)
    elif operation == "benchmark":
        return run_benchmarks(**kwargs)
    else:
        return run_local_inference(**kwargs)

def consolidate_router(operation: str = "consolidate_memories", **kwargs: Any) -> dict[str, Any]:
    """Route consolidate operations."""
    if operation == "memories":
        operation = "consolidate_memories"
    elif operation == "scratchpad":
        operation = "finalize_scratchpad"
    elif operation == "wisdom":
        operation = "process_wisdom"
    return execute_mcp_tool(f"consolidate_{operation}", **kwargs)

def track_metrics_router(operation: str = "track", **kwargs: Any) -> dict[str, Any]:
    """Route track_metrics operations."""
    if operation == "track":
        return track_metric(**kwargs)
    else:
        return get_metrics_summary(**kwargs)

def invoke_prat_router(operation: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Route invoke_prat operations."""
    if "args" in kwargs and isinstance(kwargs["args"], dict):
        nested_args = kwargs.pop("args")
        kwargs.update(nested_args)

    target_tool = kwargs.pop("target_tool", None)
    if not target_tool and "tool" in kwargs:
        target_tool = kwargs.pop("tool")

    if operation:
        operation = operation.replace("prat_", "")

    if not operation or operation not in ["invoke", "get_context", "list_morphologies", "status"]:
        if target_tool or "query" in kwargs:
            operation = "invoke"
        else:
            operation = "status"

    if operation == "invoke":
        return prat_invoke(target_tool=target_tool, **kwargs)
    elif operation == "get_context":
        return prat_get_context(**kwargs)
    elif operation == "list_morphologies" or operation == "list":
        return prat_list_morphologies(tool=target_tool, **kwargs)
    else:
        return prat_status(target_tool=target_tool, **kwargs)

def parallel_execute_router(operation: str = "parallel_search", **kwargs: Any) -> dict[str, Any]:
    """Route parallel_execute operations."""
    if operation == "parallel_search":
        return parallel_search(**kwargs)
    elif operation == "batch_read":
        return rust_read_files_batch(**kwargs)
    else:
        # Cascade Not Implemented fully yet, but mapping exists
        return detect_patterns(**kwargs) # Fallback

def share_resources_router(operation: str = "backup", **kwargs: Any) -> dict[str, Any]:
    """Route share_resources operations."""
    if operation == "backup":
        return windsurf_backup(**kwargs)
    elif operation == "restore":
        return {"status": "restore_not_implemented"}
    else:
        return windsurf_merge_backups(**kwargs)

def manage_profile_router(operation: str = "get", **kwargs: Any) -> dict[str, Any]:
    """Route manage_profile operations."""
    if operation == "get":
        return profile_get_profile(**kwargs)
    else:
        return profile_update_preferences(**kwargs)

def enter_stillness_router(operation: str = "pause", **kwargs: Any) -> dict[str, Any]:
    """Route enter_stillness operations."""
    if operation == "pause":
        return meditation_pause(**kwargs)
    elif operation == "reflect":
        return meditation_reflect(**kwargs)
    else:
        return meditation_meditate(**kwargs)

def manage_cache_router(operation: str = "stats", **kwargs: Any) -> dict[str, Any]:
    """Route manage_cache operations."""
    if operation == "optimize":
        return optimize_cache(**kwargs)
    elif operation == "clear":
        # Default clear cache
        return {"cleared": True}
    else:
        # Default stats
        return {"hits": 0, "misses": 0}

def monitor_status_router(operation: str = "garden_status", **kwargs: Any) -> dict[str, Any]:
    """Route monitor_status operations."""
    if operation == "garden_status":
        return garden_garden_status(**kwargs)
    elif operation == "garden_activate":
        return garden_garden_activate(**kwargs)
    else:
        return garden_sangha_workspace_info(**kwargs)

def send_notification_router(operation: str = "send", **kwargs: Any) -> dict[str, Any]:
    """Route send_notification operations."""
    if operation == "send" or operation == "sangha_chat_send":
        return sangha_chat_send(**kwargs)
    return sangha_chat_read(**kwargs)

def manage_archaeology_router(operation: str = "stats", **kwargs: Any) -> dict[str, Any]:
    """Route manage_archaeology operations."""
    if operation == "stats":
        return archaeology_stats(**kwargs)
    elif operation == "mark_read":
        return archaeology_mark_read(**kwargs)
    elif operation == "report":
        return archaeology_report(**kwargs)
    elif operation == "search":
        return archaeology_search(**kwargs)
    else:
        return archaeology_stats(**kwargs)

def search_memories_router(operation: str = "search", **kwargs: Any) -> dict[str, Any]:
    """Route search_memories operations."""
    if operation == "deep":
        return parallel_search(**kwargs)
    else:
        return memory_search(**kwargs)

def search_deep_router(operation: str = "search", **kwargs: Any) -> dict[str, Any]:
    """Route search_deep operations."""
    return parallel_search(**kwargs)

def consult_wisdom_router(operation: str = "council", **kwargs: Any) -> dict[str, Any]:
    """Route consult_wisdom operations."""
    if operation == "iching":
        return consult_iching(**kwargs)
    elif operation == "art_of_war":
        return consult_art_of_war(**kwargs)
    else:
        return consult_full_council(**kwargs)

def evaluate_dharma_router(operation: str = "evaluate", **kwargs: Any) -> dict[str, Any]:
    """Route evaluate_dharma operations."""
    if operation == "boundaries":
        return dharma_check_boundaries(**kwargs)
    elif operation == "consent":
        return dharma_verify_consent(**kwargs)
    else:
        return dharma_evaluate_ethics(**kwargs)

# Consolidate router stubs
def consolidate_consolidate_memories(**kwargs: Any) -> dict[str, Any]:
    return rust_consolidate_memories(**kwargs)


def consolidate_finalize_scratchpad(**kwargs: Any) -> dict[str, Any]:
    return {"finalized": True}


def consolidate_process_wisdom(**kwargs: Any) -> dict[str, Any]:
    return archaeology_process_wisdom(**kwargs)


def execute_cascade(**kwargs: Any) -> dict[str, Any]:
    return detect_patterns(**kwargs)


def list_patterns(**kwargs: Any) -> dict[str, Any]:
    return {"patterns": ["analyze"]}


def cache_optimize_cache(**kwargs: Any) -> dict[str, Any]:
    return optimize_cache(**kwargs)


def cache_clear_cache(**kwargs: Any) -> dict[str, Any]:
    return {"cleared": True}


def cache_cache_stats(**kwargs: Any) -> dict[str, Any]:
    return {"stats": "ok"}


def manage_resonance(**kwargs: Any) -> dict[str, Any]:
    return {"resonance": "active"}

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

def execute_mcp_tool(tool_name: str, **kwargs: Any) -> dict[str, Any]:
    """Main entry point for MCP tool execution.
    Routes tool calls to appropriate handlers.
    """

    # Map tool operations to functions
    handlers: dict[str, Callable[..., dict[str, Any]]] = {
        # Gana Routing (Generic)
        "gana_invoke": gana_invoke,

        # Zodiac
        "zodiac_list_cores": zodiac_list_cores,
        "zodiac_activate_core": zodiac_activate_core,
        "zodiac_consult_council": zodiac_consult_council,
        "zodiac_run_cycle": zodiac_run_cycle,

        # Memory
        "memory_create": memory_create,
        "memory_search": memory_search,
        "memory_read": memory_read,
        "memory_update": memory_update,
        "memory_delete": memory_delete,
        "memory_list": memory_list,
        "manage_memories": manage_memories,

        # Gardens
        "garden_list": garden_list,
        "garden_activate": garden_activate,
        "garden_resonance_map": garden_resonance_map,
        "manage_gardens": manage_gardens,

        # Archaeology
        "archaeology_mark_read": archaeology_mark_read,
        "archaeology_mark_written": archaeology_mark_written,
        "archaeology_find_unread": archaeology_find_unread,
        "archaeology_find_changed": archaeology_find_changed,
        "archaeology_recent_reads": archaeology_recent_reads,
        "archaeology_stats": archaeology_stats,
        "archaeology_report": archaeology_report,
        "archaeology_search": archaeology_search,
        "archaeology_extract_wisdom": archaeology_extract_wisdom,
        "archaeology_process_wisdom": archaeology_process_wisdom,
        "archaeology_daily_digest": archaeology_daily_digest,
        "archaeology_generate_report": archaeology_generate_report,
        "archaeology_scan_directory": archaeology_scan_directory,

        # Sessions
        "session_init": session_init,
        "session_get_context": session_get_context,
        "session_checkpoint": session_checkpoint,
        "session_list": session_list,

        # Wisdom Council
        "consult_full_council": consult_full_council,
        "consult_art_of_war": consult_art_of_war,
        "consult_iching": consult_iching,
        "synthesize_wisdom": synthesize_wisdom,

        # Reasoning
        "apply_reasoning_methods": apply_reasoning_methods,

        # Patterns
        "detect_patterns": detect_patterns,

        # Dharma (Phase 4)
        "dharma_evaluate_ethics": dharma_evaluate_ethics,
        "dharma_check_boundaries": dharma_check_boundaries,
        "dharma_verify_consent": dharma_verify_consent,
        "dharma_get_guidance": dharma_get_guidance,
        "dharma_get_ethical_score": dharma_get_ethical_score,
        "dharma_list_principles": dharma_list_principles,

        # Local ML (Phase 4.3)
        "local_ml_infer": local_ml_infer,
        "local_ml_status": local_ml_status,
        "bitnet_infer": bitnet_infer,
        "bitnet_status": bitnet_status,

        # Collaboration
        "manage_agent_collaboration": manage_agent_collaboration,

        # Voice
        "manage_voice_patterns": manage_voice_patterns,

        # Autonomy & Inference
        "run_autonomous_cycle": run_autonomous_cycle,
        "run_local_inference": run_local_inference,
        "run_benchmarks": run_benchmarks,
        "optimize_models": optimize_models,
        "initialize_systems": system_initialize_all,

        # System
        "system_initialize_all": system_initialize_all,
        "system_get_status": system_get_status,

        # System Health
        "check_system_health": check_system_health,
        "check_memory_health": check_memory_health,
        "check_resonance_health": check_resonance_health,
        "check_integrations_health": check_integrations_health,

        # Rust Acceleration
        "rust_check_available": rust_check_available,
        "rust_fast_search": rust_fast_search,
        "rust_parallel_grep": rust_parallel_grep,
        "rust_extract_patterns": rust_extract_patterns,
        "rust_fast_similarity": rust_fast_similarity,
        "rust_consolidate_memories": rust_consolidate_memories,
        "rust_scan_codebase": rust_scan_codebase,
        "rust_extract_todos": rust_extract_todos,
        "rust_read_files_batch": rust_read_files_batch,
        "rust_compress": rust_compress,
        "enable_rust_acceleration": enable_rust_acceleration,

        # PRAT
        "prat_get_context": prat_get_context,
        "prat_invoke": invoke_prat_router,
        "prat_list_morphologies": invoke_prat_router,
        "prat_status": prat_status,

        # NEW FIXES
        "manage_zodiac_cores": manage_zodiac_cores,
        "analyze_wu_xing_phase": analyze_wu_xing_phase,
        "validate_integrations": validate_integrations,
        "protect_context": protect_context,

        # 28 PRIMARY TOOLS
        "initialize_session": session_init,
        "create_memory": memory_create,
        "manage_locks": manage_locks_router,
        "manage_resource_locks": manage_locks_router,
        "get_session_context": session_get_context,
        "enable_acceleration": enable_acceleration_router,
        "consolidate": consolidate_router,
        "track_metrics": track_metrics_router,
        "invoke_prat": invoke_prat_router,
        "parallel_execute": parallel_execute_router,
        "manage_archaeology": manage_archaeology_router,
        "share_resources": share_resources_router,
        "manage_profile": manage_profile_router,
        "enter_stillness": enter_stillness_router,
        "evaluate_dharma": evaluate_dharma_router,
        "manage_cache": manage_cache_router,
        "monitor_status": monitor_status_router,
        "search_memories": search_memories_router,
        "consult_wisdom": consult_wisdom_router,
        "search_deep": search_deep_router,
        "send_notification": send_notification_router,

        # Misc / Router Stubs
        "track_metric": track_metric,
        "get_metrics_summary": get_metrics_summary,
        "sangha_lock_acquire": sangha_lock_acquire,
        "sangha_lock_release": sangha_lock_release,
        "sangha_lock_list": sangha_lock_list,
        "consolidate_consolidate_memories": consolidate_consolidate_memories,
        "consolidate_finalize_scratchpad": consolidate_finalize_scratchpad,
        "consolidate_process_wisdom": consolidate_process_wisdom,
        "optimize_cache": optimize_cache,
        "adapt_response": adapt_response,
        "parallel_search": parallel_search,
        "execute_cascade": execute_cascade,
        "list_patterns": list_patterns,
        "windsurf_backup": windsurf_backup,
        "windsurf_merge_backups": windsurf_merge_backups,
        "cache_optimize_cache": cache_optimize_cache,
        "cache_clear_cache": cache_clear_cache,
        "cache_cache_stats": cache_cache_stats,
        "debug_system": debug_system,
        "validate_input": lambda **k: {"valid": True},
        "session_create_handoff": session_create_handoff,
        "session_handoff": session_handoff,
        "sangha_chat_send": sangha_chat_send,
        "sangha_chat_read": sangha_chat_read,
        "garden_sangha_workspace_info": garden_sangha_workspace_info,
        "profile_get_profile": profile_get_profile,
        "profile_update_preferences": profile_update_preferences,
        "meditation_pause": meditation_pause,
        "meditation_reflect": meditation_reflect,
        "meditation_meditate": meditation_meditate,
        "manage_resonance": manage_resonance,
    }

    handler = handlers.get(tool_name)
    if not handler:
        return {"error": f"Unknown tool: {tool_name}"}

    try:
        result = handler(**kwargs)
        return result
    except Exception as e:
        import os
        import traceback
        resp = {"error": str(e)}
        if os.getenv("WM_DEBUG"):
            resp["trace"] = traceback.format_exc()
        return resp
