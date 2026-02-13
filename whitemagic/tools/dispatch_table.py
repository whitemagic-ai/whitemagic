import importlib
from collections.abc import Callable
from typing import Any, cast


class LazyHandler:
    """Lazy-loads a tool handler function from its module."""

    def __init__(self, module_name: str, function_name: str):
        self.module_name = module_name
        self.function_name = function_name
        self._cached_func: Callable | None = None

    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        if self._cached_func is None:
            mod = importlib.import_module(f"whitemagic.tools.handlers.{self.module_name}")
            self._cached_func = getattr(mod, self.function_name)
        return self._cached_func(*args, **kwargs)


class LazyHandlerAbs:
    """Lazy-loads a handler from an absolute module path."""

    def __init__(self, module_path: str, function_name: str):
        self.module_path = module_path
        self.function_name = function_name
        self._cached_func: Callable | None = None

    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        if self._cached_func is None:
            mod = importlib.import_module(self.module_path)
            self._cached_func = getattr(mod, self.function_name)
        return self._cached_func(*args, **kwargs)


# ---------------------------------------------------------------------------
# DISPATCH_TABLE: tool_name -> handler function (lazily loaded)
# ---------------------------------------------------------------------------
DISPATCH_TABLE: dict[str, Callable[..., dict[str, Any]]] = {
    # --- Yin-Yang Balance & Harmony Vector ---
    "record_yin_yang_activity": LazyHandler("balance", "handle_record_yin_yang_activity"),
    "get_yin_yang_balance": LazyHandler("balance", "handle_get_yin_yang_balance"),
    "harmony_vector": LazyHandler("balance", "handle_harmony_vector"),

    # --- Dharma ---
    "evaluate_ethics": LazyHandler("dharma", "handle_evaluate_ethics"),
    "check_boundaries": LazyHandler("dharma", "handle_check_boundaries"),
    "verify_consent": LazyHandler("dharma", "handle_verify_consent"),
    "get_ethical_score": LazyHandler("dharma", "handle_get_ethical_score"),
    "get_dharma_guidance": LazyHandler("dharma", "handle_get_dharma_guidance"),
    "karma_report": LazyHandler("dharma", "handle_karma_report"),
    "karmic_trace": LazyHandler("dharma", "handle_karmic_trace"),
    "karma.verify_chain": LazyHandler("dharma", "handle_karma_verify_chain"),
    "karma.anchor": LazyHandler("dharma", "handle_karma_anchor"),
    "karma.verify_anchor": LazyHandler("dharma", "handle_karma_verify_anchor"),
    "karma.anchor_status": LazyHandler("dharma", "handle_karma_anchor_status"),
    "dharma_rules": LazyHandler("dharma", "handle_dharma_rules"),
    "set_dharma_profile": LazyHandler("dharma", "handle_set_dharma_profile"),

    # --- Agent Capabilities ---
    "get_agent_capabilities": LazyHandler("misc", "handle_get_agent_capabilities"),

    # --- Session ---
    "session_bootstrap": LazyHandler("session", "handle_session_bootstrap"),
    "session_status": LazyHandler("session", "handle_session_status"),
    "session_handoff": LazyHandler("session", "handle_session_handoff"),
    "create_session": LazyHandler("session", "handle_create_session"),
    "checkpoint_session": LazyHandler("session", "handle_checkpoint_session"),
    "resume_session": LazyHandler("session", "handle_resume_session"),

    # --- Garden ---
    "garden_activate": LazyHandler("garden", "handle_garden_activate"),
    "garden_status": LazyHandler("garden", "handle_garden_status"),
    "garden_synergy": LazyHandler("garden", "handle_garden_synergy"),
    "garden_health": LazyHandler("garden", "handle_garden_health"),

    # --- Memory ---
    "create_memory": LazyHandler("memory", "handle_create_memory"),
    "fast_read_memory": LazyHandler("memory", "handle_fast_read_memory"),
    "batch_read_memories": LazyHandler("memory", "handle_batch_read_memories"),
    "search_memories": LazyHandler("memory", "handle_search_memories"),

    # --- Export/Import ---
    "export_memories": LazyHandler("export_import", "handle_export_memories"),
    "import_memories": LazyHandler("export_import", "handle_import_memories"),

    # --- Immune (stubs) ---
    "immune_scan": LazyHandler("misc", "handle_immune_scan"),
    "immune_heal": LazyHandler("misc", "handle_immune_heal"),

    # --- Symbolic / Oracle ---
    "cast_oracle": LazyHandler("misc", "handle_cast_oracle"),
    "wu_xing_balance": LazyHandler("misc", "handle_wu_xing_balance"),

    # --- Intelligence ---
    "thought_clone": LazyHandler("misc", "handle_thought_clone"),
    "coherence_boost": LazyHandler("misc", "handle_coherence_boost"),
    "anti_loop_check": LazyHandler("misc", "handle_anti_loop_check"),
    "token_report": LazyHandler("misc", "handle_token_report"),
    "solve_optimization": LazyHandler("solver", "handle_solve_optimization"),

    # --- SIMD Acceleration ---
    "simd.cosine": LazyHandler("simd", "handle_simd_cosine"),
    "simd.batch": LazyHandler("simd", "handle_simd_batch"),
    "simd.status": LazyHandler("simd", "handle_simd_status"),

    # --- Knowledge Graph ---
    "kg.extract": LazyHandler("knowledge_graph", "handle_kg_extract"),
    "kg.query": LazyHandler("knowledge_graph", "handle_kg_query"),
    "kg.top": LazyHandler("knowledge_graph", "handle_kg_top"),
    "kg.status": LazyHandler("knowledge_graph", "handle_kg_status"),

    # --- Tool Sandboxing ---
    "sandbox.status": LazyHandler("sandbox", "handle_sandbox_status"),
    "sandbox.violations": LazyHandler("sandbox", "handle_sandbox_violations"),
    "sandbox.set_limits": LazyHandler("sandbox", "handle_sandbox_set_limits"),

    # --- Agent Swarm Protocols ---
    "swarm.decompose": LazyHandler("swarm", "handle_swarm_decompose"),
    "swarm.route": LazyHandler("swarm", "handle_swarm_route"),
    "swarm.complete": LazyHandler("swarm", "handle_swarm_complete"),
    "swarm.vote": LazyHandler("swarm", "handle_swarm_vote"),
    "swarm.resolve": LazyHandler("swarm", "handle_swarm_resolve"),
    "swarm.plan": LazyHandler("swarm", "handle_swarm_plan"),
    "swarm.status": LazyHandler("swarm", "handle_swarm_status"),

    # --- Vector Memory Search ---
    "vector.search": LazyHandler("vector_search", "handle_vector_search"),
    "vector.index": LazyHandler("vector_search", "handle_vector_index"),
    "vector.status": LazyHandler("vector_search", "handle_vector_status"),

    # --- Cross-Session Learning ---
    "learning.patterns": LazyHandler("learning", "handle_learning_patterns"),
    "learning.suggest": LazyHandler("learning", "handle_learning_suggest"),
    "learning.status": LazyHandler("learning", "handle_learning_status"),

    # --- Anomaly Detection (unified) ---
    "anomaly": LazyHandler("anomaly", "handle_anomaly"),
    # Backward-compat aliases
    "anomaly.check": LazyHandler("anomaly", "handle_anomaly_check"),
    "anomaly.history": LazyHandler("anomaly", "handle_anomaly_history"),
    "anomaly.status": LazyHandler("anomaly", "handle_anomaly_status"),

    # --- OpenTelemetry (unified) ---
    "otel": LazyHandler("otel", "handle_otel"),
    # Backward-compat aliases
    "otel.spans": LazyHandler("otel", "handle_otel_spans"),
    "otel.metrics": LazyHandler("otel", "handle_otel_metrics"),
    "otel.status": LazyHandler("otel", "handle_otel_status"),

    # --- Context Optimizer ---
    "context.pack": LazyHandler("context_optimizer", "handle_context_pack"),
    "context.status": LazyHandler("context_optimizer", "handle_context_status"),

    # --- Prompt Templates ---
    "prompt.render": LazyHandler("prompts", "handle_prompt_render"),
    "prompt.list": LazyHandler("prompts", "handle_prompt_list"),
    "prompt.reload": LazyHandler("prompts", "handle_prompt_reload"),

    # --- Dream Cycle (unified) ---
    "dream": LazyHandler("dreaming", "handle_dream"),
    # Backward-compat aliases
    "dream_start": LazyHandler("dreaming", "handle_dream_start"),
    "dream_stop": LazyHandler("dreaming", "handle_dream_stop"),
    "dream_status": LazyHandler("dreaming", "handle_dream_status"),
    "dream_now": LazyHandler("dreaming", "handle_dream_now"),

    # --- BitNet Inference ---
    "bitnet_infer": LazyHandler("bitnet", "handle_bitnet_infer"),
    "bitnet_status": LazyHandler("bitnet", "handle_bitnet_status"),

    # --- Edge Inference ---
    "edge_infer": LazyHandler("edge", "handle_edge_infer"),
    "edge_add_rule": LazyHandler("edge", "handle_edge_add_rule"),
    "edge_batch_infer": LazyHandler("edge", "handle_edge_batch_infer"),
    "edge_stats": LazyHandler("edge", "handle_edge_stats"),

    # --- Grimoire ---
    "grimoire_list": LazyHandler("misc", "handle_grimoire_list"),
    "grimoire_suggest": LazyHandler("grimoire", "handle_grimoire_suggest"),
    "grimoire_cast": LazyHandler("grimoire", "handle_grimoire_cast"),
    "grimoire_recommend": LazyHandler("grimoire", "handle_grimoire_recommend"),
    "grimoire_auto_status": LazyHandler("grimoire", "handle_grimoire_auto_status"),
    "grimoire_walkthrough": LazyHandler("grimoire_walkthrough", "handle_grimoire_walkthrough"),
    "grimoire_read": LazyHandler("misc", "handle_grimoire_read"),

    # --- Gan Ying ---
    "ganying_emit": LazyHandler("ganying", "handle_ganying_emit"),
    "ganying_history": LazyHandler("ganying", "handle_ganying_history"),
    "ganying_listeners": LazyHandler("ganying", "handle_ganying_listeners"),
    "resonance_trace": LazyHandler("ganying", "handle_resonance_trace"),

    # --- Utility ---
    "focus_session": LazyHandler("misc", "handle_focus_session"),
    "capability_harness": LazyHandler("misc", "handle_capability_harness"),

    # --- Scratchpad (unified) ---
    "scratchpad": LazyHandler("scratchpad", "handle_scratchpad"),
    # Backward-compat aliases
    "scratchpad_create": LazyHandler("scratchpad", "handle_scratchpad_create"),
    "scratchpad_update": LazyHandler("scratchpad", "handle_scratchpad_update"),
    "analyze_scratchpad": LazyHandler("scratchpad", "handle_analyze_scratchpad"),
    "scratchpad_finalize": LazyHandler("scratchpad", "handle_scratchpad_finalize"),

    # --- Hologram ---
    "view_hologram": LazyHandler("misc", "handle_view_hologram"),

    # --- Rust Bridge ---
    "rust_audit": LazyHandler("rust_bridge", "handle_rust_audit"),
    "rust_compress": LazyHandler("rust_bridge", "handle_rust_compress"),
    "rust_similarity": LazyHandler("rust_bridge", "handle_rust_similarity"),
    "rust_status": LazyHandler("rust_bridge", "handle_rust_status"),

    # --- Archaeology (unified) ---
    "archaeology": LazyHandler("archaeology", "handle_archaeology"),
    # Backward-compat aliases (old tool names still dispatch correctly)
    "archaeology_scan_directory": LazyHandler("archaeology", "handle_archaeology_scan_directory"),
    "archaeology_mark_read": LazyHandler("archaeology", "handle_archaeology_mark_read"),
    "archaeology_mark_written": LazyHandler("archaeology", "handle_archaeology_mark_written"),
    "archaeology_have_read": LazyHandler("archaeology", "handle_archaeology_have_read"),
    "archaeology_find_unread": LazyHandler("archaeology", "handle_archaeology_find_unread"),
    "archaeology_find_changed": LazyHandler("archaeology", "handle_archaeology_find_changed"),
    "archaeology_recent_reads": LazyHandler("archaeology", "handle_archaeology_recent_reads"),
    "archaeology_stats": LazyHandler("archaeology", "handle_archaeology_stats"),
    "archaeology_report": LazyHandler("archaeology", "handle_archaeology_report"),
    "archaeology_search": LazyHandler("archaeology", "handle_archaeology_search"),
    "archaeology_process_wisdom": LazyHandler("archaeology", "handle_archaeology_process_wisdom"),
    "archaeology_daily_digest": LazyHandler("archaeology", "handle_archaeology_daily_digest"),

    # --- Governor ---
    "governor_validate": LazyHandler("governor", "handle_governor_validate"),
    "governor_validate_path": LazyHandler("governor", "handle_governor_validate_path"),
    "governor_set_goal": LazyHandler("governor", "handle_governor_set_goal"),
    "governor_check_drift": LazyHandler("governor", "handle_governor_check_drift"),
    "governor_check_budget": LazyHandler("governor", "handle_governor_check_budget"),
    "governor_check_dharma": LazyHandler("governor", "handle_governor_check_dharma"),
    "governor_stats": LazyHandler("governor", "handle_governor_stats"),

    # --- Watcher ---
    "watcher_add": LazyHandler("watcher", "handle_watcher_add"),
    "watcher_remove": LazyHandler("watcher", "handle_watcher_remove"),
    "watcher_start": LazyHandler("watcher", "handle_watcher_start"),
    "watcher_stop": LazyHandler("watcher", "handle_watcher_stop"),
    "watcher_status": LazyHandler("watcher", "handle_watcher_status"),
    "watcher_recent_events": LazyHandler("watcher", "handle_watcher_recent_events"),
    "watcher_stats": LazyHandler("watcher", "handle_watcher_stats"),
    "watcher_list": LazyHandler("watcher", "handle_watcher_list"),

    # --- Windsurf ---
    "windsurf_list_conversations": LazyHandler("windsurf_conv", "handle_windsurf_list_conversations"),
    "windsurf_read_conversation": LazyHandler("windsurf_conv", "handle_windsurf_read_conversation"),
    "windsurf_export_conversation": LazyHandler("windsurf_conv", "handle_windsurf_export_conversation"),
    "windsurf_search_conversations": LazyHandler("windsurf_conv", "handle_windsurf_search_conversations"),
    "windsurf_stats": LazyHandler("windsurf_conv", "handle_windsurf_stats"),

    # --- Browser ---
    "browser_navigate": LazyHandler("browser_tools", "handle_browser_navigate"),
    "browser_click": LazyHandler("browser_tools", "handle_browser_click"),
    "browser_type": LazyHandler("browser_tools", "handle_browser_type"),
    "browser_extract_dom": LazyHandler("browser_tools", "handle_browser_extract_dom"),
    "browser_screenshot": LazyHandler("browser_tools", "handle_browser_screenshot"),
    "browser_get_interactables": LazyHandler("browser_tools", "handle_browser_get_interactables"),

    # --- Web Research ---
    "web_fetch": LazyHandler("web_research", "handle_web_fetch"),
    "web_search": LazyHandler("web_research", "handle_web_search"),
    "web_search_and_read": LazyHandler("web_research", "handle_web_search_and_read"),
    "research_topic": LazyHandler("web_research", "handle_research_topic"),
    "browser_session_status": LazyHandler("web_research", "handle_browser_session_status"),

    # --- Simplified Aliases ---
    "remember": LazyHandler("aliases", "handle_remember"),
    "recall": LazyHandler("aliases", "handle_recall"),
    "think": LazyHandler("aliases", "handle_think"),
    "check": LazyHandler("aliases", "handle_check"),

    # --- Ollama Agent Loop ---
    "ollama.agent": LazyHandler("ollama_agent", "handle_ollama_agent"),

    # --- Galactic Dashboard ---
    "galactic.dashboard": LazyHandler("galactic_dashboard", "handle_galactic_dashboard"),

    # --- Galaxy Management (multi-galaxy) ---
    "galaxy.create": LazyHandler("galaxy", "handle_galaxy_create"),
    "galaxy.switch": LazyHandler("galaxy", "handle_galaxy_switch"),
    "galaxy.list": LazyHandler("galaxy", "handle_galaxy_list"),
    "galaxy.status": LazyHandler("galaxy", "handle_galaxy_status"),
    "galaxy.ingest": LazyHandler("galaxy", "handle_galaxy_ingest"),
    "galaxy.delete": LazyHandler("galaxy", "handle_galaxy_delete"),
    "galaxy.backup": LazyHandler("backup", "handle_galaxy_backup"),
    "galaxy.restore": LazyHandler("backup", "handle_galaxy_restore"),

    # --- v15.3 Galactic Telepathy ---
    "galaxy.transfer": LazyHandler("galaxy", "handle_galaxy_transfer"),
    "galaxy.merge": LazyHandler("galaxy", "handle_galaxy_merge"),
    "galaxy.sync": LazyHandler("galaxy", "handle_galaxy_sync"),

    # --- v15.4 Phylogenetic Lineage ---
    "galaxy.lineage": LazyHandler("galaxy", "handle_galaxy_lineage"),
    "galaxy.taxonomy": LazyHandler("galaxy", "handle_galaxy_taxonomy"),
    "galaxy.lineage_stats": LazyHandler("galaxy", "handle_galaxy_lineage_stats"),

    # --- Memory Aliases ---
    "read_memory": LazyHandler("misc", "handle_read_memory"),
    "list_memories": LazyHandler("misc", "handle_list_memories"),
    "update_memory": LazyHandler("misc", "handle_update_memory"),
    "delete_memory": LazyHandler("misc", "handle_delete_memory"),

    # --- Synthesis / Kaizen ---
    "kaizen_analyze": LazyHandler("synthesis", "handle_kaizen_analyze"),
    "kaizen_apply_fixes": LazyHandler("synthesis", "handle_kaizen_apply_fixes"),

    # --- Serendipity ---
    "serendipity_surface": LazyHandler("synthesis", "handle_serendipity_surface"),
    "serendipity_mark_accessed": LazyHandler("synthesis", "handle_serendipity_mark_accessed"),

    # --- Pattern ---
    "pattern_search": LazyHandler("synthesis", "handle_pattern_search"),
    "cluster_stats": LazyHandler("synthesis", "handle_cluster_stats"),
    "list_cascade_patterns": LazyHandler("synthesis", "handle_list_cascade_patterns"),

    # --- Gnosis Portal (MandalaOS unified introspection) ---
    "gnosis": LazyHandler("introspection", "handle_gnosis"),

    # --- Capability Matrix ---
    "capability.matrix": LazyHandler("introspection", "handle_capability_matrix"),
    "capability.status": LazyHandler("introspection", "handle_capability_status"),
    "capability.suggest": LazyHandler("introspection", "handle_capability_suggest"),

    # --- Memory Lifecycle & Consolidation (unified) ---
    "memory.lifecycle": LazyHandler("governance", "handle_memory_lifecycle"),
    # Backward-compat aliases
    "memory.lifecycle_sweep": LazyHandler("governance", "handle_lifecycle_sweep"),
    "memory.lifecycle_stats": LazyHandler("governance", "handle_lifecycle_stats"),
    "memory.consolidate": LazyHandler("governance", "handle_consolidate_memories"),
    "memory.consolidation_stats": LazyHandler("governance", "handle_consolidation_stats"),

    # --- Homeostasis & Maturity (unified) ---
    "homeostasis": LazyHandler("governance", "handle_homeostasis"),
    # Backward-compat aliases
    "homeostasis.status": LazyHandler("governance", "handle_homeostasis_status"),
    "homeostasis.check": LazyHandler("governance", "handle_homeostasis_check"),
    "maturity.assess": LazyHandler("governance", "handle_maturity_assess"),

    # --- Tool Dependency Graph & Dharma Reload ---
    "tool.graph": LazyHandler("governance", "handle_tool_graph"),
    # Backward-compat alias
    "tool.graph_full": LazyHandler("governance", "handle_tool_graph_full"),
    "dharma.reload": LazyHandler("governance", "handle_dharma_reload"),

    # --- Starter Packs & Agent Ergonomics (unified) ---
    "starter_packs": LazyHandler("agent_ergonomics", "handle_starter_packs"),
    # Backward-compat aliases
    "starter_packs.list": LazyHandler("agent_ergonomics", "handle_starter_packs_list"),
    "starter_packs.get": LazyHandler("agent_ergonomics", "handle_starter_packs_get"),
    "starter_packs.suggest": LazyHandler("agent_ergonomics", "handle_starter_packs_suggest"),
    "rate_limiter.stats": LazyHandler("agent_ergonomics", "handle_rate_limiter_stats"),
    "audit.export": LazyHandler("agent_ergonomics", "handle_audit_export"),

    # --- Explain This, Agent Trust & Mesh ---
    "explain_this": LazyHandler("agent_ergonomics", "handle_explain_this"),
    "agent.trust": LazyHandler("agent_ergonomics", "handle_agent_trust"),
    "mesh.status": LazyHandler("agent_ergonomics", "handle_mesh_status"),
    "mesh.broadcast": LazyHandler("agent_ergonomics", "handle_mesh_broadcast"),

    # --- CyberBrain modules ---
    "salience.spotlight": LazyHandler("cyberbrain", "handle_salience_spotlight"),
    "reasoning.bicameral": LazyHandler("cyberbrain", "handle_bicameral_reason"),
    "memory.retention_sweep": LazyHandler("cyberbrain", "handle_retention_sweep"),

    # --- Introspection (v12.2 Convergence) ---
    "capabilities": LazyHandler("introspection", "handle_capabilities"),
    "manifest": LazyHandler("introspection", "handle_manifest"),
    "state.paths": LazyHandler("introspection", "handle_state_paths"),
    "state.summary": LazyHandler("introspection", "handle_state_summary"),
    "repo.summary": LazyHandler("introspection", "handle_repo_summary"),
    "ship.check": LazyHandler("introspection", "handle_ship_check"),
    "get_telemetry_summary": LazyHandler("introspection", "handle_get_telemetry_summary"),

    # --- Metrics ---
    "track_metric": LazyHandler("misc", "handle_track_metric"),
    "get_metrics_summary": LazyHandler("misc", "handle_get_metrics_summary"),

    # --- Sangha (Coordination) ---
    "sangha_chat_send": LazyHandler("sangha", "handle_sangha_chat_send"),
    "sangha_chat_read": LazyHandler("sangha", "handle_sangha_chat_read"),
    "sangha_lock": LazyHandler("sangha", "handle_sangha_lock"),
    # Backward-compat aliases
    "sangha_lock_acquire": LazyHandler("sangha", "handle_sangha_lock_acquire"),
    "sangha_lock_release": LazyHandler("sangha", "handle_sangha_lock_release"),
    "sangha_lock_list": LazyHandler("sangha", "handle_sangha_lock_list"),

    # --- Intelligence (Extended) ---
    "execute_cascade": LazyHandler("misc", "handle_execute_cascade"),

    # --- Introspection (health_report) ---
    "health_report": LazyHandler("introspection", "handle_health_report"),

    # --- Broker (Redis pub/sub coordination) ---
    "broker.publish": LazyHandler("broker", "handle_broker_publish"),
    "broker.history": LazyHandler("broker", "handle_broker_history"),
    "broker.status": LazyHandler("broker", "handle_broker_status"),

    # --- Task Distribution ---
    "task.distribute": LazyHandler("task_dist", "handle_task_distribute"),
    "task.status": LazyHandler("task_dist", "handle_task_status"),
    "task.list": LazyHandler("task_dist", "handle_task_list"),
    "task.complete": LazyHandler("task_dist", "handle_task_complete"),

    # --- Voting / Ensemble ---
    "vote.create": LazyHandler("voting", "handle_vote_create"),
    "vote.cast": LazyHandler("voting", "handle_vote_cast"),
    "vote.analyze": LazyHandler("voting", "handle_vote_analyze"),
    "vote.list": LazyHandler("voting", "handle_vote_list"),
    "vote.record_outcome": LazyHandler("voting", "handle_vote_record_outcome"),

    # --- Ollama Bridge (optional local LLM) ---
    "ollama.models": LazyHandler("ollama", "handle_ollama_models"),
    "ollama.generate": LazyHandler("ollama", "handle_ollama_generate"),
    "ollama.chat": LazyHandler("ollama", "handle_ollama_chat"),

    # --- Pipelines (unified) ---
    "pipeline": LazyHandler("pipeline", "handle_pipeline"),
    # Backward-compat aliases
    "pipeline.create": LazyHandler("pipeline", "handle_pipeline_create"),
    "pipeline.status": LazyHandler("pipeline", "handle_pipeline_status"),
    "pipeline.list": LazyHandler("pipeline", "handle_pipeline_list"),

    # --- Agent Registry ---
    "agent.register": LazyHandler("agent_registry", "handle_agent_register"),
    "agent.heartbeat": LazyHandler("agent_registry", "handle_agent_heartbeat"),
    "agent.list": LazyHandler("agent_registry", "handle_agent_list"),
    "agent.capabilities": LazyHandler("agent_registry", "handle_agent_capabilities"),
    "agent.deregister": LazyHandler("agent_registry", "handle_agent_deregister"),

    # --- Ensemble Voting (unified) ---
    "ensemble": LazyHandler("ensemble", "handle_ensemble"),
    # Backward-compat aliases
    "ensemble.query": LazyHandler("ensemble", "handle_ensemble_query"),
    "ensemble.status": LazyHandler("ensemble", "handle_ensemble_status"),
    "ensemble.history": LazyHandler("ensemble", "handle_ensemble_history"),

    # --- Cross-Device Session Handoff (unified) ---
    "session.handoff": LazyHandler("session", "handle_session_handoff"),
    # Backward-compat aliases
    "session.handoff_transfer": LazyHandler("session", "handle_session_handoff_transfer"),
    "session.accept_handoff": LazyHandler("session", "handle_session_accept_handoff"),
    "session.list_handoffs": LazyHandler("session", "handle_session_list_handoffs"),

    # --- Load-Aware Task Routing ---
    "task.route_smart": LazyHandler("task_dist", "handle_task_route_smart"),

    # --- Emotion & Drive Core ---
    "drive.snapshot": LazyHandler("cyberbrain", "handle_drive_snapshot"),
    "drive.event": LazyHandler("cyberbrain", "handle_drive_event"),

    # --- Self-Model / Predictive Introspection ---
    "selfmodel.forecast": LazyHandler("cyberbrain", "handle_selfmodel_forecast"),
    "selfmodel.alerts": LazyHandler("cyberbrain", "handle_selfmodel_alerts"),

    # --- Worker Daemon ---
    "worker.status": LazyHandler("cyberbrain", "handle_worker_status"),

    # --- Gratitude Architecture ---
    "whitemagic.tip": LazyHandler("gratitude", "handle_tip"),
    "gratitude.stats": LazyHandler("gratitude", "handle_gratitude_stats"),
    "gratitude.benefits": LazyHandler("gratitude", "handle_gratitude_benefits"),

    # --- v14.0 Living Graph ---
    "hybrid_recall": LazyHandler("living_graph", "handle_hybrid_recall"),
    "graph_topology": LazyHandler("living_graph", "handle_graph_topology"),
    "graph_walk": LazyHandler("living_graph", "handle_graph_walk"),
    "surprise_stats": LazyHandler("living_graph", "handle_surprise_stats"),
    "entity_resolve": LazyHandler("living_graph", "handle_entity_resolve"),

    # --- Gana Sabha (Council Protocol — 12.108.25) ---
    "sabha.convene": LazyHandlerAbs("whitemagic.tools.gana_sabha", "handle_sabha_convene"),
    "sabha.status": LazyHandlerAbs("whitemagic.tools.gana_sabha", "handle_sabha_status"),

    # --- Gana Forge (Declarative Extension — 12.108.17) ---
    "forge.status": LazyHandlerAbs("whitemagic.tools.gana_forge", "handle_forge_status"),
    "forge.reload": LazyHandlerAbs("whitemagic.tools.gana_forge", "handle_forge_reload"),
    "forge.validate": LazyHandlerAbs("whitemagic.tools.gana_forge", "handle_forge_validate"),

    # --- Edgerunner Violet Security ---
    "mcp_integrity.snapshot": LazyHandler("violet_security", "handle_mcp_integrity_snapshot"),
    "mcp_integrity.verify": LazyHandler("violet_security", "handle_mcp_integrity_verify"),
    "mcp_integrity.status": LazyHandler("violet_security", "handle_mcp_integrity_status"),
    "model.register": LazyHandler("violet_security", "handle_model_register"),
    "model.verify": LazyHandler("violet_security", "handle_model_verify"),
    "model.list": LazyHandler("violet_security", "handle_model_list"),
    "model.hash": LazyHandler("violet_security", "handle_model_hash"),
    "model.signing_status": LazyHandler("violet_security", "handle_model_signing_status"),
    "engagement.issue": LazyHandler("violet_security", "handle_engagement_issue"),
    "engagement.validate": LazyHandler("violet_security", "handle_engagement_validate"),
    "engagement.revoke": LazyHandler("violet_security", "handle_engagement_revoke"),
    "engagement.list": LazyHandler("violet_security", "handle_engagement_list"),
    "engagement.status": LazyHandler("violet_security", "handle_engagement_status"),
    "security.alerts": LazyHandler("violet_security", "handle_security_alerts"),
    "security.monitor_status": LazyHandler("violet_security", "handle_security_monitor_status"),

    # --- v14.2: JIT Memory Researcher ---
    "jit_research": LazyHandler("v14_2_handlers", "handle_jit_research"),
    "jit_research.stats": LazyHandler("v14_2_handlers", "handle_jit_research_stats"),

    # --- v14.2: Narrative Compression ---
    "narrative.compress": LazyHandler("v14_2_handlers", "handle_narrative_compress"),
    "narrative.stats": LazyHandler("v14_2_handlers", "handle_narrative_stats"),

    # --- v14.2: Hermit Crab Mode ---
    "hermit.status": LazyHandler("v14_2_handlers", "handle_hermit_status"),
    "hermit.assess": LazyHandler("v14_2_handlers", "handle_hermit_assess"),
    "hermit.withdraw": LazyHandler("v14_2_handlers", "handle_hermit_withdraw"),
    "hermit.mediate": LazyHandler("v14_2_handlers", "handle_hermit_mediate"),
    "hermit.resolve": LazyHandler("v14_2_handlers", "handle_hermit_resolve"),
    "hermit.verify_ledger": LazyHandler("v14_2_handlers", "handle_hermit_verify_ledger"),
    "hermit.check_access": LazyHandler("v14_2_handlers", "handle_hermit_check_access"),

    # --- v14.2: Green Score Telemetry ---
    "green.report": LazyHandler("v14_2_handlers", "handle_green_report"),
    "green.record": LazyHandler("v14_2_handlers", "handle_green_record"),

    # --- v14.2: Cognitive Modes ---
    "cognitive.mode": LazyHandler("v14_2_handlers", "handle_cognitive_mode"),
    "cognitive.set": LazyHandler("v14_2_handlers", "handle_cognitive_set"),
    "cognitive.hints": LazyHandler("v14_2_handlers", "handle_cognitive_hints"),
    "cognitive.stats": LazyHandler("v14_2_handlers", "handle_cognitive_stats"),

    # --- v14.6: Physical Truth Verification ---
    "verification.request": LazyHandler("verification", "handle_verification_request"),
    "verification.attest": LazyHandler("verification", "handle_verification_attest"),
    "verification.status": LazyHandler("verification", "handle_verification_status"),

    # --- v15.2: Sovereign Sandbox (Shelter) ---
    "shelter.create": LazyHandler("shelter", "handle_shelter_create"),
    "shelter.execute": LazyHandler("shelter", "handle_shelter_execute"),
    "shelter.inspect": LazyHandler("shelter", "handle_shelter_inspect"),
    "shelter.destroy": LazyHandler("shelter", "handle_shelter_destroy"),
    "shelter.status": LazyHandler("shelter", "handle_shelter_status"),
    "shelter.policy": LazyHandler("shelter", "handle_shelter_policy"),

    # --- v15.2: Optimized Memory States (OMS) ---
    "oms.export": LazyHandler("oms", "handle_oms_export"),
    "oms.import": LazyHandler("oms", "handle_oms_import"),
    "oms.inspect": LazyHandler("oms", "handle_oms_inspect"),
    "oms.verify": LazyHandler("oms", "handle_oms_verify"),
    "oms.price": LazyHandler("oms", "handle_oms_price"),
    "oms.list": LazyHandler("oms", "handle_oms_list"),
    "oms.status": LazyHandler("oms", "handle_oms_status"),

    # --- v15.2: ILP Streaming Payments ---
    "ilp.configure": LazyHandler("ilp", "handle_ilp_configure"),
    "ilp.send": LazyHandler("ilp", "handle_ilp_send"),
    "ilp.receipt": LazyHandler("ilp", "handle_ilp_receipt"),
    "ilp.history": LazyHandler("ilp", "handle_ilp_history"),
    "ilp.balance": LazyHandler("ilp", "handle_ilp_balance"),
    "ilp.status": LazyHandler("ilp", "handle_ilp_status"),

    # --- v15.6: Cognitive Extensions ---
    "rerank": LazyHandler("cognitive_extensions", "handle_rerank"),
    "rerank.status": LazyHandler("cognitive_extensions", "handle_rerank_status"),
    "working_memory.attend": LazyHandler("cognitive_extensions", "handle_working_memory_attend"),
    "working_memory.context": LazyHandler("cognitive_extensions", "handle_working_memory_context"),
    "working_memory.status": LazyHandler("cognitive_extensions", "handle_working_memory_status"),
    "reconsolidation.mark": LazyHandler("cognitive_extensions", "handle_reconsolidation_mark"),
    "reconsolidation.update": LazyHandler("cognitive_extensions", "handle_reconsolidation_update"),
    "reconsolidation.status": LazyHandler("cognitive_extensions", "handle_reconsolidation_status"),
    "community.propagate": LazyHandler("cognitive_extensions", "handle_community_propagate"),
    "community.status": LazyHandler("cognitive_extensions", "handle_community_status"),
    "community.health": LazyHandler("cognitive_extensions", "handle_community_health"),

    # --- v15.2: Marketplace Bridge ---
    "marketplace.publish": LazyHandler("marketplace", "handle_marketplace_publish"),
    "marketplace.discover": LazyHandler("marketplace", "handle_marketplace_discover"),
    "marketplace.negotiate": LazyHandler("marketplace", "handle_marketplace_negotiate"),
    "marketplace.complete": LazyHandler("marketplace", "handle_marketplace_complete"),
    "marketplace.my_listings": LazyHandler("marketplace", "handle_marketplace_my_listings"),
    "marketplace.remove": LazyHandler("marketplace", "handle_marketplace_remove"),
    "marketplace.status": LazyHandler("marketplace", "handle_marketplace_status"),

    # --- v15.8: Pattern Analysis Engines (previously unwired) ---
    "causal.mine": LazyHandler("pattern_engines", "handle_causal_mine"),
    "causal.stats": LazyHandler("pattern_engines", "handle_causal_stats"),
    "emergence.scan": LazyHandler("pattern_engines", "handle_emergence_scan"),
    "emergence.status": LazyHandler("pattern_engines", "handle_emergence_status"),
    "association.mine": LazyHandler("pattern_engines", "handle_association_mine"),
    "association.mine_semantic": LazyHandler("pattern_engines", "handle_association_mine_semantic"),
    "constellation.detect": LazyHandler("pattern_engines", "handle_constellation_detect"),
    "constellation.stats": LazyHandler("pattern_engines", "handle_constellation_stats"),
    "constellation.merge": LazyHandler("pattern_engines", "handle_constellation_merge"),
    "satkona.fuse": LazyHandler("pattern_engines", "handle_satkona_fuse"),
    "reasoning.multispectral": LazyHandler("pattern_engines", "handle_reasoning_multispectral"),
    "novelty.detect": LazyHandler("pattern_engines", "handle_novelty_detect"),
    "novelty.stats": LazyHandler("pattern_engines", "handle_novelty_stats"),
    "bridge.synthesize": LazyHandler("pattern_engines", "handle_bridge_synthesize"),
    "galactic.sweep": LazyHandler("pattern_engines", "handle_galactic_sweep"),
    "galactic.stats": LazyHandler("pattern_engines", "handle_galactic_stats"),
    "guideline.evolve": LazyHandler("pattern_engines", "handle_guideline_evolve"),
    "elemental.optimize": LazyHandler("pattern_engines", "handle_elemental_optimize"),
    "pattern_consciousness.status": LazyHandler("pattern_engines", "handle_pattern_consciousness_status"),

    # --- v15.9: War Room & Shadow Clone Army ---
    "war_room.status": LazyHandler("war_room", "handle_war_room_status"),
    "war_room.plan": LazyHandler("war_room", "handle_war_room_plan"),
    "war_room.execute": LazyHandler("war_room", "handle_war_room_execute"),
    "war_room.hierarchy": LazyHandler("war_room", "handle_war_room_hierarchy"),
    "war_room.campaigns": LazyHandler("war_room", "handle_war_room_campaigns"),
    "war_room.phase": LazyHandler("war_room", "handle_war_room_phase"),

    # --- v15.9: Imperial Doctrine (36 Stratagems) ---
    "doctrine.summary": LazyHandler("war_room", "handle_doctrine_summary"),
    "doctrine.stratagems": LazyHandler("war_room", "handle_doctrine_stratagems"),
    "doctrine.force": LazyHandler("war_room", "handle_doctrine_force"),

    # --- v15.9: Art of War Engine ---
    "art_of_war.wisdom": LazyHandler("war_room", "handle_art_of_war_wisdom"),
    "art_of_war.terrain": LazyHandler("war_room", "handle_art_of_war_terrain"),
    "art_of_war.campaign": LazyHandler("war_room", "handle_art_of_war_campaign"),
    "art_of_war.chapter": LazyHandler("war_room", "handle_art_of_war_chapter"),

    # --- v15.9: Fool's Guard / Ralph Wiggum ---
    "fool_guard.status": LazyHandler("war_room", "handle_fool_guard_status"),
    "fool_guard.dare_to_die": LazyHandler("war_room", "handle_fool_guard_dare_to_die"),
    "fool_guard.ralph": LazyHandler("war_room", "handle_fool_guard_ralph"),
}


# ---------------------------------------------------------------------------
# Core router middleware (defined here to access DISPATCH_TABLE without
# circular imports — middleware.py has no dependency on this module)
# ---------------------------------------------------------------------------

def _mw_core_router(ctx: Any, next_fn: Callable[[Any], dict[str, Any] | None]) -> dict[str, Any] | None:
    """Gana prefix → Dispatch table → Bridge fallback."""
    result = None

    # 1. Gana prefix routing
    if ctx.tool_name.startswith("gana_"):
        try:
            from whitemagic.core.bridge.gana import gana_invoke
            result = gana_invoke(tool_name=ctx.tool_name, args=ctx.kwargs)
        except Exception as e:
            result = {"status": "error", "error": f"Gana invocation failed: {str(e)}"}

    # 2. Table lookup
    if result is None:
        handler = DISPATCH_TABLE.get(ctx.tool_name)
        if handler is not None:
            result = handler(**ctx.kwargs)

    # 3. Bridge delegation fallback (legacy v1 dispatch path)
    if result is None:
        try:
            import warnings
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", DeprecationWarning)
                from whitemagic.core.bridge.tools import execute_mcp_tool
            bridge_result = execute_mcp_tool(ctx.tool_name, **ctx.kwargs)
            if bridge_result:
                result = bridge_result
        except ImportError:
            pass

    if result is None:
        return next_fn(ctx)  # falls through to _terminal

    return result


# ---------------------------------------------------------------------------
# Pipeline construction
# ---------------------------------------------------------------------------

def _build_pipeline() -> Any:
    """Build the standard dispatch pipeline.  Called once at import time."""
    from whitemagic.tools.middleware import (
        DispatchPipeline,
        mw_circuit_breaker,
        mw_governor,
        mw_input_sanitizer,
        mw_maturity_gate,
        mw_rate_limiter,
        mw_security_monitor,
        mw_tool_permissions,
    )
    p = DispatchPipeline()
    p.use("input_sanitizer", mw_input_sanitizer)
    p.use("circuit_breaker", mw_circuit_breaker)
    p.use("rate_limiter",    mw_rate_limiter)
    p.use("security_monitor", mw_security_monitor)
    p.use("tool_permissions", mw_tool_permissions)
    p.use("maturity_gate",   mw_maturity_gate)
    p.use("governor",        mw_governor)
    p.use("core_router",     _mw_core_router)
    return p


_pipeline = _build_pipeline()


def dispatch(tool_name: str, **kwargs: Any) -> dict[str, Any] | None:
    """Dispatch a tool call through the composable middleware pipeline.

    Pipeline (in order):
      1. Input sanitizer   — validate args
      2. Circuit breaker   — fast-fail on cooldown + post-feedback
      3. Rate limiter      — per-agent, per-tool throttling
      4. Tool permissions  — per-agent RBAC
      5. Maturity gate     — developmental stage gating
      6. Governor          — ethical validation
      7. Core router       — Gana prefix → dispatch table → bridge fallback

    Post-pipeline:
      - Compact response mode (token-efficient output)

    Returns:
        The handler result, or an error dict if no handler matched.

    """
    return cast("dict[str, Any] | None", _pipeline.execute(tool_name, **kwargs))


def get_pipeline() -> Any:
    """Return the active pipeline for introspection or extension."""
    return _pipeline
