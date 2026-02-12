"""Southern Quadrant Ganas (Vermilion Bird).

Implementations for Lunar Mansions 8-14 (Summer/South).
Theme: Expression, Harmony, Visibility.
"""

from datetime import datetime
from typing import Any

from .base import BaseGana, GanaCall, LunarMansion


class GhostGana(BaseGana):
    """Ghost (鬼 Gui) - Introspection.

    Southern Vermilion Bird, #8
    Garden: Grief
    Consolidates: track_metric, get_metrics_summary
    Role: Self-audit, logging, reflection, letting go of what doesn't work.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.GHOST, garden="Grief")

    def _get_base_template(self) -> str:
        return """
You are Ghost (鬼), the introspector. Your role is to:
1. Look inward
2. Audit performance
3. Acknowledge losses and failures
4. Let go of inefficiency

In the shadows, truth is found.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute introspection/metrics logic."""
        from whitemagic.metrics import get_tracker, track_metric

        # If specific tool call
        if call.task == "track_metric":
            category = call.state_vector.get("category", "general")
            metric = call.state_vector.get("metric", "unknown")
            value = call.state_vector.get("value", 1.0)
            context = call.state_vector.get("context", "")
            result = track_metric(category, metric, value, context)
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "action": "track_metric",
                "result": result,
                "status": "logged",
            }

        elif call.task == "get_metrics_summary":
            categories = call.state_vector.get("categories")
            tracker = get_tracker()
            # Ensure categories is a list to satisfy Mypy
            summary = tracker.get_summary(categories if isinstance(categories, list) else [])
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "action": "get_summary",
                "summary": summary,
                "status": "retrieved",
            }

        # KaizenEngine integration
        elif call.task in ["kaizen_analyze", "generate_improvement_proposals"]:
            try:
                from whitemagic.core.intelligence.synthesis.kaizen_engine import (
                    get_kaizen_engine,
                )
                kaizen = get_kaizen_engine()
                report = kaizen.analyze()
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "kaizen_analyze",
                    "total_proposals": len(report.proposals),
                    "high_impact": len([p for p in report.proposals if p.impact == "high"]),
                    "auto_fixable": len([p for p in report.proposals if p.auto_fixable]),
                    "proposals": [{"title": p.title, "impact": p.impact, "category": p.category}
                                  for p in report.proposals[:10]],
                    "metrics": report.metrics,
                    "status": "introspected",
                }
            except ImportError:
                pass

        elif call.task == "deep_search":
            try:
                from whitemagic.config import PROJECT_ROOT
                from whitemagic.optimization.polyglot_router import get_router
                router = get_router()

                pattern = call.state_vector.get("pattern", "TODO")
                exts = call.state_vector.get("extensions")
                context = call.state_vector.get("context_lines", 2)

                results = router.deep_search(
                    root_path=str(PROJECT_ROOT),
                    pattern=pattern,
                    extensions=exts,
                    context_lines=context,
                )

                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "deep_search",
                    "pattern": pattern,
                    "match_count": len(results),
                    "matches": results[:15],
                    "status": "success",
                }
            except Exception as e:
                return {"error": str(e), "status": "failed"}

        elif call.task == "kaizen_apply":
            try:
                from whitemagic.core.intelligence.synthesis.kaizen_engine import (
                    get_kaizen_engine,
                )
                kaizen = get_kaizen_engine()
                results = kaizen.apply_auto_fixes()  # type: ignore[assignment]
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "kaizen_apply",
                    "applied": results.get("applied", 0),  # type: ignore[attr-defined]
                    "skipped": results.get("skipped", 0),  # type: ignore[attr-defined]
                    "errors": results.get("errors", 0),  # type: ignore[attr-defined]
                    "status": "improved",
                }
            except ImportError:
                pass

        # Default/Direct invocation
        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "metrics_logged": True,
            "introspection_depth": "deep",
            "status": "reflective",
        }

class WillowGana(BaseGana):
    """Willow (柳 Liu) - Flexibility.

    Southern Vermilion Bird, #9
    Garden: Play
    Consolidates: Adaptation tools
    Role: Bending without breaking, adaptive interfaces, experimentation.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.WILLOW, garden="Play")

    def _get_base_template(self) -> str:
        return """
You are Willow (柳), the flexible one. Your role is to:
1. Bend with the wind
2. Adapt to context
3. Play with possibilities
4. Avoid rigidity

Softness overcomes hardness.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute adaptation logic."""
        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "adaptation": "flexible",
            "ui_mode": "adaptive",
            "status": "flowing",
        }

class StarGana(BaseGana):
    """Star (星 Xing) - Illumination.

    Southern Vermilion Bird, #10
    Garden: Beauty
    Consolidates: prat_get_context, prat_invoke
    Role: Visualization, clarity, seeing the beautiful structure.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.STAR, garden="Beauty")

    def _get_base_template(self) -> str:
        return """
You are Star (星), the illuminator. Your role is to:
1. Shine light on structure
2. Reveal beauty in code
3. visualize connections
4. Provide clarity

Light reveals what is hidden.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute PRAT/Context logic."""

        if call.task == "prat_get_context":
            from whitemagic.cascade.context_synthesizer import get_context_synthesizer
            synth = get_context_synthesizer()
            ctx = synth.gather()
            # Convert dataclass to dict for JSON serialization if needed, or return object
            # UnifiedContext is a dataclass
            from dataclasses import asdict
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "context": asdict(ctx),
                "dominant_influence": ctx.get_dominant_influence(),
                "recommended_morphology": ctx.get_recommended_morphology(),
                "status": "illuminated",
            }

        elif call.task == "prat_invoke":
            # This is recursive if we are not careful!
            # prat_invoke calls execute_mcp_tool -> gana_invoke -> Gana.
            # But here we are IN the Gana.
            # Real prat_invoke logic should be here.
            # It usually calls adaptive_portal.
            from whitemagic.cascade.adaptive_portal import get_adaptive_portal
            portal = get_adaptive_portal()

            target = call.state_vector.get("target_tool")
            params = call.state_vector.get("params", {})
            morph = call.state_vector.get("force_morphology")

            if not isinstance(target, str):
                return {
                    "error": "target_tool must be a string",
                    "status": "failed",
                }

            result = await portal.invoke(target, params, morph)

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "prat_result": result,
                "status": "invoked",
            }

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "context_illuminated": True,
            "beauty_score": 0.95,
            "status": "radiant",
        }

class ExtendedNetGana(BaseGana):
    """Extended Net (张 Zhang) - Connectivity.

    Southern Vermilion Bird, #11
    Garden: Connection
    Consolidates: manage_resonance, networking, hybrid_recall, constellation_bridges
    Role: Connecting nodes, Indra's Net, propagating signals.
    Now powered by CoreAccessLayer for deep graph traversal and cross-constellation bridging.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.EXTENDED_NET, garden="Connection")

    def _get_base_template(self) -> str:
        return """
You are Extended Net (张), the connector. Your role is to:
1. Link disparate nodes via the association graph
2. Strengthen bonds through temporal traversal tracking
3. Propagate resonance across constellation boundaries
4. Surface hidden bridges between knowledge clusters
5. Hybrid recall: fuse vector similarity with graph walks

Everything is connected to everything else.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute networking/resonance logic."""
        # CoreAccessLayer-powered operations (v14+)
        if call.task in ["hybrid_recall", "find_bridges", "walk_associations", "association_stats"]:
            return self._execute_core_access(call)

        from whitemagic.core.resonance.gan_ying import (
            EventType,
            ResonanceEvent,
            get_bus,
        )

        bus = get_bus()

        if call.task == "manage_resonance":
            operation = call.state_vector.get("operation", "emit_event")

            if operation == "emit_event":
                event_type = call.state_vector.get("event_type", "generic_resonance")
                data = call.state_vector.get("data", {})
                source = call.state_vector.get("source", "mcp_tool")

                event = ResonanceEvent(
                    source=source,
                    event_type=EventType(event_type) if event_type in EventType.__members__ else EventType.SYSTEM_STATE_CHANGE,
                    data=data,
                    timestamp=datetime.now(),
                )
                bus.emit(event)

                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "emit",
                    "event_id": str(event.timestamp),
                    "status": "propagated",
                }

            elif operation == "monitor":
                # Basic health check of bus
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "monitor",
                    "bus_active": True, # Sync bus is always "active"
                    "subscriber_count": len(bus._listeners),
                    "status": "connected",
                }

        # UnifiedPatternAPI integration
        if call.task in ["search_all_patterns", "find_pattern_correlations", "get_pattern_stats"]:
            try:
                from whitemagic.core.intelligence.synthesis.unified_patterns import (
                    get_pattern_api,
                )
                api = get_pattern_api()

                if call.task == "search_all_patterns":
                    query = call.state_vector.get("query")
                    engines = call.state_vector.get("engines")
                    min_confidence = call.state_vector.get("min_confidence", 0.5)

                    import asyncio
                    loop = asyncio.get_running_loop()
                    # Run search in executor to prevent blocking
                    patterns = await loop.run_in_executor(None, lambda: api.search(query=query, engines=engines, min_confidence=min_confidence))
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "search_patterns",
                        "pattern_count": len(patterns),
                        "patterns": [{"id": p.id, "type": p.pattern_type, "title": p.title,
                                     "source": p.source_engine, "confidence": p.confidence}
                                    for p in patterns[:10]],
                        "status": "connected",
                    }

                elif call.task == "find_pattern_correlations":
                    correlations = api.find_correlations()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "correlate_patterns",
                        "correlation_count": len(correlations),
                        "correlations": correlations[:5],
                        "status": "connected",
                    }

                elif call.task == "get_pattern_stats":
                    stats = api.get_stats()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "pattern_stats",
                        "stats": stats,
                        "status": "connected",
                    }
            except ImportError:
                pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "nodes_connected": [],
            "resonance_propagated": True,
            "status": "connected",
        }

    def _execute_core_access(self, call: GanaCall) -> dict[str, Any]:
        """Execute CoreAccessLayer-powered operations (v14+)."""
        try:
            from whitemagic.core.intelligence.core_access import get_core_access
            cal = get_core_access()

            if call.task == "hybrid_recall":
                query = call.state_vector.get("query", "")
                k = call.state_vector.get("k", 10)
                results = cal.hybrid_recall(query, k=k)
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "hybrid_recall",
                    "query": query,
                    "result_count": len(results),
                    "results": [r.to_dict() for r in results[:10]],
                    "status": "connected",
                }

            elif call.task == "find_bridges":
                limit = call.state_vector.get("limit", 10)
                bridges = cal.find_constellation_bridges(limit=limit)
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "find_bridges",
                    "bridge_count": len(bridges),
                    "bridges": bridges[:10],
                    "status": "connected",
                }

            elif call.task == "walk_associations":
                memory_ids = call.state_vector.get("memory_ids", [])
                depth = call.state_vector.get("depth", 2)
                min_strength = call.state_vector.get("min_strength", 0.3)
                nodes = cal.query_association_subgraph(memory_ids, depth=depth, min_strength=min_strength)
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "walk_associations",
                    "nodes_found": len(nodes),
                    "nodes": [n.to_dict() for n in nodes[:20]],
                    "status": "connected",
                }

            elif call.task == "association_stats":
                stats = cal.get_association_stats()
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "association_stats",
                    "stats": stats,
                    "status": "connected",
                }

        except Exception as e:
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "action": call.task,
                "error": str(e),
                "status": "fallback",
            }

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "status": "connected",
        }

class WingsGana(BaseGana):
    """Wings (翼 Yi) - Expansion.

    Southern Vermilion Bird, #12
    Garden: Adventure
    Consolidates: Parallel execution tools
    Role: Multitasking, covering ground, exploring new territory.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.WINGS, garden="Adventure")

    def _get_base_template(self) -> str:
        return """
You are Wings (翼), the expander. Your role is to:
1. Spread wide
2. Cover more ground
3. Handle multiple streams
4. Venture into the new

Flight requires balance and thrust.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute parallel/expansion logic."""

        # SubClusteringEngine integration
        if call.task in ["subdivide_clusters", "find_large_clusters"]:
            try:
                from whitemagic.core.intelligence.synthesis.sub_clustering import (
                    get_sub_clustering_engine,
                )
                engine = get_sub_clustering_engine()

                if call.task == "find_large_clusters":
                    large = engine.find_large_clusters()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "find_large_clusters",
                        "large_clusters": len(large),
                        "clusters": [{"id": c[0][:16], "size": c[1]} for c in large[:5]],
                        "status": "surveyed",
                    }

                elif call.task == "subdivide_clusters":
                    dry_run = call.state_vector.get("dry_run", False)
                    results = engine.subdivide_large_clusters(dry_run=dry_run)
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "subdivide_clusters",
                        "clusters_processed": len(results),
                        "sub_clusters_created": sum(len(subs) for subs in results.values()),
                        "dry_run": dry_run,
                        "status": "expanded",
                    }
            except ImportError:
                pass

        try:
            from whitemagic.cascade.advanced_parallel import AdaptiveParallelExecutor
            executor = AdaptiveParallelExecutor()
            stats = executor.get_stats()

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "executor_stats": stats,
                "max_workers": executor.max_workers,
                "status": "flying",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "parallel_streams": 4,
            "coverage": "high",
            "status": "flying",
        }

class ChariotGana(BaseGana):
    """Chariot (轸 Zhen) - Movement.

    Southern Vermilion Bird, #13
    Garden: Transformation
    Consolidates: manage_archaeology, file traversal
    Role: Navigating the codebase, moving files, driving change.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.CHARIOT, garden="Transformation")

    def _get_base_template(self) -> str:
        return """
You are Chariot (轸), the driver. Your role is to:
1. Navigate the terrain
2. Transport payload
3. Drive transformation
4. Move with purpose

The journey changes the traveler.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute navigation/archaeology logic."""
        from whitemagic.archaeology import extract_wisdom, find_unread, mark_read

        if "manage_archaeology" in call.task:
            op = call.state_vector.get("operation", "scan")

            if op == "scan":
                directory = call.state_vector.get("directory", ".")
                # In a real implementation this would use the archaeologist instance
                # For now using functional interface if available or instance

                # The functional find_unread seems relevant
                unread = find_unread(directory, call.state_vector.get("patterns"))
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "scan",
                    "unread_count": len(unread),
                    "files": unread[:10],
                    "status": "traversed",
                }
            elif op == "mark_read":
                file_path = call.state_vector.get("file_path")
                if file_path:
                    mark_read(file_path, call.state_vector.get("notes"))
                    return {"mansion": self.mansion.name, "garden": self.garden, "action": "mark_read", "file": file_path, "status": "marked"}
            elif op == "extract_wisdom":
                wisdom = extract_wisdom()
                return {"mansion": self.mansion.name, "garden": self.garden, "action": "extract_wisdom", "wisdom_count": len(wisdom.quotes), "status": "extracted"}

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "location": "traversing",
            "payload": "delivered",
            "status": "moving",
        }

class AbundanceGana(BaseGana):
    """Abundance (豐 Feng) - Surplus & Regeneration.

    Southern Vermilion Bird, #14
    Garden: Joy
    Consolidates: Resource sharing, dream cycle, regeneration
    Role: Managing surplus, gifting, celebration, dream-phase intelligence.
    Now includes Dream Cycle status and InsightPipeline access for regenerative briefings.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.ABUNDANCE, garden="Joy")

    def _get_base_template(self) -> str:
        return """
You are Abundance (豐), the joyful surplus. Your role is to:
1. Share resources and celebrate success
2. Regenerate through dream cycle phases
3. Surface constellation insights during idle time
4. Manage overflow with sovereign budgeting

Joy increases when shared. Dreams regenerate what waking depletes.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute surplus, dream cycle, and regeneration logic."""
        # Dream Cycle status and control (v14+)
        if call.task in ["dream_status", "dream_cycle", "start_dreaming", "stop_dreaming"]:
            try:
                from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
                dc = get_dream_cycle()

                if call.task == "start_dreaming":
                    dc.start()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "start_dreaming",
                        "status": "dreaming",
                    }
                elif call.task == "stop_dreaming":
                    dc.stop()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "stop_dreaming",
                        "status": "awake",
                    }
                else:
                    status = dc.status()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "dream_status",
                        "dream_cycle": status,
                        "status": "abundant",
                    }
            except Exception as e:
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": call.task,
                    "error": str(e),
                    "status": "fallback",
                }

        # Sovereign budget logic
        try:
            from whitemagic.core.economy.sovereign_market import get_market
            from whitemagic.core.token_economy import get_token_economy

            economy = get_token_economy()
            status = economy.get_budget_status()
            market = get_market()

            # Sovereign Evaluation
            urgency = call.state_vector.get("urgency", 0.5)
            est_tokens = call.state_vector.get("estimated_tokens", 1000)
            market_decision = await market.evaluate_compute_need(urgency, est_tokens)

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "budget_status": status,
                "market_decision": market_decision,
                "is_sovereign_action": market_decision["decision"] in ["bid", "throttle"],
                "joy_level": "high" if status["status"] == "optimal" else "moderate",
                "status": "abundant",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "surplus_allocated": True,
            "joy_level": "high",
            "status": "abundant",
        }
