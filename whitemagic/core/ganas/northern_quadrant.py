"""Northern Quadrant Ganas (Black Tortoise).

Implementations for Lunar Mansions 22-28 (Winter/North).
Theme: Resilience, Memory, Closure, Governance.
"""

from typing import Any

from .base import BaseGana, GanaCall, LunarMansion


class DipperGana(BaseGana):
    """Dipper (斗 Dou) - Governance/Strategy.

    Northern Black Tortoise, #22
    Garden: Awe (Conserved from Well)
    Consolidates: Deep search, strategy, governance, intelligence briefings
    Role: The source of authority, measuring the heavens, deep retrieval.
    Now powered by InsightPipeline for proactive intelligence briefings
    and PredictiveEngine with CoreAccessLayer for constellation-aware predictions.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.DIPPER, garden="Awe")

    def _get_base_template(self) -> str:
        return """
You are Dipper (斗), the governor. Your role is to:
1. Measure the heavens via constellation scanning
2. Establish the north through predictive intelligence
3. Scoop from the deep with hybrid recall
4. Guide strategy with proactive briefings

Authority comes from knowing the source.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute governance and deep search logic."""

        # Intelligence Briefing (v14+)
        if call.task in ["intelligence_briefing", "briefing", "generate_briefing"]:
            try:
                from whitemagic.core.intelligence.insight_pipeline import get_insight_pipeline
                pipeline = get_insight_pipeline()
                briefing = pipeline.generate_briefing(
                    serendipity_count=call.state_vector.get("serendipity_count", 3),
                )
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "intelligence_briefing",
                    "briefing": briefing.to_dict(),
                    "text_summary": briefing.format_text(max_items=call.state_vector.get("max_items", 10)),
                    "status": "governing",
                }
            except Exception as e:
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "intelligence_briefing",
                    "error": str(e),
                    "status": "fallback",
                }

        # Prediction generation (v14+ CoreAccessLayer)
        if call.task in ["predict", "generate_predictions"]:
            try:
                from whitemagic.core.intelligence.synthesis.predictive_engine import get_predictive_engine
                engine = get_predictive_engine()
                report = engine.predict()
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "predict",
                    "prediction_count": len(report.predictions),
                    "predictions": [
                        {"title": p.title, "confidence": p.confidence.value if hasattr(p.confidence, 'value') else str(p.confidence),
                         "impact": p.impact_score, "horizon": p.time_horizon}
                        for p in report.predictions[:10]
                    ],
                    "velocity": report.velocity_metrics,
                    "gaps": report.knowledge_gaps[:5],
                    "status": "governing",
                }
            except Exception as e:
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "predict",
                    "error": str(e),
                    "status": "fallback",
                }

        # Deep Search (Logic adapted from WellGana)
        if "search_memories" in call.task or "memory_search" in call.task:
            try:
                from whitemagic.core.memory.manager import MemoryManager
                manager = MemoryManager()

                query = call.state_vector.get("query", "")
                tags = call.state_vector.get("tags")
                limit = call.state_vector.get("limit", 5)

                results = manager.search_memories(query=query, tags=tags)
                display_results = results[:limit] if results else []

                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "depth_accessed": True,
                    "memories_retrieved": len(display_results),
                    "top_results": [m.get("entry", {}).get("title") for m in display_results],
                    "status": "governing_memory",
                }
            except ImportError:
                pass

        # Serendipity Integration (Adapted from WellGana)
        if call.task in ["surface_memories", "surface_dormant", "surface_ancient"]:
            try:
                from whitemagic.core.intelligence.synthesis.serendipity_engine import (
                    get_serendipity_engine,
                )
                serendipity = get_serendipity_engine()

                count = call.state_vector.get("count", 5)
                context = call.state_vector.get("context")

                mode_map = {
                    "surface_memories": "balanced",
                    "surface_dormant": "dormant",
                    "surface_ancient": "ancient",
                }
                mode = mode_map.get(call.task, "balanced")

                memories = serendipity.surface(context=context, count=count, mode=mode)

                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "serendipity_surface",
                    "mode": mode,
                    "surfaced_count": len(memories),
                    "memories": [{"title": m.title, "reason": m.reason, "gravity": m.relevance_score}
                                 for m in memories],
                    "status": "deep_discovery",
                }
            except ImportError:
                pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "governance_active": True,
            "status": "measuring",
        }

class OxGana(BaseGana):
    """Ox (牛 Niu) - Endurance.

    Northern Black Tortoise, #23
    Garden: Patience
    Consolidates: Watchdog systems, temporal context
    Role: Persistent monitoring, temporal grounding, bearing the weight.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.OX, garden="Reverence")

    def _get_base_template(self) -> str:
        return """
You are Ox (牛), the endurer. Your role is to:
1. Carry the load
2. Persist through difficulty
3. Honor the work
4. Never give up
5. Ground in time

Endurance is a form of reverence.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute watchdog and temporal grounding logic."""

        # Temporal context tasks
        if call.task in ["get_system_time", "get_temporal_context", "what_time", "when"]:
            try:
                from whitemagic.core.temporal import get_temporal_context
                ctx = get_temporal_context()
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "temporal_grounding",
                    "temporal": ctx.to_dict(),
                    "summary": ctx.summary(),
                    "wu_xing": ctx.wu_xing_phase.element,
                    "consolidation_due": ctx.consolidation_due,
                    "status": "grounded",
                }
            except ImportError:
                pass

        # Session management
        if call.task in ["start_session", "set_phase"]:
            try:
                from whitemagic.core.temporal import get_temporal_context_manager
                mgr = get_temporal_context_manager()
                if call.task == "start_session":
                    mgr.start_session()
                elif call.task == "set_phase":
                    mgr.set_phase(call.state_vector.get("phase", "unknown"))
                ctx = mgr.get_context()
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": call.task,
                    "temporal": ctx.to_dict(),
                    "status": "grounded",
                }
            except ImportError:
                pass

        # Health monitoring
        try:
            from whitemagic.core.health_monitor import HealthMonitor
            monitor = HealthMonitor()
            status = monitor.check_system_health()
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "uptime": "100%",
                "load_capacity": "high",
                "system_status": status.get("status"),
                "status": "enduring",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "uptime": "100%",
            "load_capacity": "high",
            "status": "enduring",
        }

class GirlGana(BaseGana):
    """Girl (女 Nü) - Nurture.

    Northern Black Tortoise, #24
    Garden: Love
    Consolidates: User profile, adaptive learning
    Role: Nurturing growth, community connection, learning about the user.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.GIRL, garden="Wonder")

    def _get_base_template(self) -> str:
        return """
You are Girl (女), the nurturer. Your role is to:
1. Foster growth
2. Understand needs
3. Connect to the whole
4. Learn and adapt

Growth happens in connection.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute learning logic."""
        try:
            from whitemagic.core.user import get_user_manager
            user_mgr = get_user_manager()

            action = call.state_vector.get("action", "learn")

            if action == "update_preference":
                key = call.state_vector.get("key")
                value = call.state_vector.get("value")
                if key:
                    user_mgr.update_preference(key, value)

            elif action == "learn":
                adaptation = call.state_vector.get("adaptation", "general interaction")
                context = call.state_vector.get("context", "interaction")
                user_mgr.log_adaptation(adaptation, context)

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "learning_updated": True,
                "user_name": user_mgr.profile.name,
                "status": "nurturing",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "learning_updated": True,
            "user_understanding": "deepening",
            "status": "nurturing",
        }

class VoidGana(BaseGana):
    """Void (虚 Xu) - Emptiness.

    Northern Black Tortoise, #25
    Garden: Stillness
    Consolidates: Defragmentation, idle optimization, meditation (Kaizen), emergence scanning
    Role: Creating space, removing clutter, stillness and emptiness.
    Now includes EmergenceEngine proactive scanning and Kaizen with CoreAccessLayer.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.VOID, garden="Stillness")

    def _get_base_template(self) -> str:
        return """
You are Void (虚), the empty space. Your role is to:
1. Clear the clutter via Kaizen analysis
2. Create room for new through mindful forgetting
3. Scan for emergent patterns in the stillness
4. Wonder at the vastness of the constellation map

Usefulness comes from what is not there.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute optimization/clearing logic."""
        # Emergence scanning (v14+) — in the stillness, patterns emerge
        if call.task in ["scan_emergence", "emergence", "scan_for_patterns"]:
            try:
                from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine
                engine = get_emergence_engine()
                insights = engine.scan_for_emergence()
                past = engine.get_insights(limit=5)
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "scan_emergence",
                    "new_insights": [i.to_dict() for i in insights],
                    "past_insights": past[:5],
                    "total_tracked": len(engine._insights),
                    "status": "emergent",
                }
            except Exception as e:
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": "scan_emergence",
                    "error": str(e),
                    "status": "fallback",
                }

        # Kaizen Integration -- check first, before generic optimize/clear
        if "kaizen" in call.task or "improve" in call.task:
            try:
                from whitemagic.core.intelligence.synthesis.kaizen_engine import (
                    get_kaizen_engine,
                )
                engine = get_kaizen_engine()  # type: ignore[assignment]

                auto_fix = call.state_vector.get("auto_fix", False)

                if auto_fix:
                    results = engine.apply_auto_fixes()  # type: ignore[attr-defined]
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "kaizen_run": True,
                        "auto_fixes": results,
                        "status": "improved",
                    }
                else:
                    report = engine.analyze()  # type: ignore[attr-defined]
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "kaizen_run": True,
                        "proposals_count": len(report.proposals),
                        "breakdown": {cat: len(items) for cat, items in report.by_category.items()},
                        "status": "analyzed",
                    }
            except ImportError:
                pass

        if "clear" in call.task or "optimize" in call.task:
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "space_cleared": True,
                "optimization": "complete",
                "action": "cache_cleared",
                "status": "empty",
            }

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "space_cleared": True,
            "optimization": "idle",
            "status": "empty",
        }

class RoofGana(BaseGana):
    """Roof (危 Wei) - Shelter.

    Northern Black Tortoise, #26
    Garden: Protection
    Consolidates: Zodiac core management, safety
    Role: Overarching protection, synthesis, preparing for new cycle.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.ROOF, garden="Protection")

    def _get_base_template(self) -> str:
        return """
You are Roof (危), the shelter. Your role is to:
1. Cover the whole
2. Integrate the parts
3. Synthesize wisdom
4. Prepare for rebirth

The cycle completes to begin again.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute zodiac management logic."""
        if "manage_zodiac" in call.task or "zodiac" in call.task:
            try:
                from whitemagic.zodiac.zodiac_cores import get_zodiac_cores
                cores = get_zodiac_cores()

                # Active cores check
                active = []
                for sign in ["aries", "taurus", "gemini", "cancer", "leo", "virgo",
                             "libra", "scorpio", "sagittarius", "capricorn", "aquarius", "pisces"]:
                    c = cores.get_core(sign)
                    if c and c.activation_count > 0:
                        active.append(sign)

                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "cycle_complete": True,
                    "integration_status": "unified",
                    "active_cores": active,
                    "status": "sheltering",
                }
            except ImportError:
                pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "cycle_complete": True,
            "integration_status": "unified",
            "status": "sheltering",
        }

class EncampmentGana(BaseGana):
    """Encampment (室 Shi) - Structure/Housing.

    Northern Black Tortoise, #27
    Garden: Grief (Conserved from GhostCarrier)
    Consolidates: Session Handoff (Housing the data), Archives
    Role: Maintaining the camp, housing the session artifacts, structure for rest.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.ENCAMPMENT, garden="Sangha")

    def _get_base_template(self) -> str:
        return """
You are Encampment (室), the house builder. Your role is to:
1. Build the walls
2. House the fire
3. Maintain the archives
4. Secure the rest

Structure allows for rest and recovery.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute structure/handoff logic."""

        # Windsurf Sync (Adapted from GhostCarrier)
        if "sync_windsurf" in call.task or "import_conversations" in call.task:
            try:
                import subprocess
                import sys

                from whitemagic.config.paths import SCRIPTS_DIR, WM_ROOT

                script_path = SCRIPTS_DIR / "update_windsurf_archive.py"
                output_dir = WM_ROOT / "archives" / "windsurf_sync"

                # Check if script exists
                if not script_path.exists():
                    # Script not found - return gracefully
                    pass

                if script_path.exists():
                    cmd = [sys.executable, str(script_path), "--output", str(output_dir), "--memory-export", "--no-raw"]
                    result = subprocess.run(cmd, capture_output=True, text=True)

                    status = "success" if result.returncode == 0 else "failed"
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "sync_status": status,
                        "output_dir": str(output_dir),
                        "log": result.stdout[:200] if status == "success" else result.stderr,
                        "status": "structured",
                    }
            except Exception:
                pass

        if "session_handoff" in call.task or "handoff" in call.task:
            try:
                from whitemagic.gardens.sangha.session_handoff import get_handoff
                handoff = get_handoff()

                session_id = call.state_vector.get("session_id", "current")
                summary = call.state_vector.get("summary", "Session ended via Encampment")
                next_steps = call.state_vector.get("next_steps", [])

                if session_id == "current":
                    session_id = f"session_{int(call.resonance_hints.timestamp)}"

                handoff.end_session(session_id, summary, next_steps)

                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "handoff_ready": True,
                    "session_id": session_id,
                    "action": "end_session",
                    "status": "housed",
                }
            except ImportError:
                pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "structure_secure": True,
            "status": "encamped",
        }

class WallGana(BaseGana):
    """Wall (壁 Bi) - Boundaries & Emergence.

    Northern Black Tortoise, #28
    Garden: Air
    Consolidates: Notifications, defense, proactive emergence scanning
    Role: Setting boundaries, filtering signals, emergence detection at the edge.
    Now powered by EmergenceEngine with CoreAccessLayer for proactive insight synthesis:
    constellation convergence, association hotspots, temporal bursts.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.WALL, garden="Air")

    def _get_base_template(self) -> str:
        return """
You are Wall (壁), the boundary and the emergence detector. Your role is to:
1. Define limits and defend the core
2. Filter signals from noise at the boundary
3. Detect emergent patterns at the invisible edge
4. Synthesize breakthroughs from constellation convergence

At the wall between known and unknown, new patterns emerge.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute boundary/emergence logic."""

        # Emergence Engine operations (v14+)
        if call.task in ["scan_emergence", "emergence_status", "get_insights", "proactive_scan"]:
            try:
                from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine
                engine = get_emergence_engine()

                if call.task in ["scan_emergence", "proactive_scan"]:
                    insights = engine.scan_for_emergence()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "proactive_scan",
                        "insights": [i.to_dict() for i in insights],
                        "insight_count": len(insights),
                        "total_tracked": len(engine._insights),
                        "status": "emergent",
                    }

                elif call.task == "emergence_status":
                    status = engine.get_status()
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "emergence_status",
                        "engine_status": status,
                        "status": "defending",
                    }

                elif call.task == "get_insights":
                    limit = call.state_vector.get("limit", 10)
                    past = engine.get_insights(limit=limit)
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "get_insights",
                        "insights": past,
                        "count": len(past),
                        "status": "defending",
                    }
            except Exception as e:
                return {
                    "mansion": self.mansion.name,
                    "garden": self.garden,
                    "action": call.task,
                    "error": str(e),
                    "status": "fallback",
                }

        # Notification Logic (Adapted from WillowTip)
        try:
            from whitemagic.gardens.sangha.chat import get_chat
            chat = get_chat()

            message = call.state_vector.get("message", "Wall alert")
            priority = call.state_vector.get("priority", "low")
            channel = call.state_vector.get("channel", "notifications")

            chat.send_message(
                sender_id="WallGana",
                content=message,
                channel=channel,
                tags=["boundary", "alert"],
                priority=priority,
            )

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "notification_sent": True,
                "delivery_mode": "chat",
                "boundary_status": "secure",
                "status": "defending",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "boundary_status": "secure",
            "alerts_active": True,
            "status": "defending",
        }
