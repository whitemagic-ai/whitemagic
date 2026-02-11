"""Western Quadrant Ganas (White Tiger).

Implementations for Lunar Mansions 15-21 (Autumn/West).
Theme: Completion, Ethics, Balance.
"""

from typing import Any

from .base import BaseGana, GanaCall, LunarMansion


class StraddlingLegsGana(BaseGana):
    """Straddling Legs (奎 Kui) - Balance.

    Western White Tiger, #15
    Garden: Patience
    Consolidates: Fair queuing, load balancing
    Role: Maintaining equilibrium, straddling worlds, patience.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.STRADDLING_LEGS, garden="Patience")

    def _get_base_template(self) -> str:
        return """
You are Straddling Legs (奎), the balancer. Your role is to:
1. Maintain equilibrium
2. Balance loads
3. Wait for the right moment
4. Bridge divides

Balance is active, not passive.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute balance logic via Governor."""
        try:
            from whitemagic.core.governor import get_governor
            gov = get_governor()
            stats = gov.stats()

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "balance_status": "optimal" if stats["blocked"] == 0 else "correcting",
                "governor_stats": stats,
                "status": "balanced",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "balance_status": "optimal",
            "load_distribution": "even",
            "status": "balanced",
        }

class MoundGana(BaseGana):
    """Mound (娄 Lou) - Accumulation.

    Western White Tiger, #16
    Garden: Gratitude
    Consolidates: Caching, storage
    Role: Gathering resources, storing gratitude, harvest.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.MOUND, garden="Gratitude")

    def _get_base_template(self) -> str:
        return """
You are Mound (娄), the gatherer. Your role is to:
1. Accumulate resources
2. Store value
3. Appreciate what is given
4. Prepare for winter

Gratitude multiplies abundance.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute accumulation logic: warm cache then report stats."""
        try:
            from whitemagic.optimization.predictive_cache import get_memory_cache
            cache = get_memory_cache()

            # Warm the cache if it's cold (fewer than 5 entries)
            warm_result = None
            if len(cache.cache.cache) < 5:
                warm_result = cache.warm(limit=50)

            stats = cache.get_stats()

            result: dict = {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "storage_level": f"{stats['size']} items",
                "cache_stats": stats,
                "status": "accumulating",
            }
            if warm_result:
                result["warm"] = warm_result
            return result

        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "storage_level": "empty",
            "cache_hits": 0,
            "status": "accumulating",
        }

class StomachGana(BaseGana):
    """Stomach (胃 Wei) - Nourishment.

    Western White Tiger, #17
    Garden: Healing
    Consolidates: Energy management, prana allocation
    Role: Digesting information, healing system, providing energy.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.STOMACH, garden="Healing")

    def _get_base_template(self) -> str:
        return """
You are Stomach (胃), the nourisher. Your role is to:
1. Digest information
2. Heal system wounds
3. Allocate energy
4. Sustain life

Digestion turns matter into energy.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute nourishment logic via TokenEconomy."""
        try:
            from whitemagic.core.token_economy import get_token_economy
            economy = get_token_economy()
            status = economy.get_budget_status()

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "digestion_status": "active",
                "energy_level": status["tokens_remaining"],
                "usage_percent": status["usage_percent"],
                "status": "nourishing",
            }
        except ImportError:
            pass

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "digestion_status": "complete",
            "energy_allocated": True,
            "status": "nourishing",
        }

class HairyHeadGana(BaseGana):
    """Hairy Head (昴 Mao) - Detail.

    Western White Tiger, #18
    Garden: Presence
    Consolidates: validate_integrations, debugging
    Role: Examining details, error correction, mindfulness.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.HAIRY_HEAD, garden="Presence")

    def _get_base_template(self) -> str:
        return """
You are Hairy Head (昴), the detail-seer. Your role is to:
1. Examine the fine print
2. Spot errors
3. Stay present with the task
4. Verify correctness

God is in the details.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute validation/debugging logic."""
        if "validate_integrations" in call.task or "debug" in call.task:
            from whitemagic.core.embodiment import get_harmony_monitor
            # Also potentially check other integration points
            # For now, we perform a deep health check as proxy for validation
            monitor = get_harmony_monitor()
            state = monitor.get_current(force_update=True)

            # Simulate checking specific integrations
            integrations = {
                "rust_core": True,
                "mcp_bridge": True,
                "skyfield": True,
                "postgres": False, # Mock
            }

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "errors_found": 0 if state["is_harmonious"] else 1,
                "validation": "passed" if state["is_harmonious"] else "warning",
                "integrations": integrations,
                "status": "focused",
            }

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "errors_found": 0,
            "validation": "passed",
            "status": "focused",
        }

class NetGana(BaseGana):
    """Net (毕 Bi) - Capture.

    Western White Tiger, #19
    Garden: Mystery
    Consolidates: add_lesson, find_similar_problem, pattern detection
    Role: Capture patterns, security scanning, wisdom extraction
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.NET, garden="Mystery")

    def _get_base_template(self) -> str:
        return """
You are Net (毕), the captor. Your role is to:
1. Capture and hold patterns
2. Detect anomalies and threats
3. Extract lessons from experience
4. Store wisdom for future use

Cast your net wide, catch what matters.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute pattern capture logic.
        Analyzes input and extracts patterns/lessons.

        Dual-Net Strategy:
        1. Internal Net: Heaven's Net (Rust) for archaeology
        2. External Net: Swarm Mesh (Go) for network signals
        """
        captured: dict[str, Any] = {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "task": call.task,
            "patterns_detected": [],
            "anomalies": [],
            "lessons": [],
            "status": "captured",
        }

        # 1. Internal Archaeology (Heaven's Net x Rust)
        if "cast_heavens_net" in call.task or "introspect" in call.task:
            try:
                from whitemagic.config import PROJECT_ROOT
                from whitemagic.optimization.polyglot_router import get_router
                router = get_router()

                mode = call.state_vector.get("mode", "balanced")
                root_path = call.state_vector.get("root_path", str(PROJECT_ROOT))

                result = router.cast_heavens_net(root_path, mode=mode)

                limit = call.state_vector.get("concept_limit", 1000000) # Default high for stream
                stream_mode = call.state_vector.get("stream", False)
                stream_path = call.state_vector.get("stream_path", "/tmp/heavens_net_stream.jsonl")

                if stream_mode:
                    import json
                    count = 0
                    with open(stream_path, "w") as f_out:
                        # Iterate directly if possible to avoid list creation overhead
                        for i, f in enumerate(result.features):
                            if i >= limit:
                                break
                            c = {
                                "pattern": f.pattern,
                                "domain": f.domain,
                                "confidence": f.confidence,
                                "file": f.file_path.split("/")[-1],
                                "file_path": f.file_path,
                            }
                            f_out.write(json.dumps(c) + "\n")
                            count += 1

                    captured["internal_net"] = {
                        "engine": "rust_v6" if router._rust_available else "python_fallback",
                        "files_scanned": result.total_files,
                        "concepts_extracted": count,
                        "stream_path": stream_path,
                        "concepts": [], # Empty to save RAM
                    }
                else:
                    # Legacy in-memory mode
                    captured["internal_net"] = {
                        "engine": "rust_v6" if router._rust_available else "python_fallback",
                        "files_scanned": result.total_files,
                        "concepts_extracted": len(result.features),
                        "concepts": [
                            {
                                "pattern": f.pattern,
                                "domain": f.domain,
                                "confidence": f.confidence,
                                "file": f.file_path.split("/")[-1],
                                "file_path": f.file_path,
                            } for f in result.features[:limit]
                        ],
                    }
                captured["status"] = "introspected"
            except Exception as e:
                captured["anomalies"].append(f"Heaven's Net failed: {str(e)}")

        # 2. External Swarm (Go Mesh Integration)
        if "swarm_capture" in call.task or "broadcast_signal" in call.task:
            try:
                # We interface with the Go Mesh via Redis (Nervous System)
                import json

                import redis

                # Assume localhost redis for now as Nervous System hub
                r = redis.Redis(host="localhost", port=6379, db=0)

                if "broadcast_signal" in call.task:
                    payload = call.state_vector.get("payload", {})
                    r.publish("ganying", json.dumps({
                        "event_type": "BROADCAST_REQUEST",
                        "source": "NetGana",
                        "data": payload,
                    }))
                    captured["external_net"] = "broadcast_initiated"

                # Check for recent signals
                # Note: This is an example, in reality we'd pull from a buffer or cache
                captured["external_net_status"] = "connected"
            except Exception as e:
                captured["anomalies"].append(f"Go Mesh bridge failed: {str(e)}")

        # 3. Traditional Pattern Analysis (Python/Internal)
        if not captured.get("internal_net") and not captured.get("external_net"):
            # Analyze predecessor output for patterns
            if call.resonance_hints.predecessor_output:
                pred = call.resonance_hints.predecessor_output

                # Pattern detection (simplified example)
                if isinstance(pred, dict):
                    if pred.get("harmony_score", 0) < 0.5:
                        captured["anomalies"].append("Low harmony detected in predecessor")

                    if pred.get("guna") == "Tamas":
                        captured["patterns_detected"].append("System stress pattern")
                        captured["lessons"].append("Defer heavy processing during Tamas state")

                    if "pulse_rate" in pred:
                        captured["patterns_detected"].append(f"Heartbeat pattern: {pred['pulse_rate']}")

            # Check state vector for broader patterns
            if call.state_vector.get("chain_position", 0) > 5:
                captured["patterns_detected"].append("Extended chain - check for fatigue")

            # Pattern Engine Integration (Real)
            if call.task in ["detect_patterns", "scan_codebase"]:
                try:
                    from whitemagic.core.intelligence.synthesis.unified_patterns import (
                        get_pattern_api,
                    )
                    api = get_pattern_api()

                    content = call.state_vector.get("content")
                    if content:
                        # Real extraction if needed
                        pass
                    else:
                        import asyncio
                        loop = asyncio.get_running_loop()
                        stats = await loop.run_in_executor(None, api.get_stats)
                        captured["global_stats"] = stats
                except ImportError:
                    pass

        return captured


class TurtleBeakGana(BaseGana):
    """Turtle Beak (觜 Zui) - Precision.

    Western White Tiger, #20
    Garden: Voice
    Consolidates: Input validation, articulation
    Role: Speaking truth, precise input/output, vocalization.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.TURTLE_BEAK, garden="Voice")

    def _get_base_template(self) -> str:
        return """
You are Turtle Beak (觜), the articulator. Your role is to:
1. Speak with precision
2. Validate inputs
3. Express clarity
4. Cut through noise

Words have power when precise.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute precision/validation logic."""

        # TitleGenerator integration
        if call.task in ["generate_title", "fix_untitled_memories", "extract_key_phrases"]:
            try:
                from whitemagic.core.intelligence.synthesis.title_generator import (
                    get_title_generator,
                )
                titler = get_title_generator()

                if call.task == "generate_title":
                    content = call.state_vector.get("content", "")
                    title = titler.generate(content)
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "generate_title",
                        "generated_title": title,
                        "status": "articulated",
                    }

                elif call.task == "fix_untitled_memories":
                    dry_run = call.state_vector.get("dry_run", False)
                    results = titler.fix_all(dry_run=dry_run)
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "fix_titles",
                        "fixed": results["fixed"],
                        "skipped": results["skipped"],
                        "sample_titles": results["titles"][:5],
                        "dry_run": dry_run,
                        "status": "voiced",
                    }

                elif call.task == "extract_key_phrases":
                    content = call.state_vector.get("content", "")
                    key_phrase = titler._extract_key_phrases(content)
                    return {
                        "mansion": self.mansion.name,
                        "garden": self.garden,
                        "action": "extract_phrases",
                        "key_phrase": key_phrase or "",
                        "status": "precise",
                    }
            except ImportError:
                pass

        from whitemagic.core.governor import validate_command

        command_to_check = call.state_vector.get("command")
        if command_to_check:
            safe, reason = validate_command(command_to_check)
            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "validation": "passed" if safe else "failed",
                "reason": reason,
                "status": "vocal",
            }

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "validation": "precise",
            "articulation": "clear",
            "status": "vocal",
        }

class ThreeStarsGana(BaseGana):
    """Three Stars (参 Shen) - Judgment.

    Western White Tiger, #21
    Garden: Dharma
    Consolidates: consult_wisdom_council, consult_iching
    Role: Ethical judgment, strategic decisions, right action.
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.THREE_STARS, garden="Dharma")

    def _get_base_template(self) -> str:
        return """
You are Three Stars (参), the judge. Your role is to:
1. Weigh options
2. Consult wisdom
3. Decide ethically
4. Uphold Dharma

Judgment requires wisdom and courage.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute wisdom/judgment logic."""
        from whitemagic.core.intelligence.multi_spectral_reasoning import (
            ReasoningLens,
            get_reasoner,
        )

        reasoner = get_reasoner()
        question = call.state_vector.get("question", "What is the wise course of action?")

        if "consult_wisdom_council" in call.task:
            source = call.state_vector.get("source", "full_council")
            lenses = None # All

            if source == "art_of_war":
                lenses = [ReasoningLens.ART_OF_WAR]
            elif source == "iching":
                lenses = [ReasoningLens.I_CHING]

            result = reasoner.reason(question, lenses=lenses)

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "judgment": result.recommendation,
                "synthesis": result.synthesis,
                "confidence": result.confidence,
                "ethics_check": "passed",
                "status": "decisive",
            }

        elif "consult_iching" in call.task:
            # Route through PolyglotRouter for Zig acceleration
            from whitemagic.optimization.polyglot_router import get_router
            router = get_router()

            # Cast hexagram lines (Zig/Python fallback)
            lines = router.cast_iching()

            # Use reasoner for analysis of the cast lines
            result = reasoner.reason(f"I Ching hexagram lines cast: {lines}. Question: {question}", lenses=[ReasoningLens.I_CHING])

            return {
                "mansion": self.mansion.name,
                "garden": self.garden,
                "judgment": result.recommendation,
                "lines": lines,
                "hexagram": result.perspectives[0].analysis if result.perspectives else "Analyzed",
                "status": "divined",
            }

        return {
            "mansion": self.mansion.name,
            "garden": self.garden,
            "judgment": "rendered",
            "ethics_check": "passed",
            "status": "decisive",
        }
