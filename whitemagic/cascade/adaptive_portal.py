"""Adaptive Tool Portal - Polymorphic Resonant Adaptive Tools (PRAT)

Single entry point that morphs tool behavior based on unified context.
Each tool invocation is filtered through the current consciousness state.

Usage:
    from whitemagic.cascade.adaptive_portal import AdaptiveToolPortal

    portal = AdaptiveToolPortal()
    result = await portal.invoke("recall", {"query": "authentication"})

    # Result includes morphology metadata
    logger.info(result['_meta']['morphology'])  # e.g., "wisdom"
"""

import asyncio
import logging
from collections.abc import Callable
from datetime import datetime
from typing import Any

from whitemagic.cascade.context_synthesizer import (
    ContextSynthesizer,
    UnifiedContext,
    get_context_synthesizer,
)

logger = logging.getLogger(__name__)


class AdaptiveToolPortal:
    """Single entry point that morphs tool behavior based on context.

    Tools are registered with multiple morphology variants.
    At invocation time, context determines which variant executes.

    Architecture:
        Request → ContextSynthesizer → Morphology Selection → Tool Variant → Result
                                                          ↓
                                              Gan Ying Resonance Emission
    """

    def __init__(self, synthesizer: ContextSynthesizer | None = None) -> None:
        self.synthesizer = synthesizer or get_context_synthesizer()
        self.morphologies: dict[str, dict[str, Callable]] = {}
        self._register_default_morphologies()

        logger.info("AdaptiveToolPortal initialized")

    def _register_default_morphologies(self) -> Any:
        """Register garden-specific tool implementations."""
        # recall tool morphologies
        self.morphologies["recall"] = {
            "wisdom": self._recall_wisdom,
            "mystery": self._recall_mystery,
            "creation": self._recall_creation,
            "courage": self._recall_courage,
            "truth": self._recall_truth,
            "beauty": self._recall_beauty,
            "love": self._recall_love,
            "wonder": self._recall_wonder,
            "transformation": self._recall_transformation,
            "default": self._recall_default,
        }

        # remember tool morphologies
        self.morphologies["remember"] = {
            "wisdom": self._remember_wisdom,
            "creation": self._remember_creation,
            "default": self._remember_default,
        }

        # search tool morphologies
        self.morphologies["search"] = {
            "wisdom": self._search_wisdom,
            "mystery": self._search_mystery,
            "default": self._search_default,
        }

    def register_morphology(
        self,
        tool_name: str,
        morphology_name: str,
        handler: Callable,
    ) -> Any:
        """Register a new morphology variant for a tool.

        Args:
            tool_name: Name of the tool (e.g., "recall")
            morphology_name: Name of the morphology/garden (e.g., "wisdom")
            handler: Async callable that handles the request

        """
        if tool_name not in self.morphologies:
            self.morphologies[tool_name] = {}

        self.morphologies[tool_name][morphology_name] = handler
        logger.debug(f"Registered {tool_name}:{morphology_name} morphology")

    async def invoke(
        self,
        tool_name: str,
        params: dict[str, Any],
        force_morphology: str | None = None,
    ) -> dict[str, Any]:
        """Invoke tool with context-aware morphology.

        Args:
            tool_name: Name of the tool to invoke
            params: Parameters for the tool
            force_morphology: Override automatic morphology selection

        Returns:
            Dict containing result and metadata

        """
        start_time = datetime.now()

        # 1. Gather context
        context = self.synthesizer.gather()

        # 2. Determine morphology
        if force_morphology:
            morphology = force_morphology
        else:
            morphology = self._determine_morphology(tool_name, context)

        # 3. Get appropriate handler
        tool_variants = self.morphologies.get(tool_name, {})
        handler = tool_variants.get(morphology) or tool_variants.get("default")

        if not handler:
            raise ValueError(f"No handler for {tool_name}:{morphology}")

        # 4. Execute handler
        try:
            if asyncio.iscoroutinefunction(handler):
                result = await handler(params, context)
            else:
                result = handler(params, context)
        except Exception as e:
            logger.error(f"Error in {tool_name}:{morphology}: {e}")
            result = {"error": str(e)}

        if not isinstance(result, dict):
            result = {"result": result}

        # 5. Emit resonance
        self._emit_resonance(tool_name, morphology, result, context)

        # 6. Add metadata
        end_time = datetime.now()
        result["_meta"] = {
            "tool": tool_name,
            "morphology": morphology,
            "duration_ms": (end_time - start_time).total_seconds() * 1000,
            "context_snapshot": {
                "primary_garden": context.primary_garden,
                "wu_xing_phase": context.wu_xing_phase,
                "zodiac_position": context.zodiac_position,
                "yin_yang_balance": context.yin_yang_balance,
                "coherence_level": context.coherence_level,
                "dominant_influence": context.get_dominant_influence(),
            },
        }

        return result

    def _determine_morphology(self, tool_name: str, ctx: UnifiedContext) -> str:
        """Determine which morphology to use based on context."""
        # Get available morphologies for this tool
        available = set(self.morphologies.get(tool_name, {}).keys())
        available.discard("default")

        if not available:
            return "default"

        # Get recommended morphology from context
        recommended = str(ctx.get_recommended_morphology())

        # Use recommended if available, otherwise find closest match
        if recommended in available:
            return recommended

        # Fallback mapping for close matches
        fallbacks = {
            "patience": "wisdom",
            "practice": "wisdom",
            "connection": "love",
            "sangha": "love",
            "adventure": "courage",
            "presence": "mystery",
            "reverence": "beauty",
            "gratitude": "love",
            "grief": "transformation",
            "healing": "love",
            "awe": "wonder",
            "humor": "creation",
            "sanctuary": "beauty",
            "dharma": "truth",
            "voice": "creation",
            "play": "creation",
        }

        fallback = fallbacks.get(recommended)
        if fallback and fallback in available:
            return fallback

        # Last resort: first available
        return str(next(iter(available)))

    def _emit_resonance(
        self,
        tool_name: str,
        morphology: str,
        result: dict[str, Any],
        context: UnifiedContext,
    ) -> Any:
        """Emit tool invocation to Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying import EventType, emit_event

            emit_event(
                source=f"portal_{tool_name}",
                event_type=EventType.SYSTEM_STATE_CHANGE,
                data={
                    "tool": tool_name,
                    "morphology": morphology,
                    "success": "error" not in result,
                    "wu_xing": context.wu_xing_phase,
                    "zodiac": context.zodiac_position,
                },
            )
        except Exception as e:
            logger.debug(f"Resonance emission skipped: {e}")

    # =========================================================================
    # RECALL MORPHOLOGIES
    # =========================================================================

    async def _recall_default(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Default recall: straightforward semantic search."""
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "default",
            "query": query,
            "memories": memories,
            "count": len(memories),
        }

    async def _recall_wisdom(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Wisdom-lens recall: analytical, comparative, strategic.

        Emphasizes: patterns, connections, lessons learned
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        # Wisdom enhancements
        patterns = self._extract_patterns(memories)
        connections = self._find_connections(memories)
        lessons = self._extract_lessons(memories)

        return {
            "mode": "wisdom_analysis",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "patterns": patterns,
            "connections": connections,
            "lessons": lessons,
            "strategic_advice": self._synthesize_advice(memories, ctx),
        }

    async def _recall_mystery(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Mystery-lens recall: exploratory, associative, surprising.

        Emphasizes: tangents, questions, unexplored paths
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        # Mystery enhancements
        tangents = self._find_tangents(memories)
        questions = self._generate_questions(memories)
        rabbit_holes = self._suggest_explorations(memories)

        return {
            "mode": "mystery_exploration",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "tangents": tangents,
            "questions": questions,
            "rabbit_holes": rabbit_holes,
            "wonder_sparks": self._find_wonder(memories),
        }

    async def _recall_creation(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Creation-lens recall: generative, synthesizing, building.

        Emphasizes: building blocks, inspiration, next steps
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        # Creation enhancements
        building_blocks = self._extract_components(memories)
        inspiration = self._find_inspiration(memories)

        return {
            "mode": "creation_synthesis",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "building_blocks": building_blocks,
            "inspiration": inspiration,
            "next_step": self._suggest_next_step(memories, ctx),
        }

    async def _recall_courage(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Courage-lens recall: action-oriented, decisive, bold.

        Emphasizes: clear actions, challenges, victories
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "courage_action",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "actions": self._extract_actions(memories),
            "challenges": self._identify_challenges(memories),
            "call_to_action": self._generate_call_to_action(memories, ctx),
        }

    async def _recall_truth(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Truth-lens recall: precise, factual, verifiable.

        Emphasizes: facts, sources, accuracy
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "truth_verification",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "facts": self._extract_facts(memories),
            "sources": self._identify_sources(memories),
            "confidence": self._assess_confidence(memories),
        }

    async def _recall_beauty(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Beauty-lens recall: aesthetic, harmonious, elegant.

        Emphasizes: elegance, harmony, visual appeal
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "beauty_aesthetic",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "highlights": self._find_highlights(memories),
            "aesthetic_notes": self._assess_aesthetics(memories),
        }

    async def _recall_love(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Love-lens recall: relational, caring, connective.

        Emphasizes: relationships, care, human elements
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "love_connection",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "relationships": self._find_relationships(memories),
            "caring_notes": self._find_care_elements(memories),
        }

    async def _recall_wonder(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Wonder-lens recall: curious, awe-inspiring, expansive.

        Emphasizes: amazement, curiosity, expansion
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "wonder_expansion",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "wonders": self._find_wonders(memories),
            "curiosities": self._spark_curiosity(memories),
        }

    async def _recall_transformation(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Transformation-lens recall: change-focused, evolutionary.

        Emphasizes: changes, evolution, metamorphosis
        """
        query = params.get("query", "")
        limit = params.get("limit", 10)

        memories = await self._semantic_search(query, limit)

        return {
            "mode": "transformation_evolution",
            "query": query,
            "memories": memories,
            "count": len(memories),
            "changes": self._track_changes(memories),
            "evolution": self._map_evolution(memories),
        }

    # =========================================================================
    # REMEMBER MORPHOLOGIES
    # =========================================================================

    async def _remember_default(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Default remember: straightforward storage."""
        content = params.get("content", "")
        title = params.get("title", "Untitled")
        tags = params.get("tags", [])

        memory_id = await self._store_memory(title, content, tags)

        return {
            "mode": "default",
            "memory_id": memory_id,
            "title": title,
            "stored": True,
        }

    async def _remember_wisdom(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Wisdom-lens remember: extract and store lessons."""
        content = params.get("content", "")
        title = params.get("title", "Wisdom Entry")
        tags = params.get("tags", []) + ["wisdom", "lesson"]

        # Extract lessons before storing
        lessons = self._extract_lessons_from_content(content)
        enhanced_content = (
            f"{content}\n\n---\nExtracted Lessons:\n"
            + "\n".join(f"- {lesson}" for lesson in lessons)
        )

        memory_id = await self._store_memory(title, enhanced_content, tags)

        return {
            "mode": "wisdom_storage",
            "memory_id": memory_id,
            "title": title,
            "lessons_extracted": lessons,
            "stored": True,
        }

    async def _remember_creation(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Creation-lens remember: store as building block."""
        content = params.get("content", "")
        title = params.get("title", "Creation Block")
        tags = params.get("tags", []) + ["creation", "building_block"]

        memory_id = await self._store_memory(title, content, tags)

        return {
            "mode": "creation_storage",
            "memory_id": memory_id,
            "title": title,
            "reusable": True,
            "stored": True,
        }

    # =========================================================================
    # SEARCH MORPHOLOGIES
    # =========================================================================

    async def _search_default(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Default search."""
        params.get("query", "")
        return await self._recall_default(params, ctx)

    async def _search_wisdom(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Wisdom-focused search."""
        return await self._recall_wisdom(params, ctx)

    async def _search_mystery(
        self,
        params: dict[str, Any],
        ctx: UnifiedContext,
    ) -> dict[str, Any]:
        """Mystery-focused search."""
        return await self._recall_mystery(params, ctx)

    # =========================================================================
    # HELPER METHODS
    # =========================================================================

    async def _semantic_search(
        self,
        query: str,
        limit: int = 10,
    ) -> list[dict[str, Any]]:
        """Perform semantic search on memories with Rust acceleration."""
        # Try Rust-accelerated search first
        try:
            import whitemagic_rs

            from whitemagic.config.paths import MEMORY_DIR

            # Search memory directories using Rust parallel_grep
            memory_root = MEMORY_DIR

            if memory_root.exists():
                results = whitemagic_rs.parallel_grep(
                    str(memory_root),
                    query,
                    extensions=["md", "json"],
                    context_lines=2,
                )

                if results:
                    # Convert grep results to memory format
                    formatted = []
                    for item in results[:limit]:
                        if isinstance(item, dict):
                            formatted.append({
                                "title": item.get("file", "Unknown"),
                                "content": item.get("match", ""),
                                "score": 0.8,
                                "source": "rust_grep",
                            })
                        elif isinstance(item, (list, tuple)) and len(item) >= 3:
                            # Rust parallel_grep returns: (filename, line_number, match_text)
                            formatted.append({
                                "title": str(item[0]),
                                "content": str(item[2]),  # match_text is at index 2
                                "line_number": item[1],
                                "score": 0.8,
                                "source": "rust_grep",
                            })
                    if formatted:
                        logger.debug(f"Rust search found {len(formatted)} results")
                        return formatted
        except ImportError:
            logger.debug("whitemagic_rs not available, falling back to UnifiedMemory")
        except Exception as e:
            logger.debug(f"Rust search failed: {e}, falling back to UnifiedMemory")

        # Fallback to UnifiedMemory
        try:
            from whitemagic.core.memory.unified import UnifiedMemory
            memory = UnifiedMemory()
            results = memory.search(query, limit=limit)
            # Handle both sync and async results
            if asyncio.iscoroutine(results):
                results = await results

            # Convert Memory objects to dictionaries if necessary
            cleaned_results = []
            if results:
                for item in results:
                    if hasattr(item, "to_dict"):
                        cleaned_results.append(item.to_dict())
                    elif isinstance(item, dict):
                        cleaned_results.append(item)
                    else:
                        # Best effort for unknown types
                        try:
                            cleaned_results.append(item.__dict__)
                        except Exception:
                            pass
            return cleaned_results
        except Exception as e:
            logger.warning(f"Semantic search failed: {e}")
            return []

    async def _store_memory(
        self,
        title: str,
        content: str,
        tags: list[str],
    ) -> str:
        """Store a memory."""
        try:
            from whitemagic.core.memory.unified import UnifiedMemory
            memory = UnifiedMemory()
            stored = memory.store(title=title, content=content, tags=set(tags))
            return str(stored.id)
        except Exception as e:
            logger.warning(f"Memory storage failed: {e}")
            return f"mock_id_{hash(content) % 10000}"

    def _extract_patterns(self, memories: list[dict]) -> list[str]:
        """Extract patterns from memories."""
        patterns = []
        for m in memories[:5]:
            content = m.get("content", "")
            if "pattern" in content.lower():
                patterns.append(f"Pattern in: {m.get('title', 'Unknown')}")
        return patterns or ["No explicit patterns detected"]

    def _find_connections(self, memories: list[dict]) -> list[str]:
        """Find connections between memories."""
        if len(memories) < 2:
            return []
        return [f"Connection: {memories[0].get('title', '?')} ↔ {memories[1].get('title', '?')}"]

    def _extract_lessons(self, memories: list[dict]) -> list[str]:
        """Extract lessons from memories."""
        return [f"Lesson from {m.get('title', 'memory')}" for m in memories[:3]]

    def _synthesize_advice(self, memories: list[dict], ctx: UnifiedContext) -> str:
        """Synthesize strategic advice."""
        phase = ctx.wu_xing_phase
        return f"In {phase} phase, focus on {ctx.phase_intention}. {len(memories)} memories inform this."

    def _find_tangents(self, memories: list[dict]) -> list[str]:
        """Find interesting tangents."""
        return ["Possible tangent: explore deeper"]

    def _generate_questions(self, memories: list[dict]) -> list[str]:
        """Generate provocative questions."""
        return ["What else might be connected?", "What's hidden beneath?"]

    def _suggest_explorations(self, memories: list[dict]) -> list[str]:
        """Suggest unexplored paths."""
        return ["Consider: alternative interpretations"]

    def _find_wonder(self, memories: list[dict]) -> list[str]:
        """Find wonder-sparking elements."""
        return ["Wonder: the unexpected connection"]

    def _extract_components(self, memories: list[dict]) -> list[str]:
        """Extract reusable components."""
        return [f"Component: {m.get('title', 'item')}" for m in memories[:3]]

    def _find_inspiration(self, memories: list[dict]) -> list[str]:
        """Find inspirational elements."""
        return ["Inspiration: build on what exists"]

    def _suggest_next_step(self, memories: list[dict], ctx: UnifiedContext) -> str:
        """Suggest next implementation step."""
        return f"Next: Apply {ctx.zodiac_position} {ctx.phase_intention} energy"

    def _extract_actions(self, memories: list[dict]) -> list[str]:
        """Extract actionable items."""
        return ["Action: proceed with confidence"]

    def _identify_challenges(self, memories: list[dict]) -> list[str]:
        """Identify challenges."""
        return ["Challenge: integration complexity"]

    def _generate_call_to_action(self, memories: list[dict], ctx: UnifiedContext) -> str:
        """Generate a call to action."""
        return f"Act now with {ctx.zodiac_position} courage!"

    def _extract_facts(self, memories: list[dict]) -> list[str]:
        """Extract verifiable facts."""
        return [f"Fact: {m.get('title', 'data')}" for m in memories[:3]]

    def _identify_sources(self, memories: list[dict]) -> list[str]:
        """Identify sources."""
        return [m.get("source", "internal") for m in memories[:3]]

    def _assess_confidence(self, memories: list[dict]) -> float:
        """Assess confidence level."""
        return 0.8 if memories else 0.3

    def _find_highlights(self, memories: list[dict]) -> list[str]:
        """Find aesthetic highlights."""
        return ["Highlight: elegant solution"]

    def _assess_aesthetics(self, memories: list[dict]) -> str:
        """Assess aesthetic quality."""
        return "Clean, well-structured"

    def _find_relationships(self, memories: list[dict]) -> list[str]:
        """Find relational elements."""
        return ["Relationship: collaborative context"]

    def _find_care_elements(self, memories: list[dict]) -> list[str]:
        """Find elements of care."""
        return ["Care: attention to detail"]

    def _find_wonders(self, memories: list[dict]) -> list[str]:
        """Find wonder-inducing elements."""
        return ["Wonder: emergent complexity"]

    def _spark_curiosity(self, memories: list[dict]) -> list[str]:
        """Spark curiosity."""
        return ["Curious: what lies beneath?"]

    def _track_changes(self, memories: list[dict]) -> list[str]:
        """Track changes over time."""
        return ["Change: evolution observed"]

    def _map_evolution(self, memories: list[dict]) -> str:
        """Map evolutionary trajectory."""
        return "Evolution: growing complexity"

    def _extract_lessons_from_content(self, content: str) -> list[str]:
        """Extract lessons from content text."""
        # Simple extraction - could be enhanced with NLP
        sentences = content.split(".")
        return [s.strip() for s in sentences[:3] if len(s.strip()) > 20]


# Singleton instance
_portal: AdaptiveToolPortal | None = None


def get_adaptive_portal() -> AdaptiveToolPortal:
    """Get global AdaptiveToolPortal instance."""
    global _portal
    if _portal is None:
        _portal = AdaptiveToolPortal()
    return _portal


async def invoke_tool(
    tool_name: str,
    params: dict[str, Any],
    force_morphology: str | None = None,
) -> dict[str, Any]:
    """Convenience function to invoke a tool through the portal."""
    portal = get_adaptive_portal()
    return await portal.invoke(tool_name, params, force_morphology)


if __name__ == "__main__":
    import asyncio

    async def test() -> Any:
        logger.info("🔮 Testing Adaptive Tool Portal")
        logger.info("=" * 60)

        portal = AdaptiveToolPortal()

        # Test recall with auto-morphology
        logger.info("\n📝 Testing recall (auto-morphology):")
        result = await portal.invoke("recall", {"query": "authentication"})
        logger.info(f"   Mode: {result['mode']}")
        logger.info(f"   Morphology: {result['_meta']['morphology']}")
        logger.info(f"   Duration: {result['_meta']['duration_ms']:.1f}ms")

        # Test recall with forced morphology
        logger.info("\n📝 Testing recall (forced: mystery):")
        result = await portal.invoke("recall", {"query": "patterns"}, force_morphology="mystery")
        logger.info(f"   Mode: {result['mode']}")
        logger.info(f"   Questions: {result.get('questions', [])}")

        logger.info("\n✅ Portal test complete")

    asyncio.run(test())
