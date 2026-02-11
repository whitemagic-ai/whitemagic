
from typing import Any, cast


def apply_reasoning_methods(
    method: str,
    problem: str | None = None,
    objective: str | None = None,
    reasoning_text: str | None = None,
    clone_results: list[dict[str, Any]] | None = None,
    methods: list[str] | None = None,
    clone_count: int = 5,
    **kwargs: Any,
) -> dict[str, Any]:
    """Apply advanced reasoning methods."""
    method = (method or "").lower()

    if method == "multi_spectral":
        from whitemagic.core.intelligence.multi_spectral_reasoning import (
            MultiSpectralReasoner,
            ReasoningLens,
        )

        reasoner = MultiSpectralReasoner()
        lenses = None
        if methods:
            lens_map = {lens.value: lens for lens in ReasoningLens}
            lenses = [lens_map[m] for m in methods if m in lens_map]

        result = reasoner.reason(question=problem or objective or "Apply multi-spectral reasoning", lenses=lenses)
        return {
            "question": getattr(result, "question", problem),
            "synthesis": getattr(result, "synthesis", ""),
            "recommendation": getattr(result, "recommendation", ""),
            "confidence": getattr(result, "confidence", 0.0),
            "perspectives": getattr(result, "perspectives", []),
            "timestamp": getattr(result, "timestamp", "unknown"),
        }

    if method == "thought_clones":
        import asyncio

        from whitemagic.edge.thought_clones_async import AsyncThoughtCloneArmy

        prompt = problem or objective or "Explore this problem"
        army = AsyncThoughtCloneArmy()
        exploration = asyncio.run(army.parallel_explore(prompt, num_clones=clone_count))
        return {
            "prompt": prompt,
            "best_path": {
                "strategy": exploration.strategy,
                "content": exploration.content,
                "confidence": exploration.confidence,
                "tokens": exploration.tokens,
                "clone_id": exploration.clone_id,
                "duration_ms": exploration.duration_ms,
                "metadata": exploration.metadata,
            },
            "stats": getattr(army, "_stats", {}),
        }

    if method == "synthesize":
        if not clone_results:
            return {"error": "clone_results are required for synthesize"}
        # Simple synthesis: pick highest-confidence results and combine
        ranked = sorted(clone_results, key=lambda item: item.get("confidence", 0.0), reverse=True)
        top = ranked[:3]
        synthesis = "\n".join(item.get("content", "") for item in top if item.get("content"))
        return {
            "summary": synthesis,
            "top_results": top,
            "count": len(clone_results),
        }

    if method == "detect_biases":
        from whitemagic.ai.safety import BiasDetector

        detector = BiasDetector()
        return cast(dict[str, Any], detector.analyze(reasoning_text or ""))

    return {"error": f"Unknown reasoning method: {method}"}
