
from typing import Any


def consult_full_council(question: str, urgency: str = "normal", **kwargs: Any) -> dict[str, Any]:
    """Consult the full wisdom council using MultiSpectralReasoner."""
    from whitemagic.core.intelligence.multi_spectral_reasoning import (
        MultiSpectralReasoner,
    )

    reasoner = MultiSpectralReasoner()
    result = reasoner.reason(question=question)

    return {
        "question": question,
        "urgency": urgency,
        "perspectives": getattr(result, "perspectives", []),
        "recommendation": getattr(result, "recommendation", ""),
        "synthesis": getattr(result, "synthesis", ""),
        "confidence": getattr(result, "confidence", 0.0),
        "timestamp": getattr(result, "timestamp", "unknown"),
    }


def consult_art_of_war(question: str, situation: str = "", urgency: str = "normal", **kwargs: Any) -> dict[str, Any]:
    """Consult Art of War via the strategic lens in MultiSpectralReasoner."""
    from whitemagic.core.intelligence.multi_spectral_reasoning import (
        MultiSpectralReasoner,
        ReasoningLens,
    )

    reasoner = MultiSpectralReasoner()
    result = reasoner.reason(question=question, lenses=[ReasoningLens.ART_OF_WAR])

    return {
        "question": question,
        "situation": situation,
        "recommendation": getattr(result, "recommendation", ""),
        "synthesis": getattr(result, "synthesis", ""),
        "confidence": getattr(result, "confidence", 0.0),
        "timestamp": getattr(result, "timestamp", "unknown"),
    }


def consult_iching(
    question: str | None = None,
    urgency: str = "normal",
    operation: str = "cast",
    method: str = "coin",
    hexagram_number: int | None = None,
    context: str | None = None,
    original: int | None = None,
    transformed: int | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Consult I Ching for divination."""
    from whitemagic.oracle.quantum_iching import HexagramDatabase, QuantumIChing

    op = (operation or "cast").lower()
    iching = QuantumIChing()
    db = HexagramDatabase()

    if op == "cast":
        if not question:
            return {"error": "question is required for cast"}
        consult_fn = getattr(iching.consult, "__wrapped__", iching.consult)
        result = consult_fn(question=question, context={"urgency": urgency, "method": method})
        return {
            "question": question,
            "method": method,
            "primary_hexagram": result.primary_hexagram,
            "primary_name": result.primary_name,
            "primary_judgment": result.primary_judgment,
            "primary_image": result.primary_image,
            "changing_lines": result.changing_lines,
            "transformed_hexagram": result.transformed_hexagram,
            "transformed_name": result.transformed_name,
            "transformed_judgment": result.transformed_judgment,
            "wisdom": result.wisdom,
            "guidance": result.guidance,
            "resonance_score": result.resonance_score,
            "context_analysis": result.context_analysis,
            "timestamp": result.timestamp,
        }

    if op == "interpret":
        if not hexagram_number:
            return {"error": "hexagram_number is required for interpret"}
        hex_data = db.get_hexagram(hexagram_number)
        if not hex_data:
            return {"error": f"Unknown hexagram: {hexagram_number}"}
        return {
            "hexagram_number": hexagram_number,
            "name": hex_data.get("name"),
            "judgment": hex_data.get("judgment"),
            "image": hex_data.get("image"),
            "keywords": hex_data.get("keywords", []),
            "context": context,
        }

    if op == "track_transformations":
        if not original or not transformed:
            return {"error": "original and transformed are required for track_transformations"}
        original_data = db.get_hexagram(original)
        transformed_data = db.get_hexagram(transformed)
        if not original_data or not transformed_data:
            return {"error": "Unknown hexagram in transformation pair"}
        return {
            "original": {
                "hexagram_number": original,
                "name": original_data.get("name"),
                "judgment": original_data.get("judgment"),
                "image": original_data.get("image"),
            },
            "transformed": {
                "hexagram_number": transformed,
                "name": transformed_data.get("name"),
                "judgment": transformed_data.get("judgment"),
                "image": transformed_data.get("image"),
            },
        }

    return {"error": f"Unknown I Ching operation: {operation}"}


def synthesize_wisdom(sources: list, urgency: str = "normal", **kwargs: Any) -> dict[str, Any]:
    """Synthesize wisdom from multiple sources."""
    from whitemagic.core.intelligence.multi_spectral_reasoning import (
        MultiSpectralReasoner,
    )

    reasoner = MultiSpectralReasoner()
    # Use reasoner with all lenses, then filter by requested sources if provided
    result = reasoner.reason(question="Synthesize wisdom", lenses=None)

    return {
        "synthesis": getattr(result, "synthesis", ""),
        "recommendation": getattr(result, "recommendation", ""),
        "confidence": getattr(result, "confidence", 0.0),
        "sources_used": sources,
        "timestamp": getattr(result, "timestamp", "unknown"),
    }
