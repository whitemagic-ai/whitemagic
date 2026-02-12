
from typing import Any

from whitemagic.core.bridge.utils import _emit_resonance_event


def detect_patterns(
    pattern_type: str | None = None,
    type: str | None = None,
    content: str | None = None,
    min_confidence: float = 0.6,
    sensitivity: float | None = None,
    use_rust: bool = True,
    limit: int = 20,
    **kwargs: Any,
) -> dict[str, Any]:
    """Detect patterns across memory and reports."""
    from whitemagic.core.patterns import PatternEngine

    engine = PatternEngine()
    if sensitivity is not None:
        min_confidence = sensitivity
    if not pattern_type and type:
        pattern_type = type

    alias_map = {
        "patterns": None,
        "anomalies": "anti_patterns",
        "emergence": "optimizations",
        "strange_loops": "heuristics",
    }
    if pattern_type in alias_map:
        pattern_type = alias_map[pattern_type]

    report = engine.extract_patterns(min_confidence=min_confidence, use_rust=use_rust)
    data = report.to_dict()

    if pattern_type:
        key = pattern_type.lower()
        if key in data:
            data = {key: data[key], "total_memories": data["total_memories"], "patterns_found": data["patterns_found"]}
        else:
            return {"error": f"Unknown pattern_type: {pattern_type}"}

    # Apply limit to any list values
    for k, v in list(data.items()):
        if isinstance(v, list):
            data[k] = v[:limit]

    _emit_resonance_event(
        "PATTERN_DETECTED",
        {
            "pattern_type": pattern_type or "all",
            "count": data.get("patterns_found", 0),
            "content_hint": content[:120] if content else None,
        },
    )
    return data
