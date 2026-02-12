
from typing import Any

from whitemagic.core.bridge.utils import _emit_resonance_event

_voice_synthesis = None

def manage_voice_patterns(
    operation: str = "signature",
    text: str | None = None,
    content: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Manage voice pattern analysis and synthesis."""
    global _voice_synthesis
    from whitemagic.gardens.voice.voice_synthesis import VoiceSynthesis

    if _voice_synthesis is None:
        _voice_synthesis = VoiceSynthesis()

    op = (operation or "signature").lower()

    if op == "analyze":
        if not text:
            return {"error": "text is required for analyze"}
        result = _voice_synthesis.analyze_expression(text)
        _emit_resonance_event("VOICE_EXPRESSED", {"length": result.get("length", 0)})
        return result

    if op == "learn":
        if not text:
            return {"error": "text is required for learn"}
        result = _voice_synthesis.analyze_expression(text)
        return {"learned": True, "analysis": result}

    if op == "signature":
        return _voice_synthesis.get_voice_signature()

    if op == "generate":
        if not content:
            return {"error": "content is required for generate"}
        return {"generated": _voice_synthesis.generate_in_my_voice(content)}

    return {"error": f"Unknown voice operation: {operation}"}
