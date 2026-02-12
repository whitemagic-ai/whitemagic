"""Scratchpad tool handlers."""
import logging
from collections import defaultdict
from collections.abc import Callable
from pathlib import Path
from typing import Any, cast

logger = logging.getLogger(__name__)


ScratchpadHandler = Callable[..., dict[str, Any]]


def _emit(event_type: str, data: dict[str, Any]) -> None:
    from whitemagic.tools.unified_api import _emit_gan_ying
    _emit_gan_ying(event_type, data)


def _resolve_base_path(kwargs: dict[str, Any]) -> Path:
    from whitemagic.tools.unified_api import _resolve_base_path as _rbp
    return cast("Path", _rbp(kwargs))


def handle_scratchpad(**kwargs: Any) -> dict[str, Any]:
    """Unified scratchpad handler — routes by action parameter."""
    action = kwargs.get("action", "create")
    dispatch: dict[str, ScratchpadHandler] = {
        "create": handle_scratchpad_create,
        "update": handle_scratchpad_update,
        "finalize": handle_scratchpad_finalize,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_scratchpad_create(**kwargs: Any) -> dict[str, Any]:
    from uuid import uuid4

    from whitemagic.core.memory.scratchpad_interleave import ScratchpadManager
    from whitemagic.utils import slugify
    base_path = _resolve_base_path(kwargs)
    name = kwargs.get("name", "scratchpad")
    scratchpad_id = slugify(name) or f"scratch-{uuid4().hex[:6]}"
    focus = kwargs.get("session_id")
    manager = ScratchpadManager(scratch_dir=base_path / "scratchpads")
    pad = manager.create(scratchpad_id, focus=focus)
    _emit("WORKING_MEMORY_UPDATED", {"action": "create", "scratchpad_id": scratchpad_id, "focus": focus})
    return {
        "status": "success",
        "scratchpad": {"id": scratchpad_id, "name": pad.name, "focus": pad.focus, "created": pad.created.isoformat()},
    }


def handle_scratchpad_update(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.scratchpad_interleave import ScratchpadManager
    base_path = _resolve_base_path(kwargs)
    scratchpad_id = kwargs.get("scratchpad_id")
    if not scratchpad_id:
        raise ValueError("scratchpad_id is required")
    content = kwargs.get("content", "")
    section = kwargs.get("section")
    manager = ScratchpadManager(scratch_dir=base_path / "scratchpads")
    if scratchpad_id not in manager.scratchpads:
        raise ValueError(f"Scratchpad not found: {scratchpad_id}")
    manager.write_to(scratchpad_id, content, tag=section)
    _emit("WORKING_MEMORY_UPDATED", {"action": "update", "scratchpad_id": scratchpad_id, "section": section})
    return {"status": "success", "scratchpad_id": scratchpad_id, "section": section}


def handle_analyze_scratchpad(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.intelligence.multi_spectral_scratchpad import (
        analyze_scratchpad,
    )
    from whitemagic.core.memory.scratchpad_interleave import ScratchpadManager
    base_path = _resolve_base_path(kwargs)
    scratchpad_id = kwargs.get("scratchpad_id")
    if not scratchpad_id:
        raise ValueError("scratchpad_id is required")
    manager = ScratchpadManager(scratch_dir=base_path / "scratchpads")
    pad = manager.scratchpads.get(scratchpad_id)
    if not pad:
        raise ValueError(f"Scratchpad not found: {scratchpad_id}")
    sections = defaultdict(list)
    for entry in pad.entries:
        sections[entry.get("tag") or "notes"].append(entry.get("content", ""))
    content_dict = {name: "\n".join(items) for name, items in sections.items()}
    analysis = analyze_scratchpad(content_dict)
    return {
        "status": "success",
        "scratchpad_id": scratchpad_id,
        "analysis": {
            "synthesis": analysis.synthesis,
            "wisdom": analysis.wisdom,
            "confidence": analysis.confidence,
            "perspectives": analysis.perspectives,
            "patterns_matched": analysis.patterns_matched,
            "reasoning_chain": analysis.reasoning_chain,
        },
    }


def handle_scratchpad_finalize(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.manager import MemoryManager
    from whitemagic.core.memory.scratchpad_interleave import ScratchpadManager
    base_path = _resolve_base_path(kwargs)
    scratchpad_id = kwargs.get("scratchpad_id")
    if not scratchpad_id:
        raise ValueError("scratchpad_id is required")
    memory_type = kwargs.get("memory_type", "long_term")
    auto_analyze = kwargs.get("auto_analyze", True)
    manager = ScratchpadManager(scratch_dir=base_path / "scratchpads")
    pad = manager.scratchpads.get(scratchpad_id)
    if not pad:
        raise ValueError(f"Scratchpad not found: {scratchpad_id}")
    sections = defaultdict(list)
    for entry in pad.entries:
        sections[entry.get("tag") or "notes"].append(entry.get("content", ""))
    content_dict = {name: "\n".join(items) for name, items in sections.items()}
    analysis_result = None
    if auto_analyze:
        try:
            from whitemagic.core.intelligence.multi_spectral_scratchpad import (
                analyze_scratchpad,
                serialize_scratchpad_with_analysis,
            )
            analysis = analyze_scratchpad(content_dict)
            content = serialize_scratchpad_with_analysis(content_dict, analysis, title=f"Scratchpad: {pad.name}")
            analysis_result = {
                "confidence": analysis.confidence,
                "patterns_matched": analysis.patterns_matched,
                "perspectives_used": len(analysis.perspectives),
            }
        except Exception as e:
            logger.warning(f"Multi-spectral analysis failed: {e}")
            lines = [f"# Scratchpad: {pad.name}"]
            if pad.focus:
                lines.append(f"Focus: {pad.focus}")
            for section_name, items in sections.items():
                lines.append(f"\n## {section_name.title()}")
                for item in items:
                    lines.append(item)
            content = "\n".join(lines).strip()
    else:
        lines = [f"# Scratchpad: {pad.name}"]
        if pad.focus:
            lines.append(f"Focus: {pad.focus}")
        for section_name, items in sections.items():
            lines.append(f"\n## {section_name.title()}")
            for item in items:
                lines.append(item)
        content = "\n".join(lines).strip()
    memory_manager = MemoryManager(base_dir=str(base_path))
    tags = ["scratchpad", "auto-finalized"]
    if analysis_result:
        tags.append("multi-spectral")
        tags.append(f"confidence-{int(analysis_result['confidence'] * 100)}")
    memory_path = memory_manager.create_memory(title=f"Scratchpad: {pad.name}", content=content, memory_type=memory_type, tags=tags)
    scratchpad_file = manager.scratch_dir / f"{pad.name}.json"
    if scratchpad_file.exists():
        scratchpad_file.unlink()
    if pad.name in manager.scratchpads:
        del manager.scratchpads[pad.name]
    event_data = {"action": "finalize", "scratchpad_id": scratchpad_id, "memory_path": str(memory_path), "analyzed": auto_analyze}
    if analysis_result:
        event_data.update(analysis_result)
    _emit("SCRATCHPAD_FINALIZED", event_data)
    result = {"status": "success", "scratchpad_id": scratchpad_id, "memory_path": str(memory_path), "analyzed": auto_analyze}
    if analysis_result:
        result["analysis"] = analysis_result
    return result
