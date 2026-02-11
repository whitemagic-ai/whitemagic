"""Scratchpad Multi-Spectral Analysis.
=================================

Small wrapper around `MultiSpectralReasoner` that:
- accepts scratchpad sectioned content (dict[str, str])
- returns a lightweight analysis object used by `whitemagic.tools.unified_api`
- can serialize scratchpad + analysis into a Markdown memory entry

This module is intentionally dependency-light: if the underlying wisdom systems
aren't available, it degrades gracefully.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.core.intelligence.multi_spectral_reasoning import (
    MultiSpectralReasoner,
    ReasoningContext,
    ReasoningLens,
)


@dataclass(frozen=True)
class ScratchpadAnalysis:
    synthesis: str
    wisdom: str
    confidence: float
    perspectives: list[dict[str, Any]]
    patterns_matched: int
    reasoning_chain: list[str]
    timestamp: datetime


def analyze_scratchpad(
    scratchpad_content: dict[str, str],
    question: str | None = None,
    lenses: Sequence[ReasoningLens] | None = None,
    base_dir: str | None = None,
) -> ScratchpadAnalysis:
    """Analyze scratchpad content using the multi-spectral reasoning engine.

    `scratchpad_content` is expected to be section -> text, e.g.
    {"current_focus": "...", "decisions": "...", "questions": "..."}.
    """
    if question is None:
        question = (
            scratchpad_content.get("current_focus")
            or scratchpad_content.get("focus")
            or scratchpad_content.get("question")
            or "What should I focus on next?"
        )

    # Cheap heuristics: derive task type from labels so Wu Xing / Art of War have a hint.
    lower = " ".join([question] + list(scratchpad_content.values())).lower()
    if any(w in lower for w in ("bug", "fix", "error", "traceback")):
        task_type = "debugging"
    elif any(w in lower for w in ("implement", "build", "ship", "release")):
        task_type = "implementation"
    elif any(w in lower for w in ("plan", "roadmap", "strategy", "design")):
        task_type = "planning"
    else:
        task_type = "analysis"

    context = ReasoningContext(
        question=question,
        task_type=task_type,
        metadata={"scratchpad_sections": sorted(scratchpad_content.keys())},
    )

    # Default to WM_ROOT so tools run hermetically under WM_STATE_ROOT.
    base_dir_path = WM_ROOT if base_dir is None else Path(base_dir).expanduser().resolve()
    reasoner = MultiSpectralReasoner(base_dir=base_dir_path)
    result = reasoner.reason(question=question, lenses=lenses, context=context)

    perspectives = [
        {
            "lens": p.lens.value,
            "analysis": p.analysis,
            "guidance": p.guidance,
            "confidence": p.confidence,
            "details": p.details,
        }
        for p in result.perspectives
    ]

    return ScratchpadAnalysis(
        synthesis=result.synthesis,
        wisdom=result.recommendation,
        confidence=result.confidence,
        perspectives=perspectives,
        patterns_matched=len(result.patterns_matched or []),
        reasoning_chain=list(result.reasoning_chain or []),
        timestamp=result.timestamp,
    )


def serialize_scratchpad_with_analysis(
    scratchpad_content: dict[str, str],
    analysis: ScratchpadAnalysis,
    title: str = "Scratchpad",
) -> str:
    """Serialize scratchpad + analysis into a Markdown memory body."""
    lines: list[str] = []
    lines.append(f"# {title}".strip())
    lines.append("")
    lines.append("## Analysis")
    lines.append(f"Confidence: {analysis.confidence:.2%}")
    lines.append("")
    lines.append("### Synthesis")
    lines.append(analysis.synthesis.strip() if analysis.synthesis else "")
    lines.append("")
    lines.append("### Recommendation")
    lines.append(analysis.wisdom.strip() if analysis.wisdom else "")
    lines.append("")
    lines.append(f"Patterns matched: {analysis.patterns_matched}")
    lines.append("")

    if analysis.perspectives:
        lines.append("### Perspectives")
        for p in analysis.perspectives:
            lens = p.get("lens", "unknown")
            conf = p.get("confidence", 0.0)
            guidance = (p.get("guidance") or "").strip()
            lines.append(f"- {lens} ({conf:.2f}): {guidance}")
        lines.append("")

    lines.append("## Scratchpad")
    for section, content in scratchpad_content.items():
        section_title = section.replace("_", " ").title()
        lines.append(f"### {section_title}")
        lines.append((content or "").rstrip())
        lines.append("")

    return "\n".join(lines).rstrip() + "\n"
