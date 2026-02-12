"""
Prompt Template Engine
======================
Loads YAML prompt templates from $WM_STATE_ROOT/prompts/ and built-in defaults.
Supports variable substitution, Wu Xing tone selection, and versioning.

Template format (YAML):
    name: session_greeting
    version: 1
    description: Opening prompt for a new session
    wu_xing:
      wood: "Begin with decisive energy..."
      fire: "Let's ignite this session..."
      earth: "Ground ourselves first..."
      metal: "Precisely and methodically..."
      water: "Flow into this session..."
    default: "Welcome to this session. {{agent_name}} reporting."
    variables:
      - agent_name
      - session_id
      - context

Usage:
    engine = get_prompt_engine()
    text = engine.render("session_greeting", agent_name="WhiteMagic", wu_xing="fire")
"""

from __future__ import annotations

import logging
import os
import re
import threading
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)

# Try to import yaml, fall back gracefully
try:
    import yaml
    HAS_YAML = True
except ImportError:
    HAS_YAML = False


WU_XIG_ELEMENTS = ("wood", "fire", "earth", "metal", "water")

# Variable pattern: {{variable_name}}
_VAR_PATTERN = re.compile(r"\{\{\s*(\w+)\s*\}\}")


@dataclass
class PromptTemplate:
    """A single prompt template."""
    name: str
    description: str = ""
    version: int = 1
    default: str = ""
    wu_xing: Dict[str, str] = field(default_factory=dict)
    variables: List[str] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    source: str = "builtin"  # "builtin" or file path

    def render(self, wu_xing: Optional[str] = None, **kwargs: Any) -> str:
        """Render the template with variable substitution."""
        # Pick Wu Xing variant or default
        if wu_xing and wu_xing in self.wu_xing:
            text = self.wu_xing[wu_xing]
        else:
            text = self.default

        # Substitute variables
        def _replace(match: Any) -> Any:
            var_name = match.group(1)
            return str(kwargs.get(var_name, f"{{{{{var_name}}}}}"))

        return _VAR_PATTERN.sub(_replace, text)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "description": self.description,
            "version": self.version,
            "variables": self.variables,
            "wu_xing_variants": list(self.wu_xing.keys()),
            "tags": self.tags,
            "source": self.source,
        }


# ---------------------------------------------------------------------------
# Built-in templates
# ---------------------------------------------------------------------------

_BUILTIN_TEMPLATES: List[PromptTemplate] = [
    PromptTemplate(
        name="session_greeting",
        description="Opening prompt for a new AI session",
        version=1,
        default="Session initialized. {{agent_name}} ready. Context: {{context}}",
        wu_xing={
            "wood": "Sharp initiation. {{agent_name}} locked in. Context: {{context}} — moving decisively.",
            "fire": "Igniting session. {{agent_name}} ablaze with purpose. Context: {{context}} — full intensity.",
            "earth": "Grounding into session. {{agent_name}} present and stable. Context: {{context}} — steady footing.",
            "metal": "Precise initialization. {{agent_name}} calibrated. Context: {{context}} — methodical approach.",
            "water": "Flowing into session. {{agent_name}} attuned. Context: {{context}} — deep awareness.",
        },
        variables=["agent_name", "context"],
        tags=["session", "greeting"],
    ),
    PromptTemplate(
        name="memory_synthesis",
        description="Prompt for synthesizing multiple memories into insight",
        version=1,
        default=(
            "Synthesize the following {{count}} memories into a coherent insight:\n\n"
            "{{memories}}\n\n"
            "Focus on: patterns, contradictions, and actionable knowledge."
        ),
        wu_xing={
            "wood": "Cut through these {{count}} memories — find the decisive pattern:\n\n{{memories}}",
            "fire": "Illuminate connections in these {{count}} memories — what truth blazes forth?\n\n{{memories}}",
            "earth": "Carefully weigh these {{count}} memories — what stable ground emerges?\n\n{{memories}}",
            "metal": "Distill these {{count}} memories to their essence — precise extraction:\n\n{{memories}}",
            "water": "Let these {{count}} memories flow together — what emerges from the depths?\n\n{{memories}}",
        },
        variables=["count", "memories"],
        tags=["memory", "synthesis"],
    ),
    PromptTemplate(
        name="ethical_review",
        description="Prompt for Dharma-guided ethical evaluation",
        version=1,
        default=(
            "Evaluate the following action through the lens of Dharma:\n\n"
            "Action: {{action}}\n"
            "Context: {{context}}\n"
            "Current profile: {{profile}}\n\n"
            "Consider: harm potential, consent, reversibility, proportionality."
        ),
        variables=["action", "context", "profile"],
        tags=["dharma", "ethics"],
    ),
    PromptTemplate(
        name="dream_reflection",
        description="Prompt for reflecting on dream cycle outputs",
        version=1,
        default=(
            "The Dream Cycle produced the following during idle time:\n\n"
            "Phase: {{phase}}\n"
            "Details: {{details}}\n\n"
            "Integrate this into waking operations. What adjustments are suggested?"
        ),
        variables=["phase", "details"],
        tags=["dream", "reflection"],
    ),
    PromptTemplate(
        name="tool_recommendation",
        description="Prompt for recommending tools based on task",
        version=1,
        default=(
            "Given the task: {{task}}\n"
            "Available tools: {{tools}}\n"
            "Current Wu Xing phase: {{wu_xing}}\n\n"
            "Recommend the optimal tool sequence and explain your reasoning."
        ),
        variables=["task", "tools", "wu_xing"],
        tags=["tools", "recommendation"],
    ),
    PromptTemplate(
        name="bicameral_query",
        description="Prompt for dual-hemisphere reasoning",
        version=1,
        default=(
            "ANALYTICAL HEMISPHERE:\n"
            "Analyze: {{query}}\n"
            "Evidence: {{evidence}}\n\n"
            "INTUITIVE HEMISPHERE:\n"
            "What does pattern recognition suggest about: {{query}}?\n\n"
            "SYNTHESIS:\n"
            "Integrate both perspectives into a unified response."
        ),
        variables=["query", "evidence"],
        tags=["reasoning", "bicameral"],
    ),
]


class PromptEngine:
    """
    Manages prompt templates from built-in defaults and YAML files.

    Templates in $WM_STATE_ROOT/prompts/ override built-ins by name.
    """

    def __init__(self, prompts_dir: Optional[str] = None) -> None:
        self._templates: Dict[str, PromptTemplate] = {}
        self._lock = threading.Lock()

        # Load built-ins
        for t in _BUILTIN_TEMPLATES:
            self._templates[t.name] = t

        # Load from disk
        self._prompts_dir = prompts_dir or os.path.join(
            os.environ.get("WM_STATE_ROOT", os.path.expanduser("~/.whitemagic")),
            "prompts",
        )
        self._load_from_disk()

    def _load_from_disk(self) -> None:
        """Load YAML templates from the prompts directory."""
        if not HAS_YAML:
            return

        prompts_path = Path(self._prompts_dir)
        if not prompts_path.is_dir():
            return

        for yaml_file in sorted(prompts_path.glob("*.yaml")) + sorted(prompts_path.glob("*.yml")):
            try:
                with open(yaml_file, "r") as f:
                    data = yaml.safe_load(f)
                if not isinstance(data, dict) or "name" not in data:
                    continue

                template = PromptTemplate(
                    name=data["name"],
                    description=data.get("description", ""),
                    version=data.get("version", 1),
                    default=data.get("default", ""),
                    wu_xing=data.get("wu_xing", {}),
                    variables=data.get("variables", []),
                    tags=data.get("tags", []),
                    source=str(yaml_file),
                )
                self._templates[template.name] = template
                logger.debug("Loaded prompt template: %s from %s", template.name, yaml_file)
            except Exception as e:
                logger.warning("Failed to load prompt template %s: %s", yaml_file, e)

    def render(self, name: str, wu_xing: Optional[str] = None, **kwargs: Any) -> str:
        """Render a named template with variables."""
        template = self._templates.get(name)
        if template is None:
            return f"[unknown template: {name}]"
        return template.render(wu_xing=wu_xing, **kwargs)

    def list_templates(self, tag: Optional[str] = None) -> List[Dict[str, Any]]:
        """List all available templates, optionally filtered by tag."""
        results = []
        for t in self._templates.values():
            if tag and tag not in t.tags:
                continue
            results.append(t.to_dict())
        return sorted(results, key=lambda x: x["name"])

    def get_template(self, name: str) -> Optional[PromptTemplate]:
        """Get a template by name."""
        return self._templates.get(name)

    def register(self, template: PromptTemplate) -> None:
        """Register a template at runtime."""
        with self._lock:
            self._templates[template.name] = template

    def reload(self) -> None:
        """Reload templates from disk (preserves built-ins)."""
        with self._lock:
            for t in _BUILTIN_TEMPLATES:
                self._templates[t.name] = t
            self._load_from_disk()

    def status(self) -> Dict[str, Any]:
        """Get engine status."""
        return {
            "total_templates": len(self._templates),
            "builtin_count": sum(1 for t in self._templates.values() if t.source == "builtin"),
            "disk_count": sum(1 for t in self._templates.values() if t.source != "builtin"),
            "prompts_dir": self._prompts_dir,
            "yaml_available": HAS_YAML,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_engine: Optional[PromptEngine] = None
_engine_lock = threading.Lock()


def get_prompt_engine() -> PromptEngine:
    """Get the global PromptEngine instance."""
    global _engine
    if _engine is None:
        with _engine_lock:
            if _engine is None:
                _engine = PromptEngine()
    return _engine
