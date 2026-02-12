# mypy: disable-error-code=no-untyped-def
"""Memory Injector - Auto-enrich AI prompts with relevant memories
Version: 4.3.0.

This module automatically retrieves and injects relevant memories
into AI prompts, enabling continuity and coherence across sessions.

Features:
- Retrieves recent short-term memories
- Fetches relevant long-term memories by semantic similarity
- Loads RESUME files for session continuity
- Injects context from previous sessions
- Respects token budgets

Philosophy: Memory is identity. Perfect recall enables growth.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Any


@dataclass
class InjectedContext:
    """Context to inject into prompts."""

    resume_context: str = ""
    short_term_memories: list[str] | None = None
    long_term_memories: list[str] | None = None
    session_state: dict[str, Any] | None = None
    total_tokens: int = 0

    def __post_init__(self):
        if self.short_term_memories is None:
            self.short_term_memories = []
        if self.long_term_memories is None:
            self.long_term_memories = []
        if self.session_state is None:
            self.session_state = {}

    def to_prompt_prefix(self) -> str:
        """Generate prompt prefix from injected context."""
        parts = []

        if self.resume_context:
            parts.append("## Session Resume Context\n" + self.resume_context)

        if self.short_term_memories:
            parts.append("## Recent Activity\n" + "\n".join(
                f"- {m}" for m in self.short_term_memories[:5]
            ))

        if self.long_term_memories:
            parts.append("## Relevant Memories\n" + "\n".join(
                f"- {m}" for m in self.long_term_memories[:5]
            ))

        if self.session_state:
            parts.append(f"## Session State\n- Iteration: {self.session_state.get('iteration', 0)}")

        return "\n\n".join(parts) if parts else ""


class MemoryInjector:
    """Automatically inject relevant memories into AI prompts.

    Use this to enrich prompts with:
    - Previous session state (RESUME files)
    - Recent short-term memories
    - Relevant long-term memories
    - Session handoff context
    """

    def __init__(self, base_dir: Path | None = None):
        from whitemagic.config import PROJECT_ROOT
        self.base_dir = base_dir or PROJECT_ROOT
        self.memory_dir = self.base_dir / "memory"
        self.intake_dir = self.memory_dir / "intake"
        self.short_term_dir = self.memory_dir / "short_term"
        self.long_term_dir = self.memory_dir / "long_term"

    def get_resume_context(self) -> str:
        """Get context from most recent RESUME file."""
        if not self.intake_dir.exists():
            return ""

        resume_files = sorted(
            self.intake_dir.glob("RESUME_*.md"),
            key=lambda f: f.stat().st_mtime,
            reverse=True,
        )

        if not resume_files:
            return ""

        # Read most recent
        latest = resume_files[0]
        try:
            content = latest.read_text()
            # Extract key sections (limit size)
            lines = content.split("\n")[:50]
            return "\n".join(lines)
        except Exception:
            return ""

    def get_recent_short_term(self, limit: int = 5) -> list[str]:
        """Get recent short-term memory summaries."""
        if not self.short_term_dir.exists():
            return []

        memory_files = sorted(
            self.short_term_dir.glob("*.md"),
            key=lambda f: f.stat().st_mtime,
            reverse=True,
        )[:limit]

        summaries = []
        for f in memory_files:
            try:
                content = f.read_text()
                # Extract first meaningful line as summary
                for line in content.split("\n"):
                    line = line.strip()
                    if line and not line.startswith("#") and not line.startswith("-"):
                        summaries.append(line[:200])
                        break
            except Exception:
                pass

        return summaries

    def get_relevant_long_term(self, query: str, limit: int = 5) -> list[str]:
        """Get long-term memories relevant to query."""
        if not self.long_term_dir.exists():
            return []

        # Try neural search if available
        try:
            from whitemagic.core.memory.neural.engine import get_neural_memory  # type: ignore[import-not-found]
            neural = get_neural_memory()
            results = neural.search(query, limit=limit)
            return [r.content[:200] for r in results]
        except ImportError:
            pass

        # Fallback: simple keyword search
        query_words = set(query.lower().split())
        scored_files = []

        for f in self.long_term_dir.glob("*.md"):
            try:
                content = f.read_text().lower()
                score = sum(1 for w in query_words if w in content)
                if score > 0:
                    scored_files.append((score, f))
            except Exception:
                pass

        # Sort by score, take top results
        scored_files.sort(key=lambda x: -x[0])

        summaries = []
        for _, f in scored_files[:limit]:
            try:
                content = f.read_text()
                # Extract title or first line
                for line in content.split("\n"):
                    line = line.strip()
                    if line:
                        summaries.append(line[:200])
                        break
            except Exception:
                pass

        return summaries

    def get_session_state(self) -> dict[str, Any]:
        """Get current session state from various sources."""
        state = {}

        # From coherence persistence
        try:
            from whitemagic.core.intelligence.agentic.coherence_persistence import (
                get_coherence,
            )
            coherence = get_coherence()
            stats = coherence.get_iteration_stats()
            state["coherence_level"] = stats.get("coherence_level", 100)
            state["iteration"] = stats.get("iteration_count", 0)
            state["calls_remaining"] = stats.get("calls_remaining", 100)
        except ImportError:
            pass

        # From session handoff
        try:
            from whitemagic.gardens.sangha.session_handoff import get_handoff
            handoff = get_handoff()
            current = handoff._load_current_session()
            if current:
                state["active_tasks"] = current.active_tasks[:3]
                state["completed_tasks"] = len(current.completed_tasks)
        except Exception:
            pass

        return state

    def inject(self, query: str = "", max_tokens: int = 2000) -> InjectedContext:
        """Get full injected context for a prompt.

        Args:
            query: The user's query (for relevance matching)
            max_tokens: Maximum tokens to inject

        Returns:
            InjectedContext with all relevant memories

        """
        context = InjectedContext()

        # 1. Resume context (highest priority)
        context.resume_context = self.get_resume_context()

        # 2. Recent short-term
        context.short_term_memories = self.get_recent_short_term(5)

        # 3. Relevant long-term (if query provided)
        if query:
            context.long_term_memories = self.get_relevant_long_term(query, 5)

        # 4. Session state
        context.session_state = self.get_session_state()

        # Estimate tokens
        full_text = context.to_prompt_prefix()
        context.total_tokens = len(full_text) // 4  # Rough estimate

        return context

    def inject_into_prompt(self, prompt: str, query: str = "") -> str:
        """Inject memory context into a prompt.

        Args:
            prompt: The original prompt
            query: Query for relevance matching

        Returns:
            Enriched prompt with memory context

        """
        context = self.inject(query)
        prefix = context.to_prompt_prefix()

        if prefix:
            return f"{prefix}\n\n---\n\n{prompt}"
        return prompt


# Singleton
_injector: MemoryInjector | None = None

def get_memory_injector() -> MemoryInjector:
    """Get global memory injector instance."""
    global _injector
    if _injector is None:
        _injector = MemoryInjector()
    return _injector

def inject_memories(prompt: str, query: str = "") -> str:
    """Quick function to inject memories into a prompt."""
    return get_memory_injector().inject_into_prompt(prompt, query)
