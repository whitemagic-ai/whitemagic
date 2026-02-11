"""Cross-Session Learning — Aggregate Tool Patterns Across Sessions.
=================================================================
Tracks tool co-occurrence, sequences, and failure correlations across
sessions. Feeds insights into Grimoire recommendations and starter packs.

Persisted to $WM_STATE_ROOT/learning/tool_patterns.json

Usage:
    from whitemagic.core.learning.cross_session import get_session_learner
    learner = get_session_learner()
    learner.record_tool_use("gnosis")
    learner.record_tool_use("harmony_vector")
    learner.end_session()
    patterns = learner.get_patterns()
"""

from __future__ import annotations

import json
import logging
import os
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class SessionRecord:
    """Record of tool usage in a single session."""

    session_id: str
    tools_used: list[str] = field(default_factory=list)
    tool_counts: dict[str, int] = field(default_factory=lambda: defaultdict(int))
    errors: dict[str, int] = field(default_factory=lambda: defaultdict(int))
    started_at: float = 0.0
    ended_at: float = 0.0


class CrossSessionLearner:
    """Aggregates tool usage patterns across sessions.

    Tracks:
      - Tool co-occurrence (which tools are used together)
      - Tool sequences (common orderings)
      - Failure correlations (which tools fail together)
      - Session archetypes (clusters of similar sessions)
    """

    def __init__(self, storage_dir: str | None = None):
        self._lock = threading.RLock()
        self._storage_path = Path(storage_dir or os.path.join(
            os.environ.get("WM_STATE_ROOT", os.path.expanduser("~/.whitemagic")),
            "learning",
        ))
        self._storage_path.mkdir(parents=True, exist_ok=True)
        self._data_file = self._storage_path / "tool_patterns.json"

        # Current session
        self._current_session: SessionRecord | None = None

        # Aggregated patterns
        self._cooccurrence: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
        self._sequences: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
        self._tool_totals: dict[str, int] = defaultdict(int)
        self._error_totals: dict[str, int] = defaultdict(int)
        self._session_count: int = 0
        self._last_tool: str | None = None

        self._load()

    def start_session(self, session_id: str = "") -> None:
        """Start tracking a new session."""
        with self._lock:
            self._current_session = SessionRecord(
                session_id=session_id or f"s_{int(time.time())}",
                started_at=time.time(),
            )
            self._last_tool = None

    def record_tool_use(self, tool_name: str, success: bool = True) -> None:
        """Record a tool invocation in the current session."""
        with self._lock:
            if self._current_session is None:
                self.start_session()

            assert self._current_session is not None
            session = self._current_session
            session.tools_used.append(tool_name)
            session.tool_counts[tool_name] = session.tool_counts.get(tool_name, 0) + 1
            self._tool_totals[tool_name] += 1

            if not success:
                session.errors[tool_name] = session.errors.get(tool_name, 0) + 1
                self._error_totals[tool_name] += 1

            # Track sequences
            if self._last_tool and self._last_tool != tool_name:
                self._sequences[self._last_tool][tool_name] += 1

            # Track co-occurrence within session
            for prev_tool in set(session.tools_used[:-1]):
                if prev_tool != tool_name:
                    self._cooccurrence[prev_tool][tool_name] += 1
                    self._cooccurrence[tool_name][prev_tool] += 1

            self._last_tool = tool_name

    def end_session(self) -> None:
        """End the current session and persist patterns."""
        with self._lock:
            if self._current_session:
                self._current_session.ended_at = time.time()
                self._session_count += 1
                self._current_session = None
                self._last_tool = None
            self._save()

    def get_patterns(self) -> dict[str, Any]:
        """Get aggregated cross-session patterns."""
        with self._lock:
            # Most-used tools
            top_tools = sorted(self._tool_totals.items(), key=lambda x: x[1], reverse=True)[:15]

            # Strongest co-occurrences
            cooccur_pairs: list[tuple[str, str, int]] = []
            seen: set[tuple[str, ...]] = set()
            for t1, neighbors in self._cooccurrence.items():
                for t2, count in neighbors.items():
                    pair = tuple(sorted([t1, t2]))
                    if pair not in seen and count >= 2:
                        seen.add(pair)
                        cooccur_pairs.append((pair[0], pair[1], count))
            cooccur_pairs.sort(key=lambda x: x[2], reverse=True)

            # Common sequences
            seq_list: list[dict[str, int | str]] = []
            for t1, followers in self._sequences.items():
                for t2, count in followers.items():
                    if count >= 2:
                        seq_list.append({"from": t1, "to": t2, "count": count})
            seq_list.sort(key=lambda x: int(x["count"]), reverse=True)

            # Error-prone tools
            error_tools = sorted(
                [(t, c, round(c / max(self._tool_totals.get(t, 1), 1), 3))
                 for t, c in self._error_totals.items() if c > 0],
                key=lambda x: x[2], reverse=True,
            )

            return {
                "sessions_analyzed": self._session_count,
                "unique_tools_seen": len(self._tool_totals),
                "top_tools": [{"tool": t, "count": c} for t, c in top_tools],
                "co_occurrences": [
                    {"tool_a": a, "tool_b": b, "count": c}
                    for a, b, c in cooccur_pairs[:15]
                ],
                "common_sequences": seq_list[:15],
                "error_prone": [
                    {"tool": t, "errors": e, "error_rate": r}
                    for t, e, r in error_tools[:10]
                ],
            }

    def suggest_next(self, current_tool: str) -> list[dict[str, Any]]:
        """Suggest next tools based on learned sequences."""
        with self._lock:
            followers = self._sequences.get(current_tool, {})
            if not followers:
                return []
            total = sum(followers.values())
            suggestions = sorted(followers.items(), key=lambda x: x[1], reverse=True)
            return [
                {"tool": t, "probability": round(c / total, 3), "count": c}
                for t, c in suggestions[:5]
            ]

    def _save(self) -> None:
        """Persist patterns to disk."""
        try:
            data = {
                "session_count": self._session_count,
                "tool_totals": dict(self._tool_totals),
                "error_totals": dict(self._error_totals),
                "cooccurrence": {k: dict(v) for k, v in self._cooccurrence.items()},
                "sequences": {k: dict(v) for k, v in self._sequences.items()},
            }
            with open(self._data_file, "w") as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            logger.debug("Failed to save tool patterns: %s", e)

    def _load(self) -> None:
        """Load patterns from disk."""
        if not self._data_file.exists():
            return
        try:
            with open(self._data_file, "r") as f:
                data = json.load(f)
            self._session_count = data.get("session_count", 0)
            self._tool_totals = defaultdict(int, data.get("tool_totals", {}))
            self._error_totals = defaultdict(int, data.get("error_totals", {}))
            for k, v in data.get("cooccurrence", {}).items():
                self._cooccurrence[k] = defaultdict(int, v)
            for k, v in data.get("sequences", {}).items():
                self._sequences[k] = defaultdict(int, v)
        except Exception as e:
            logger.debug("Failed to load tool patterns: %s", e)

    def status(self) -> dict[str, Any]:
        with self._lock:
            return {
                "sessions_analyzed": self._session_count,
                "unique_tools": len(self._tool_totals),
                "total_invocations": sum(self._tool_totals.values()),
                "current_session_active": self._current_session is not None,
                "data_file": str(self._data_file),
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_learner: CrossSessionLearner | None = None
_learner_lock = threading.Lock()


def get_session_learner() -> CrossSessionLearner:
    global _learner
    if _learner is None:
        with _learner_lock:
            if _learner is None:
                _learner = CrossSessionLearner()
    return _learner
