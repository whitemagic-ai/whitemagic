"""Solution Library — Phase 26 Infrastructure.
==========================================
Structured repository for "Discovered Solutions" and "Code Primitives".
Bridges the gap between raw pattern extraction and actionable code.
"""

import json
import logging
import sqlite3
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.core.memory.holographic import get_holographic_memory

logger = logging.getLogger(__name__)

@dataclass
class Solution:
    """A verified or discovered solution pattern."""

    id: str
    title: str
    description: str
    code_snippet: str | None = None
    pattern_type: str = "solution" # solution, anti_pattern, heuristic
    confidence: float = 0.5
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    created_at: str = ""

    def __post_init__(self) -> None:
        if not self.created_at:
            self.created_at = datetime.now().isoformat()

class SolutionLibrary:
    """Manages the lifecycle of discovered strategic and technical solutions."""

    def __init__(self, db_path: Path | None = None) -> None:
        from whitemagic.config.paths import DB_PATH
        self.db_path = db_path or DB_PATH

        self._init_db()

    def _init_db(self) -> Any:
        """Ensure solution-specific tables exist."""
        conn = sqlite3.connect(str(self.db_path))
        conn.execute("""
            CREATE TABLE IF NOT EXISTS solutions (
                id TEXT PRIMARY KEY,
                title TEXT,
                description TEXT,
                code_snippet TEXT,
                pattern_type TEXT,
                confidence REAL,
                tags TEXT,
                metadata TEXT,
                created_at TEXT
            )
        """)
        conn.commit()
        conn.close()

    def add_solution(self, solution: Solution, index_holographically: bool = True) -> Any:
        """Add a solution to the library and optionally index it in 4D space."""
        conn = sqlite3.connect(str(self.db_path))
        conn.execute("""
            INSERT OR REPLACE INTO solutions (id, title, description, code_snippet, pattern_type, confidence, tags, metadata, created_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            solution.id, solution.title, solution.description, solution.code_snippet,
            solution.pattern_type, solution.confidence, json.dumps(solution.tags),
            json.dumps(solution.metadata), solution.created_at,
        ))
        conn.commit()
        conn.close()

        if index_holographically:
            holo = get_holographic_memory()
            # Construct a memory-like dict for the encoder
            mem_data = {
                "content": f"{solution.title}\n{solution.description}\n{solution.code_snippet}",
                "importance": solution.confidence,
                "tags": solution.tags,
            }
            holo.index_memory(solution.id, mem_data)

    def get_solution(self, solution_id: str) -> Solution | None:
        """Retrieve a specific solution."""
        conn = sqlite3.connect(str(self.db_path))
        conn.row_factory = sqlite3.Row
        row = conn.execute("SELECT * FROM solutions WHERE id = ?", (solution_id,)).fetchone()
        conn.close()

        if row:
            return Solution(
                id=row["id"],
                title=row["title"],
                description=row["description"],
                code_snippet=row["code_snippet"],
                pattern_type=row["pattern_type"],
                confidence=row["confidence"],
                tags=json.loads(row["tags"]),
                metadata=json.loads(row["metadata"]),
                created_at=row["created_at"],
            )
        return None

    def find_nearest_solutions(self, query_context: dict[str, Any], k: int = 5) -> list[Solution]:
        """Find solutions spatially near a given context (uses Holographic Index)."""
        holo = get_holographic_memory()
        results = holo.query_nearest(query_context, k=k)

        solutions = []
        for res in results:
            sol = self.get_solution(res.memory_id)
            if sol:
                solutions.append(sol)
        return solutions

    def apply(self, solution_id: str, context: dict[str, Any] | None = None) -> bool:
        """Apply a solution by executing its code snippet or associated logic.
        (Phase 27: Sovereign Autonomy).
        """
        sol = self.get_solution(solution_id)
        if not sol:
            logger.error(f"Cannot apply non-existent solution: {solution_id}")
            return False

        print(f"  [SolutionLibrary] Applying: {sol.title} ({sol.id})...")

        # 1. Check AI Contract (Alignment)
        try:
            from whitemagic.ai_contract import AIContract  # type: ignore[import-not-found]
            contract = AIContract(capabilities=[sol.pattern_type, f"solution_{sol.id}"])
            if not contract.validate_action(f"apply_{sol.id}"):
                logger.warning(f"Solution apply blocked by AI Contract: {sol.id}")
                # For Phase 27, we might log and proceed if in 'bypass' mode,
                # but standard is to return False.
                return False
        except ImportError:
            pass

        # 2. Execute code snippet if present
        if sol.code_snippet:
            try:
                # Potential security risk — in a real system, use a sandbox
                # For now, we use a restricted exec or dispatch to AcceleratorBridge
                from whitemagic.core.intelligence.synthesis.accelerator_bridge import (
                    get_accelerator_bridge,
                )
                bridge = get_accelerator_bridge()
                return bool(bridge.execute_snippet(sol.code_snippet, context or {}))
            except Exception as e:
                logger.error(f"Execution of solution {sol.id} failed: {e}")
                return False

        # 3. Fallback: Log that solution was validated but had no executable code
        logger.info(f"Solution {sol.id} validated but contains no executable snippet.")
        return True

    def migrate_from_cluster_patterns(self) -> Any:
        """Migrate existing patterns from the 'cluster_patterns' table into the Solution Library."""
        conn = sqlite3.connect(str(self.db_path))
        conn.row_factory = sqlite3.Row
        patterns = conn.execute("SELECT * FROM cluster_patterns").fetchall()

        count = 0
        for p in patterns:
            # Generate a stable ID based on cluster and content
            import hashlib
            stable_source = f"{p['cluster_id']}{p['content']}"
            sol_id = f"sol_{hashlib.md5(stable_source.encode()).hexdigest()[:8]}"

            sol = Solution(
                id=sol_id,
                title=f"Cluster {p['cluster_id']} Pattern",
                description=p["content"],
                pattern_type="solution" if p["pattern_type"] == "Sol" else "anti_pattern",
                metadata={"source_cluster": p["cluster_id"]},
            )
            self.add_solution(sol)
            count += 1

        conn.close()
        return count

# Singleton
_solution_library = None

def get_solution_library() -> SolutionLibrary:
    global _solution_library
    if _solution_library is None:
        _solution_library = SolutionLibrary()
    return _solution_library
