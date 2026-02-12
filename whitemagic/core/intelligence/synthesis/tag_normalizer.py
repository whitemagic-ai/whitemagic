"""TagNormalizer - Clean and unify memory taxonomy.

Handles case normalization, plural/singular, synonyms, and orphan tags.
"""

import re
import sqlite3
from typing import Any


class TagNormalizer:
    """Normalize and clean memory tags."""

    SYNONYMS = {
        "complete": ["completed", "completion", "done", "finished"],
        "test": ["tests", "testing", "tested"],
        "fix": ["fixes", "fixed", "fixing", "bugfix"],
        "feature": ["features", "feat"],
        "doc": ["docs", "documentation", "documented"],
        "config": ["configuration", "configure", "configured"],
        "setup": ["set-up", "set_up"],
        "api": ["apis"],
        "mcp": ["mcp-tool", "mcp_tool"],
    }

    def __init__(self, db_path: str | None = None) -> None:
        from pathlib import Path

        from whitemagic.config.paths import DB_PATH
        self.db_path = str(Path(db_path)) if db_path else str(DB_PATH)
        self._conn: sqlite3.Connection | None = None

    def _get_conn(self) -> sqlite3.Connection:
        conn = self._conn
        if conn is None:
            conn = sqlite3.connect(self.db_path)
            conn.row_factory = sqlite3.Row
            self._conn = conn
        return conn

    def normalize(self, dry_run: bool = False) -> dict[str, Any]:
        """Run full normalization."""
        results = {"lowercased": 0, "merged": 0, "errors": 0}

        conn = self._get_conn()
        cur = conn.cursor()

        # Step 1: Lowercase all tags
        cur.execute("SELECT DISTINCT tag FROM tags WHERE tag != LOWER(tag)")
        upper_tags = [r["tag"] for r in cur.fetchall()]

        for tag in upper_tags:
            if not dry_run:
                cur.execute("UPDATE tags SET tag = LOWER(tag) WHERE tag = ?", (tag,))
            results["lowercased"] += 1

        # Step 2: Merge synonyms
        for canonical, variants in self.SYNONYMS.items():
            for variant in variants:
                cur.execute("SELECT COUNT(*) FROM tags WHERE tag = ?", (variant,))
                count = cur.fetchone()[0]
                if count > 0:
                    if not dry_run:
                        cur.execute("UPDATE tags SET tag = ? WHERE tag = ?",
                                   (canonical, variant))
                    results["merged"] += count

        if not dry_run:
            conn.commit()

        return results

    def find_orphans(self) -> list[str]:
        """Find tags used only once."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT tag, COUNT(*) as cnt FROM tags
            GROUP BY tag HAVING cnt = 1
            ORDER BY tag
        """)

        return [r["tag"] for r in cur.fetchall()]

    def find_similar(self, threshold: float = 0.8) -> list[tuple]:
        """Find similar tags that might be duplicates."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("SELECT DISTINCT tag FROM tags")
        tags = [r["tag"] for r in cur.fetchall()]

        # Try Rust acceleration
        try:
            from whitemagic.utils.rust_helper import get_rust_module, is_rust_available
            if is_rust_available():
                rs = get_rust_module()
                synthesis = getattr(rs, "synthesis", None) if rs is not None else None
                if synthesis is not None and hasattr(synthesis, "find_similar_strings"):
                    # Convert threshold to max distance (approximate)
                    # For short tags (avg 5-10 chars), 0.8 threshold means ~1-2 edits allowed
                    # Let's use a dynamic approach or fixed max distance of 2 for now
                    max_dist = 2

                    # Rust returns list of (s1, s2, distance)
                    similar_raw = synthesis.find_similar_strings(tags, max_dist)

                    similar_pairs = []
                    for s1, s2, dist in similar_raw:
                        # Convert distance back to similarity score
                        max_len = max(len(s1), len(s2))
                        sim = 1.0 - (dist / max_len)
                        if sim >= threshold:
                            similar_pairs.append((s1, s2, sim))

                    return sorted(similar_pairs, key=lambda x: -x[2])
        except ImportError:
            pass
        except Exception:
            # Fallback to Python if Rust fails
            pass

        # Python Fallback
        similar_pairs = []
        for i, tag1 in enumerate(tags):
            for tag2 in tags[i+1:]:
                similarity = self._levenshtein_similarity(tag1, tag2)
                if similarity >= threshold and tag1 != tag2:
                    similar_pairs.append((tag1, tag2, similarity))

        return sorted(similar_pairs, key=lambda x: -x[2])

    def _levenshtein_similarity(self, s1: str, s2: str) -> float:
        """Calculate Levenshtein similarity ratio."""
        if s1 == s2:
            return 1.0

        len1, len2 = len(s1), len(s2)
        if len1 == 0 or len2 == 0:
            return 0.0

        # Simple Levenshtein distance
        matrix = [[0] * (len2 + 1) for _ in range(len1 + 1)]

        for i in range(len1 + 1):
            matrix[i][0] = i
        for j in range(len2 + 1):
            matrix[0][j] = j

        for i in range(1, len1 + 1):
            for j in range(1, len2 + 1):
                cost = 0 if s1[i-1] == s2[j-1] else 1
                matrix[i][j] = min(
                    matrix[i-1][j] + 1,
                    matrix[i][j-1] + 1,
                    matrix[i-1][j-1] + cost,
                )

        distance = matrix[len1][len2]
        max_len = max(len1, len2)
        return 1.0 - (distance / max_len)

    def infer_tags(self, memory_id: str, content: str) -> list[str]:
        """Infer tags from content."""
        tags = set()
        content_lower = content.lower()

        # Check for common keywords
        keywords = {
            "rust": ["rust", "cargo", "whitemagic_rs", "maturin"],
            "python": ["python", ".py", "pip", "pytest"],
            "mcp": ["mcp", "tool", "server"],
            "memory": ["memory", "recall", "store", "remember"],
            "test": ["test", "testing", "pytest", "assert"],
            "fix": ["fix", "bug", "error", "issue"],
            "feature": ["feature", "implement", "add", "new"],
            "refactor": ["refactor", "clean", "reorganize"],
            "docs": ["documentation", "readme", "guide"],
            "config": ["config", "settings", "setup"],
            "api": ["api", "endpoint", "route"],
            "performance": ["performance", "speed", "optimize", "fast"],
            "security": ["security", "auth", "permission"],
        }

        for tag, markers in keywords.items():
            for marker in markers:
                if marker in content_lower:
                    tags.add(tag)
                    break

        # Check for version patterns
        if re.search(r"v\d+\.\d+", content):
            tags.add("version")

        # Check for phase markers
        if re.search(r"phase \d+", content_lower):
            tags.add("milestone")

        return list(tags)

    def auto_tag_untagged(self, dry_run: bool = False) -> dict[str, Any]:
        """Auto-tag memories without tags."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT m.id, m.content FROM memories m
            LEFT JOIN tags t ON m.id = t.memory_id
            WHERE t.tag IS NULL
        """)

        untagged = cur.fetchall()
        results = {"tagged": 0, "skipped": 0}

        for mem in untagged:
            tags = self.infer_tags(mem["id"], mem["content"] or "")

            if tags:
                if not dry_run:
                    for tag in tags:
                        cur.execute("INSERT OR IGNORE INTO tags (memory_id, tag) VALUES (?, ?)",
                                   (mem["id"], tag))
                results["tagged"] += 1
            else:
                results["skipped"] += 1

        if not dry_run:
            conn.commit()

        return results

# Global instance
_tag_normalizer = None

def get_tag_normalizer() -> TagNormalizer:
    global _tag_normalizer
    if _tag_normalizer is None:
        _tag_normalizer = TagNormalizer()
    return _tag_normalizer
