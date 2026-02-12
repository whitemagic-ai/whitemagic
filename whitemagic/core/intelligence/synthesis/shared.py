"""Shared Synthesis Utilities - v12.3 Deduplication
Unifies logic for velocity tracking, intent classification, and pattern bridging.
"""

import logging
import sqlite3
from datetime import datetime
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

def calculate_unified_velocity(db_path: str | None = None, task_file: Path | None = None) -> dict[str, Any]:
    """Calculate unified velocity metrics across database (memories) and task files.

    Returns:
        Dict containing memory_velocity, task_willpower, and combined_momentum.

    """
    from whitemagic.config.paths import DB_PATH, WM_ROOT

    db_path = db_path or str(DB_PATH)
    task_file = task_file or (WM_ROOT / "tasks" / "task.md")

    # 1. Memory Velocity (from DB)
    memory_stats = {"last_7d": 0, "last_30d": 0, "last_7_days": 0, "last_30_days": 0, "total": 0}
    try:
        conn = sqlite3.connect(db_path)
        conn.row_factory = sqlite3.Row
        cur = conn.cursor()
        cur.execute("""
            SELECT
                COUNT(*) as total,
                COUNT(CASE WHEN created_at > datetime('now', '-7 days') THEN 1 END) as last_7d,
                COUNT(CASE WHEN created_at > datetime('now', '-30 days') THEN 1 END) as last_30d
            FROM memories
        """)
        row = cur.fetchone()
        if row:
            memory_stats = {
                "last_7d": row["last_7d"],
                "last_30d": row["last_30d"],
                "last_7_days": row["last_7d"],
                "last_30_days": row["last_30d"],
                "total": row["total"],
            }
        conn.close()
    except Exception as e:
        logger.warning(f"Failed to fetch memory velocity: {e}")

    # 2. Task Willpower (from task.md)
    task_stats = {"willpower": 0.0, "velocity": 0.0, "tasks_done": 0, "tasks_total": 0, "momentum": 0}
    if task_file.exists():
        try:
            content = task_file.read_text(encoding="utf-8")
            lines = content.splitlines()
            total = sum(1 for line in lines if "[ ]" in line or "[x]" in line or "[/]" in line)
            done = sum(1 for line in lines if "[x]" in line)
            in_progress = sum(1 for line in lines if "[/]" in line)

            willpower = (done + (in_progress * 0.5)) / max(1, total)
            task_stats = {
                "willpower": round(willpower, 2),
                "velocity": round(done / max(1, total), 2),
                "tasks_done": done,
                "tasks_total": total,
                "momentum": in_progress,
            }
        except Exception as e:
            logger.warning(f"Failed to parse task willpower: {e}")

    # 3. Combined Metrics
    # Normalize memory velocity to a 0-1 scale relative to a high watermark (e.g., 50 per week)
    mem_velocity_score = min(1.0, memory_stats["last_7d"] / 50.0)

    combined_momentum = (mem_velocity_score + task_stats["willpower"]) / 2.0

    return {
        "memory": memory_stats,
        "tasks": task_stats,
        "combined_momentum": round(combined_momentum, 2),
        "timestamp": datetime.now().isoformat(),
    }

def classify_intent(text: str) -> str:
    """Unified intent classification for Zodiac and Predictive engines.
    """
    text_lower = text.lower()

    mappings = {
        "creative": ["create", "design", "imagine", "invent", "art", "express", "poem", "story"],
        "analytical": ["analyze", "organize", "plan", "structure", "optimize", "debug", "audit", "logic"],
        "communication": ["communicate", "explain", "teach", "write", "speak", "connect", "learn"],
        "transformation": ["transform", "change", "refactor", "deepen", "integrate", "heal", "purify"],
        "nurturing": ["nurture", "support", "care", "guide", "protect", "comfort", "feel", "intuition"],
        "action": ["execute", "implement", "build", "run", "start", "launch", "act", "do"],
    }

    scores = {}
    for intent, keywords in mappings.items():
        scores[intent] = sum(1 for k in keywords if k in text_lower)

    # Default to action if no matches
    if not any(scores.values()):
        return "action"

    return max(scores.items(), key=lambda x: x[1])[0]
