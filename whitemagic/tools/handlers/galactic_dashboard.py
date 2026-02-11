"""MCP handler for Memory Lifecycle / Galactic Map dashboard data."""

from typing import Any


def handle_galactic_dashboard(**kwargs: Any) -> dict[str, Any]:
    """Rich Galactic Map dashboard data for Nexus visualization.
    Returns zone counts, top memories per zone, retention stats,
    and dream cycle activity.
    """
    import sqlite3

    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
    except Exception as e:
        return {"status": "error", "error": f"Memory system unavailable: {e}"}

    try:
        with um.backend.pool.connection() as conn:
            conn.row_factory = sqlite3.Row

            # Zone counts
            zones = conn.execute("""
                SELECT
                  SUM(CASE WHEN galactic_distance < 0.15 THEN 1 ELSE 0 END) as core,
                  SUM(CASE WHEN galactic_distance >= 0.15 AND galactic_distance < 0.40 THEN 1 ELSE 0 END) as inner_rim,
                  SUM(CASE WHEN galactic_distance >= 0.40 AND galactic_distance < 0.65 THEN 1 ELSE 0 END) as mid_band,
                  SUM(CASE WHEN galactic_distance >= 0.65 AND galactic_distance < 0.85 THEN 1 ELSE 0 END) as outer_rim,
                  SUM(CASE WHEN galactic_distance >= 0.85 THEN 1 ELSE 0 END) as far_edge,
                  COUNT(*) as total,
                  SUM(is_protected) as protected,
                  AVG(retention_score) as avg_retention,
                  AVG(galactic_distance) as avg_distance
                FROM memories
            """).fetchone()

            zone_data = {
                "core": zones["core"] or 0,
                "inner_rim": zones["inner_rim"] or 0,
                "mid_band": zones["mid_band"] or 0,
                "outer_rim": zones["outer_rim"] or 0,
                "far_edge": zones["far_edge"] or 0,
                "total": zones["total"] or 0,
                "protected": zones["protected"] or 0,
                "avg_retention": round(zones["avg_retention"] or 0, 4),
                "avg_distance": round(zones["avg_distance"] or 0, 4),
            }

            # Type distribution
            type_rows = conn.execute("""
                SELECT memory_type, COUNT(*) as cnt
                FROM memories GROUP BY memory_type ORDER BY cnt DESC
            """).fetchall()
            type_dist = {r["memory_type"]: r["cnt"] for r in type_rows}

            # Top CORE memories (crown jewels)
            crown_jewels = []
            for row in conn.execute("""
                SELECT id, title, memory_type, importance, galactic_distance, retention_score
                FROM memories WHERE galactic_distance < 0.15
                ORDER BY importance DESC LIMIT 10
            """).fetchall():
                crown_jewels.append({
                    "id": row["id"][:12],
                    "title": (row["title"] or "")[:80],
                    "type": row["memory_type"],
                    "importance": round(row["importance"] or 0, 3),
                    "distance": round(row["galactic_distance"] or 0, 4),
                    "retention": round(row["retention_score"] or 0, 3),
                })

            # Recently swept (last retention sweep)
            recently_swept = conn.execute("""
                SELECT COUNT(*) as cnt FROM memories
                WHERE last_retention_sweep IS NOT NULL
                AND last_retention_sweep > datetime('now', '-1 day')
            """).fetchone()["cnt"]

    except Exception as e:
        return {"status": "error", "error": f"Database query failed: {e}"}

    # Dream cycle status
    dream_info = {}
    try:
        from whitemagic.core.dreaming import get_dream_cycle
        dc = get_dream_cycle()
        dream_info = dc.status()
    except Exception:
        dream_info = {"running": False, "dreaming": False}

    return {
        "status": "success",
        "zones": zone_data,
        "type_distribution": type_dist,
        "crown_jewels": crown_jewels,
        "recently_swept_24h": recently_swept,
        "dream_cycle": dream_info,
    }
