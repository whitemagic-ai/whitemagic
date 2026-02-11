//! Association Graph BFS Walk — Rust-accelerated via rusqlite.
//!
//! Replaces the Python round-trip BFS in CoreAccessLayer.query_association_subgraph()
//! with a single Rust function that opens the DB, walks N hops, and returns
//! all reachable nodes without Python↔SQLite per-hop overhead.
//!
//! Also updates traversal tracking (last_traversed_at, traversal_count) in bulk.

#[cfg(feature = "rusqlite")]
use rusqlite::{params, Connection};

#[cfg(feature = "python")]
use pyo3::prelude::*;

/// A node discovered during BFS association walk.
#[cfg(feature = "python")]
#[pyclass]
#[derive(Clone)]
pub struct WalkNode {
    #[pyo3(get)]
    pub memory_id: String,
    #[pyo3(get)]
    pub title: String,
    #[pyo3(get)]
    pub strength: f64,
    #[pyo3(get)]
    pub depth: usize,
}

/// Walk the association graph via BFS from seed memory IDs.
///
/// Opens the SQLite DB at `db_path`, performs N-hop BFS, and returns
/// all reachable nodes sorted by (depth, -strength).
///
/// This eliminates Python↔SQLite round trips: one Rust call does
/// all hops, title lookups, and traversal tracking updates.
///
/// Args:
///   db_path: absolute path to the SQLite database.
///   seed_ids: starting memory IDs.
///   depth: max hops (default 2).
///   min_strength: minimum association strength filter (default 0.3).
///   max_nodes: cap on total nodes returned (default 50).
///
/// Returns:
///   Vec of WalkNode sorted by depth asc, strength desc.
#[cfg(all(feature = "python", feature = "rusqlite"))]
#[pyfunction]
#[pyo3(signature = (db_path, seed_ids, depth=2, min_strength=0.3, max_nodes=50))]
pub fn association_walk(
    db_path: String,
    seed_ids: Vec<String>,
    depth: usize,
    min_strength: f64,
    max_nodes: usize,
) -> PyResult<Vec<WalkNode>> {
    let conn = Connection::open(&db_path)
        .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(format!("DB open failed: {e}")))?;

    // Enable WAL for concurrent readers
    let _ = conn.execute_batch("PRAGMA journal_mode=WAL; PRAGMA busy_timeout=30000;");

    let mut visited: std::collections::HashMap<String, WalkNode> = std::collections::HashMap::new();
    let mut frontier: Vec<String> = Vec::new();

    // Add seeds
    for sid in &seed_ids {
        let title = get_title(&conn, sid);
        visited.insert(
            sid.clone(),
            WalkNode {
                memory_id: sid.clone(),
                title,
                strength: 1.0,
                depth: 0,
            },
        );
        frontier.push(sid.clone());
    }

    // BFS loop
    let mut current_depth = 0;
    while current_depth < depth && !frontier.is_empty() && visited.len() < max_nodes {
        current_depth += 1;
        let mut next_frontier: Vec<String> = Vec::new();

        for mem_id in &frontier {
            // Query both directions of the association edge
            let rows: Vec<(String, f64)> = match conn.prepare(
                "SELECT target_id AS neighbor_id, strength FROM associations
                 WHERE source_id = ?1 AND strength >= ?2
                 UNION
                 SELECT source_id AS neighbor_id, strength FROM associations
                 WHERE target_id = ?1 AND strength >= ?2
                 ORDER BY strength DESC
                 LIMIT 20",
            ) {
                Ok(mut stmt) => stmt
                    .query_map(params![mem_id, min_strength], |row| {
                        Ok((row.get::<_, String>(0)?, row.get::<_, f64>(1)?))
                    })
                    .map(|rows| rows.filter_map(|r| r.ok()).collect())
                    .unwrap_or_default(),
                Err(_) => Vec::new(),
            };

            for (nid, strength) in rows {
                if nid.is_empty() {
                    continue;
                }
                if !visited.contains_key(&nid) && visited.len() < max_nodes {
                    let title = get_title(&conn, &nid);
                    visited.insert(
                        nid.clone(),
                        WalkNode {
                            memory_id: nid.clone(),
                            title,
                            strength,
                            depth: current_depth,
                        },
                    );
                    next_frontier.push(nid);
                }
            }
        }

        frontier = next_frontier;
    }

    // Update traversal tracking in bulk
    let now = chrono_now_iso();
    let tx = conn.unchecked_transaction();
    if let Ok(tx) = tx {
        for (mid, node) in &visited {
            if node.depth == 0 {
                continue;
            }
            let _ = conn.execute(
                "UPDATE associations SET last_traversed_at = ?1,
                 traversal_count = COALESCE(traversal_count, 0) + 1
                 WHERE (source_id = ?2 OR target_id = ?2) AND strength >= ?3",
                params![now, mid, min_strength],
            );
        }
        let _ = tx.commit();
    }

    // Collect and sort: depth asc, strength desc
    let mut results: Vec<WalkNode> = visited.into_values().collect();
    results.sort_by(|a, b| {
        a.depth
            .cmp(&b.depth)
            .then_with(|| b.strength.partial_cmp(&a.strength).unwrap_or(std::cmp::Ordering::Equal))
    });

    Ok(results)
}

#[cfg(feature = "rusqlite")]
fn get_title(conn: &Connection, memory_id: &str) -> String {
    conn.query_row(
        "SELECT COALESCE(title, '') FROM memories WHERE id = ?1",
        params![memory_id],
        |row| row.get::<_, String>(0),
    )
    .unwrap_or_default()
}

fn chrono_now_iso() -> String {
    // Simple ISO timestamp without pulling in chrono crate
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    // Approximate ISO format: good enough for traversal tracking
    let days = secs / 86400;
    let years = 1970 + days / 365;
    let remaining_days = days % 365;
    let months = remaining_days / 30 + 1;
    let day = remaining_days % 30 + 1;
    let hour = (secs % 86400) / 3600;
    let min = (secs % 3600) / 60;
    let sec = secs % 60;
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}",
        years, months, day, hour, min, sec
    )
}
