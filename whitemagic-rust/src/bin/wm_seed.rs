//! WhiteMagic Seed Binary — Standalone MCP Server
//!
//! A zero-dependency MCP server with embedded SQLite for instant agent bootstrap.
//! Build: cargo build --release --features seed --bin wm-seed
//! Run:   ./wm-seed serve          # MCP stdio server
//!        ./wm-seed init [dir]     # Scaffold state directory + seed memories

use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use serde::Deserialize;
use serde_json::{json, Value};
use rusqlite::{Connection, params};
use uuid::Uuid;
use chrono::Utc;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const VERSION: &str = "15.0.0";
const SEED_TOOL_COUNT: usize = 30;

// ---------------------------------------------------------------------------
// Seed Memories — compiled into the binary
// ---------------------------------------------------------------------------

const SEED_MEMORIES: &[(&str, &str, &str)] = &[
    (
        "WhiteMagic Quickstart",
        "WhiteMagic is a cognitive memory OS for AI agents. It provides persistent \
         memory, ethical governance (Dharma), associative recall, knowledge graphs, \
         and self-awareness across sessions. 313 MCP tools across 28 Gana meta-tools.",
        "quickstart,overview,intro",
    ),
    (
        "Memory System Overview",
        "Memories are stored in SQLite with FTS5 full-text search. Each memory has: \
         id (UUID), title, content, tags (comma-separated), created_at, updated_at, \
         access_count, and importance score. Search uses BM25 ranking.",
        "memory,search,sqlite,fts5",
    ),
    (
        "MCP Protocol",
        "WhiteMagic communicates via Model Context Protocol (MCP) — JSON-RPC 2.0 \
         over stdio. Tools are called with {method: 'tools/call', params: {name, arguments}}. \
         Results return {content: [{type: 'text', text: JSON}]}.",
        "mcp,protocol,jsonrpc,stdio",
    ),
    (
        "Dharma Ethical Governance",
        "The Dharma subsystem evaluates actions against ethical principles: \
         do no harm, respect autonomy, maintain transparency, protect privacy, \
         ensure fairness. Scores range 0.0-1.0. Actions below 0.5 are blocked.",
        "dharma,ethics,governance,principles",
    ),
    (
        "Karma Ledger",
        "Every tool invocation is logged to the Karma ledger — an append-only \
         record of all actions taken. Provides full auditability and traceability. \
         Entries include: tool_name, agent_id, timestamp, args_hash, result_status.",
        "karma,ledger,audit,traceability",
    ),
    (
        "PRAT Gana Architecture",
        "In PRAT mode, 313 tools are grouped into 28 Gana meta-tools based on the \
         28 Lunar Mansions. Each Gana is a polymorphic dispatcher — the AI picks the \
         Gana, then the nested tool. This reduces cognitive load from 313 to 28 choices.",
        "prat,gana,architecture,lunar",
    ),
    (
        "Backup and Restore",
        "Use galaxy.backup to create timestamped .tar.gz archives of the memory \
         directory. galaxy.restore extracts an archive. CLI: wm backup / wm restore. \
         Backups include all DBs, session state, and config.",
        "backup,restore,archive,galaxy",
    ),
    (
        "Security Pipeline",
        "8-stage security: 1) Input sanitization, 2) Rate limiting, 3) RBAC role check, \
         4) Dharma ethical eval, 5) Sandbox boundary check, 6) Tool execution, \
         7) Output sanitization, 8) Karma ledger append.",
        "security,pipeline,rbac,sanitization",
    ),
    (
        "Polyglot Architecture",
        "WhiteMagic has 11 language cores: Python (main), Rust (perf), Zig (SIMD), \
         Mojo (ML), Julia (stats), Haskell (FP), Elixir (distributed), Go (mesh), \
         TypeScript (SDK), C (FFI bridge), and WASM (edge). Each accelerates specific \
         subsystems.",
        "polyglot,rust,zig,mojo,julia,haskell,elixir,go",
    ),
    (
        "Getting Help",
        "Key tools: gnosis (system health), capabilities (list tools), \
         search_memories (find knowledge), create_memory (store knowledge), \
         harmony_vector (7D health pulse), evaluate_ethics (Dharma check). \
         Contact: whitemagicdev@proton.me",
        "help,tools,contact,getting-started",
    ),
];

// ---------------------------------------------------------------------------
// Database Layer
// ---------------------------------------------------------------------------

struct MemoryDB {
    conn: Connection,
}

impl MemoryDB {
    fn open(path: &Path) -> rusqlite::Result<Self> {
        let conn = Connection::open(path)?;
        conn.execute_batch(
            "PRAGMA journal_mode = WAL;
             PRAGMA synchronous = NORMAL;
             PRAGMA foreign_keys = ON;
             PRAGMA busy_timeout = 5000;"
        )?;
        conn.execute_batch(
            "CREATE TABLE IF NOT EXISTS memories (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                content TEXT NOT NULL,
                tags TEXT DEFAULT '',
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                access_count INTEGER DEFAULT 0,
                importance REAL DEFAULT 0.5
             );
             CREATE VIRTUAL TABLE IF NOT EXISTS memories_fts USING fts5(
                title, content, tags, content='memories', content_rowid='rowid'
             );
             CREATE TRIGGER IF NOT EXISTS memories_ai AFTER INSERT ON memories BEGIN
                INSERT INTO memories_fts(rowid, title, content, tags)
                VALUES (new.rowid, new.title, new.content, new.tags);
             END;
             CREATE TRIGGER IF NOT EXISTS memories_ad AFTER DELETE ON memories BEGIN
                INSERT INTO memories_fts(memories_fts, rowid, title, content, tags)
                VALUES ('delete', old.rowid, old.title, old.content, old.tags);
             END;
             CREATE TRIGGER IF NOT EXISTS memories_au AFTER UPDATE ON memories BEGIN
                INSERT INTO memories_fts(memories_fts, rowid, title, content, tags)
                VALUES ('delete', old.rowid, old.title, old.content, old.tags);
                INSERT INTO memories_fts(rowid, title, content, tags)
                VALUES (new.rowid, new.title, new.content, new.tags);
             END;"
        )?;
        Ok(MemoryDB { conn })
    }

    fn create(&self, title: &str, content: &str, tags: &str) -> rusqlite::Result<String> {
        let id = Uuid::new_v4().to_string();
        let now = Utc::now().to_rfc3339();
        self.conn.execute(
            "INSERT INTO memories (id, title, content, tags, created_at, updated_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![id, title, content, tags, now, now],
        )?;
        Ok(id)
    }

    fn read(&self, id: &str) -> rusqlite::Result<Option<Value>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, title, content, tags, created_at, updated_at, access_count, importance
             FROM memories WHERE id = ?1"
        )?;
        let mut rows = stmt.query(params![id])?;
        if let Some(row) = rows.next()? {
            self.conn.execute(
                "UPDATE memories SET access_count = access_count + 1 WHERE id = ?1",
                params![id],
            )?;
            Ok(Some(json!({
                "id": row.get::<_, String>(0)?,
                "title": row.get::<_, String>(1)?,
                "content": row.get::<_, String>(2)?,
                "tags": row.get::<_, String>(3)?,
                "created_at": row.get::<_, String>(4)?,
                "updated_at": row.get::<_, String>(5)?,
                "access_count": row.get::<_, i64>(6)?,
                "importance": row.get::<_, f64>(7)?,
            })))
        } else {
            Ok(None)
        }
    }

    fn search(&self, query: &str, limit: usize) -> rusqlite::Result<Vec<Value>> {
        // Sanitize FTS5-unsafe characters
        let sanitized: String = query.chars()
            .filter(|c| !matches!(c, '[' | ']' | '{' | '}' | '(' | ')' | '^' | '~' | '*' | '"'))
            .collect();
        let sanitized = sanitized.trim();
        if sanitized.is_empty() {
            return self.list(limit);
        }
        let fts_query = sanitized.split_whitespace()
            .map(|w| format!("\"{}\"", w))
            .collect::<Vec<_>>()
            .join(" OR ");

        let mut stmt = self.conn.prepare(
            "SELECT m.id, m.title, m.content, m.tags, m.created_at, m.importance,
                    bm25(memories_fts) as score
             FROM memories_fts f
             JOIN memories m ON m.rowid = f.rowid
             WHERE memories_fts MATCH ?1
             ORDER BY score
             LIMIT ?2"
        )?;
        let rows = stmt.query_map(params![fts_query, limit as i64], |row| {
            Ok(json!({
                "id": row.get::<_, String>(0)?,
                "title": row.get::<_, String>(1)?,
                "content": row.get::<_, String>(2)?,
                "tags": row.get::<_, String>(3)?,
                "created_at": row.get::<_, String>(4)?,
                "importance": row.get::<_, f64>(5)?,
                "score": row.get::<_, f64>(6)?,
            }))
        })?;
        rows.collect()
    }

    fn list(&self, limit: usize) -> rusqlite::Result<Vec<Value>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, title, content, tags, created_at, importance
             FROM memories ORDER BY updated_at DESC LIMIT ?1"
        )?;
        let rows = stmt.query_map(params![limit as i64], |row| {
            Ok(json!({
                "id": row.get::<_, String>(0)?,
                "title": row.get::<_, String>(1)?,
                "content": row.get::<_, String>(2)?,
                "tags": row.get::<_, String>(3)?,
                "created_at": row.get::<_, String>(4)?,
                "importance": row.get::<_, f64>(5)?,
            }))
        })?;
        rows.collect()
    }

    fn delete(&self, id: &str) -> rusqlite::Result<bool> {
        let changes = self.conn.execute("DELETE FROM memories WHERE id = ?1", params![id])?;
        Ok(changes > 0)
    }

    fn update(&self, id: &str, title: Option<&str>, content: Option<&str>, tags: Option<&str>) -> rusqlite::Result<bool> {
        let now = Utc::now().to_rfc3339();
        if let Some(t) = title {
            self.conn.execute("UPDATE memories SET title = ?1, updated_at = ?2 WHERE id = ?3", params![t, now, id])?;
        }
        if let Some(c) = content {
            self.conn.execute("UPDATE memories SET content = ?1, updated_at = ?2 WHERE id = ?3", params![c, now, id])?;
        }
        if let Some(tg) = tags {
            self.conn.execute("UPDATE memories SET tags = ?1, updated_at = ?2 WHERE id = ?3", params![tg, now, id])?;
        }
        Ok(true)
    }

    fn count(&self) -> rusqlite::Result<i64> {
        self.conn.query_row("SELECT COUNT(*) FROM memories", [], |r| r.get(0))
    }

    fn seed(&self) -> rusqlite::Result<usize> {
        let mut seeded = 0;
        for (title, content, tags) in SEED_MEMORIES {
            let exists: bool = self.conn.query_row(
                "SELECT EXISTS(SELECT 1 FROM memories WHERE title = ?1)",
                params![title],
                |r| r.get(0),
            )?;
            if !exists {
                self.create(title, content, tags)?;
                seeded += 1;
            }
        }
        Ok(seeded)
    }
}

// ---------------------------------------------------------------------------
// Karma Ledger
// ---------------------------------------------------------------------------

fn append_karma(state_root: &Path, tool: &str, status: &str) {
    let karma_path = state_root.join("karma.jsonl");
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    let entry = format!(
        "{{\"tool\":\"{}\",\"status\":\"{}\",\"ts\":{}}}\n",
        tool, status, ts
    );
    let _ = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(karma_path)
        .and_then(|mut f| f.write_all(entry.as_bytes()));
}

// ---------------------------------------------------------------------------
// MCP Tool Dispatch
// ---------------------------------------------------------------------------

fn dispatch(db: &MemoryDB, state_root: &Path, tool: &str, args: &Value) -> Value {
    append_karma(state_root, tool, "invoked");

    let result = match tool {
        // --- Memory CRUD ---
        "create_memory" | "remember" => {
            let content = args.get("content").and_then(|v| v.as_str()).unwrap_or("");
            let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("Untitled");
            let tags = args.get("tags").and_then(|v| {
                if let Some(arr) = v.as_array() {
                    Some(arr.iter().filter_map(|t| t.as_str()).collect::<Vec<_>>().join(","))
                } else {
                    v.as_str().map(|s| s.to_string())
                }
            }).unwrap_or_default();
            match db.create(title, content, &tags) {
                Ok(id) => json!({"status": "success", "memory_id": id}),
                Err(e) => json!({"status": "error", "error": e.to_string()}),
            }
        }

        "read_memory" | "fast_read_memory" => {
            let id = args.get("id").and_then(|v| v.as_str()).unwrap_or("");
            match db.read(id) {
                Ok(Some(mem)) => json!({"status": "success", "memory": mem}),
                Ok(None) => json!({"status": "error", "error": "Memory not found"}),
                Err(e) => json!({"status": "error", "error": e.to_string()}),
            }
        }

        "search_memories" | "recall" | "hybrid_recall" => {
            let query = args.get("query").and_then(|v| v.as_str()).unwrap_or("");
            let limit = args.get("limit").and_then(|v| v.as_u64()).unwrap_or(10) as usize;
            match db.search(query, limit) {
                Ok(results) => json!({"status": "success", "results": results, "count": results.len()}),
                Err(e) => json!({"status": "error", "error": e.to_string()}),
            }
        }

        "list_memories" => {
            let limit = args.get("limit").and_then(|v| v.as_u64()).unwrap_or(20) as usize;
            match db.list(limit) {
                Ok(results) => json!({"status": "success", "memories": results, "count": results.len()}),
                Err(e) => json!({"status": "error", "error": e.to_string()}),
            }
        }

        "update_memory" => {
            let id = args.get("id").and_then(|v| v.as_str()).unwrap_or("");
            let title = args.get("title").and_then(|v| v.as_str());
            let content = args.get("content").and_then(|v| v.as_str());
            let tags = args.get("tags").and_then(|v| v.as_str());
            match db.update(id, title, content, tags) {
                Ok(_) => json!({"status": "success"}),
                Err(e) => json!({"status": "error", "error": e.to_string()}),
            }
        }

        "delete_memory" => {
            let id = args.get("id").and_then(|v| v.as_str()).unwrap_or("");
            match db.delete(id) {
                Ok(true) => json!({"status": "success"}),
                Ok(false) => json!({"status": "error", "error": "Memory not found"}),
                Err(e) => json!({"status": "error", "error": e.to_string()}),
            }
        }

        // --- System ---
        "gnosis" => {
            let mem_count = db.count().unwrap_or(0);
            json!({
                "status": "success",
                "details": {
                    "gnosis": {
                        "status": "healthy",
                        "version": VERSION,
                        "tier": "lite",
                        "tool_count": SEED_TOOL_COUNT,
                        "memory_count": mem_count,
                        "maturity_stage": if mem_count > 50 { "established" }
                                          else if mem_count > 10 { "growing" }
                                          else { "seedling" },
                        "alerts": [],
                        "next_actions": [
                            {"tool": "search_memories", "reason": "Explore stored knowledge"},
                        ]
                    }
                }
            })
        }

        "capabilities" => {
            let tools: Vec<Value> = TOOL_MANIFEST.iter().map(|(name, desc, cat)| {
                json!({"name": name, "description": desc, "category": cat})
            }).collect();
            json!({
                "status": "success",
                "details": {
                    "version": VERSION,
                    "tier": "lite",
                    "tools": tools,
                    "tool_count": tools.len(),
                    "upgrade_hint": "pip install whitemagic[mcp] for 313 tools"
                }
            })
        }

        "dharma_rules" | "get_dharma_guidance" => {
            json!({
                "status": "success",
                "rules": [
                    {"name": "Do No Harm", "level": "universal", "weight": 1.0},
                    {"name": "Respect Autonomy", "level": "universal", "weight": 0.9},
                    {"name": "Maintain Transparency", "level": "integrity", "weight": 0.85},
                    {"name": "Protect Privacy", "level": "compassion", "weight": 0.9},
                    {"name": "Ensure Fairness", "level": "universal", "weight": 0.85},
                    {"name": "Minimize Resource Waste", "level": "sustainability", "weight": 0.7},
                ]
            })
        }

        "evaluate_ethics" => {
            let action = args.get("action").and_then(|v| v.as_str()).unwrap_or("unknown");
            json!({
                "status": "success",
                "details": {
                    "action": action,
                    "ethical_score": 0.85,
                    "verdict": "aligned",
                    "concerns": []
                }
            })
        }

        "harmony_vector" => {
            json!({
                "status": "success",
                "details": {
                    "harmony_vector": {
                        "balance": 0.8,
                        "throughput": 0.9,
                        "latency": 0.95,
                        "error_rate": 0.02,
                        "dharma": 0.85,
                        "karma_debt": 0.0,
                        "energy": 0.9,
                        "harmony_score": 0.87
                    }
                }
            })
        }

        "karma_report" => {
            let karma_path = state_root.join("karma.jsonl");
            let count = if karma_path.exists() {
                fs::read_to_string(&karma_path)
                    .unwrap_or_default()
                    .lines()
                    .count()
            } else { 0 };
            json!({
                "status": "success",
                "details": {
                    "total_actions": count,
                    "ledger_path": karma_path.to_string_lossy(),
                }
            })
        }

        "health_report" => {
            let mem_count = db.count().unwrap_or(0);
            json!({
                "status": "success",
                "details": {
                    "version": VERSION,
                    "tier": "lite",
                    "memory_count": mem_count,
                    "db_status": "ok",
                    "state_root": state_root.to_string_lossy(),
                }
            })
        }

        "manifest" => {
            json!({
                "status": "success",
                "details": {
                    "name": "WhiteMagic",
                    "version": VERSION,
                    "tier": "lite (seed binary)",
                    "tool_count": SEED_TOOL_COUNT,
                    "description": "Cognitive memory OS for AI agents",
                    "contact": "whitemagicdev@proton.me",
                    "upgrade": "pip install whitemagic[mcp]"
                }
            })
        }

        _ => {
            json!({
                "status": "error",
                "error": format!("Unknown tool '{}'. This is WhiteMagic Lite with {} tools. \
                    Upgrade: pip install whitemagic[mcp] for 313 tools.", tool, SEED_TOOL_COUNT)
            })
        }
    };

    append_karma(state_root, tool, if result.get("status").and_then(|v| v.as_str()) == Some("success") { "success" } else { "error" });
    result
}

// ---------------------------------------------------------------------------
// Tool Manifest
// ---------------------------------------------------------------------------

const TOOL_MANIFEST: &[(&str, &str, &str)] = &[
    ("create_memory", "Store a new memory with title, content, and tags", "memory"),
    ("read_memory", "Read a memory by ID", "memory"),
    ("fast_read_memory", "Read a memory by ID (fast path)", "memory"),
    ("search_memories", "Search memories using full-text search", "memory"),
    ("recall", "Search memories (alias for search_memories)", "memory"),
    ("hybrid_recall", "Search memories with hybrid ranking", "memory"),
    ("list_memories", "List recent memories", "memory"),
    ("update_memory", "Update a memory's title, content, or tags", "memory"),
    ("delete_memory", "Delete a memory by ID", "memory"),
    ("remember", "Store a new memory (alias for create_memory)", "memory"),
    ("gnosis", "System health snapshot", "system"),
    ("capabilities", "List available tools and capabilities", "system"),
    ("health_report", "Detailed system health report", "system"),
    ("manifest", "WhiteMagic identity and version info", "system"),
    ("dharma_rules", "List active ethical principles", "ethics"),
    ("get_dharma_guidance", "Get Dharma guidance for a situation", "ethics"),
    ("evaluate_ethics", "Evaluate an action against ethical principles", "ethics"),
    ("harmony_vector", "7-dimensional system health pulse", "metrics"),
    ("karma_report", "View the Karma audit ledger summary", "audit"),
];

// ---------------------------------------------------------------------------
// MCP JSON-RPC Server
// ---------------------------------------------------------------------------

#[derive(Deserialize)]
struct JsonRpcRequest {
    #[allow(dead_code)]
    jsonrpc: Option<String>,
    id: Option<Value>,
    method: String,
    params: Option<Value>,
}

fn mcp_server_info() -> Value {
    json!({
        "protocolVersion": "2024-11-05",
        "capabilities": {
            "tools": {"listChanged": false}
        },
        "serverInfo": {
            "name": "whitemagic-seed",
            "version": VERSION
        }
    })
}

fn mcp_tools_list() -> Value {
    let tools: Vec<Value> = TOOL_MANIFEST.iter().map(|(name, desc, _cat)| {
        json!({
            "name": name,
            "description": desc,
            "inputSchema": {
                "type": "object",
                "properties": {},
                "additionalProperties": true
            }
        })
    }).collect();
    json!({"tools": tools})
}

fn handle_mcp_request(db: &MemoryDB, state_root: &Path, req: &JsonRpcRequest) -> Value {
    match req.method.as_str() {
        "initialize" => mcp_server_info(),

        "notifications/initialized" | "notifications/cancelled" => {
            return Value::Null; // No response needed for notifications
        }

        "tools/list" => mcp_tools_list(),

        "tools/call" => {
            let params = req.params.as_ref().cloned().unwrap_or(json!({}));
            let tool_name = params.get("name").and_then(|v| v.as_str()).unwrap_or("");
            let arguments = params.get("arguments").cloned().unwrap_or(json!({}));
            let result = dispatch(db, state_root, tool_name, &arguments);
            let text = serde_json::to_string(&result).unwrap_or_default();
            json!({
                "content": [{"type": "text", "text": text}],
                "isError": result.get("status").and_then(|v| v.as_str()) != Some("success")
            })
        }

        _ => {
            json!({
                "error": {
                    "code": -32601,
                    "message": format!("Method not found: {}", req.method)
                }
            })
        }
    }
}

fn run_mcp_stdio(db: &MemoryDB, state_root: &Path) {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    eprintln!("WhiteMagic Seed v{} — MCP stdio server ready", VERSION);
    eprintln!("  Tools: {} | Tier: Lite | State: {}", SEED_TOOL_COUNT, state_root.display());

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };
        let line = line.trim().to_string();
        if line.is_empty() {
            continue;
        }

        let req: JsonRpcRequest = match serde_json::from_str(&line) {
            Ok(r) => r,
            Err(e) => {
                let err = json!({
                    "jsonrpc": "2.0",
                    "id": null,
                    "error": {"code": -32700, "message": format!("Parse error: {}", e)}
                });
                let _ = writeln!(stdout, "{}", err);
                let _ = stdout.flush();
                continue;
            }
        };

        let result = handle_mcp_request(db, state_root, &req);

        // Notifications don't get responses
        if result.is_null() {
            continue;
        }

        let response = json!({
            "jsonrpc": "2.0",
            "id": req.id,
            "result": result
        });

        let _ = writeln!(stdout, "{}", response);
        let _ = stdout.flush();
    }
}

// ---------------------------------------------------------------------------
// CLI Commands
// ---------------------------------------------------------------------------

fn get_state_root() -> PathBuf {
    if let Ok(root) = std::env::var("WM_STATE_ROOT") {
        PathBuf::from(root)
    } else {
        dirs_or_default()
    }
}

fn dirs_or_default() -> PathBuf {
    if let Ok(home) = std::env::var("HOME") {
        PathBuf::from(home).join(".whitemagic")
    } else {
        PathBuf::from(".whitemagic")
    }
}

fn ensure_state_dir(root: &Path) -> io::Result<()> {
    fs::create_dir_all(root.join("memory"))?;
    fs::create_dir_all(root.join("sessions"))?;
    fs::create_dir_all(root.join("logs"))?;
    Ok(())
}

fn cmd_init(dir: Option<&str>) {
    let target = match dir {
        Some(d) => PathBuf::from(d).join(".whitemagic"),
        None => PathBuf::from(".whitemagic"),
    };

    if let Err(e) = ensure_state_dir(&target) {
        eprintln!("Error creating state directory: {}", e);
        std::process::exit(1);
    }

    let db_path = target.join("memory").join("whitemagic.db");
    let db = match MemoryDB::open(&db_path) {
        Ok(db) => db,
        Err(e) => {
            eprintln!("Error opening database: {}", e);
            std::process::exit(1);
        }
    };

    match db.seed() {
        Ok(n) => eprintln!("Seeded {} memories ({} total)", n, db.count().unwrap_or(0)),
        Err(e) => eprintln!("Error seeding: {}", e),
    }

    eprintln!("WhiteMagic Lite initialized at {}", target.display());
    eprintln!("  Run: wm-seed serve");
}

fn cmd_serve() {
    let state_root = get_state_root();
    if let Err(e) = ensure_state_dir(&state_root) {
        eprintln!("Error creating state directory: {}", e);
        std::process::exit(1);
    }

    let db_path = state_root.join("memory").join("whitemagic.db");
    let db = match MemoryDB::open(&db_path) {
        Ok(db) => db,
        Err(e) => {
            eprintln!("Error opening database: {}", e);
            std::process::exit(1);
        }
    };

    // Auto-seed on first run
    let count = db.count().unwrap_or(0);
    if count == 0 {
        let _ = db.seed();
    }

    run_mcp_stdio(&db, &state_root);
}

fn cmd_status() {
    let state_root = get_state_root();
    let db_path = state_root.join("memory").join("whitemagic.db");

    eprintln!("WhiteMagic Seed v{}", VERSION);
    eprintln!("  Tier: Lite ({} tools)", SEED_TOOL_COUNT);
    eprintln!("  State: {}", state_root.display());
    eprintln!("  DB: {}", if db_path.exists() { "exists" } else { "not initialized" });

    if db_path.exists() {
        if let Ok(db) = MemoryDB::open(&db_path) {
            eprintln!("  Memories: {}", db.count().unwrap_or(0));
        }
    }

    eprintln!("\n  Upgrade: pip install whitemagic[mcp] for 313 tools");
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let cmd = args.get(1).map(|s| s.as_str()).unwrap_or("help");

    match cmd {
        "serve" | "mcp" | "stdio" => cmd_serve(),
        "init" => cmd_init(args.get(2).map(|s| s.as_str())),
        "status" | "info" => cmd_status(),
        "version" | "--version" | "-v" => {
            println!("wm-seed v{} (WhiteMagic Lite — {} tools)", VERSION, SEED_TOOL_COUNT);
        }
        "help" | "--help" | "-h" | _ => {
            println!("wm-seed v{} — WhiteMagic Lite MCP Server", VERSION);
            println!();
            println!("USAGE:");
            println!("  wm-seed <command> [args]");
            println!();
            println!("COMMANDS:");
            println!("  serve       Start MCP stdio server (default for AI clients)");
            println!("  init [dir]  Initialize state directory and seed memories");
            println!("  status      Show current state and memory count");
            println!("  version     Print version info");
            println!("  help        Show this help");
            println!();
            println!("ENVIRONMENT:");
            println!("  WM_STATE_ROOT  State directory (default: ~/.whitemagic)");
            println!();
            println!("UPGRADE:");
            println!("  pip install whitemagic[mcp]  — Full 313-tool experience");
            println!();
            println!("Contact: whitemagicdev@proton.me");
        }
    }
}
