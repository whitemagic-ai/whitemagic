# TypeScript SDK — Design Document

> Architecture for a lightweight TypeScript client SDK aligned with WhiteMagic's local-first, encrypted memory model.
> Target: `@whitemagic/sdk` npm package.

---

## Goals

1. **TypeScript-native** client for WhiteMagic's MCP server (stdio or HTTP)
2. **Local-first** architecture — secrets and memory stay on the user's machine
3. **Encrypted local vault** — API keys stored in AES-GCM encrypted SQLite (same pattern as Python)
4. **Context sanitization** — Redact secrets before sending to LLM providers
5. **Zero server dependency** — Works entirely against local WhiteMagic instance

---

## Package Structure

```
@whitemagic/sdk/
├── src/
│   ├── client.ts        # MCP client (stdio + HTTP transport)
│   ├── types.ts         # TypeScript types for tool schemas and envelopes
│   ├── vault.ts         # Local encrypted secret storage
│   ├── sanitizer.ts     # Context sanitization (redact API keys before LLM calls)
│   ├── memory.ts        # High-level memory operations (create, search, read)
│   ├── galaxy.ts        # Galaxy management helpers
│   ├── governance.ts    # Dharma, karma, ethics helpers
│   └── index.ts         # Public API exports
├── package.json
├── tsconfig.json
└── README.md
```

---

## Core Client (`client.ts`)

```typescript
import { spawn, ChildProcess } from 'child_process';

export interface ToolResult {
  status: 'success' | 'error';
  tool: string;
  request_id: string;
  message: string;
  details: Record<string, unknown>;
  error_code?: string;
  retryable: boolean;
  timestamp: string;
}

export class WhiteMagicClient {
  private process: ChildProcess | null = null;
  private baseUrl: string | null = null;

  /**
   * Connect via stdio (recommended for local use).
   */
  async connectStdio(command = 'python', args = ['-m', 'whitemagic.run_mcp_lean']): Promise<void> {
    this.process = spawn(command, args, {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: { ...process.env, WM_MCP_PRAT: '1' },
    });
    // MCP stdio protocol initialization...
  }

  /**
   * Connect via HTTP (for remote or containerized instances).
   */
  async connectHttp(url = 'http://localhost:8770'): Promise<void> {
    this.baseUrl = url;
  }

  /**
   * Call a WhiteMagic tool.
   */
  async callTool(name: string, params: Record<string, unknown> = {}): Promise<ToolResult> {
    if (this.baseUrl) {
      return this.callToolHttp(name, params);
    }
    return this.callToolStdio(name, params);
  }

  // ... transport implementations
}
```

---

## Local Vault (`vault.ts`)

Encrypted local storage for API keys and secrets — mirrors the Python `wm vault` design.

```typescript
import { createCipheriv, createDecipheriv, randomBytes, scryptSync } from 'crypto';
import Database from 'better-sqlite3';

export class Vault {
  private db: Database.Database;
  private masterKey: Buffer;

  constructor(dbPath: string, passphrase: string) {
    this.db = new Database(dbPath);
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS vault (
        name TEXT PRIMARY KEY,
        encrypted_value BLOB NOT NULL,
        nonce BLOB NOT NULL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    `);

    // Derive master key from passphrase via scrypt
    const salt = this.getOrCreateSalt();
    this.masterKey = scryptSync(passphrase, salt, 32);
  }

  set(name: string, value: string): void {
    const nonce = randomBytes(12);
    const cipher = createCipheriv('aes-256-gcm', this.masterKey, nonce);
    const encrypted = Buffer.concat([cipher.update(value, 'utf8'), cipher.final(), cipher.getAuthTag()]);

    const now = new Date().toISOString();
    this.db.prepare(`
      INSERT OR REPLACE INTO vault (name, encrypted_value, nonce, created_at, updated_at)
      VALUES (?, ?, ?, COALESCE((SELECT created_at FROM vault WHERE name = ?), ?), ?)
    `).run(name, encrypted, nonce, name, now, now);
  }

  get(name: string): string | null {
    const row = this.db.prepare('SELECT encrypted_value, nonce FROM vault WHERE name = ?').get(name) as any;
    if (!row) return null;

    const nonce = row.nonce as Buffer;
    const encrypted = row.encrypted_value as Buffer;
    const authTag = encrypted.slice(-16);
    const ciphertext = encrypted.slice(0, -16);

    const decipher = createDecipheriv('aes-256-gcm', this.masterKey, nonce);
    decipher.setAuthTag(authTag);
    return decipher.update(ciphertext) + decipher.final('utf8');
  }

  list(): string[] {
    const rows = this.db.prepare('SELECT name FROM vault ORDER BY name').all() as any[];
    return rows.map(r => r.name);
  }

  private getOrCreateSalt(): Buffer {
    // Store salt alongside the DB
    const saltPath = this.db.name.replace(/\.db$/, '.salt');
    const fs = require('fs');
    if (fs.existsSync(saltPath)) {
      return fs.readFileSync(saltPath);
    }
    const salt = randomBytes(32);
    fs.writeFileSync(saltPath, salt);
    return salt;
  }
}
```

---

## Context Sanitizer (`sanitizer.ts`)

Same patterns as the Python `_sanitize_context()` — redact secrets before sending to LLMs.

```typescript
const SANITIZE_PATTERNS: [RegExp, string][] = [
  [/sk-[A-Za-z0-9]{20,}/g, '[REDACTED_API_KEY]'],
  [/sk-proj-[A-Za-z0-9_-]{40,}/g, '[REDACTED_API_KEY]'],
  [/ghp_[A-Za-z0-9]{36,}/g, '[REDACTED_GITHUB_TOKEN]'],
  [/gho_[A-Za-z0-9]{36,}/g, '[REDACTED_GITHUB_TOKEN]'],
  [/xox[bpsar]-[A-Za-z0-9-]{10,}/g, '[REDACTED_SLACK_TOKEN]'],
  [/-----BEGIN [A-Z ]*PRIVATE KEY-----[\s\S]*?-----END [A-Z ]*PRIVATE KEY-----/g, '[REDACTED_PRIVATE_KEY]'],
  [/eyJ[A-Za-z0-9_-]{20,}\.eyJ[A-Za-z0-9_-]{20,}\.[A-Za-z0-9_-]{20,}/g, '[REDACTED_JWT]'],
  [/AKIA[0-9A-Z]{16}/g, '[REDACTED_AWS_KEY]'],
];

export function sanitizeContext(text: string): string {
  for (const [pattern, replacement] of SANITIZE_PATTERNS) {
    text = text.replace(pattern, replacement);
  }
  return text;
}
```

---

## High-Level Memory API (`memory.ts`)

```typescript
export class MemoryClient {
  constructor(private client: WhiteMagicClient) {}

  async create(title: string, content: string, tags: string[] = [], importance = 0.5): Promise<ToolResult> {
    return this.client.callTool('create_memory', { title, content, tags, importance });
  }

  async search(query: string, limit = 10): Promise<ToolResult> {
    return this.client.callTool('search_memories', { query, limit });
  }

  async read(id: string): Promise<ToolResult> {
    return this.client.callTool('fast_read_memory', { id });
  }

  async recall(query: string, limit = 10): Promise<ToolResult> {
    return this.client.callTool('hybrid_recall', { query, limit });
  }
}
```

---

## Usage Example

```typescript
import { WhiteMagicClient, MemoryClient, Vault, sanitizeContext } from '@whitemagic/sdk';

// 1. Connect to local WhiteMagic
const wm = new WhiteMagicClient();
await wm.connectStdio();

// 2. Use the vault for API keys
const vault = new Vault('~/.whitemagic/vault.db', process.env.WM_PASSPHRASE || 'default');
vault.set('OPENAI_API_KEY', 'sk-abc123...');

// 3. Memory operations
const mem = new MemoryClient(wm);
await mem.create('Meeting Notes', 'Discussed Q2 roadmap with team...', ['meetings', 'q2']);
const results = await mem.search('Q2 roadmap');

// 4. Sanitize before sending to LLM
const context = sanitizeContext(results.details.results.map(r => r.content).join('\n'));
// context has all API keys redacted
```

---

## Existing TypeScript Code

WhiteMagic already has TypeScript code in two locations:

1. **`nexus/`** — Tauri desktop app with React frontend
   - `nexus/src/` — React components, App.tsx
   - Could be the primary consumer of `@whitemagic/sdk`

2. **`whitemagic-rust/materialized_artifact/`** — Contains `sdk/client.ts`, `sdk/types.ts`, `sdk/index.ts`
   - ~775 lines of existing SDK code
   - Needs alignment with current tool schemas and vault patterns

### Migration Path
1. Consolidate existing TS code into `@whitemagic/sdk` package
2. Add vault + sanitizer modules
3. Update `nexus/` to use the SDK
4. Publish to npm alongside PyPI release

---

## Dependencies

| Package | Purpose | Size |
|---------|---------|------|
| `better-sqlite3` | Local SQLite (vault + cache) | ~7MB (native addon) |
| `node-fetch` | HTTP transport (if not using Node 18+ built-in) | ~100KB |
| — | `crypto` module is built-in | 0 |

---

## Relationship to Python SDK

The TypeScript SDK is a **client**, not a reimplementation. It:
- Connects to the Python WhiteMagic server via MCP (stdio or HTTP)
- Provides TypeScript types for all tool schemas
- Adds local vault + sanitizer for the TS ecosystem
- Does NOT reimplement the 302 tools in TypeScript

For the full engine, agents should use `pip install whitemagic`. The TS SDK is for:
- Node.js/Deno applications integrating with WhiteMagic
- The Nexus desktop app
- Web frontends that talk to a WhiteMagic HTTP endpoint

---

*Design document: February 2026 — Target: WhiteMagic v15.0*
