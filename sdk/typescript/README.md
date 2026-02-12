# @whitemagic/sdk

TypeScript client SDK for [WhiteMagic](https://github.com/whitemagic-ai/whitemagic) — memory, governance, and orchestration for AI agents.

## Install

```bash
npm install @whitemagic/sdk
```

> Requires WhiteMagic Python server: `pip install whitemagic[mcp]`

## Quick Start

```typescript
import { WhiteMagicClient, MemoryClient } from '@whitemagic/sdk';

// Connect to local WhiteMagic server
const wm = new WhiteMagicClient();
await wm.connectStdio();

// High-level memory API
const mem = new MemoryClient(wm);
await mem.create('Meeting Notes', 'Discussed Q2 roadmap...', ['meetings']);
const results = await mem.search('Q2 roadmap');

// Or call any of 302+ tools directly
const health = await wm.callTool('gnosis', { compact: true });
console.log(health);

wm.disconnect();
```

## Features

- **Typed MCP client** — stdio and HTTP transports
- **Memory API** — create, search, read, recall, consolidate
- **Context sanitizer** — redact API keys before sending to LLMs
- **Full tool access** — call any WhiteMagic tool by name

## Context Sanitization

```typescript
import { sanitizeContext } from '@whitemagic/sdk';

const safe = sanitizeContext('My key is sk-abc123456789012345678901');
// → 'My key is [REDACTED_API_KEY]'
```

## License

MIT
