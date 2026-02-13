/**
 * @whitemagic/sdk — TypeScript client for WhiteMagic.
 *
 * Provides typed access to WhiteMagic's 302+ MCP tools via stdio or HTTP transport.
 *
 * @example
 * ```typescript
 * import { WhiteMagicClient, MemoryClient, sanitizeContext } from '@whitemagic/sdk';
 *
 * const wm = new WhiteMagicClient();
 * await wm.connectStdio();
 *
 * const mem = new MemoryClient(wm);
 * await mem.create('Hello', 'World', ['test']);
 *
 * const results = await mem.search('hello');
 * const safe = sanitizeContext(JSON.stringify(results));
 * ```
 */

export { WhiteMagicClient } from "./client.js";
export { MemoryClient } from "./memory.js";
export { sanitizeContext } from "./sanitizer.js";
export type {
  ClientConfig,
  Galaxy,
  GnosisSnapshot,
  HarmonyVector,
  McpRequest,
  McpResponse,
  Memory,
  ToolResult,
} from "./types.js";
