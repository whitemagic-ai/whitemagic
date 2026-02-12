/**
 * WhiteMagic SDK — Core TypeScript types.
 *
 * These types mirror the Python tool envelope and memory structures.
 */

/** Standard tool result envelope returned by all WhiteMagic tools. */
export interface ToolResult {
  status: "success" | "error";
  tool: string;
  request_id: string;
  message: string;
  details: Record<string, unknown>;
  error_code?: string;
  retryable: boolean;
  timestamp: string;
}

/** A memory record as returned by search/read operations. */
export interface Memory {
  id: string;
  title: string;
  content: string;
  tags: string[];
  importance: number;
  memory_type: "short_term" | "long_term";
  created_at: string;
  updated_at: string;
  metadata?: Record<string, unknown>;
}

/** Galaxy info as returned by galaxy.list. */
export interface Galaxy {
  name: string;
  memory_count?: number;
  size_mb?: number;
  last_accessed?: string;
}

/** Harmony Vector — 7-dimensional health metric. */
export interface HarmonyVector {
  balance: number;
  throughput: number;
  latency: number;
  error_rate: number;
  dharma: number;
  karma_debt: number;
  energy: number;
  harmony_score: number;
}

/** Gnosis compact snapshot. */
export interface GnosisSnapshot {
  status: string;
  maturity_stage: string;
  alerts: string[];
  next_actions: Array<{ tool: string; reason: string } | string>;
}

/** MCP JSON-RPC request/response for stdio transport. */
export interface McpRequest {
  jsonrpc: "2.0";
  id: number;
  method: string;
  params: Record<string, unknown>;
}

export interface McpResponse {
  jsonrpc: "2.0";
  id: number;
  result?: unknown;
  error?: { code: number; message: string; data?: unknown };
}

/** Configuration for WhiteMagicClient. */
export interface ClientConfig {
  /** Python command (default: "python") */
  pythonCommand?: string;
  /** MCP server module args (default: ["-m", "whitemagic.run_mcp_lean"]) */
  serverArgs?: string[];
  /** HTTP base URL (for HTTP transport) */
  httpUrl?: string;
  /** Environment variables to pass to the MCP server */
  env?: Record<string, string>;
}
