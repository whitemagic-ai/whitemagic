/**
 * WhiteMagic SDK — High-level Memory API.
 *
 * Provides typed convenience methods for common memory operations.
 */

import type { WhiteMagicClient } from "./client.js";
import type { ToolResult } from "./types.js";

export class MemoryClient {
  constructor(private client: WhiteMagicClient) {}

  /** Create a new memory. */
  async create(
    title: string,
    content: string,
    tags: string[] = [],
    importance = 0.5
  ): Promise<ToolResult> {
    return this.client.callTool("create_memory", {
      title,
      content,
      tags,
      importance,
    });
  }

  /** Full-text search across memories. */
  async search(query: string, limit = 10): Promise<ToolResult> {
    return this.client.callTool("search_memories", { query, limit });
  }

  /** Read a specific memory by ID or filename. */
  async read(id: string): Promise<ToolResult> {
    return this.client.callTool("fast_read_memory", { filename: id });
  }

  /** Multi-hop graph-aware recall. */
  async recall(query: string, limit = 10): Promise<ToolResult> {
    return this.client.callTool("hybrid_recall", {
      query,
      final_limit: limit,
    });
  }

  /** Consolidate related memories into synthesized clusters. */
  async consolidate(dryRun = true): Promise<ToolResult> {
    return this.client.callTool("memory.consolidate", { dry_run: dryRun });
  }

  /** Run a memory lifecycle sweep. */
  async lifecycleSweep(dryRun = true): Promise<ToolResult> {
    return this.client.callTool("memory.lifecycle_sweep", {
      dry_run: dryRun,
    });
  }
}
