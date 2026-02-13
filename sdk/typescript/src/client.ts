/**
 * WhiteMagic SDK — MCP Client.
 *
 * Connects to a WhiteMagic MCP server via stdio or HTTP and provides
 * a typed `callTool()` interface for all 302+ tools.
 */

import { spawn, type ChildProcess } from "child_process";
import type {
  ClientConfig,
  McpRequest,
  McpResponse,
  ToolResult,
} from "./types.js";

export class WhiteMagicClient {
  private process: ChildProcess | null = null;
  private httpUrl: string | null = null;
  private requestId = 0;
  private pendingRequests = new Map<
    number,
    { resolve: (v: unknown) => void; reject: (e: Error) => void }
  >();
  private buffer = "";

  /**
   * Connect via stdio (recommended for local use).
   */
  async connectStdio(config: ClientConfig = {}): Promise<void> {
    const cmd = config.pythonCommand ?? "python";
    const args = config.serverArgs ?? ["-m", "whitemagic.run_mcp_lean"];
    const env = {
      ...process.env,
      WM_MCP_PRAT: "1",
      WM_SILENT_INIT: "1",
      ...(config.env ?? {}),
    };

    this.process = spawn(cmd, args, {
      stdio: ["pipe", "pipe", "pipe"],
      env,
    });

    this.process.stdout?.on("data", (chunk: Buffer) => {
      this.buffer += chunk.toString();
      this._processBuffer();
    });

    this.process.on("error", (err) => {
      for (const p of this.pendingRequests.values()) {
        p.reject(err);
      }
      this.pendingRequests.clear();
    });

    // Wait for server to be ready (first message or short delay)
    await new Promise<void>((resolve) => setTimeout(resolve, 2000));
  }

  /**
   * Connect via HTTP (for remote or containerized instances).
   */
  async connectHttp(url = "http://localhost:8770"): Promise<void> {
    this.httpUrl = url;
  }

  /**
   * Call a WhiteMagic tool by name.
   */
  async callTool(
    name: string,
    params: Record<string, unknown> = {}
  ): Promise<ToolResult> {
    if (this.httpUrl) {
      return this._callToolHttp(name, params);
    }
    return this._callToolStdio(name, params);
  }

  /**
   * Disconnect from the server.
   */
  disconnect(): void {
    if (this.process) {
      this.process.kill();
      this.process = null;
    }
  }

  // ── Private ──────────────────────────────────────────────

  private async _callToolHttp(
    name: string,
    params: Record<string, unknown>
  ): Promise<ToolResult> {
    const resp = await fetch(`${this.httpUrl}/mcp/v1`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: ++this.requestId,
        method: "tools/call",
        params: { name, arguments: params },
      }),
    });

    const json = (await resp.json()) as McpResponse;
    if (json.error) {
      throw new Error(`MCP error ${json.error.code}: ${json.error.message}`);
    }
    return json.result as ToolResult;
  }

  private _callToolStdio(
    name: string,
    params: Record<string, unknown>
  ): Promise<ToolResult> {
    return new Promise((resolve, reject) => {
      if (!this.process?.stdin) {
        reject(new Error("Not connected via stdio"));
        return;
      }

      const id = ++this.requestId;
      const request: McpRequest = {
        jsonrpc: "2.0",
        id,
        method: "tools/call",
        params: { name, arguments: params },
      };

      this.pendingRequests.set(id, {
        resolve: (v) => resolve(v as ToolResult),
        reject,
      });

      this.process.stdin.write(JSON.stringify(request) + "\n");
    });
  }

  private _processBuffer(): void {
    const lines = this.buffer.split("\n");
    this.buffer = lines.pop() ?? "";

    for (const line of lines) {
      if (!line.trim()) continue;
      try {
        const msg = JSON.parse(line) as McpResponse;
        const pending = this.pendingRequests.get(msg.id);
        if (pending) {
          this.pendingRequests.delete(msg.id);
          if (msg.error) {
            pending.reject(
              new Error(`MCP error ${msg.error.code}: ${msg.error.message}`)
            );
          } else {
            pending.resolve(msg.result);
          }
        }
      } catch {
        // Not JSON — ignore (stderr leak, log messages, etc.)
      }
    }
  }
}
