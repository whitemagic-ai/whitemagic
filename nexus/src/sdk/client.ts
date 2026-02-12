/**
 * @whitemagic/sdk — TypeScript Client SDK for WhiteMagic
 *
 * Provides a type-safe client for interacting with WhiteMagic's
 * MCP tools, memory system, and real-time event streams.
 *
 * Usage:
 *   import { WhiteMagicClient } from './sdk/client';
 *   const wm = new WhiteMagicClient({ baseUrl: 'http://localhost:8765' });
 *   const gnosis = await wm.gnosis();
 *   const memories = await wm.memory.search('quantum computing');
 */

import type {
  GnosisSnapshot,
  HarmonyVector,
  MemoryRecord,
  MemorySearchResult,
  ToolCallResult,
  GalacticStatus,
  DreamStatus,
  AgentInfo,
  ResonanceState,
  CapabilityMatrix,
  DharmaProfile,
  PipelineResult,
} from './types';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface WhiteMagicConfig {
  baseUrl: string;
  apiKey?: string;
  timeout?: number;
  retries?: number;
  onEvent?: (event: GanYingEvent) => void;
}

export interface GanYingEvent {
  type: string;
  timestamp: string;
  data: Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// Client
// ---------------------------------------------------------------------------

export class WhiteMagicClient {
  private config: Required<Omit<WhiteMagicConfig, 'apiKey' | 'onEvent'>> & Pick<WhiteMagicConfig, 'apiKey' | 'onEvent'>;
  private ws: WebSocket | null = null;

  public readonly memory: MemoryClient;
  public readonly tools: ToolClient;
  public readonly agents: AgentClient;
  public readonly governance: GovernanceClient;

  constructor(config: WhiteMagicConfig) {
    this.config = {
      timeout: 30000,
      retries: 2,
      ...config,
    };

    this.memory = new MemoryClient(this);
    this.tools = new ToolClient(this);
    this.agents = new AgentClient(this);
    this.governance = new GovernanceClient(this);
  }

  // -------------------------------------------------------------------------
  // Core API
  // -------------------------------------------------------------------------

  /** Get unified system introspection snapshot. */
  async gnosis(compact?: boolean): Promise<GnosisSnapshot> {
    return this.get('/api/gnosis', { compact: compact ? '1' : undefined });
  }

  /** Get the 7-dimension Harmony Vector. */
  async harmony(): Promise<HarmonyVector> {
    return this.get('/api/harmony');
  }

  /** Get Galactic Map status (zone counts, distances). */
  async galactic(): Promise<GalacticStatus> {
    return this.get('/api/galactic');
  }

  /** Get dream cycle status. */
  async dreamStatus(): Promise<DreamStatus> {
    return this.get('/api/dream');
  }

  /** Get system health check. */
  async health(): Promise<{ status: string; version: string; tools: number }> {
    return this.get('/api/health');
  }

  /** Get system metrics (tool count, memory count, uptime). */
  async metrics(): Promise<Record<string, unknown>> {
    return this.get('/api/metrics');
  }

  /** Get resonance state (PRAT session context). */
  async resonance(): Promise<ResonanceState> {
    return this.get('/api/resonance');
  }

  /** Get capability matrix (subsystems, fusions, suggestions). */
  async capabilities(): Promise<CapabilityMatrix> {
    return this.get('/api/capabilities');
  }

  // -------------------------------------------------------------------------
  // WebSocket (Gan Ying event stream)
  // -------------------------------------------------------------------------

  /** Connect to the Gan Ying real-time event stream. */
  connectEventStream(): void {
    const wsUrl = this.config.baseUrl.replace('http', 'ws') + '/ws/ganying';
    this.ws = new WebSocket(wsUrl);

    this.ws.onmessage = (event) => {
      try {
        const parsed = JSON.parse(event.data) as GanYingEvent;
        this.config.onEvent?.(parsed);
      } catch {
        // Ignore malformed messages
      }
    };

    this.ws.onclose = () => {
      // Auto-reconnect after 5s
      setTimeout(() => this.connectEventStream(), 5000);
    };
  }

  /** Disconnect from the event stream. */
  disconnectEventStream(): void {
    this.ws?.close();
    this.ws = null;
  }

  // -------------------------------------------------------------------------
  // HTTP helpers
  // -------------------------------------------------------------------------

  async get<T>(path: string, params?: Record<string, string | undefined>): Promise<T> {
    const url = new URL(path, this.config.baseUrl);
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) url.searchParams.set(key, value);
      });
    }

    const response = await this.fetchWithRetry(url.toString(), { method: 'GET' });
    return response.json();
  }

  async post<T>(path: string, body?: unknown): Promise<T> {
    const url = new URL(path, this.config.baseUrl).toString();
    const response = await this.fetchWithRetry(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: body ? JSON.stringify(body) : undefined,
    });
    return response.json();
  }

  private async fetchWithRetry(url: string, init: RequestInit): Promise<Response> {
    const headers: Record<string, string> = {
      ...(init.headers as Record<string, string>),
    };
    if (this.config.apiKey) {
      headers['Authorization'] = `Bearer ${this.config.apiKey}`;
    }

    let lastError: Error | null = null;
    for (let attempt = 0; attempt <= this.config.retries; attempt++) {
      try {
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.config.timeout);

        const response = await fetch(url, {
          ...init,
          headers,
          signal: controller.signal,
        });

        clearTimeout(timeoutId);

        if (!response.ok) {
          throw new WhiteMagicError(
            `HTTP ${response.status}: ${response.statusText}`,
            response.status,
          );
        }

        return response;
      } catch (error) {
        lastError = error as Error;
        if (attempt < this.config.retries) {
          await new Promise((resolve) => setTimeout(resolve, 1000 * (attempt + 1)));
        }
      }
    }

    throw lastError ?? new Error('Request failed');
  }
}

// ---------------------------------------------------------------------------
// Sub-clients
// ---------------------------------------------------------------------------

export class MemoryClient {
  private client: WhiteMagicClient;
  constructor(client: WhiteMagicClient) {
    this.client = client;
  }

  /** Search memories by query string. */
  async search(query: string, limit?: number): Promise<MemorySearchResult[]> {
    return this.client.get('/api/memory/search', {
      q: query,
      limit: limit?.toString(),
    });
  }

  /** Create a new memory. */
  async create(title: string, content: string, tags?: string[]): Promise<MemoryRecord> {
    return this.client.post('/api/memory', { title, content, tags });
  }

  /** Recall a specific memory by ID. */
  async recall(id: string): Promise<MemoryRecord> {
    return this.client.get(`/api/memory/${id}`);
  }

  /** List recent memories. */
  async list(limit?: number, zone?: string): Promise<MemoryRecord[]> {
    return this.client.get('/api/memory', {
      limit: limit?.toString(),
      zone,
    });
  }

  /** Trigger memory consolidation. */
  async consolidate(): Promise<{ consolidated: number; duration_ms: number }> {
    return this.client.post('/api/memory/consolidate');
  }

  /** Get galactic zone distribution. */
  async zones(): Promise<Record<string, number>> {
    return this.client.get('/api/memory/zones');
  }

  /** Export memories in various formats. */
  async export(format: 'json' | 'csv' | 'md'): Promise<string> {
    return this.client.get('/api/memory/export', { format });
  }
}

export class ToolClient {
  private client: WhiteMagicClient;
  constructor(client: WhiteMagicClient) {
    this.client = client;
  }

  /** Call a tool by name with arguments. */
  async call(toolName: string, args?: Record<string, unknown>): Promise<ToolCallResult> {
    return this.client.post('/api/tools/call', { tool: toolName, args: args ?? {} });
  }

  /** List all available tools. */
  async list(): Promise<{ name: string; description: string; category: string }[]> {
    return this.client.get('/api/tools');
  }

  /** Get tool dependency graph. */
  async graph(): Promise<{ nodes: string[]; edges: { from: string; to: string; type: string }[] }> {
    return this.client.get('/api/tools/graph');
  }

  /** Create and execute a pipeline. */
  async pipeline(
    steps: { tool: string; args?: Record<string, unknown> }[],
    mode?: 'sequential' | 'parallel',
  ): Promise<PipelineResult> {
    return this.client.post('/api/tools/pipeline', { steps, mode: mode ?? 'sequential' });
  }

  /** Get PRAT (28 Gana) routing info. */
  async pratInfo(): Promise<Record<string, string[]>> {
    return this.client.get('/api/tools/prat');
  }
}

export class AgentClient {
  private client: WhiteMagicClient;
  constructor(client: WhiteMagicClient) {
    this.client = client;
  }

  /** List registered agents. */
  async list(): Promise<AgentInfo[]> {
    return this.client.get('/api/agents');
  }

  /** Register a new agent. */
  async register(name: string, capabilities: string[]): Promise<AgentInfo> {
    return this.client.post('/api/agents/register', { name, capabilities });
  }

  /** Send heartbeat for an agent. */
  async heartbeat(agentId: string): Promise<{ status: string }> {
    return this.client.post(`/api/agents/${agentId}/heartbeat`);
  }

  /** Distribute a task to the agent swarm. */
  async distributeTask(
    toolName: string,
    args: Record<string, unknown>,
    priority?: number,
  ): Promise<{ task_id: string; assigned_to: string }> {
    return this.client.post('/api/agents/distribute', {
      tool: toolName,
      args,
      priority: priority ?? 5,
    });
  }
}

export class GovernanceClient {
  private client: WhiteMagicClient;
  constructor(client: WhiteMagicClient) {
    this.client = client;
  }

  /** Get current Dharma profile and rules. */
  async dharmaRules(): Promise<{ profile: string; rules: unknown[] }> {
    return this.client.get('/api/governance/dharma');
  }

  /** Set Dharma profile. */
  async setProfile(profile: DharmaProfile): Promise<{ status: string }> {
    return this.client.post('/api/governance/dharma/profile', { profile });
  }

  /** Get karma report. */
  async karmaReport(): Promise<Record<string, unknown>> {
    return this.client.get('/api/governance/karma');
  }

  /** Get maturity stage info. */
  async maturity(): Promise<{ stage: string; locked_tools: string[]; unlocked_tools: string[] }> {
    return this.client.get('/api/governance/maturity');
  }

  /** Get homeostasis status. */
  async homeostasis(): Promise<{ status: string; corrections: number }> {
    return this.client.get('/api/governance/homeostasis');
  }

  /** Get circuit breaker states. */
  async circuitBreakers(): Promise<Record<string, { state: string; failures: number }>> {
    return this.client.get('/api/governance/circuit-breakers');
  }
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

export class WhiteMagicError extends Error {
  public readonly statusCode: number;
  constructor(
    message: string,
    statusCode: number,
  ) {
    super(message);
    this.name = 'WhiteMagicError';
    this.statusCode = statusCode;
  }
}
