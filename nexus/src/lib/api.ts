import { WHITEMAGIC_API_URL } from "./constants";
import { WhiteMagicClient } from "../sdk/client";

// SDK singleton — provides type-safe access to all WhiteMagic APIs
export const wmClient = new WhiteMagicClient({
  baseUrl: WHITEMAGIC_API_URL,
  timeout: 5000,
  retries: 1,
});

interface ApiOptions {
  timeout?: number;
}

async function apiFetch<T>(
  path: string,
  opts: ApiOptions & RequestInit = {}
): Promise<T | null> {
  const { timeout = 3000, ...fetchOpts } = opts;
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeout);
  try {
    const res = await fetch(`${WHITEMAGIC_API_URL}${path}`, {
      ...fetchOpts,
      signal: controller.signal,
      headers: {
        "Content-Type": "application/json",
        ...(fetchOpts.headers || {}),
      },
    });
    clearTimeout(timer);
    if (!res.ok) return null;
    return (await res.json()) as T;
  } catch {
    clearTimeout(timer);
    return null;
  }
}

// ── Metrics / Health ──────────────────────────────────────────────
export interface HealthData {
  status: string;
  version: string;
  uptime_seconds?: number;
  memory_count?: number;
  tools_registered?: number;
}

export async function fetchHealth(): Promise<HealthData | null> {
  return apiFetch<HealthData>("/health");
}

export interface MetricsData {
  memoryCount: number;
  seenCount: number;
  eventCount: number;
  clients: number;
  currentPhase?: string;
  health?: HealthData;
  tokenData?: unknown[];
  sessions?: unknown[];
  memoryStats?: Record<string, unknown>;
}

export async function fetchMetrics(): Promise<MetricsData | null> {
  return apiFetch<MetricsData>("/api/v1/metrics");
}

// ── Gnosis (unified introspection) ────────────────────────────────
export interface GnosisSnapshot {
  harmony?: {
    balance: number;
    throughput: number;
    latency: number;
    error_rate: number;
    dharma: number;
    karma_debt: number;
    energy: number;
    guna?: string;
  };
  dharma?: {
    profile: string;
    rules_count: number;
    violations: number;
  };
  karma?: {
    total_calls: number;
    clean: number;
    undeclared_effects: number;
  };
  circuit_breakers?: Record<
    string,
    { state: string; failure_count: number; success_count: number }
  >;
  temporal?: {
    fast: { queued: number; flushed: number };
    medium: { queued: number; flushed: number };
    slow: { queued: number; flushed: number };
  };
  maturity?: {
    current_stage: string;
    locked_tools: string[];
    unlocked_tools: string[];
  };
  homeostasis?: {
    status: string;
    corrections: number;
  };
  agents?: Record<string, unknown>[];
  galactic?: {
    core: number;
    inner_rim: number;
    mid_band: number;
    outer_rim: number;
    far_edge: number;
  };
}

export async function fetchGnosis(): Promise<GnosisSnapshot | null> {
  return apiFetch<GnosisSnapshot>("/api/v1/tools/call", {
    method: "POST",
    body: JSON.stringify({ tool: "gnosis", args: {} }),
  });
}

// ── Memories ──────────────────────────────────────────────────────
export interface Memory {
  id: string;
  title: string;
  content: string;
  type: string;
  tags: string[];
  created_at?: string;
  galactic_distance?: number;
  retention_score?: number;
}

export async function fetchMemories(
  query?: string,
  limit = 50
): Promise<Memory[]> {
  const params = new URLSearchParams({ limit: String(limit) });
  if (query) params.set("q", query);
  const res = await apiFetch<{ memories: Memory[] }>(
    `/api/v1/tools/call`,
    {
      method: "POST",
      body: JSON.stringify({
        tool: "search_memories",
        args: { query: query || "", limit },
      }),
    }
  );
  return res?.memories || [];
}

// ── Harmony Vector ────────────────────────────────────────────────
export interface HarmonyData {
  balance: number;
  throughput: number;
  latency: number;
  error_rate: number;
  dharma: number;
  karma_debt: number;
  energy: number;
  guna: string;
}

export async function fetchHarmony(): Promise<HarmonyData | null> {
  return apiFetch<HarmonyData>("/api/v1/tools/call", {
    method: "POST",
    body: JSON.stringify({ tool: "harmony_vector", args: {} }),
  });
}

// ── Resonance events (Gan Ying) ───────────────────────────────────
export interface ResonanceEvent {
  time: string;
  type: string;
  source: string;
  confidence: number;
  data?: Record<string, unknown>;
}

export async function fetchResonanceEvents(): Promise<ResonanceEvent[]> {
  const res = await apiFetch<{ events: ResonanceEvent[] }>(
    "/api/v1/resonance"
  );
  return res?.events || [];
}

// ── Gardens ───────────────────────────────────────────────────────
export interface GardenStatus {
  name: string;
  active: boolean;
  memory_count: number;
  last_activated?: string;
}

export async function fetchGardens(): Promise<GardenStatus[]> {
  const res = await apiFetch<{ gardens: GardenStatus[] }>(
    "/api/v1/tools/call",
    {
      method: "POST",
      body: JSON.stringify({ tool: "garden_status", args: {} }),
    }
  );
  return res?.gardens || [];
}

// ── Tool Dependency Graph ─────────────────────────────────────────
export interface ToolGraphData {
  nodes: { id: string; category: string }[];
  edges: { source: string; target: string; type: string }[];
}

export async function fetchToolGraph(): Promise<ToolGraphData | null> {
  return apiFetch<ToolGraphData>("/api/v1/tools/call", {
    method: "POST",
    body: JSON.stringify({ tool: "tool.graph_full", args: {} }),
  });
}

// ── Connection check ──────────────────────────────────────────────
export async function checkConnection(): Promise<boolean> {
  const data = await fetchHealth();
  return data !== null;
}
