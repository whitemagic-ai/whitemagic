/**
 * @whitemagic/sdk — Shared TypeScript types for WhiteMagic API.
 *
 * These types are shared between the TypeScript SDK client and the
 * Nexus frontend, ensuring type safety across the full stack.
 */

// ---------------------------------------------------------------------------
// Memory Types
// ---------------------------------------------------------------------------

export interface MemoryRecord {
  id: string;
  title: string;
  content: string;
  memory_type: MemoryType;
  importance: number;
  emotional_valence: number;
  recall_count: number;
  tags: string[];
  created_at: string;
  last_accessed: string;
  galactic_distance: number;
  galactic_zone: GalacticZone;
  retention_score: number;
  holographic_coords?: HolographicCoord5D;
  is_protected: boolean;
  source?: string;
}

export type MemoryType =
  | 'SHORT_TERM'
  | 'LONG_TERM'
  | 'PATTERN'
  | 'DREAM'
  | 'STRATEGY'
  | 'ASSOCIATION';

export type GalacticZone = 'core' | 'inner_rim' | 'mid_band' | 'outer_rim' | 'far_edge';

export interface HolographicCoord5D {
  x: number; // Logic ↔ Emotion      [-1, +1]
  y: number; // Micro ↔ Macro        [-1, +1]
  z: number; // Time / Chronos       [-1, +1]
  w: number; // Importance / Gravity  [0, 2+]
  v: number; // Vitality / Distance   [0, 1]
}

export interface MemorySearchResult {
  id: string;
  title: string;
  score: number;
  zone: GalacticZone;
  snippet: string;
}

// ---------------------------------------------------------------------------
// Harmony Vector
// ---------------------------------------------------------------------------

export interface HarmonyVector {
  balance: number;
  throughput: number;
  latency: number;
  error_rate: number;
  dharma: number;
  karma_debt: number;
  energy: number;
  overall: number;
  guna: GunaClassification;
  timestamp: string;
}

export type GunaClassification = 'sattvic' | 'rajasic' | 'tamasic';

// ---------------------------------------------------------------------------
// Gnosis (Introspection)
// ---------------------------------------------------------------------------

export interface GnosisSnapshot {
  harmony: HarmonyVector;
  dharma: DharmaStatus;
  karma: KarmaStatus;
  circuit_breakers: Record<string, CircuitBreakerState>;
  telemetry: TelemetrySnapshot;
  resonance?: ResonanceState;
  capabilities?: CapabilityMatrix;
  galactic?: GalacticStatus;
  dream?: DreamStatus;
  agents?: AgentSummary;
  timestamp: string;
  version: string;
}

export interface DharmaStatus {
  profile: DharmaProfile;
  rules_count: number;
  recent_evaluations: number;
  violations: number;
}

export type DharmaProfile = 'default' | 'creative' | 'secure';

export interface KarmaStatus {
  total_declared: number;
  total_actual: number;
  karma_debt: number;
  recent_entries: number;
}

export interface CircuitBreakerState {
  state: 'closed' | 'open' | 'half_open';
  failures: number;
  last_failure: string | null;
  cooldown_until: string | null;
}

export interface TelemetrySnapshot {
  total_calls: number;
  success_rate: number;
  avg_latency_ms: number;
  tools_invoked: number;
  uptime_seconds: number;
}

// ---------------------------------------------------------------------------
// Galactic Map
// ---------------------------------------------------------------------------

export interface GalacticStatus {
  total_memories: number;
  zone_counts: Record<GalacticZone, number>;
  avg_distance: number;
  core_count: number;
  protected_count: number;
  last_sweep: string | null;
}

// ---------------------------------------------------------------------------
// Dream Cycle
// ---------------------------------------------------------------------------

export interface DreamStatus {
  is_dreaming: boolean;
  current_phase: DreamPhase | null;
  phases_completed: number;
  last_dream_start: string | null;
  last_dream_end: string | null;
  memories_consolidated: number;
}

export type DreamPhase =
  | 'CONSOLIDATION'
  | 'SERENDIPITY'
  | 'KAIZEN'
  | 'ORACLE'
  | 'DECAY';

// ---------------------------------------------------------------------------
// Agents
// ---------------------------------------------------------------------------

export interface AgentInfo {
  id: string;
  name: string;
  status: AgentStatus;
  capabilities: string[];
  current_load: number;
  max_load: number;
  tasks_completed: number;
  tasks_failed: number;
  last_heartbeat: string;
  registered_at: string;
}

export type AgentStatus = 'idle' | 'busy' | 'overloaded' | 'offline' | 'draining';

export interface AgentSummary {
  total: number;
  active: number;
  idle: number;
  offline: number;
}

// ---------------------------------------------------------------------------
// Tool System
// ---------------------------------------------------------------------------

export interface ToolCallResult {
  status: 'ok' | 'error';
  result?: Record<string, unknown>;
  error?: string;
  error_code?: string;
  duration_ms: number;
  tool: string;
  _resonance?: ResonanceMetadata;
}

export interface ToolDefinition {
  name: string;
  description: string;
  category: string;
  gana?: string;
  garden?: string;
  quadrant?: string;
  element?: string;
  parameters: Record<string, ToolParameter>;
}

export interface ToolParameter {
  type: string;
  description: string;
  required?: boolean;
  default?: unknown;
  enum?: string[];
}

export interface PipelineResult {
  pipeline_id: string;
  status: 'completed' | 'partial' | 'failed';
  steps: PipelineStepResult[];
  duration_ms: number;
}

export interface PipelineStepResult {
  step: number;
  tool: string;
  status: 'ok' | 'error' | 'skipped';
  result?: Record<string, unknown>;
  error?: string;
}

// ---------------------------------------------------------------------------
// Resonance (PRAT)
// ---------------------------------------------------------------------------

export interface ResonanceState {
  session_id: string;
  call_count: number;
  gana_sequence: string[];
  quadrant_distribution: Record<string, number>;
  current_mood: string;
  resonance_depth: number;
}

export interface ResonanceMetadata {
  gana: string;
  quadrant: string;
  predecessor: string | null;
  successor: string | null;
  resonance_depth: number;
  wu_xing_boost: number;
}

// ---------------------------------------------------------------------------
// Capability Matrix
// ---------------------------------------------------------------------------

export interface CapabilityMatrix {
  subsystems: SubsystemInfo[];
  active_fusions: FusionInfo[];
  suggestions: string[];
  total_active: number;
  total_fusions: number;
}

export interface SubsystemInfo {
  name: string;
  status: 'active' | 'degraded' | 'inactive';
  language: string;
  description: string;
}

export interface FusionInfo {
  name: string;
  source: string;
  target: string;
  status: 'active' | 'planned';
  description: string;
}

// ---------------------------------------------------------------------------
// Wu Xing (Five Phases)
// ---------------------------------------------------------------------------

export type WuXingPhase = 'wood' | 'fire' | 'earth' | 'metal' | 'water';

export interface WuXingState {
  current_phase: WuXingPhase;
  wood: number;
  fire: number;
  earth: number;
  metal: number;
  water: number;
}

// ---------------------------------------------------------------------------
// Maturity Gates
// ---------------------------------------------------------------------------

export type MaturityStage =
  | 'SEED'
  | 'BICAMERAL'
  | 'REFLECTIVE'
  | 'RADIANT'
  | 'COLLECTIVE'
  | 'LOGOS';

export interface MaturityInfo {
  stage: MaturityStage;
  tools_executed: number;
  sessions: number;
  dharma_score: number;
  harmony_score: number;
  next_stage_requirements: string[];
  locked_categories: string[];
  unlocked_categories: string[];
}
