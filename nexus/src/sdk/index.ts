/**
 * @whitemagic/sdk — WhiteMagic TypeScript SDK
 *
 * A complete TypeScript client for interacting with WhiteMagic's
 * memory system, tool dispatch, agent coordination, and governance.
 *
 * @example
 * ```typescript
 * import { WhiteMagicClient } from '@whitemagic/sdk';
 *
 * const wm = new WhiteMagicClient({
 *   baseUrl: 'http://localhost:8765',
 *   onEvent: (event) => console.log('Gan Ying:', event.type),
 * });
 *
 * // Introspection
 * const snapshot = await wm.gnosis();
 * console.log('Harmony:', snapshot.harmony.overall);
 *
 * // Memory operations
 * const results = await wm.memory.search('quantum computing');
 * const memory = await wm.memory.create('Session Notes', 'Today we...');
 *
 * // Tool invocation
 * const result = await wm.tools.call('grimoire_suggest', { context: 'debugging' });
 *
 * // Agent coordination
 * const agents = await wm.agents.list();
 *
 * // Real-time events
 * wm.connectEventStream();
 * ```
 *
 * @packageDocumentation
 */

export {
  WhiteMagicClient,
  WhiteMagicError,
  MemoryClient,
  ToolClient,
  AgentClient,
  GovernanceClient,
} from './client';

export type { WhiteMagicConfig, GanYingEvent } from './client';

// Re-export all types
export type {
  MemoryRecord,
  MemoryType,
  GalacticZone,
  HolographicCoord5D,
  MemorySearchResult,
  HarmonyVector,
  GunaClassification,
  GnosisSnapshot,
  DharmaStatus,
  DharmaProfile,
  KarmaStatus,
  CircuitBreakerState,
  TelemetrySnapshot,
  GalacticStatus,
  DreamStatus,
  DreamPhase,
  AgentInfo,
  AgentStatus,
  AgentSummary,
  ToolCallResult,
  ToolDefinition,
  ToolParameter,
  PipelineResult,
  PipelineStepResult,
  ResonanceState,
  ResonanceMetadata,
  CapabilityMatrix,
  SubsystemInfo,
  FusionInfo,
  WuXingPhase,
  WuXingState,
  MaturityStage,
  MaturityInfo,
} from './types';
