import { useState, useEffect, useCallback } from "react";
import { useNexusStore } from "../../store/nexus";
import { WU_XING_META } from "../../lib/constants";
import {
  fetchMetrics,
  fetchGnosis,
  checkConnection,
  type GnosisSnapshot,
  type MetricsData,
} from "../../lib/api";
import {
  RefreshCw,
  Activity,
  Shield,
  Gauge,
  Orbit,
  Zap,
} from "lucide-react";

// ── Mock data for when API is unavailable ─────────────────────────
const MOCK_GNOSIS: GnosisSnapshot = {
  harmony: {
    balance: 0.87,
    throughput: 142.3,
    latency: 12.8,
    error_rate: 0.002,
    dharma: 0.95,
    karma_debt: 0.01,
    energy: 0.91,
    guna: "sattvic",
  },
  dharma: { profile: "default", rules_count: 12, violations: 0 },
  karma: { total_calls: 1847, clean: 1832, undeclared_effects: 15 },
  circuit_breakers: {
    create_memory: { state: "CLOSED", failure_count: 0, success_count: 312 },
    search_memories: { state: "CLOSED", failure_count: 1, success_count: 589 },
    edge_infer: { state: "CLOSED", failure_count: 0, success_count: 204 },
    execute_cascade: { state: "HALF_OPEN", failure_count: 3, success_count: 18 },
  },
  temporal: {
    fast: { queued: 0, flushed: 142 },
    medium: { queued: 3, flushed: 891 },
    slow: { queued: 12, flushed: 47 },
  },
  maturity: {
    current_stage: "APPRENTICE",
    locked_tools: ["execute_cascade", "sangha_lock_acquire"],
    unlocked_tools: ["create_memory", "search_memories", "edge_infer"],
  },
  homeostasis: { status: "STABLE", corrections: 3 },
  galactic: { core: 39, inner_rim: 1, mid_band: 1880, outer_rim: 96671, far_edge: 8577 },
};

function HarmonyGauge({
  label,
  value,
  max = 1,
  color,
  unit = "",
}: {
  label: string;
  value: number;
  max?: number;
  color: string;
  unit?: string;
}) {
  const pct = Math.min(100, (value / max) * 100);
  return (
    <div className="space-y-1">
      <div className="flex justify-between text-xs">
        <span className="text-gray-400">{label}</span>
        <span style={{ color }}>
          {value.toFixed(max === 1 ? 2 : 1)}
          {unit}
        </span>
      </div>
      <div className="h-1.5 bg-wm-bg rounded-full overflow-hidden">
        <div
          className="h-full rounded-full transition-all duration-700"
          style={{ width: `${pct}%`, backgroundColor: color }}
        />
      </div>
    </div>
  );
}

function BreakerChip({
  name,
  state,
}: {
  name: string;
  state: string;
}) {
  const colors: Record<string, string> = {
    CLOSED: "bg-green-500/20 text-green-400 border-green-500/30",
    OPEN: "bg-red-500/20 text-red-400 border-red-500/30",
    HALF_OPEN: "bg-yellow-500/20 text-yellow-400 border-yellow-500/30",
  };
  return (
    <div
      className={`flex items-center justify-between px-3 py-1.5 rounded-lg border text-xs ${
        colors[state] || colors.CLOSED
      }`}
    >
      <span className="font-mono truncate mr-2">{name}</span>
      <span className="font-bold flex-shrink-0">{state}</span>
    </div>
  );
}

export default function Dashboard() {
  const phase = useNexusStore((s) => s.wuXingPhase);
  const setStatus = useNexusStore((s) => s.setStatus);
  const [gnosis, setGnosis] = useState<GnosisSnapshot>(MOCK_GNOSIS);
  const [metrics, setMetrics] = useState<MetricsData | null>(null);
  const [connected, setConnected] = useState(false);
  const [loading, setLoading] = useState(false);

  const refresh = useCallback(async () => {
    setLoading(true);
    const isConnected = await checkConnection();
    setConnected(isConnected);
    if (isConnected) {
      const [m, g] = await Promise.all([fetchMetrics(), fetchGnosis()]);
      if (m) setMetrics(m);
      if (g) setGnosis(g);
      setStatus({
        apiConnected: true,
        memoryCount: m?.memoryCount || 0,
        patternCount: 0,
      });
    }
    setLoading(false);
  }, [setStatus]);

  useEffect(() => {
    refresh();
    const timer = setInterval(refresh, 8000);
    return () => clearInterval(timer);
  }, [refresh]);

  const h = gnosis.harmony;
  const galactic = gnosis.galactic;
  const totalMemories = galactic
    ? galactic.core + galactic.inner_rim + galactic.mid_band + galactic.outer_rim + galactic.far_edge
    : metrics?.memoryCount || 0;

  return (
    <div className="h-full overflow-auto p-5 bg-wm-bg relative">
      {/* Background ambience */}
      <div className="absolute top-0 left-0 w-[400px] h-[400px] bg-purple-900/15 rounded-full blur-[100px] -z-10 pointer-events-none" />
      <div className="absolute bottom-0 right-0 w-[400px] h-[400px] bg-cyan-900/15 rounded-full blur-[100px] -z-10 pointer-events-none" />

      {/* Header */}
      <div className="flex items-center justify-between mb-5">
        <h2 className="text-xl font-bold bg-gradient-to-r from-white via-purple-200 to-cyan-200 bg-clip-text text-transparent">
          System Dashboard
        </h2>
        <div className="flex items-center gap-2">
          <span
            className={`text-xs px-2 py-0.5 rounded-full border ${
              connected
                ? "text-green-400 border-green-500/30 bg-green-500/10"
                : "text-yellow-400 border-yellow-500/30 bg-yellow-500/10"
            }`}
          >
            {connected ? "API Connected" : "Mock Data"}
          </span>
          <button
            onClick={refresh}
            disabled={loading}
            className="p-1.5 rounded-lg hover:bg-white/5 text-gray-400 hover:text-white transition disabled:opacity-50"
          >
            <RefreshCw size={14} className={loading ? "animate-spin" : ""} />
          </button>
        </div>
      </div>

      {/* Top stats */}
      <div className="grid grid-cols-5 gap-3 mb-5">
        {[
          { label: "Memories", value: totalMemories, color: "border-purple-500", text: "text-purple-300" },
          { label: "Tools", value: 65, color: "border-cyan-500", text: "text-cyan-300" },
          { label: "Gardens", value: 30, color: "border-green-500", text: "text-green-300" },
          { label: "Guna", value: h?.guna || "sattvic", color: "border-yellow-500", text: "text-yellow-300" },
          { label: "Maturity", value: gnosis.maturity?.current_stage || "—", color: "border-orange-500", text: "text-orange-300" },
        ].map((s, i) => (
          <div key={i} className={`glass-card p-3 border-l-4 ${s.color}`}>
            <div className="text-lg font-bold text-white mb-0.5">
              {typeof s.value === "number" ? s.value.toLocaleString() : s.value}
            </div>
            <div className={`text-[10px] uppercase tracking-wider font-semibold ${s.text}`}>
              {s.label}
            </div>
          </div>
        ))}
      </div>

      {/* Main grid */}
      <div className="grid grid-cols-12 gap-4">
        {/* Wu Xing Phase — 3 cols */}
        <div className="col-span-3 glass-panel p-4">
          <h3 className="text-sm font-semibold mb-3 flex items-center gap-2 text-gray-300">
            <span>&#9775;&#65039;</span> Wu Xing Phase
          </h3>
          <div className="flex flex-col items-center gap-3">
            <div className="relative w-32 h-32">
              <svg viewBox="0 0 200 200" className="w-full h-full">
                {Object.entries(WU_XING_META).map(([key, meta], i) => {
                  const angle = (i * 72 - 90) * (Math.PI / 180);
                  const x = 100 + 65 * Math.cos(angle);
                  const y = 100 + 65 * Math.sin(angle);
                  const isActive = key === phase;
                  return (
                    <g key={key}>
                      {isActive && (
                        <circle
                          cx={x}
                          cy={y}
                          r={30}
                          fill={meta.color + "11"}
                          className="animate-pulse-slow"
                        />
                      )}
                      <circle
                        cx={x}
                        cy={y}
                        r={isActive ? 22 : 16}
                        fill={isActive ? meta.color + "33" : "#1e1e2e"}
                        stroke={meta.color}
                        strokeWidth={isActive ? 2.5 : 1}
                        className="transition-all duration-500"
                      />
                      <text
                        x={x}
                        y={y}
                        textAnchor="middle"
                        dominantBaseline="central"
                        style={{ fontSize: isActive ? "20px" : "14px" }}
                      >
                        {meta.emoji}
                      </text>
                    </g>
                  );
                })}
                <circle cx="100" cy="100" r="12" fill="#9333ea22" stroke="#9333ea" strokeWidth="1" />
              </svg>
            </div>
            <div className="text-center">
              <div className="text-base font-semibold" style={{ color: WU_XING_META[phase].color }}>
                {WU_XING_META[phase].label}
              </div>
              <div className="text-[10px] text-gray-500 mt-0.5">{WU_XING_META[phase].season}</div>
              <div className="text-xs text-gray-400 mt-1.5 max-w-[200px]">
                {WU_XING_META[phase].advice}
              </div>
            </div>
          </div>
        </div>

        {/* Harmony Vector — 5 cols */}
        <div className="col-span-5 glass-panel p-4">
          <h3 className="text-sm font-semibold mb-3 flex items-center gap-2 text-gray-300">
            <Activity size={14} className="text-wm-purple-400" /> Harmony Vector
          </h3>
          {h ? (
            <div className="space-y-2.5">
              <HarmonyGauge label="Balance" value={h.balance} color="#a855f7" />
              <HarmonyGauge label="Throughput" value={h.throughput} max={200} color="#22d3ee" unit="/s" />
              <HarmonyGauge label="Latency" value={h.latency} max={100} color="#f59e0b" unit="ms" />
              <HarmonyGauge label="Dharma" value={h.dharma} color="#4ade80" />
              <HarmonyGauge label="Energy" value={h.energy} color="#3b82f6" />
              <HarmonyGauge label="Karma Debt" value={h.karma_debt} color={h.karma_debt > 0.1 ? "#ef4444" : "#4ade80"} />
              <HarmonyGauge label="Error Rate" value={h.error_rate} color={h.error_rate > 0.05 ? "#ef4444" : "#4ade80"} />
            </div>
          ) : (
            <div className="text-gray-500 text-sm text-center py-8">No harmony data</div>
          )}
        </div>

        {/* Galactic Map — 4 cols */}
        <div className="col-span-4 glass-panel p-4">
          <h3 className="text-sm font-semibold mb-3 flex items-center gap-2 text-gray-300">
            <Orbit size={14} className="text-yellow-400" /> Galactic Map
          </h3>
          {galactic ? (
            <div className="flex flex-col items-center">
              {/* Orbital rings visualization */}
              <div className="relative w-44 h-44 mb-3">
                <svg viewBox="0 0 200 200" className="w-full h-full">
                  {[
                    { r: 15, label: "Core", count: galactic.core, color: "#fbbf24" },
                    { r: 40, label: "Inner", count: galactic.inner_rim, color: "#f59e0b" },
                    { r: 65, label: "Mid", count: galactic.mid_band, color: "#a855f7" },
                    { r: 85, label: "Outer", count: galactic.outer_rim, color: "#6366f1" },
                    { r: 98, label: "Far", count: galactic.far_edge, color: "#1e1e2e" },
                  ].map((zone, i) => (
                    <g key={i}>
                      <circle
                        cx="100"
                        cy="100"
                        r={zone.r}
                        fill="none"
                        stroke={zone.color}
                        strokeWidth={i === 0 ? 12 : 8}
                        opacity={0.3 + (1 - i / 5) * 0.5}
                      />
                    </g>
                  ))}
                  {/* Center glow */}
                  <circle cx="100" cy="100" r="8" fill="#fbbf24" opacity="0.8" />
                  <circle cx="100" cy="100" r="4" fill="#fff" opacity="0.9" />
                </svg>
              </div>
              <div className="w-full space-y-1.5">
                {[
                  { label: "Core", count: galactic.core, color: "#fbbf24", dist: "0–0.15" },
                  { label: "Inner Rim", count: galactic.inner_rim, color: "#f59e0b", dist: "0.15–0.40" },
                  { label: "Mid Band", count: galactic.mid_band, color: "#a855f7", dist: "0.40–0.65" },
                  { label: "Outer Rim", count: galactic.outer_rim, color: "#6366f1", dist: "0.65–0.85" },
                  { label: "Far Edge", count: galactic.far_edge, color: "#4b5563", dist: "0.85–1.0" },
                ].map((zone, i) => (
                  <div key={i} className="flex items-center justify-between text-xs">
                    <div className="flex items-center gap-2">
                      <span className="w-2 h-2 rounded-full" style={{ backgroundColor: zone.color }} />
                      <span className="text-gray-400">{zone.label}</span>
                    </div>
                    <span className="text-gray-300 font-mono">{zone.count.toLocaleString()}</span>
                  </div>
                ))}
              </div>
            </div>
          ) : (
            <div className="text-gray-500 text-sm text-center py-8">No galactic data</div>
          )}
        </div>

        {/* Circuit Breakers — 4 cols */}
        <div className="col-span-4 glass-panel p-4">
          <h3 className="text-sm font-semibold mb-3 flex items-center gap-2 text-gray-300">
            <Zap size={14} className="text-red-400" /> Circuit Breakers
          </h3>
          <div className="space-y-2">
            {gnosis.circuit_breakers &&
              Object.entries(gnosis.circuit_breakers).map(([name, cb]) => (
                <BreakerChip key={name} name={name} state={cb.state} />
              ))}
          </div>
        </div>

        {/* Dharma + Karma — 4 cols */}
        <div className="col-span-4 glass-panel p-4">
          <h3 className="text-sm font-semibold mb-3 flex items-center gap-2 text-gray-300">
            <Shield size={14} className="text-green-400" /> Dharma & Karma
          </h3>
          <div className="space-y-3">
            {gnosis.dharma && (
              <div className="space-y-2">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Profile</span>
                  <span className="text-green-400 font-medium">{gnosis.dharma.profile}</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Active Rules</span>
                  <span className="text-gray-300">{gnosis.dharma.rules_count}</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Violations</span>
                  <span className={gnosis.dharma.violations > 0 ? "text-red-400" : "text-green-400"}>
                    {gnosis.dharma.violations}
                  </span>
                </div>
              </div>
            )}
            {gnosis.karma && (
              <div className="border-t border-wm-border pt-2 space-y-2">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Total Calls</span>
                  <span className="text-gray-300">{gnosis.karma.total_calls.toLocaleString()}</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Clean</span>
                  <span className="text-green-400">{gnosis.karma.clean.toLocaleString()}</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Undeclared Effects</span>
                  <span className={gnosis.karma.undeclared_effects > 0 ? "text-yellow-400" : "text-green-400"}>
                    {gnosis.karma.undeclared_effects}
                  </span>
                </div>
              </div>
            )}
          </div>
        </div>

        {/* Maturity & Homeostasis — 4 cols */}
        <div className="col-span-4 glass-panel p-4">
          <h3 className="text-sm font-semibold mb-3 flex items-center gap-2 text-gray-300">
            <Gauge size={14} className="text-orange-400" /> Maturity & Homeostasis
          </h3>
          <div className="space-y-3">
            {gnosis.maturity && (
              <div className="space-y-2">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Stage</span>
                  <span className="text-orange-400 font-bold">{gnosis.maturity.current_stage}</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Unlocked Tools</span>
                  <span className="text-green-400">{gnosis.maturity.unlocked_tools.length}</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Locked Tools</span>
                  <span className="text-gray-500">{gnosis.maturity.locked_tools.length}</span>
                </div>
              </div>
            )}
            {gnosis.homeostasis && (
              <div className="border-t border-wm-border pt-2 space-y-2">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Status</span>
                  <span className={gnosis.homeostasis.status === "STABLE" ? "text-green-400" : "text-yellow-400"}>
                    {gnosis.homeostasis.status}
                  </span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Corrections</span>
                  <span className="text-gray-300">{gnosis.homeostasis.corrections}</span>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
