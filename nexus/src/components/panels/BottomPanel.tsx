import {
  Terminal as TerminalIcon,
  Clock,
  Layers,
} from "lucide-react";
import { useNexusStore, type BottomTab } from "../../store/nexus";
import TerminalPanel from "./TerminalPanel";

const bottomTabs: { id: BottomTab; icon: typeof TerminalIcon; title: string }[] = [
  { id: "terminal", icon: TerminalIcon, title: "Terminal" },
  { id: "timeline", icon: Clock, title: "Timeline" },
  { id: "temporal", icon: Layers, title: "Temporal Lanes" },
];

const SAMPLE_TIMELINE = [
  { time: "16:08:42", action: "create_memory", agent: "Coder", detail: "Stored session config" },
  { time: "16:08:38", action: "search_memories", agent: "Architect", detail: "Query: 'rust bridge'" },
  { time: "16:08:30", action: "garden_activate", agent: "System", detail: "Activated wisdom garden" },
  { time: "16:08:25", action: "governor_validate", agent: "Governor", detail: "Validated: safe" },
  { time: "16:08:20", action: "edge_infer", agent: "Coder", detail: "Local inference: pattern match" },
];

export default function BottomPanel() {
  const activeTab = useNexusStore((s) => s.bottomTab);
  const setTab = useNexusStore((s) => s.setBottomTab);

  return (
    <div className="flex flex-col h-full">
      {/* Tab bar */}
      <div className="flex items-center border-b border-wm-border flex-shrink-0 px-2">
        {bottomTabs.map((tab) => {
          const Icon = tab.icon;
          return (
            <button
              key={tab.id}
              onClick={() => setTab(tab.id)}
              className={`flex items-center gap-1.5 px-3 py-1.5 text-xs transition border-b-2 ${
                activeTab === tab.id
                  ? "text-wm-purple-400 border-wm-purple-400"
                  : "text-gray-500 hover:text-gray-300 border-transparent"
              }`}
            >
              <Icon size={13} />
              {tab.title}
            </button>
          );
        })}
      </div>

      {/* Content */}
      <div className="flex-1 overflow-auto">
        {activeTab === "terminal" && <TerminalPanel />}

        {activeTab === "timeline" && (
          <div className="p-3">
            <div className="space-y-1">
              {SAMPLE_TIMELINE.map((evt, i) => (
                <div
                  key={i}
                  className="flex items-center gap-3 px-3 py-2 rounded hover:bg-white/5 transition text-xs"
                >
                  <span className="text-gray-600 font-mono w-16 flex-shrink-0">
                    {evt.time}
                  </span>
                  <span className="text-wm-purple-400 font-mono font-medium w-36 flex-shrink-0 truncate">
                    {evt.action}
                  </span>
                  <span className="text-wm-cyan-400 w-20 flex-shrink-0">
                    {evt.agent}
                  </span>
                  <span className="text-gray-500 truncate">{evt.detail}</span>
                </div>
              ))}
            </div>
          </div>
        )}

        {activeTab === "temporal" && (
          <div className="p-3 grid grid-cols-3 gap-3 h-full">
            {/* FAST Lane */}
            <div className="glass-card p-3">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-bold text-red-400 uppercase tracking-wider">
                  Fast
                </span>
                <span className="text-xs text-gray-500">&lt;10ms</span>
              </div>
              <div className="text-xs text-gray-500 mb-1">
                Safety, system events, threats
              </div>
              <div className="space-y-1 mt-3">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Queued</span>
                  <span className="text-gray-300">0</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Flushed</span>
                  <span className="text-gray-300">142</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Avg flush</span>
                  <span className="text-gray-300">0.02ms</span>
                </div>
              </div>
            </div>

            {/* MEDIUM Lane */}
            <div className="glass-card p-3">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-bold text-yellow-400 uppercase tracking-wider">
                  Medium
                </span>
                <span className="text-xs text-gray-500">~1s</span>
              </div>
              <div className="text-xs text-gray-500 mb-1">
                Planning, tool dispatch, reasoning
              </div>
              <div className="space-y-1 mt-3">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Queued</span>
                  <span className="text-yellow-400">3</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Flushed</span>
                  <span className="text-gray-300">891</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Avg flush</span>
                  <span className="text-gray-300">1.24ms</span>
                </div>
              </div>
            </div>

            {/* SLOW Lane */}
            <div className="glass-card p-3">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-bold text-blue-400 uppercase tracking-wider">
                  Slow
                </span>
                <span className="text-xs text-gray-500">60s+</span>
              </div>
              <div className="text-xs text-gray-500 mb-1">
                Consolidation, patterns, wisdom
              </div>
              <div className="space-y-1 mt-3">
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Queued</span>
                  <span className="text-blue-400">12</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Flushed</span>
                  <span className="text-gray-300">47</span>
                </div>
                <div className="flex justify-between text-xs">
                  <span className="text-gray-400">Avg flush</span>
                  <span className="text-gray-300">8.31ms</span>
                </div>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
