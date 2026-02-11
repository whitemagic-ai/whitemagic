import { useState, useRef, useEffect } from "react";
import {
  Send,
  Bot,
  User,
  Sparkles,
  Radio,
  Users,
} from "lucide-react";
import { useNexusStore, type RightTab } from "../../store/nexus";

const rightTabs: { id: RightTab; icon: typeof Sparkles; title: string }[] = [
  { id: "chat", icon: Sparkles, title: "AI Chat" },
  { id: "ganying", icon: Radio, title: "Gan Ying" },
  { id: "orchestrator", icon: Users, title: "Agents" },
];

interface Message {
  id: string;
  role: "user" | "assistant";
  content: string;
}

interface GanYingEvent {
  time: string;
  type: string;
  source: string;
  confidence: number;
}

const EVENT_POOL: { type: string; source: string; confRange: [number, number] }[] = [
  { type: "MEMORY_CREATED", source: "mcp", confRange: [0.9, 1.0] },
  { type: "MEMORY_ACCESSED", source: "mcp", confRange: [0.85, 1.0] },
  { type: "GARDEN_ACTIVATED", source: "wisdom", confRange: [0.8, 0.95] },
  { type: "GARDEN_SYNTHESIS", source: "synthesis", confRange: [0.85, 0.98] },
  { type: "BALANCE_RESTORED", source: "wu_xing", confRange: [0.95, 1.0] },
  { type: "INSIGHT_CRYSTALLIZED", source: "synthesis", confRange: [0.88, 0.97] },
  { type: "FLOW_STATE_ENTERED", source: "temporal", confRange: [0.8, 0.92] },
  { type: "DHARMA_CHECKED", source: "governor", confRange: [0.95, 1.0] },
  { type: "PATTERN_DETECTED", source: "consolidation", confRange: [0.75, 0.9] },
  { type: "TOOL_DISPATCHED", source: "dispatch", confRange: [0.9, 1.0] },
  { type: "BREAKER_PROBE", source: "circuit_breaker", confRange: [0.7, 0.85] },
  { type: "KARMA_RECORDED", source: "karma_ledger", confRange: [0.9, 1.0] },
  { type: "HARMONY_UPDATED", source: "harmony", confRange: [0.85, 0.98] },
  { type: "RETENTION_SWEEP", source: "lifecycle", confRange: [0.8, 0.95] },
  { type: "GALACTIC_ROTATION", source: "galactic_map", confRange: [0.9, 1.0] },
  { type: "HOMEOSTASIS_CHECK", source: "homeostasis", confRange: [0.88, 0.99] },
  { type: "MATURITY_ASSESSED", source: "maturity_gates", confRange: [0.9, 1.0] },
  { type: "SALIENCE_SCORED", source: "salience_arbiter", confRange: [0.7, 0.95] },
  { type: "BICAMERAL_REASON", source: "bicameral", confRange: [0.82, 0.96] },
  { type: "PIPELINE_STEP", source: "pipeline", confRange: [0.88, 1.0] },
  { type: "AGENT_HEARTBEAT", source: "agent_registry", confRange: [0.95, 1.0] },
];

function generateEvent(): GanYingEvent {
  const template = EVENT_POOL[Math.floor(Math.random() * EVENT_POOL.length)];
  const now = new Date();
  const conf = template.confRange[0] + Math.random() * (template.confRange[1] - template.confRange[0]);
  return {
    time: now.toLocaleTimeString("en-US", { hour12: false }),
    type: template.type,
    source: template.source,
    confidence: Math.round(conf * 100) / 100,
  };
}

function getEventColor(type: string): string {
  if (type.includes("JOY")) return "text-yellow-400";
  if (type.includes("BEAUTY")) return "text-pink-400";
  if (type.includes("TRUTH")) return "text-blue-400";
  if (type.includes("WISDOM") || type.includes("INSIGHT")) return "text-purple-400";
  if (type.includes("DHARMA") || type.includes("BALANCE")) return "text-green-400";
  if (type.includes("PATTERN")) return "text-cyan-400";
  if (type.includes("MEMORY")) return "text-indigo-400";
  if (type.includes("GARDEN")) return "text-emerald-400";
  if (type.includes("FLOW")) return "text-orange-400";
  return "text-gray-400";
}

export default function RightPanel() {
  const activeTab = useNexusStore((s) => s.rightTab);
  const setTab = useNexusStore((s) => s.setRightTab);
  const [messages, setMessages] = useState<Message[]>([
    {
      id: "1",
      role: "assistant",
      content:
        "Hello! I'm connected to WhiteMagic Core. I can search memories, activate gardens, check Wu Xing phase, or run any of the 58 MCP tools. What would you like to explore?",
    },
  ]);
  const [input, setInput] = useState("");
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const [events, setEvents] = useState<GanYingEvent[]>(() =>
    Array.from({ length: 8 }, () => generateEvent())
  );
  const [paused, setPaused] = useState(false);
  const eventsEndRef = useRef<HTMLDivElement>(null);

  // Live event feed — real WebSocket with mock fallback
  const wsRef = useRef<WebSocket | null>(null);
  const [wsConnected, setWsConnected] = useState(false);

  useEffect(() => {
    if (paused) return;

    // Try real WebSocket first
    const wsUrl = (import.meta.env.VITE_WM_API_URL || "http://localhost:8000")
      .replace(/^http/, "ws") + "/ws/ganying";

    let fallbackInterval: ReturnType<typeof setInterval> | null = null;
    let ws: WebSocket | null = null;

    try {
      ws = new WebSocket(wsUrl);
      wsRef.current = ws;

      ws.onopen = () => {
        setWsConnected(true);
        if (fallbackInterval) clearInterval(fallbackInterval);
      };

      ws.onmessage = (evt) => {
        try {
          const data = JSON.parse(evt.data);
          const event: GanYingEvent = {
            time: new Date().toLocaleTimeString("en-US", { hour12: false }),
            type: data.event_type || data.type || data.dream_event || "UNKNOWN",
            source: data.source || "ws",
            confidence: data.confidence ?? data.salience ?? 0.9,
          };
          setEvents((prev) => [event, ...prev].slice(0, 50));
        } catch {
          // ignore malformed messages
        }
      };

      ws.onclose = () => {
        setWsConnected(false);
        // Start mock fallback on disconnect
        if (!paused) {
          fallbackInterval = setInterval(() => {
            setEvents((prev) => [generateEvent(), ...prev].slice(0, 50));
          }, 1500 + Math.random() * 2000);
        }
      };

      ws.onerror = () => {
        ws?.close();
      };
    } catch {
      // WebSocket not available — use mock fallback
    }

    // Start mock fallback immediately (will be cleared if WS connects)
    fallbackInterval = setInterval(() => {
      setEvents((prev) => [generateEvent(), ...prev].slice(0, 50));
    }, 1500 + Math.random() * 2000);

    return () => {
      if (fallbackInterval) clearInterval(fallbackInterval);
      if (ws && ws.readyState <= 1) ws.close();
    };
  }, [paused]);

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  const handleSend = () => {
    if (!input.trim()) return;
    const userMsg: Message = {
      id: Date.now().toString(),
      role: "user",
      content: input,
    };
    setMessages((prev) => [
      ...prev,
      userMsg,
      {
        id: (Date.now() + 1).toString(),
        role: "assistant",
        content:
          "AI chat will connect to Ollama/WhiteMagic API in Phase 2. For now, this is the placeholder interface.",
      },
    ]);
    setInput("");
  };

  return (
    <div className="flex flex-col h-full">
      {/* Tab bar */}
      <div className="flex border-b border-wm-border flex-shrink-0">
        {rightTabs.map((tab) => {
          const Icon = tab.icon;
          return (
            <button
              key={tab.id}
              onClick={() => setTab(tab.id)}
              className={`flex-1 p-2 flex items-center justify-center gap-1.5 text-xs transition ${
                activeTab === tab.id
                  ? "text-wm-purple-400 border-b-2 border-wm-purple-400 bg-wm-purple-500/10"
                  : "text-gray-500 hover:text-gray-300 hover:bg-wm-border/30"
              }`}
              title={tab.title}
            >
              <Icon size={14} />
              {tab.title}
            </button>
          );
        })}
      </div>

      {/* Content */}
      <div className="flex-1 overflow-hidden flex flex-col">
        {activeTab === "chat" && (
          <>
            {/* Messages */}
            <div className="flex-1 overflow-auto p-3 space-y-3">
              {messages.map((msg) => (
                <div
                  key={msg.id}
                  className={`flex gap-2 ${
                    msg.role === "user" ? "flex-row-reverse" : ""
                  }`}
                >
                  <div
                    className={`w-6 h-6 rounded-full flex items-center justify-center flex-shrink-0 ${
                      msg.role === "user"
                        ? "bg-wm-purple-500/30 text-wm-purple-400"
                        : "bg-wm-cyan-500/30 text-wm-cyan-400"
                    }`}
                  >
                    {msg.role === "user" ? (
                      <User size={12} />
                    ) : (
                      <Bot size={12} />
                    )}
                  </div>
                  <div
                    className={`max-w-[85%] rounded-lg px-3 py-2 text-sm ${
                      msg.role === "user"
                        ? "bg-wm-purple-500/20 text-gray-200"
                        : "bg-wm-bg text-gray-300"
                    }`}
                  >
                    {msg.content}
                  </div>
                </div>
              ))}
              <div ref={messagesEndRef} />
            </div>

            {/* Input */}
            <div className="p-3 border-t border-wm-border flex-shrink-0">
              <div className="flex gap-2">
                <input
                  type="text"
                  value={input}
                  onChange={(e) => setInput(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Enter" && !e.shiftKey) {
                      e.preventDefault();
                      handleSend();
                    }
                  }}
                  placeholder="Ask anything..."
                  className="flex-1 px-3 py-2 rounded-lg bg-wm-bg border border-wm-border text-sm focus:border-wm-purple-500 focus:outline-none text-gray-200 placeholder-gray-500"
                />
                <button
                  onClick={handleSend}
                  disabled={!input.trim()}
                  className="px-3 py-2 rounded-lg bg-wm-purple-500 hover:bg-wm-purple-600 disabled:opacity-50 disabled:cursor-not-allowed transition"
                >
                  <Send size={14} />
                </button>
              </div>
            </div>
          </>
        )}

        {activeTab === "ganying" && (
          <div className="flex-1 overflow-auto p-3">
            <div className="flex items-center justify-between mb-3">
              <div className="text-xs text-gray-500 uppercase tracking-wider flex items-center gap-1.5">
                <Radio size={12} className="text-wm-purple-400" />
                Resonance Feed
                <span className="text-gray-600">({events.length})</span>
              </div>
              <div className="flex items-center gap-2">
                <button
                  onClick={() => setPaused(!paused)}
                  className={`text-[10px] px-2 py-0.5 rounded-full border transition ${
                    paused
                      ? "text-yellow-400 border-yellow-500/30 bg-yellow-500/10"
                      : "text-green-400 border-green-500/30 bg-green-500/10"
                  }`}
                >
                  {paused ? "Paused" : "Live"}
                </button>
                {!paused && (
                  <span className="w-1.5 h-1.5 rounded-full bg-green-500 animate-pulse" />
                )}
              </div>
            </div>
            <div className="space-y-1.5">
              {events.map((evt, i) => (
                <div
                  key={`${evt.time}-${evt.type}-${i}`}
                  className={`flex items-center justify-between glass-card px-3 py-1.5 text-xs transition ${
                    i === 0 ? "animate-fade-in" : ""
                  }`}
                >
                  <div className="flex items-center gap-2">
                    <span className="text-gray-600 font-mono text-[10px]">
                      {evt.time}
                    </span>
                    <span
                      className={`font-mono font-bold ${getEventColor(
                        evt.type
                      )}`}
                    >
                      {evt.type}
                    </span>
                  </div>
                  <div className="flex items-center gap-2">
                    <span className="text-gray-600 italic text-[10px]">{evt.source}</span>
                    <span className="text-wm-purple-400 bg-wm-purple-500/10 px-1.5 py-0.5 rounded-full border border-wm-purple-500/20 font-medium">
                      {(evt.confidence * 100).toFixed(0)}%
                    </span>
                  </div>
                </div>
              ))}
              <div ref={eventsEndRef} />
            </div>
            <div className="mt-4 grid grid-cols-4 gap-2 text-center text-xs">
              {[
                { name: "Joy", emoji: "\u{1F31F}", color: "text-yellow-400" },
                { name: "Beauty", emoji: "\u{1F338}", color: "text-pink-400" },
                { name: "Truth", emoji: "\u{1F48E}", color: "text-blue-400" },
                { name: "Wisdom", emoji: "\u{1F52E}", color: "text-purple-400" },
              ].map((g, i) => (
                <div
                  key={i}
                  className="p-2 rounded-lg hover:bg-white/5 transition-colors"
                >
                  <div className={`font-bold mb-1 ${g.color}`}>{g.name}</div>
                  <div className="text-lg">{g.emoji}</div>
                </div>
              ))}
            </div>
          </div>
        )}

        {activeTab === "orchestrator" && (
          <div className="flex-1 overflow-auto p-3">
            <div className="text-xs text-gray-500 uppercase tracking-wider mb-3 flex items-center gap-1.5">
              <Users size={12} />
              Multi-Agent Orchestrator
            </div>
            <div className="space-y-2">
              {[
                {
                  role: "Architect",
                  emoji: "\u{1F3D7}\u{FE0F}",
                  status: "idle",
                  budget: "50/100",
                },
                {
                  role: "Coder",
                  emoji: "\u{1F4BB}",
                  status: "active",
                  budget: "32/100",
                },
                {
                  role: "Critic",
                  emoji: "\u{1F50D}",
                  status: "idle",
                  budget: "10/50",
                },
                {
                  role: "Runner",
                  emoji: "\u{26A1}",
                  status: "idle",
                  budget: "5/30",
                },
              ].map((agent, i) => (
                <div
                  key={i}
                  className="glass-card px-3 py-2.5 flex items-center gap-3"
                >
                  <span className="text-lg">{agent.emoji}</span>
                  <div className="flex-1">
                    <div className="text-sm text-gray-300 font-medium">
                      {agent.role}
                    </div>
                    <div className="text-xs text-gray-500">
                      Budget: {agent.budget} calls
                    </div>
                  </div>
                  <span
                    className={`text-xs px-2 py-0.5 rounded-full ${
                      agent.status === "active"
                        ? "bg-green-500/20 text-green-400"
                        : "bg-gray-500/20 text-gray-500"
                    }`}
                  >
                    {agent.status}
                  </span>
                </div>
              ))}
            </div>
            <button className="w-full mt-3 py-2 rounded-lg border border-dashed border-wm-border text-xs text-gray-500 hover:text-wm-purple-400 hover:border-wm-purple-500/50 transition">
              + Add Agent
            </button>
          </div>
        )}
      </div>
    </div>
  );
}
