import { useState, useEffect, useRef, useCallback } from "react";
import {
  Search,
  Code2,
  LayoutDashboard,
  Network,
  GitBranch,
  Terminal,
  Clock,
  Layers,
  PanelLeft,
  PanelRight,
  PanelBottom,
  Brain,
  Sparkles,
  Shield,
  Radio,
  Users,
  Command,
} from "lucide-react";
import { useNexusStore } from "../store/nexus";

interface PaletteAction {
  id: string;
  label: string;
  shortcut?: string;
  icon: typeof Search;
  category: string;
  action: () => void;
}

export default function CommandPalette() {
  const [open, setOpen] = useState(false);
  const [query, setQuery] = useState("");
  const [selectedIndex, setSelectedIndex] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);

  const setCenterTab = useNexusStore((s) => s.setCenterTab);
  const setLeftTab = useNexusStore((s) => s.setLeftTab);
  const setRightTab = useNexusStore((s) => s.setRightTab);
  const setBottomTab = useNexusStore((s) => s.setBottomTab);
  const togglePanel = useNexusStore((s) => s.togglePanel);

  const actions: PaletteAction[] = [
    // View tabs
    { id: "editor", label: "Open Editor", shortcut: "Ctrl+1", icon: Code2, category: "View", action: () => setCenterTab("editor") },
    { id: "dashboard", label: "Open Dashboard", shortcut: "Ctrl+2", icon: LayoutDashboard, category: "View", action: () => setCenterTab("dashboard") },
    { id: "graph", label: "Open Memory Graph", shortcut: "Ctrl+3", icon: Network, category: "View", action: () => setCenterTab("graph") },
    { id: "tools", label: "Open Tool Graph", shortcut: "Ctrl+4", icon: GitBranch, category: "View", action: () => setCenterTab("tools") },

    // Panel toggles
    { id: "toggle-left", label: "Toggle Left Panel", shortcut: "Ctrl+B", icon: PanelLeft, category: "Panel", action: () => togglePanel("left") },
    { id: "toggle-right", label: "Toggle Right Panel", shortcut: "Ctrl+J", icon: PanelRight, category: "Panel", action: () => togglePanel("right") },
    { id: "toggle-bottom", label: "Toggle Bottom Panel", shortcut: "Ctrl+`", icon: PanelBottom, category: "Panel", action: () => togglePanel("bottom") },

    // Left panel tabs
    { id: "explorer", label: "File Explorer", icon: Code2, category: "Explorer", action: () => { togglePanel("left"); setLeftTab("explorer"); } },
    { id: "search", label: "Search Files & Memories", shortcut: "Ctrl+Shift+F", icon: Search, category: "Explorer", action: () => { togglePanel("left"); setLeftTab("search"); } },
    { id: "memories-tab", label: "Memories Panel", icon: Brain, category: "Explorer", action: () => { togglePanel("left"); setLeftTab("memories"); } },
    { id: "gardens-tab", label: "Gardens Panel", icon: Sparkles, category: "Explorer", action: () => { togglePanel("left"); setLeftTab("gardens"); } },
    { id: "dharma-tab", label: "Dharma Panel", icon: Shield, category: "Explorer", action: () => { togglePanel("left"); setLeftTab("dharma"); } },

    // Right panel tabs
    { id: "chat", label: "AI Chat", icon: Sparkles, category: "Assistant", action: () => { togglePanel("right"); setRightTab("chat"); } },
    { id: "ganying", label: "Gan Ying Feed", icon: Radio, category: "Assistant", action: () => { togglePanel("right"); setRightTab("ganying"); } },
    { id: "orchestrator", label: "Agent Orchestrator", icon: Users, category: "Assistant", action: () => { togglePanel("right"); setRightTab("orchestrator"); } },

    // Bottom panel tabs
    { id: "terminal", label: "Terminal", shortcut: "Ctrl+`", icon: Terminal, category: "Terminal", action: () => { togglePanel("bottom"); setBottomTab("terminal"); } },
    { id: "timeline", label: "Session Timeline", icon: Clock, category: "Terminal", action: () => { togglePanel("bottom"); setBottomTab("timeline"); } },
    { id: "temporal", label: "Temporal Lanes", icon: Layers, category: "Terminal", action: () => { togglePanel("bottom"); setBottomTab("temporal"); } },
  ];

  const filtered = query
    ? actions.filter(
        (a) =>
          a.label.toLowerCase().includes(query.toLowerCase()) ||
          a.category.toLowerCase().includes(query.toLowerCase())
      )
    : actions;

  useEffect(() => {
    setSelectedIndex(0);
  }, [query]);

  const execute = useCallback(
    (action: PaletteAction) => {
      action.action();
      setOpen(false);
      setQuery("");
    },
    []
  );

  // Global keyboard shortcuts
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      // Ctrl+K or Ctrl+P → open palette
      if ((e.ctrlKey || e.metaKey) && (e.key === "k" || e.key === "p")) {
        e.preventDefault();
        setOpen((prev) => !prev);
        return;
      }

      // Escape → close
      if (e.key === "Escape" && open) {
        setOpen(false);
        setQuery("");
        return;
      }

      // Direct shortcuts when palette is closed
      if (!open && (e.ctrlKey || e.metaKey)) {
        if (e.key === "1") { e.preventDefault(); setCenterTab("editor"); return; }
        if (e.key === "2") { e.preventDefault(); setCenterTab("dashboard"); return; }
        if (e.key === "3") { e.preventDefault(); setCenterTab("graph"); return; }
        if (e.key === "4") { e.preventDefault(); setCenterTab("tools"); return; }
        if (e.key === "b") { e.preventDefault(); togglePanel("left"); return; }
        if (e.key === "j") { e.preventDefault(); togglePanel("right"); return; }
        if (e.key === "`") { e.preventDefault(); togglePanel("bottom"); return; }
      }

      // Navigation within palette
      if (open) {
        if (e.key === "ArrowDown") {
          e.preventDefault();
          setSelectedIndex((i) => Math.min(i + 1, filtered.length - 1));
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          setSelectedIndex((i) => Math.max(i - 1, 0));
        } else if (e.key === "Enter" && filtered[selectedIndex]) {
          e.preventDefault();
          execute(filtered[selectedIndex]);
        }
      }
    };

    window.addEventListener("keydown", handler);
    return () => window.removeEventListener("keydown", handler);
  }, [open, filtered, selectedIndex, execute, setCenterTab, togglePanel]);

  // Focus input when opened
  useEffect(() => {
    if (open) {
      requestAnimationFrame(() => inputRef.current?.focus());
    }
  }, [open]);

  if (!open) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-start justify-center pt-[15vh]">
      {/* Backdrop */}
      <div
        className="absolute inset-0 bg-black/60 backdrop-blur-sm"
        onClick={() => {
          setOpen(false);
          setQuery("");
        }}
      />

      {/* Palette */}
      <div className="relative w-[520px] bg-wm-surface border border-wm-border rounded-xl shadow-2xl shadow-purple-900/20 animate-fade-in overflow-hidden">
        {/* Search input */}
        <div className="flex items-center gap-3 px-4 py-3 border-b border-wm-border">
          <Command size={16} className="text-wm-purple-400 flex-shrink-0" />
          <input
            ref={inputRef}
            type="text"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            placeholder="Type a command..."
            className="flex-1 bg-transparent text-sm text-white placeholder-gray-500 focus:outline-none"
          />
          <kbd className="text-[10px] text-gray-500 bg-wm-bg px-1.5 py-0.5 rounded border border-wm-border">
            ESC
          </kbd>
        </div>

        {/* Results */}
        <div className="max-h-[300px] overflow-auto py-1">
          {filtered.length === 0 && (
            <div className="text-center text-gray-500 text-sm py-6">
              No matching commands
            </div>
          )}
          {filtered.map((action, i) => {
            const Icon = action.icon;
            const isSelected = i === selectedIndex;
            return (
              <button
                key={action.id}
                onClick={() => execute(action)}
                onMouseEnter={() => setSelectedIndex(i)}
                className={`w-full flex items-center gap-3 px-4 py-2 text-sm transition ${
                  isSelected
                    ? "bg-wm-purple-500/15 text-white"
                    : "text-gray-400 hover:text-white hover:bg-white/5"
                }`}
              >
                <Icon
                  size={14}
                  className={isSelected ? "text-wm-purple-400" : "text-gray-500"}
                />
                <span className="flex-1 text-left">{action.label}</span>
                <span className="text-[10px] text-gray-600">{action.category}</span>
                {action.shortcut && (
                  <kbd className="text-[10px] text-gray-500 bg-wm-bg px-1.5 py-0.5 rounded border border-wm-border ml-2">
                    {action.shortcut}
                  </kbd>
                )}
              </button>
            );
          })}
        </div>

        {/* Footer */}
        <div className="flex items-center justify-between px-4 py-2 border-t border-wm-border text-[10px] text-gray-600">
          <span>
            <kbd className="bg-wm-bg px-1 py-0.5 rounded border border-wm-border mr-1">↑↓</kbd>
            navigate
            <kbd className="bg-wm-bg px-1 py-0.5 rounded border border-wm-border mx-1">↵</kbd>
            select
          </span>
          <span>
            <kbd className="bg-wm-bg px-1 py-0.5 rounded border border-wm-border mr-1">Ctrl+K</kbd>
            toggle
          </span>
        </div>
      </div>
    </div>
  );
}
