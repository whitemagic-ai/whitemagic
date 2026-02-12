import {
  PanelLeft,
  PanelBottom,
  PanelRight,
  Code2,
  LayoutDashboard,
  Network,
  GitBranch,
} from "lucide-react";
import { useNexusStore, type CenterTab } from "../store/nexus";
import { WU_XING_META } from "../lib/constants";

const centerTabs: { id: CenterTab; label: string; icon: typeof Code2 }[] = [
  { id: "editor", label: "Editor", icon: Code2 },
  { id: "dashboard", label: "Dashboard", icon: LayoutDashboard },
  { id: "graph", label: "Memory Graph", icon: Network },
  { id: "tools", label: "Tool Graph", icon: GitBranch },
];

export default function Header() {
  const panels = useNexusStore((s) => s.panels);
  const togglePanel = useNexusStore((s) => s.togglePanel);
  const centerTab = useNexusStore((s) => s.centerTab);
  const setCenterTab = useNexusStore((s) => s.setCenterTab);
  const phase = useNexusStore((s) => s.wuXingPhase);
  const phaseMeta = WU_XING_META[phase];

  return (
    <header className="flex items-center justify-between h-10 px-3 bg-wm-surface border-b border-wm-border flex-shrink-0 select-none">
      {/* Left: Logo + Center tabs */}
      <div className="flex items-center gap-4">
        {/* Logo */}
        <div className="flex items-center gap-2">
          <span className="text-lg">🔮</span>
          <span className="font-display text-sm font-semibold tracking-wider bg-gradient-to-r from-wm-purple-400 to-wm-cyan-400 bg-clip-text text-transparent">
            NEXUS
          </span>
        </div>

        {/* Center tab switcher */}
        <div className="flex items-center bg-wm-bg/60 rounded-lg p-0.5 border border-wm-border">
          {centerTabs.map((tab) => {
            const Icon = tab.icon;
            const active = centerTab === tab.id;
            return (
              <button
                key={tab.id}
                onClick={() => setCenterTab(tab.id)}
                className={`flex items-center gap-1.5 px-3 py-1 rounded-md text-xs font-medium transition-all ${
                  active
                    ? "bg-wm-purple-500/20 text-wm-purple-400 shadow-sm"
                    : "text-gray-500 hover:text-gray-300 hover:bg-white/5"
                }`}
              >
                <Icon size={13} />
                {tab.label}
              </button>
            );
          })}
        </div>
      </div>

      {/* Right: Wu Xing phase + Panel toggles */}
      <div className="flex items-center gap-3">
        {/* Wu Xing Phase */}
        <div
          className="flex items-center gap-1.5 px-2.5 py-1 rounded-full text-xs border border-white/10 bg-white/5"
          title={`${phaseMeta.label} Phase (${phaseMeta.season}) — ${phaseMeta.advice}`}
        >
          <span>{phaseMeta.emoji}</span>
          <span style={{ color: phaseMeta.color }} className="font-medium">
            {phaseMeta.label}
          </span>
        </div>

        {/* Panel toggles */}
        <div className="flex items-center gap-1">
          <button
            onClick={() => togglePanel("left")}
            className={`p-1.5 rounded transition ${
              panels.left.visible
                ? "bg-wm-purple-500/20 text-wm-purple-400"
                : "hover:bg-wm-border text-gray-500 hover:text-gray-300"
            }`}
            title="Toggle Left Panel"
          >
            <PanelLeft size={15} />
          </button>
          <button
            onClick={() => togglePanel("bottom")}
            className={`p-1.5 rounded transition ${
              panels.bottom.visible
                ? "bg-wm-purple-500/20 text-wm-purple-400"
                : "hover:bg-wm-border text-gray-500 hover:text-gray-300"
            }`}
            title="Toggle Bottom Panel"
          >
            <PanelBottom size={15} />
          </button>
          <button
            onClick={() => togglePanel("right")}
            className={`p-1.5 rounded transition ${
              panels.right.visible
                ? "bg-wm-purple-500/20 text-wm-purple-400"
                : "hover:bg-wm-border text-gray-500 hover:text-gray-300"
            }`}
            title="Toggle Right Panel"
          >
            <PanelRight size={15} />
          </button>
        </div>
      </div>
    </header>
  );
}
