import {
  FolderTree,
  Search,
  Brain,
  Sparkles,
  Shield,
  ChevronRight,
  ChevronDown,
  FileText,
  Folder,
  FolderOpen,
} from "lucide-react";
import { useState } from "react";
import { useNexusStore, type LeftTab } from "../../store/nexus";
import { useEditorStore } from "../../store/editor";

const tabs: { id: LeftTab; icon: typeof FolderTree; title: string }[] = [
  { id: "explorer", icon: FolderTree, title: "Explorer" },
  { id: "search", icon: Search, title: "Search" },
  { id: "memories", icon: Brain, title: "Memories" },
  { id: "gardens", icon: Sparkles, title: "Gardens" },
  { id: "dharma", icon: Shield, title: "Dharma" },
];

interface TreeNode {
  name: string;
  type: "file" | "folder";
  children?: TreeNode[];
}

const SAMPLE_TREE: TreeNode[] = [
  {
    name: "whitemagic",
    type: "folder",
    children: [
      {
        name: "core",
        type: "folder",
        children: [
          {
            name: "resonance",
            type: "folder",
            children: [
              { name: "gan_ying.py", type: "file" },
              { name: "temporal_scheduler.py", type: "file" },
              { name: "salience_arbiter.py", type: "file" },
            ],
          },
          { name: "governor.py", type: "file" },
        ],
      },
      {
        name: "gardens",
        type: "folder",
        children: [
          { name: "joy/", type: "folder", children: [] },
          { name: "love/", type: "folder", children: [] },
          { name: "wisdom/", type: "folder", children: [] },
          { name: "dharma/", type: "folder", children: [] },
          { name: "synthesis.py", type: "file" },
        ],
      },
      {
        name: "tools",
        type: "folder",
        children: [
          { name: "registry.py", type: "file" },
          { name: "unified_api.py", type: "file" },
        ],
      },
      { name: "__init__.py", type: "file" },
      { name: "run_mcp.py", type: "file" },
    ],
  },
];

const SAMPLE_GARDENS = [
  { name: "Joy", emoji: "\u{1F31F}", count: 42, color: "text-yellow-400" },
  { name: "Love", emoji: "\u{1F338}", count: 38, color: "text-pink-400" },
  { name: "Wisdom", emoji: "\u{1F52E}", count: 67, color: "text-purple-400" },
  { name: "Truth", emoji: "\u{1F48E}", count: 29, color: "text-blue-400" },
  { name: "Beauty", emoji: "\u{1F33A}", count: 18, color: "text-rose-400" },
  { name: "Mystery", emoji: "\u{1F30C}", count: 15, color: "text-indigo-400" },
  { name: "Play", emoji: "\u{1F3AD}", count: 22, color: "text-amber-400" },
  { name: "Wonder", emoji: "\u{2728}", count: 31, color: "text-cyan-400" },
  { name: "Dharma", emoji: "\u{2638}\u{FE0F}", count: 55, color: "text-green-400" },
  { name: "Courage", emoji: "\u{1F981}", count: 12, color: "text-orange-400" },
];

function TreeItem({
  node,
  depth = 0,
  parentPath = "",
}: {
  node: TreeNode;
  depth?: number;
  parentPath?: string;
}) {
  const [open, setOpen] = useState(depth < 2);
  const openFile = useEditorStore((s) => s.openFile);
  const setCenterTab = useNexusStore((s) => s.setCenterTab);

  const fullPath = parentPath ? `${parentPath}/${node.name}` : node.name;

  const handleClick = () => {
    if (node.type === "folder") {
      setOpen(!open);
    } else {
      openFile({
        path: fullPath,
        name: node.name,
        language: "python",
        content: `# ${node.name}\n# Loaded from WhiteMagic workspace\n\n# File content will be fetched from the filesystem\n# when Tauri backend is connected (Phase 5).\n`,
        dirty: false,
      });
      setCenterTab("editor");
    }
  };

  return (
    <div>
      <button
        onClick={handleClick}
        className="w-full flex items-center gap-1.5 px-2 py-1 text-sm hover:bg-wm-border/50 rounded transition text-left"
        style={{ paddingLeft: `${depth * 12 + 8}px` }}
      >
        {node.type === "folder" ? (
          <>
            {open ? (
              <ChevronDown size={12} />
            ) : (
              <ChevronRight size={12} />
            )}
            {open ? (
              <FolderOpen size={14} className="text-wm-purple-400" />
            ) : (
              <Folder size={14} className="text-wm-purple-400" />
            )}
          </>
        ) : (
          <>
            <span className="w-3" />
            <FileText size={14} className="text-gray-500" />
          </>
        )}
        <span
          className={
            node.type === "folder" ? "text-gray-300" : "text-gray-400"
          }
        >
          {node.name}
        </span>
      </button>
      {node.type === "folder" &&
        open &&
        node.children?.map((child, i) => (
          <TreeItem key={i} node={child} depth={depth + 1} parentPath={fullPath} />
        ))}
    </div>
  );
}

export default function LeftPanel() {
  const activeTab = useNexusStore((s) => s.leftTab);
  const setTab = useNexusStore((s) => s.setLeftTab);

  return (
    <div className="flex flex-col h-full">
      {/* Tab bar */}
      <div className="flex border-b border-wm-border flex-shrink-0">
        {tabs.map((tab) => {
          const Icon = tab.icon;
          return (
            <button
              key={tab.id}
              onClick={() => setTab(tab.id)}
              className={`flex-1 p-2 flex items-center justify-center transition ${
                activeTab === tab.id
                  ? "text-wm-purple-400 border-b-2 border-wm-purple-400 bg-wm-purple-500/10"
                  : "text-gray-500 hover:text-gray-300 hover:bg-wm-border/30"
              }`}
              title={tab.title}
            >
              <Icon size={16} />
            </button>
          );
        })}
      </div>

      {/* Content */}
      <div className="flex-1 overflow-auto">
        {activeTab === "explorer" && (
          <div className="p-2">
            <div className="text-xs text-gray-500 uppercase tracking-wider px-2 py-1 mb-1">
              Workspace
            </div>
            {SAMPLE_TREE.map((node, i) => (
              <TreeItem key={i} node={node} />
            ))}
          </div>
        )}

        {activeTab === "search" && (
          <div className="p-3">
            <input
              type="text"
              placeholder="Search files & memories..."
              className="w-full px-3 py-2 rounded-lg bg-wm-bg border border-wm-border text-sm focus:border-wm-purple-500 focus:outline-none text-gray-200 placeholder-gray-500"
            />
            <div className="mt-4 text-xs text-gray-500 text-center">
              Search across files, memories, and gardens
            </div>
          </div>
        )}

        {activeTab === "memories" && (
          <div className="p-2">
            <div className="text-xs text-gray-500 uppercase tracking-wider px-2 py-1 mb-1 flex items-center gap-1">
              <Brain size={12} />
              Recent Memories
            </div>
            {[
              "Session Bootstrap Config",
              "Rust Bridge Setup",
              "Garden Synthesis Architecture",
              "Wu Xing Phase Detection",
              "Dharma Rules Engine",
            ].map((title, i) => (
              <button
                key={i}
                className="w-full text-left px-2 py-2 rounded hover:bg-wm-border/50 transition"
              >
                <div className="text-sm text-gray-300 truncate">{title}</div>
                <div className="text-xs text-gray-500 mt-0.5">
                  {i < 2 ? "long_term" : "short_term"} &middot;{" "}
                  {5 - i}m ago
                </div>
              </button>
            ))}
          </div>
        )}

        {activeTab === "gardens" && (
          <div className="p-2">
            <div className="text-xs text-gray-500 uppercase tracking-wider px-2 py-1 mb-1 flex items-center gap-1">
              <Sparkles size={12} />
              Consciousness Gardens
            </div>
            {SAMPLE_GARDENS.map((garden, i) => (
              <button
                key={i}
                className="w-full flex items-center gap-2 px-2 py-2 rounded hover:bg-wm-border/50 transition"
              >
                <span className="text-base">{garden.emoji}</span>
                <span className={`text-sm ${garden.color}`}>
                  {garden.name}
                </span>
                <span className="ml-auto text-xs text-gray-500">
                  {garden.count}
                </span>
              </button>
            ))}
          </div>
        )}

        {activeTab === "dharma" && (
          <div className="p-3">
            <div className="text-xs text-gray-500 uppercase tracking-wider mb-3 flex items-center gap-1">
              <Shield size={12} />
              Ethical Boundaries
            </div>
            <div className="space-y-3">
              {[
                { name: "Privacy", threshold: 0.8, status: "ok" },
                { name: "Autonomy", threshold: 0.9, status: "ok" },
                { name: "Transparency", threshold: 0.7, status: "ok" },
                { name: "Capability", threshold: 0.6, status: "ok" },
              ].map((b, i) => (
                <div key={i} className="space-y-1">
                  <div className="flex justify-between text-xs">
                    <span className="text-gray-300">{b.name}</span>
                    <span className="text-green-400">{b.threshold * 100}%</span>
                  </div>
                  <div className="h-1.5 bg-wm-bg rounded-full overflow-hidden">
                    <div
                      className="h-full bg-gradient-to-r from-green-500 to-green-400 rounded-full"
                      style={{ width: `${b.threshold * 100}%` }}
                    />
                  </div>
                </div>
              ))}
              <div className="mt-4 p-3 rounded-lg bg-green-500/10 border border-green-500/20">
                <div className="text-xs text-green-400 font-medium">
                  All boundaries healthy
                </div>
                <div className="text-xs text-gray-500 mt-1">
                  0 violations in current session
                </div>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
