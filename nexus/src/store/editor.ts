import { create } from "zustand";

export interface OpenFile {
  path: string;
  name: string;
  language: string;
  content: string;
  dirty: boolean;
}

interface EditorState {
  openFiles: OpenFile[];
  activeFile: string | null;
  openFile: (file: OpenFile) => void;
  closeFile: (path: string) => void;
  setActiveFile: (path: string) => void;
  updateContent: (path: string, content: string) => void;
}

function detectLanguage(name: string): string {
  const ext = name.split(".").pop()?.toLowerCase() || "";
  const map: Record<string, string> = {
    py: "python",
    ts: "typescript",
    tsx: "typescriptreact",
    js: "javascript",
    jsx: "javascriptreact",
    rs: "rust",
    go: "go",
    json: "json",
    yaml: "yaml",
    yml: "yaml",
    md: "markdown",
    toml: "toml",
    css: "css",
    html: "html",
    sh: "shell",
    zig: "zig",
    ex: "elixir",
    exs: "elixir",
    hs: "haskell",
    mojo: "python",
    proto: "protobuf",
  };
  return map[ext] || "plaintext";
}

const WELCOME_CONTENT = `"""
WhiteMagic Nexus — Unified IDE & Command Center
================================================

Welcome! This editor is powered by Monaco (VS Code engine).

Quick start:
  - Use the file explorer (left panel) to browse files
  - Switch to Dashboard tab for system health & Wu Xing
  - Switch to Memory Graph tab to explore the knowledge space
  - The Gan Ying feed (right panel) shows live system events

Loaded subsystems:
  - 65+ MCP tools across 18 categories
  - 30 consciousness gardens
  - Harmony Vector (7-dimensional health)
  - Dharma Rules Engine (YAML-driven ethics)
  - Karma Ledger (side-effect audit)
  - Circuit Breakers (per-tool resilience)
  - Galactic Map (memory lifecycle orbits)
  - Temporal Scheduler (FAST/MEDIUM/SLOW lanes)
  - Homeostatic Loop (self-regulating feedback)
  - Maturity Gates (developmental milestones)
  - Tool Dependency Graph (affinity planning)
  - Gnosis Portal (unified introspection)
"""

from whitemagic.core.resonance import get_bus, EventType
from whitemagic.tools.gnosis import gnosis_snapshot
from whitemagic.harmony.vector import get_harmony_vector

# Take a full system snapshot
snapshot = gnosis_snapshot()
print(f"Harmony guna: {snapshot['harmony']['guna']}")
print(f"Dharma profile: {snapshot['dharma']['profile']}")
print(f"Maturity stage: {snapshot['maturity']['current_stage']}")
print(f"Circuit breakers: {len(snapshot['circuit_breakers'])} tracked")

# Check the harmony vector
hv = get_harmony_vector()
print(f"Balance: {hv.balance:.2f}")
print(f"Energy:  {hv.energy:.2f}")
print(f"Karma debt: {hv.karma_debt:.3f}")
`;

export const useEditorStore = create<EditorState>((set) => ({
  openFiles: [
    {
      path: "/welcome.py",
      name: "welcome.py",
      language: "python",
      content: WELCOME_CONTENT,
      dirty: false,
    },
  ],
  activeFile: "/welcome.py",

  openFile: (file) =>
    set((state) => {
      const exists = state.openFiles.find((f) => f.path === file.path);
      if (exists) return { activeFile: file.path };
      return {
        openFiles: [
          ...state.openFiles,
          { ...file, language: detectLanguage(file.name) },
        ],
        activeFile: file.path,
      };
    }),

  closeFile: (path) =>
    set((state) => {
      const files = state.openFiles.filter((f) => f.path !== path);
      const active =
        state.activeFile === path
          ? files[files.length - 1]?.path || null
          : state.activeFile;
      return { openFiles: files, activeFile: active };
    }),

  setActiveFile: (path) => set({ activeFile: path }),

  updateContent: (path, content) =>
    set((state) => ({
      openFiles: state.openFiles.map((f) =>
        f.path === path ? { ...f, content, dirty: true } : f
      ),
    })),
}));
