import { create } from "zustand";

export type WuXingPhase = "wood" | "fire" | "earth" | "metal" | "water";

export interface PanelState {
  left: { visible: boolean; width: number };
  right: { visible: boolean; width: number };
  bottom: { visible: boolean; height: number };
}

export type CenterTab = "editor" | "dashboard" | "graph" | "tools";
export type LeftTab = "explorer" | "search" | "memories" | "gardens" | "dharma";
export type RightTab = "chat" | "ganying" | "orchestrator";
export type BottomTab = "terminal" | "timeline" | "temporal";

export interface NexusState {
  // Panel layout
  panels: PanelState;
  togglePanel: (panel: keyof PanelState) => void;
  setPanelSize: (panel: keyof PanelState, size: number) => void;

  // Tab state
  centerTab: CenterTab;
  setCenterTab: (tab: CenterTab) => void;
  leftTab: LeftTab;
  setLeftTab: (tab: LeftTab) => void;
  rightTab: RightTab;
  setRightTab: (tab: RightTab) => void;
  bottomTab: BottomTab;
  setBottomTab: (tab: BottomTab) => void;

  // Wu Xing phase
  wuXingPhase: WuXingPhase;
  setWuXingPhase: (phase: WuXingPhase) => void;

  // System status
  status: {
    cpu: number;
    memory: number;
    memoryCount: number;
    patternCount: number;
    dharmaOk: boolean;
    apiConnected: boolean;
  };
  setStatus: (status: Partial<NexusState["status"]>) => void;
}

function detectWuXingPhase(): WuXingPhase {
  const hour = new Date().getHours();
  if (hour >= 6 && hour < 10) return "wood";
  if (hour >= 10 && hour < 14) return "fire";
  if (hour >= 14 && hour < 18) return "earth";
  if (hour >= 18 && hour < 22) return "metal";
  return "water";
}

export const useNexusStore = create<NexusState>((set) => ({
  panels: {
    left: { visible: true, width: 260 },
    right: { visible: true, width: 320 },
    bottom: { visible: true, height: 220 },
  },
  togglePanel: (panel) =>
    set((state) => ({
      panels: {
        ...state.panels,
        [panel]: { ...state.panels[panel], visible: !state.panels[panel].visible },
      },
    })),
  setPanelSize: (panel, size) =>
    set((state) => ({
      panels: {
        ...state.panels,
        [panel]: {
          ...state.panels[panel],
          ...(panel === "bottom" ? { height: size } : { width: size }),
        },
      },
    })),

  centerTab: "editor",
  setCenterTab: (tab) => set({ centerTab: tab }),
  leftTab: "explorer",
  setLeftTab: (tab) => set({ leftTab: tab }),
  rightTab: "chat",
  setRightTab: (tab) => set({ rightTab: tab }),
  bottomTab: "terminal",
  setBottomTab: (tab) => set({ bottomTab: tab }),

  wuXingPhase: detectWuXingPhase(),
  setWuXingPhase: (phase) => set({ wuXingPhase: phase }),

  status: {
    cpu: 0,
    memory: 0,
    memoryCount: 0,
    patternCount: 0,
    dharmaOk: true,
    apiConnected: false,
  },
  setStatus: (partial) =>
    set((state) => ({ status: { ...state.status, ...partial } })),
}));
