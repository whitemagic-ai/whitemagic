import type { WuXingPhase } from "../store/nexus";

export const WU_XING_META: Record<
  WuXingPhase,
  { emoji: string; label: string; color: string; season: string; advice: string }
> = {
  wood: {
    emoji: "\u{1F331}",
    label: "Wood",
    color: "#4ade80",
    season: "Spring",
    advice: "Explore freely, gather knowledge, expand understanding.",
  },
  fire: {
    emoji: "\u{1F525}",
    label: "Fire",
    color: "#f97316",
    season: "Summer",
    advice: "Execute rapidly, maximize action, minimize overhead.",
  },
  earth: {
    emoji: "\u{1F30D}",
    label: "Earth",
    color: "#eab308",
    season: "Late Summer",
    advice: "Consolidate learnings, integrate systems, document thoroughly.",
  },
  metal: {
    emoji: "\u{2699}\u{FE0F}",
    label: "Metal",
    color: "#94a3b8",
    season: "Autumn",
    advice: "Refine boundaries, optimize quality, enforce principles.",
  },
  water: {
    emoji: "\u{1F4A7}",
    label: "Water",
    color: "#38bdf8",
    season: "Winter",
    advice: "Reflect deeply, adapt to feedback, flow with changes.",
  },
};

export const MIN_PANEL_SIZE = 150;
export const MAX_LEFT_WIDTH = 400;
export const MAX_RIGHT_WIDTH = 500;
export const MAX_BOTTOM_HEIGHT = 400;

export const WHITEMAGIC_API_URL =
  import.meta.env.VITE_WM_API_URL || "http://localhost:8000";
