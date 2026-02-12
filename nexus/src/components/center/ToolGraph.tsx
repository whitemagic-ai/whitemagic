import { useEffect, useRef, useState } from "react";

interface ToolNode {
  id: string;
  category: string;
  x: number;
  y: number;
  vx: number;
  vy: number;
}

interface ToolEdge {
  source: string;
  target: string;
  type: "requires" | "suggests" | "provides";
}

const CATEGORY_COLORS: Record<string, string> = {
  MEMORY: "#a855f7",
  SESSION: "#3b82f6",
  GARDEN: "#22c55e",
  METRICS: "#f59e0b",
  ARCHAEOLOGY: "#8b5cf6",
  GOVERNOR: "#ef4444",
  SYSTEM: "#6366f1",
  DHARMA: "#10b981",
  BALANCE: "#ec4899",
  INTROSPECTION: "#06b6d4",
  AGENT: "#f97316",
  PIPELINE: "#14b8a6",
};

const TOOLS: { id: string; category: string }[] = [
  { id: "create_memory", category: "MEMORY" },
  { id: "search_memories", category: "MEMORY" },
  { id: "read_memory", category: "MEMORY" },
  { id: "list_memories", category: "MEMORY" },
  { id: "update_memory", category: "MEMORY" },
  { id: "create_session", category: "SESSION" },
  { id: "session_bootstrap", category: "SESSION" },
  { id: "scratchpad_create", category: "SESSION" },
  { id: "edge_infer", category: "SYSTEM" },
  { id: "harmony_vector", category: "BALANCE" },
  { id: "garden_activate", category: "GARDEN" },
  { id: "garden_status", category: "GARDEN" },
  { id: "garden_health", category: "GARDEN" },
  { id: "archaeology_mark_read", category: "ARCHAEOLOGY" },
  { id: "archaeology_find_unread", category: "ARCHAEOLOGY" },
  { id: "governor_validate", category: "GOVERNOR" },
  { id: "governor_set_goal", category: "GOVERNOR" },
  { id: "evaluate_ethics", category: "DHARMA" },
  { id: "dharma_rules", category: "DHARMA" },
  { id: "karma_report", category: "DHARMA" },
  { id: "gnosis", category: "INTROSPECTION" },
  { id: "kaizen_analyze", category: "INTROSPECTION" },
  { id: "execute_cascade", category: "SYSTEM" },
  { id: "rust_similarity", category: "SYSTEM" },
  { id: "sangha_chat_send", category: "AGENT" },
  { id: "agent.register", category: "AGENT" },
  { id: "pipeline.create", category: "PIPELINE" },
  { id: "pipeline.status", category: "PIPELINE" },
  { id: "tool.graph", category: "INTROSPECTION" },
  { id: "homeostasis.check", category: "BALANCE" },
  { id: "maturity.assess", category: "GOVERNOR" },
  { id: "memory.consolidate", category: "MEMORY" },
  { id: "memory.lifecycle_sweep", category: "MEMORY" },
];

const EDGES: ToolEdge[] = [
  { source: "create_memory", target: "search_memories", type: "suggests" },
  { source: "search_memories", target: "read_memory", type: "suggests" },
  { source: "create_session", target: "session_bootstrap", type: "requires" },
  { source: "session_bootstrap", target: "create_memory", type: "provides" },
  { source: "garden_activate", target: "garden_status", type: "suggests" },
  { source: "garden_activate", target: "harmony_vector", type: "provides" },
  { source: "evaluate_ethics", target: "dharma_rules", type: "requires" },
  { source: "evaluate_ethics", target: "karma_report", type: "suggests" },
  { source: "governor_validate", target: "evaluate_ethics", type: "requires" },
  { source: "governor_set_goal", target: "governor_validate", type: "suggests" },
  { source: "gnosis", target: "harmony_vector", type: "requires" },
  { source: "gnosis", target: "dharma_rules", type: "requires" },
  { source: "gnosis", target: "homeostasis.check", type: "requires" },
  { source: "gnosis", target: "maturity.assess", type: "requires" },
  { source: "execute_cascade", target: "governor_validate", type: "requires" },
  { source: "kaizen_analyze", target: "search_memories", type: "requires" },
  { source: "pipeline.create", target: "pipeline.status", type: "suggests" },
  { source: "agent.register", target: "sangha_chat_send", type: "suggests" },
  { source: "memory.consolidate", target: "search_memories", type: "requires" },
  { source: "memory.lifecycle_sweep", target: "memory.consolidate", type: "suggests" },
  { source: "homeostasis.check", target: "harmony_vector", type: "requires" },
  { source: "maturity.assess", target: "governor_validate", type: "suggests" },
  { source: "tool.graph", target: "gnosis", type: "suggests" },
];

const EDGE_STYLES: Record<string, { color: string; dash: number[] }> = {
  requires: { color: "rgba(239, 68, 68, 0.3)", dash: [] },
  suggests: { color: "rgba(168, 85, 247, 0.2)", dash: [4, 4] },
  provides: { color: "rgba(34, 197, 94, 0.25)", dash: [2, 3] },
};

export default function ToolGraph() {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const [nodes, setNodes] = useState<ToolNode[]>([]);
  const [hoveredNode, setHoveredNode] = useState<ToolNode | null>(null);
  const [dims, setDims] = useState({ w: 800, h: 600 });
  const animRef = useRef<number | undefined>(undefined);

  // Initialize nodes with category clustering
  useEffect(() => {
    const categories = [...new Set(TOOLS.map((t) => t.category))];
    const catAngles: Record<string, number> = {};
    categories.forEach((c, i) => {
      catAngles[c] = (i / categories.length) * Math.PI * 2;
    });

    setNodes(
      TOOLS.map((tool) => {
        const angle = catAngles[tool.category] + (Math.random() - 0.5) * 0.8;
        const dist = 120 + Math.random() * 80;
        return {
          id: tool.id,
          category: tool.category,
          x: dims.w / 2 + Math.cos(angle) * dist,
          y: dims.h / 2 + Math.sin(angle) * dist,
          vx: 0,
          vy: 0,
        };
      })
    );
  }, [dims]);

  // Resize
  useEffect(() => {
    const el = containerRef.current;
    if (!el) return;
    const obs = new ResizeObserver((entries) => {
      for (const entry of entries) {
        setDims({ w: entry.contentRect.width, h: entry.contentRect.height });
      }
    });
    obs.observe(el);
    return () => obs.disconnect();
  }, []);

  // Physics
  useEffect(() => {
    if (nodes.length === 0) return;
    const { w, h } = dims;

    const tick = () => {
      setNodes((prev) => {
        const next = prev.map((n) => ({ ...n }));
        for (let i = 0; i < next.length; i++) {
          const node = next[i];
          for (let j = i + 1; j < next.length; j++) {
            const other = next[j];
            const dx = node.x - other.x;
            const dy = node.y - other.y;
            const dist = Math.sqrt(dx * dx + dy * dy) || 1;
            const sameCategory = node.category === other.category;
            const repulsion = sameCategory ? 400 : 600;
            const force = repulsion / (dist * dist);
            const fx = (dx / dist) * force;
            const fy = (dy / dist) * force;
            node.vx += fx;
            node.vy += fy;
            other.vx -= fx;
            other.vy -= fy;
          }
          for (const edge of EDGES) {
            if (edge.source === node.id || edge.target === node.id) {
              const otherId = edge.source === node.id ? edge.target : edge.source;
              const other = next.find((n) => n.id === otherId);
              if (other) {
                const strength = edge.type === "requires" ? 0.01 : 0.005;
                node.vx += (other.x - node.x) * strength;
                node.vy += (other.y - node.y) * strength;
              }
            }
          }
          node.vx += (w / 2 - node.x) * 0.001;
          node.vy += (h / 2 - node.y) * 0.001;
          node.vx *= 0.88;
          node.vy *= 0.88;
          node.x = Math.max(50, Math.min(w - 50, node.x + node.vx));
          node.y = Math.max(50, Math.min(h - 50, node.y + node.vy));
        }
        return next;
      });
      animRef.current = requestAnimationFrame(tick);
    };
    animRef.current = requestAnimationFrame(tick);
    return () => {
      if (animRef.current) cancelAnimationFrame(animRef.current);
    };
  }, [nodes.length, dims]);

  // Render
  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;
    ctx.clearRect(0, 0, dims.w, dims.h);

    // Edges
    for (const edge of EDGES) {
      const src = nodes.find((n) => n.id === edge.source);
      const tgt = nodes.find((n) => n.id === edge.target);
      if (src && tgt) {
        const style = EDGE_STYLES[edge.type];
        ctx.strokeStyle = style.color;
        ctx.setLineDash(style.dash);
        ctx.lineWidth = edge.type === "requires" ? 1.5 : 1;
        ctx.beginPath();
        ctx.moveTo(src.x, src.y);
        ctx.lineTo(tgt.x, tgt.y);
        ctx.stroke();
        ctx.setLineDash([]);

        // Arrow
        const angle = Math.atan2(tgt.y - src.y, tgt.x - src.x);
        const len = 6;
        const mx = (src.x + tgt.x) / 2;
        const my = (src.y + tgt.y) / 2;
        ctx.fillStyle = style.color;
        ctx.beginPath();
        ctx.moveTo(mx + len * Math.cos(angle), my + len * Math.sin(angle));
        ctx.lineTo(
          mx + len * Math.cos(angle + 2.5),
          my + len * Math.sin(angle + 2.5)
        );
        ctx.lineTo(
          mx + len * Math.cos(angle - 2.5),
          my + len * Math.sin(angle - 2.5)
        );
        ctx.fill();
      }
    }

    // Nodes
    for (const node of nodes) {
      const color = CATEGORY_COLORS[node.category] || "#6b7280";
      const isHovered = hoveredNode?.id === node.id;
      const r = isHovered ? 8 : 6;

      ctx.shadowColor = color;
      ctx.shadowBlur = isHovered ? 16 : 8;
      ctx.beginPath();
      ctx.arc(node.x, node.y, r, 0, Math.PI * 2);
      ctx.fillStyle = color;
      ctx.fill();
      ctx.shadowBlur = 0;

      if (isHovered) {
        ctx.strokeStyle = "#fff";
        ctx.lineWidth = 1.5;
        ctx.stroke();
      }

      // Label
      ctx.fillStyle = "rgba(255,255,255,0.5)";
      ctx.font = `${isHovered ? 10 : 9}px 'JetBrains Mono', monospace`;
      ctx.textAlign = "center";
      ctx.fillText(node.id, node.x, node.y + r + 12);
    }
  }, [nodes, dims, hoveredNode]);

  const handleMouseMove = (e: React.MouseEvent<HTMLCanvasElement>) => {
    const rect = canvasRef.current?.getBoundingClientRect();
    if (!rect) return;
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    const hit = nodes.find((n) => {
      const dx = n.x - x;
      const dy = n.y - y;
      return Math.sqrt(dx * dx + dy * dy) < 12;
    });
    setHoveredNode(hit || null);
  };

  return (
    <div ref={containerRef} className="h-full relative bg-black/20 overflow-hidden">
      <canvas
        ref={canvasRef}
        width={dims.w}
        height={dims.h}
        onMouseMove={handleMouseMove}
        className="cursor-crosshair"
      />

      {hoveredNode && (
        <div
          className="absolute bg-black/90 backdrop-blur-md text-white px-3 py-2 rounded-lg text-xs shadow-xl border border-white/10 pointer-events-none z-10"
          style={{
            left: Math.min(hoveredNode.x + 15, dims.w - 200),
            top: hoveredNode.y - 10,
          }}
        >
          <div className="font-bold font-mono" style={{ color: CATEGORY_COLORS[hoveredNode.category] }}>
            {hoveredNode.id}
          </div>
          <div className="text-gray-400 text-[10px]">{hoveredNode.category}</div>
          <div className="text-gray-500 text-[10px] mt-1">
            {EDGES.filter((e) => e.source === hoveredNode.id).length} outgoing,{" "}
            {EDGES.filter((e) => e.target === hoveredNode.id).length} incoming
          </div>
        </div>
      )}

      {/* Legend */}
      <div className="absolute bottom-4 left-4 flex flex-wrap gap-3 text-[10px] bg-black/40 backdrop-blur px-4 py-2 rounded-lg border border-white/5 z-10 max-w-[400px]">
        {Object.entries(CATEGORY_COLORS).map(([cat, color]) => (
          <div key={cat} className="flex items-center gap-1.5">
            <span className="w-2 h-2 rounded-full" style={{ backgroundColor: color }} />
            <span className="text-gray-400">{cat}</span>
          </div>
        ))}
      </div>

      {/* Edge type legend */}
      <div className="absolute bottom-4 right-4 flex gap-4 text-[10px] bg-black/40 backdrop-blur px-4 py-2 rounded-lg border border-white/5 z-10">
        <div className="flex items-center gap-1.5">
          <span className="w-4 h-0.5 bg-red-500/60" />
          <span className="text-gray-400">requires</span>
        </div>
        <div className="flex items-center gap-1.5">
          <span className="w-4 h-0.5 bg-purple-500/40 border-t border-dashed border-purple-500/40" />
          <span className="text-gray-400">suggests</span>
        </div>
        <div className="flex items-center gap-1.5">
          <span className="w-4 h-0.5 bg-green-500/50" />
          <span className="text-gray-400">provides</span>
        </div>
      </div>

      {/* Title */}
      <div className="absolute top-4 left-4 text-sm font-semibold text-gray-400 flex items-center gap-2 z-10">
        <span className="text-lg">🔗</span>
        Tool Dependency Graph
        <span className="text-xs text-gray-600">
          ({TOOLS.length} tools, {EDGES.length} edges)
        </span>
      </div>
    </div>
  );
}
