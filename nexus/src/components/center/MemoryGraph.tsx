import { useEffect, useRef, useState, useCallback } from "react";
import { fetchMemories, type Memory } from "../../lib/api";
import { RefreshCw, ZoomIn, ZoomOut } from "lucide-react";

interface GraphNode {
  id: string;
  label: string;
  type: "memory" | "tag" | "topic" | "garden";
  x: number;
  y: number;
  vx: number;
  vy: number;
  size: number;
  color: string;
  data?: Memory;
}

interface GraphEdge {
  source: string;
  target: string;
}

const TYPE_COLORS: Record<string, string> = {
  topic: "#a855f7",
  tag: "#22c55e",
  memory: "#3b82f6",
  garden: "#f59e0b",
};

const GARDEN_NODES: { id: string; label: string }[] = [
  { id: "g-joy", label: "Joy" },
  { id: "g-love", label: "Love" },
  { id: "g-wisdom", label: "Wisdom" },
  { id: "g-truth", label: "Truth" },
  { id: "g-beauty", label: "Beauty" },
  { id: "g-dharma", label: "Dharma" },
  { id: "g-mystery", label: "Mystery" },
  { id: "g-play", label: "Play" },
];

function buildGraph(memories: Memory[]): {
  nodes: GraphNode[];
  edges: GraphEdge[];
} {
  const nodes: GraphNode[] = [];
  const edges: GraphEdge[] = [];
  const tagSet = new Set<string>();
  const topicSet = new Set<string>();

  // Add garden nodes
  GARDEN_NODES.forEach((g) => {
    nodes.push({
      id: g.id,
      label: g.label,
      type: "garden",
      x: 400 + (Math.random() - 0.5) * 300,
      y: 300 + (Math.random() - 0.5) * 200,
      vx: 0,
      vy: 0,
      size: 16,
      color: TYPE_COLORS.garden,
    });
  });

  // Process memories
  memories.forEach((mem) => {
    const nodeId = `m-${mem.id}`;
    nodes.push({
      id: nodeId,
      label: mem.title.length > 25 ? mem.title.slice(0, 25) + "..." : mem.title,
      type: "memory",
      x: 400 + (Math.random() - 0.5) * 500,
      y: 300 + (Math.random() - 0.5) * 400,
      vx: 0,
      vy: 0,
      size: 8 + Math.min(6, (mem.retention_score || 0.5) * 8),
      color: TYPE_COLORS.memory,
      data: mem,
    });

    // Tags → edges
    (mem.tags || []).forEach((tag) => {
      const tagId = `tag-${tag}`;
      if (!tagSet.has(tag)) {
        tagSet.add(tag);
        nodes.push({
          id: tagId,
          label: tag,
          type: "tag",
          x: 400 + (Math.random() - 0.5) * 400,
          y: 300 + (Math.random() - 0.5) * 300,
          vx: 0,
          vy: 0,
          size: 12,
          color: TYPE_COLORS.tag,
        });
      }
      edges.push({ source: nodeId, target: tagId });
    });
  });

  // Infer topics from tag clusters
  const topicMap: Record<string, string[]> = {
    "Memory Systems": ["memory", "recall", "storage", "consolidation", "galactic"],
    "AI Intelligence": ["ai", "inference", "model", "neural", "bicameral"],
    "Ethics & Dharma": ["dharma", "ethics", "karma", "boundary", "consent"],
    "Resonance": ["resonance", "ganying", "event", "temporal", "salience"],
  };

  Object.entries(topicMap).forEach(([topic, keywords]) => {
    const matchingTags = [...tagSet].filter((t) =>
      keywords.some((k) => t.toLowerCase().includes(k))
    );
    if (matchingTags.length > 0 || memories.length === 0) {
      const topicId = `topic-${topic.replace(/\s/g, "_")}`;
      if (!topicSet.has(topic)) {
        topicSet.add(topic);
        nodes.push({
          id: topicId,
          label: topic,
          type: "topic",
          x: 400 + (Math.random() - 0.5) * 200,
          y: 300 + (Math.random() - 0.5) * 150,
          vx: 0,
          vy: 0,
          size: 20,
          color: TYPE_COLORS.topic,
        });
      }
      matchingTags.forEach((t) => {
        edges.push({ source: topicId, target: `tag-${t}` });
      });
    }
  });

  // Connect gardens to topic nodes
  edges.push({ source: "g-dharma", target: "topic-Ethics_&_Dharma" });
  edges.push({ source: "g-wisdom", target: "topic-AI_Intelligence" });
  edges.push({ source: "g-joy", target: "g-love" });
  edges.push({ source: "g-love", target: "g-beauty" });
  edges.push({ source: "g-truth", target: "g-wisdom" });

  return { nodes, edges };
}

// Demo memories used when API is unavailable
const DEMO_MEMORIES: Memory[] = [
  { id: "1", title: "Session Bootstrap Config", content: "", type: "long_term", tags: ["session", "config", "memory"], retention_score: 0.9 },
  { id: "2", title: "Rust Bridge Setup", content: "", type: "long_term", tags: ["rust", "bridge", "performance"], retention_score: 0.85 },
  { id: "3", title: "Garden Synthesis Architecture", content: "", type: "long_term", tags: ["garden", "synthesis", "resonance"], retention_score: 0.92 },
  { id: "4", title: "Wu Xing Phase Detection", content: "", type: "short_term", tags: ["wu_xing", "temporal", "ai"], retention_score: 0.7 },
  { id: "5", title: "Dharma Rules Engine", content: "", type: "long_term", tags: ["dharma", "ethics", "boundary"], retention_score: 0.95 },
  { id: "6", title: "Tool Registry v11", content: "", type: "long_term", tags: ["tools", "registry", "ai"], retention_score: 0.88 },
  { id: "7", title: "Temporal Lane Config", content: "", type: "short_term", tags: ["temporal", "event", "resonance"], retention_score: 0.65 },
  { id: "8", title: "Karma Ledger Design", content: "", type: "long_term", tags: ["karma", "ethics", "dharma"], retention_score: 0.91 },
  { id: "9", title: "Circuit Breaker Thresholds", content: "", type: "short_term", tags: ["resilience", "tools", "boundary"], retention_score: 0.6 },
  { id: "10", title: "Galactic Map Migration", content: "", type: "long_term", tags: ["galactic", "memory", "storage"], retention_score: 0.93 },
  { id: "11", title: "Bicameral Reasoning Tests", content: "", type: "short_term", tags: ["bicameral", "ai", "inference"], retention_score: 0.55 },
  { id: "12", title: "Mindful Forgetting Signals", content: "", type: "long_term", tags: ["memory", "recall", "consolidation"], retention_score: 0.87 },
  { id: "13", title: "Homeostatic Loop Tuning", content: "", type: "short_term", tags: ["homeostasis", "harmony", "balance"], retention_score: 0.72 },
  { id: "14", title: "Maturity Gate Stages", content: "", type: "long_term", tags: ["maturity", "governance", "tools"], retention_score: 0.89 },
  { id: "15", title: "Pipeline Variable Engine", content: "", type: "short_term", tags: ["pipeline", "tools", "ai"], retention_score: 0.68 },
];

export default function MemoryGraph() {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const [nodes, setNodes] = useState<GraphNode[]>([]);
  const [edges, setEdges] = useState<GraphEdge[]>([]);
  const [hoveredNode, setHoveredNode] = useState<GraphNode | null>(null);
  const [selectedNode, setSelectedNode] = useState<GraphNode | null>(null);
  const [dimensions, setDimensions] = useState({ w: 800, h: 600 });
  const [zoom, setZoom] = useState(1);
  const [loading, setLoading] = useState(true);
  const animRef = useRef<number | undefined>(undefined);

  // Load data
  const loadData = useCallback(async () => {
    setLoading(true);
    let memories = await fetchMemories("", 50);
    if (memories.length === 0) memories = DEMO_MEMORIES;
    const graph = buildGraph(memories);
    setNodes(graph.nodes);
    setEdges(graph.edges);
    setLoading(false);
  }, []);

  useEffect(() => {
    loadData();
  }, [loadData]);

  // Resize observer
  useEffect(() => {
    const el = containerRef.current;
    if (!el) return;
    const obs = new ResizeObserver((entries) => {
      for (const entry of entries) {
        setDimensions({
          w: entry.contentRect.width,
          h: entry.contentRect.height,
        });
      }
    });
    obs.observe(el);
    return () => obs.disconnect();
  }, []);

  // Force simulation
  useEffect(() => {
    if (nodes.length === 0) return;
    const { w, h } = dimensions;
    const cx = w / 2;
    const cy = h / 2;

    const tick = () => {
      setNodes((prev) => {
        const next = prev.map((n) => ({ ...n }));
        for (let i = 0; i < next.length; i++) {
          const node = next[i];
          // Repulsion
          for (let j = i + 1; j < next.length; j++) {
            const other = next[j];
            const dx = node.x - other.x;
            const dy = node.y - other.y;
            const dist = Math.sqrt(dx * dx + dy * dy) || 1;
            const force = 800 / (dist * dist);
            const fx = (dx / dist) * force;
            const fy = (dy / dist) * force;
            node.vx += fx;
            node.vy += fy;
            other.vx -= fx;
            other.vy -= fy;
          }
          // Edge attraction
          for (const edge of edges) {
            if (edge.source === node.id || edge.target === node.id) {
              const otherId = edge.source === node.id ? edge.target : edge.source;
              const other = next.find((n) => n.id === otherId);
              if (other) {
                node.vx += (other.x - node.x) * 0.006;
                node.vy += (other.y - node.y) * 0.006;
              }
            }
          }
          // Center gravity
          node.vx += (cx - node.x) * 0.001;
          node.vy += (cy - node.y) * 0.001;
          // Damping
          node.vx *= 0.9;
          node.vy *= 0.9;
          // Update
          node.x = Math.max(40, Math.min(w - 40, node.x + node.vx));
          node.y = Math.max(40, Math.min(h - 40, node.y + node.vy));
        }
        return next;
      });
      animRef.current = requestAnimationFrame(tick);
    };
    animRef.current = requestAnimationFrame(tick);
    return () => {
      if (animRef.current) cancelAnimationFrame(animRef.current);
    };
  }, [nodes.length, edges, dimensions]);

  // Render
  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;
    const { w, h } = dimensions;
    ctx.clearRect(0, 0, w, h);
    ctx.save();
    ctx.scale(zoom, zoom);

    // Edges
    ctx.lineWidth = 0.8;
    for (const edge of edges) {
      const src = nodes.find((n) => n.id === edge.source);
      const tgt = nodes.find((n) => n.id === edge.target);
      if (src && tgt) {
        ctx.strokeStyle = "rgba(168, 85, 247, 0.12)";
        ctx.beginPath();
        ctx.moveTo(src.x, src.y);
        ctx.lineTo(tgt.x, tgt.y);
        ctx.stroke();
      }
    }

    // Nodes
    for (const node of nodes) {
      const isHovered = hoveredNode?.id === node.id;
      const isSelected = selectedNode?.id === node.id;
      const glow = isHovered || isSelected;

      if (glow) {
        ctx.shadowColor = node.color;
        ctx.shadowBlur = 20;
      }

      ctx.beginPath();
      ctx.arc(node.x, node.y, node.size * (isHovered ? 1.3 : 1), 0, Math.PI * 2);
      ctx.fillStyle = glow ? node.color : node.color + "cc";
      ctx.fill();

      if (isSelected) {
        ctx.strokeStyle = "#ffffff";
        ctx.lineWidth = 2;
        ctx.stroke();
      }

      ctx.shadowBlur = 0;

      // Labels for large nodes
      if (node.size >= 12 || glow) {
        ctx.fillStyle = "rgba(255,255,255,0.7)";
        ctx.font = `${glow ? 11 : 10}px Inter, system-ui`;
        ctx.textAlign = "center";
        ctx.fillText(node.label, node.x, node.y + node.size + 14);
      }
    }

    ctx.restore();
  }, [nodes, edges, dimensions, zoom, hoveredNode, selectedNode]);

  const handleMouseMove = (e: React.MouseEvent<HTMLCanvasElement>) => {
    const rect = canvasRef.current?.getBoundingClientRect();
    if (!rect) return;
    const x = (e.clientX - rect.left) / zoom;
    const y = (e.clientY - rect.top) / zoom;
    const hit = nodes.find((n) => {
      const dx = n.x - x;
      const dy = n.y - y;
      return Math.sqrt(dx * dx + dy * dy) < n.size + 6;
    });
    setHoveredNode(hit || null);
    if (canvasRef.current) {
      canvasRef.current.style.cursor = hit ? "pointer" : "crosshair";
    }
  };

  const handleClick = (e: React.MouseEvent<HTMLCanvasElement>) => {
    const rect = canvasRef.current?.getBoundingClientRect();
    if (!rect) return;
    const x = (e.clientX - rect.left) / zoom;
    const y = (e.clientY - rect.top) / zoom;
    const hit = nodes.find((n) => {
      const dx = n.x - x;
      const dy = n.y - y;
      return Math.sqrt(dx * dx + dy * dy) < n.size + 6;
    });
    setSelectedNode(hit || null);
  };

  return (
    <div ref={containerRef} className="h-full relative bg-black/20 overflow-hidden">
      {loading && (
        <div className="absolute inset-0 flex items-center justify-center bg-wm-bg/80 z-20">
          <div className="text-center">
            <div className="text-4xl mb-2 animate-pulse">🕸️</div>
            <div className="text-sm text-gray-400">Building memory graph...</div>
          </div>
        </div>
      )}

      <canvas
        ref={canvasRef}
        width={dimensions.w}
        height={dimensions.h}
        onMouseMove={handleMouseMove}
        onClick={handleClick}
      />

      {/* Hover tooltip */}
      {hoveredNode && (
        <div
          className="absolute bg-black/90 backdrop-blur-md text-white px-3 py-2 rounded-lg text-sm shadow-xl border border-white/10 pointer-events-none z-10 max-w-[220px]"
          style={{
            left: Math.min(hoveredNode.x * zoom + 20, dimensions.w - 240),
            top: hoveredNode.y * zoom - 10,
          }}
        >
          <div className="font-bold" style={{ color: hoveredNode.color }}>
            {hoveredNode.label}
          </div>
          <div className="text-gray-400 text-xs uppercase tracking-wider">
            {hoveredNode.type}
          </div>
          {hoveredNode.data && (
            <div className="text-gray-500 text-xs mt-1 border-t border-white/10 pt-1">
              {hoveredNode.data.type} &middot; {(hoveredNode.data.tags || []).join(", ")}
            </div>
          )}
        </div>
      )}

      {/* Selected node detail */}
      {selectedNode && (
        <div className="absolute top-4 right-4 w-64 glass-card p-4 z-10">
          <div className="flex items-center justify-between mb-2">
            <span className="font-bold text-sm" style={{ color: selectedNode.color }}>
              {selectedNode.label}
            </span>
            <button
              onClick={() => setSelectedNode(null)}
              className="text-gray-500 hover:text-white text-xs"
            >
              &times;
            </button>
          </div>
          <div className="text-xs text-gray-400 uppercase mb-2">{selectedNode.type}</div>
          {selectedNode.data && (
            <div className="space-y-1 text-xs">
              <div className="flex justify-between">
                <span className="text-gray-500">Type</span>
                <span className="text-gray-300">{selectedNode.data.type}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-500">Tags</span>
                <span className="text-gray-300">{(selectedNode.data.tags || []).join(", ")}</span>
              </div>
              {selectedNode.data.retention_score !== undefined && (
                <div className="flex justify-between">
                  <span className="text-gray-500">Retention</span>
                  <span className="text-gray-300">{(selectedNode.data.retention_score * 100).toFixed(0)}%</span>
                </div>
              )}
              {selectedNode.data.galactic_distance !== undefined && (
                <div className="flex justify-between">
                  <span className="text-gray-500">Galactic Dist</span>
                  <span className="text-gray-300">{selectedNode.data.galactic_distance.toFixed(2)}</span>
                </div>
              )}
            </div>
          )}
          {/* Connected nodes */}
          <div className="mt-2 border-t border-wm-border pt-2">
            <div className="text-[10px] text-gray-500 uppercase mb-1">Connections</div>
            <div className="flex flex-wrap gap-1">
              {edges
                .filter((e) => e.source === selectedNode.id || e.target === selectedNode.id)
                .map((e, i) => {
                  const otherId = e.source === selectedNode.id ? e.target : e.source;
                  const other = nodes.find((n) => n.id === otherId);
                  return other ? (
                    <span
                      key={i}
                      className="text-[10px] px-1.5 py-0.5 rounded-full bg-white/5 border border-white/10"
                      style={{ color: other.color }}
                    >
                      {other.label}
                    </span>
                  ) : null;
                })}
            </div>
          </div>
        </div>
      )}

      {/* Controls */}
      <div className="absolute bottom-4 right-4 flex gap-1 z-10">
        <button
          onClick={() => setZoom((z) => Math.min(2, z + 0.1))}
          className="p-2 glass-card hover:bg-white/10 transition"
        >
          <ZoomIn size={14} />
        </button>
        <button
          onClick={() => setZoom((z) => Math.max(0.5, z - 0.1))}
          className="p-2 glass-card hover:bg-white/10 transition"
        >
          <ZoomOut size={14} />
        </button>
        <button
          onClick={loadData}
          className="p-2 glass-card hover:bg-white/10 transition"
        >
          <RefreshCw size={14} />
        </button>
      </div>

      {/* Legend */}
      <div className="absolute bottom-4 left-4 flex gap-4 text-xs bg-black/40 backdrop-blur px-4 py-2 rounded-full border border-white/5 z-10">
        {Object.entries(TYPE_COLORS).map(([type, color]) => (
          <div key={type} className="flex items-center gap-2">
            <span
              className="w-2 h-2 rounded-full"
              style={{ backgroundColor: color, boxShadow: `0 0 6px ${color}` }}
            />
            <span className="text-gray-300 capitalize">{type}s</span>
          </div>
        ))}
      </div>

      {/* Title */}
      <div className="absolute top-4 left-4 text-sm font-semibold text-gray-400 flex items-center gap-2 z-10">
        <span className="text-lg">🕸️</span>
        Memory Graph
        <span className="text-xs text-gray-600">
          ({nodes.length} nodes, {edges.length} edges)
        </span>
      </div>
    </div>
  );
}
