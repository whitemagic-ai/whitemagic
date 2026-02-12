/**
 * GalaxyVisualizer — D3 force-directed memory star cluster.
 *
 * Renders WhiteMagic memories as a galaxy: each memory is a star positioned
 * by its 5D holographic coordinates (projected to 2D), colored by zone,
 * and sized by importance. Associations are drawn as luminous edges.
 *
 * Zones → Colors:
 *   CORE       → #ffd700 (gold)
 *   INNER_RIM  → #00bfff (deep sky blue)
 *   MID_BAND   → #9370db (medium purple)
 *   OUTER_RIM  → #708090 (slate gray)
 *   FAR_EDGE   → #2f4f4f (dark slate)
 *
 * Uses d3-force for layout and vanilla SVG for rendering.
 */

import { useEffect, useRef, useState } from "react";
import * as d3 from "d3";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface MemoryNode extends d3.SimulationNodeDatum {
  id: string;
  title: string;
  zone: string;
  importance: number;
  x?: number;
  y?: number;
}

interface AssociationLink extends d3.SimulationLinkDatum<MemoryNode> {
  source: string | MemoryNode;
  target: string | MemoryNode;
  strength: number;
}

interface GalaxyData {
  memories: MemoryNode[];
  associations: AssociationLink[];
}

// ---------------------------------------------------------------------------
// Zone colors
// ---------------------------------------------------------------------------

const ZONE_COLORS: Record<string, string> = {
  CORE: "#ffd700",
  INNER_RIM: "#00bfff",
  MID_BAND: "#9370db",
  OUTER_RIM: "#708090",
  FAR_EDGE: "#2f4f4f",
};

const ZONE_GLOW: Record<string, string> = {
  CORE: "rgba(255, 215, 0, 0.6)",
  INNER_RIM: "rgba(0, 191, 255, 0.4)",
  MID_BAND: "rgba(147, 112, 219, 0.3)",
  OUTER_RIM: "rgba(112, 128, 144, 0.2)",
  FAR_EDGE: "rgba(47, 79, 79, 0.1)",
};

// ---------------------------------------------------------------------------
// Demo data (used when no MCP connection is available)
// ---------------------------------------------------------------------------

function generateDemoData(): GalaxyData {
  const zones = ["CORE", "INNER_RIM", "MID_BAND", "OUTER_RIM", "FAR_EDGE"];
  const titles = [
    "Project Architecture", "Authentication Flow", "Database Schema",
    "API Design", "Error Handling", "Caching Strategy", "Deploy Pipeline",
    "User Preferences", "Session Nov 21", "Dream Consolidation",
    "Dharma Rules v3", "Harmony Baseline", "Agent Trust Scores",
    "Grimoire Chapter 1", "Sangha Config", "Ollamsa Setup",
    "Memory Lifecycle", "Association Mining", "PRAT Routing",
    "Circuit Breaker State", "Karma Ledger Entry", "Galactic Map",
    "XYZWV Coordinates", "FTS5 Index", "Embedding Cache",
    "Retention Scoring", "Surprise Gate", "Hebbian Weights",
    "Knowledge Graph", "Narrative Arc",
  ];

  const memories: MemoryNode[] = titles.map((title, i) => ({
    id: `mem-${i}`,
    title,
    zone: zones[Math.floor(i / 6)] || "FAR_EDGE",
    importance: 0.3 + Math.random() * 0.7,
  }));

  const associations: AssociationLink[] = [];
  for (let i = 0; i < 40; i++) {
    const a = Math.floor(Math.random() * memories.length);
    let b = Math.floor(Math.random() * memories.length);
    while (b === a) b = Math.floor(Math.random() * memories.length);
    associations.push({
      source: memories[a].id,
      target: memories[b].id,
      strength: 0.2 + Math.random() * 0.8,
    });
  }

  return { memories, associations };
}

// ---------------------------------------------------------------------------
// Component
// ---------------------------------------------------------------------------

export default function GalaxyVisualizer() {
  const svgRef = useRef<SVGSVGElement>(null);
  const [selectedMemory, setSelectedMemory] = useState<MemoryNode | null>(null);
  const [data] = useState<GalaxyData>(() => generateDemoData());

  useEffect(() => {
    if (!svgRef.current) return;

    const svg = d3.select(svgRef.current);
    const width = svgRef.current.clientWidth || 800;
    const height = svgRef.current.clientHeight || 600;

    svg.selectAll("*").remove();

    // Background
    svg
      .append("rect")
      .attr("width", width)
      .attr("height", height)
      .attr("fill", "#0a0a1a");

    // Subtle radial gradient for galaxy center glow
    const defs = svg.append("defs");
    const gradient = defs
      .append("radialGradient")
      .attr("id", "galaxy-glow")
      .attr("cx", "50%")
      .attr("cy", "50%")
      .attr("r", "50%");
    gradient.append("stop").attr("offset", "0%").attr("stop-color", "rgba(255,215,0,0.08)");
    gradient.append("stop").attr("offset", "100%").attr("stop-color", "rgba(0,0,0,0)");

    svg
      .append("circle")
      .attr("cx", width / 2)
      .attr("cy", height / 2)
      .attr("r", Math.min(width, height) * 0.4)
      .attr("fill", "url(#galaxy-glow)");

    // Zone labels (concentric rings)
    const zones = ["CORE", "INNER_RIM", "MID_BAND", "OUTER_RIM", "FAR_EDGE"];
    zones.forEach((zone, i) => {
      const r = (i + 1) * Math.min(width, height) * 0.08;
      svg
        .append("circle")
        .attr("cx", width / 2)
        .attr("cy", height / 2)
        .attr("r", r)
        .attr("fill", "none")
        .attr("stroke", ZONE_COLORS[zone])
        .attr("stroke-width", 0.5)
        .attr("stroke-dasharray", "2,4")
        .attr("opacity", 0.3);
    });

    // Force simulation
    const simulation = d3
      .forceSimulation<MemoryNode>(data.memories)
      .force(
        "link",
        d3
          .forceLink<MemoryNode, AssociationLink>(data.associations)
          .id((d) => d.id)
          .distance(80)
          .strength((d) => (d as AssociationLink).strength * 0.3)
      )
      .force("charge", d3.forceManyBody().strength(-120))
      .force("center", d3.forceCenter(width / 2, height / 2))
      .force("collision", d3.forceCollide().radius(15));

    // Links (association edges)
    const links = svg
      .append("g")
      .selectAll("line")
      .data(data.associations)
      .join("line")
      .attr("stroke", "rgba(147, 112, 219, 0.15)")
      .attr("stroke-width", (d) => d.strength * 2);

    // Nodes (memory stars)
    const nodes = svg
      .append("g")
      .selectAll("circle")
      .data(data.memories)
      .join("circle")
      .attr("r", (d) => 3 + d.importance * 8)
      .attr("fill", (d) => ZONE_COLORS[d.zone] || "#666")
      .attr("stroke", (d) => ZONE_GLOW[d.zone] || "transparent")
      .attr("stroke-width", 2)
      .attr("cursor", "pointer")
      .attr("opacity", 0.85)
      .on("mouseover", function (_, d) {
        d3.select(this)
          .attr("opacity", 1)
          .attr("stroke-width", 4)
          .attr("r", 4 + d.importance * 10);
      })
      .on("mouseout", function (_, d) {
        d3.select(this)
          .attr("opacity", 0.85)
          .attr("stroke-width", 2)
          .attr("r", 3 + d.importance * 8);
      })
      .on("click", (_, d) => {
        setSelectedMemory(d);
      });

    // Drag behavior
    const drag = d3
      .drag<SVGCircleElement, MemoryNode>()
      .on("start", (event, d) => {
        if (!event.active) simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on("drag", (event, d) => {
        d.fx = event.x;
        d.fy = event.y;
      })
      .on("end", (event, d) => {
        if (!event.active) simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      });

    nodes.call(drag);

    // Title labels for important memories
    const labels = svg
      .append("g")
      .selectAll("text")
      .data(data.memories.filter((m) => m.importance > 0.7))
      .join("text")
      .text((d) => d.title.length > 20 ? d.title.slice(0, 20) + "…" : d.title)
      .attr("fill", "rgba(255,255,255,0.6)")
      .attr("font-size", "9px")
      .attr("font-family", "monospace")
      .attr("pointer-events", "none")
      .attr("dx", 12)
      .attr("dy", 3);

    // Tick
    simulation.on("tick", () => {
      links
        .attr("x1", (d) => (d.source as MemoryNode).x ?? 0)
        .attr("y1", (d) => (d.source as MemoryNode).y ?? 0)
        .attr("x2", (d) => (d.target as MemoryNode).x ?? 0)
        .attr("y2", (d) => (d.target as MemoryNode).y ?? 0);

      nodes.attr("cx", (d) => d.x ?? 0).attr("cy", (d) => d.y ?? 0);

      labels.attr("x", (d) => d.x ?? 0).attr("y", (d) => d.y ?? 0);
    });

    return () => {
      simulation.stop();
    };
  }, [data]);

  return (
    <div className="flex flex-col h-full bg-[#0a0a1a]">
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-2 border-b border-gray-800">
        <div className="flex items-center gap-2">
          <span className="text-lg">✦</span>
          <h2 className="text-sm font-semibold text-gray-200">Galaxy Visualizer</h2>
        </div>
        <div className="flex items-center gap-3 text-xs text-gray-500">
          {Object.entries(ZONE_COLORS).map(([zone, color]) => (
            <span key={zone} className="flex items-center gap-1">
              <span
                className="inline-block w-2 h-2 rounded-full"
                style={{ backgroundColor: color }}
              />
              {zone.replace("_", " ")}
            </span>
          ))}
        </div>
      </div>

      {/* SVG Canvas */}
      <svg
        ref={svgRef}
        className="flex-1 w-full"
        style={{ minHeight: 400 }}
      />

      {/* Selected Memory Detail */}
      {selectedMemory && (
        <div className="px-4 py-2 border-t border-gray-800 bg-[#111128]">
          <div className="flex items-center justify-between">
            <div>
              <span className="text-sm font-medium text-gray-200">
                {selectedMemory.title}
              </span>
              <span
                className="ml-2 px-1.5 py-0.5 rounded text-xs"
                style={{
                  backgroundColor: ZONE_COLORS[selectedMemory.zone] + "33",
                  color: ZONE_COLORS[selectedMemory.zone],
                }}
              >
                {selectedMemory.zone}
              </span>
            </div>
            <div className="text-xs text-gray-500">
              Importance: {(selectedMemory.importance * 100).toFixed(0)}%
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
