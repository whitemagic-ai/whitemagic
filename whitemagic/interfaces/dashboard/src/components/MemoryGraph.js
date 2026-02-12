import React, { useEffect, useRef, useState } from 'react';
import * as d3 from 'd3';
import { Network } from 'lucide-react';

const MemoryGraph = ({ memories, selectedMemory, onMemorySelect, loading }) => {
  const svgRef = useRef(null);
  const [dimensions, setDimensions] = useState({ width: 800, height: 600 });

  useEffect(() => {
    const updateDimensions = () => {
      const container = svgRef.current?.parentElement;
      if (container) {
        setDimensions({
          width: container.clientWidth,
          height: 600
        });
      }
    };

    updateDimensions();
    window.addEventListener('resize', updateDimensions);
    return () => window.removeEventListener('resize', updateDimensions);
  }, []);

  useEffect(() => {
    if (!memories.length || loading) return;

    const svg = d3.select(svgRef.current);
    svg.selectAll('*').remove();

    // Create force simulation
    const simulation = d3.forceSimulation(memories)
      .force('link', d3.forceLink().id(d => d.id).distance(100))
      .force('charge', d3.forceManyBody().strength(-300))
      .force('center', d3.forceCenter(dimensions.width / 2, dimensions.height / 2))
      .force('collision', d3.forceCollide().radius(30));

    // Create links from relationships
    const links = [];
    memories.forEach(memory => {
      if (memory.related) {
        memory.related.forEach(relatedId => {
          const target = memories.find(m => m.id === relatedId);
          if (target) {
            links.push({ source: memory.id, target: relatedId });
          }
        });
      }
    });

    // Create scales
    const colorScale = d3.scaleOrdinal(d3.schemeCategory10);
    const sizeScale = d3.scaleLinear()
      .domain(d3.extent(memories, d => d.importance || 1))
      .range([5, 20]);

    // Create container
    const container = svg.append('g');

    // Add zoom behavior
    const zoom = d3.zoom()
      .scaleExtent([0.1, 4])
      .on('zoom', (event) => {
        container.attr('transform', event.transform);
      });
    svg.call(zoom);

    // Create links
    const link = container.append('g')
      .attr('class', 'links')
      .selectAll('line')
      .data(links)
      .enter().append('line')
      .attr('stroke', '#999')
      .attr('stroke-opacity', 0.6)
      .attr('stroke-width', 2);

    // Create nodes
    const node = container.append('g')
      .attr('class', 'nodes')
      .selectAll('g')
      .data(memories)
      .enter().append('g')
      .attr('class', 'node')
      .style('cursor', 'pointer')
      .call(d3.drag()
        .on('start', dragstarted)
        .on('drag', dragged)
        .on('end', dragended));

    // Add circles to nodes
    node.append('circle')
      .attr('r', d => sizeScale(d.importance || 1))
      .attr('fill', d => colorScale(d.type || 'default'))
      .attr('stroke', '#fff')
      .attr('stroke-width', 2)
      .on('click', (event, d) => {
        event.stopPropagation();
        onMemorySelect(d);
      });

    // Add labels to nodes
    node.append('text')
      .text(d => {
        const title = d.title || d.id;
        return title.length > 20 ? title.substring(0, 20) + '...' : title;
      })
      .attr('x', 0)
      .attr('y', d => sizeScale(d.importance || 1) + 15)
      .attr('text-anchor', 'middle')
      .attr('font-size', '12px')
      .attr('fill', '#333')
      .style('pointer-events', 'none');

    // Highlight selected node
    if (selectedMemory) {
      node.selectAll('circle')
        .attr('stroke', d => d.id === selectedMemory.id ? '#ff6b6b' : '#fff')
        .attr('stroke-width', d => d.id === selectedMemory.id ? 4 : 2);
    }

    // Update positions on tick
    simulation.on('tick', () => {
      link
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);

      node.attr('transform', d => `translate(${d.x},${d.y})`);
    });

    // Drag functions
    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(event, d) {
      d.fx = event.x;
      d.fy = event.y;
    }

    function dragended(event, d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }

    return () => {
      simulation.stop();
    };
  }, [memories, dimensions, selectedMemory, onMemorySelect, loading]);

  if (loading) {
    return (
      <div className="flex items-center justify-center h-96">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-purple-600"></div>
      </div>
    );
  }

  if (!memories.length) {
    return (
      <div className="flex flex-col items-center justify-center h-96 text-gray-500">
        <Network className="w-12 h-12 mb-4" />
        <p>No memories to display</p>
      </div>
    );
  }

  return (
    <div className="relative">
      <svg
        ref={svgRef}
        width={dimensions.width}
        height={dimensions.height}
        className="border border-gray-200 rounded-lg"
      />
      <div className="absolute bottom-4 left-4 bg-white bg-opacity-90 p-2 rounded text-xs text-gray-600">
        Drag to pan • Scroll to zoom • Click node for details
      </div>
    </div>
  );
};

export default MemoryGraph;
