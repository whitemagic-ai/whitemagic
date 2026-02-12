# Workflow: Memory Maintenance

Periodic maintenance to keep the Data Sea healthy.

## Steps

1. **Run lifecycle sweep** — decay old memories, promote active ones
   ```
   gana_ghost(tool="memory.lifecycle", args={"operation": "sweep"})
   ```

2. **Check retention stats** — see zone distribution
   ```
   gana_ghost(tool="memory.lifecycle", args={"operation": "stats"})
   ```

3. **Detect constellations** — find emerging clusters
   ```
   gana_extended_net(tool="cluster_stats", args={})
   ```

4. **Surface patterns** — find cross-cutting themes
   ```
   gana_extended_net(tool="pattern_search", args={"query": "recent"})
   ```

5. **Check health** — verify everything is clean
   ```
   gana_root(tool="health_report", args={})
   ```
