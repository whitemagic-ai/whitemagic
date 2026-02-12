# Workflow: New Session Setup

Start every conversation with this sequence to establish context.

## Steps

1. **Bootstrap session** — loads quickstart guides, recent memories, galaxy status
   ```
   gana_horn(tool="session_bootstrap", args={})
   ```

2. **Check system health** — verify all subsystems operational
   ```
   gana_root(tool="health_report", args={})
   ```

3. **Introspect** — get unified system snapshot
   ```
   gana_ghost(tool="gnosis", args={"compact": true})
   ```

4. **Surface serendipity** — discover unexpected connections
   ```
   gana_abundance(tool="serendipity_surface", args={"count": 3})
   ```
