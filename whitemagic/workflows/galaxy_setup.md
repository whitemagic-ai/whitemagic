# Workflow: Galaxy Setup

Create and populate a new galaxy (isolated memory namespace).

## Steps

1. **Create galaxy** — create the isolated namespace
   ```
   gana_void(tool="galaxy.create", args={"name": "<galaxy_name>"})
   ```

2. **Switch to galaxy** — make it active
   ```
   gana_void(tool="galaxy.switch", args={"name": "<galaxy_name>"})
   ```

3. **Ingest content** — bulk-import files into the galaxy
   ```
   gana_void(tool="galaxy.ingest", args={"name": "<galaxy_name>", "source_dir": "<path>"})
   ```

4. **Check status** — verify the galaxy is healthy
   ```
   gana_void(tool="galaxy.status", args={"name": "<galaxy_name>"})
   ```

5. **List galaxies** — see all available galaxies
   ```
   gana_void(tool="galaxy.list", args={})
   ```
