# Workflow: Deep Research

Multi-step research workflow combining memory search, knowledge graph, and synthesis.

## Steps

1. **Search memories** — find relevant prior knowledge
   ```
   gana_winnowing_basket(tool="search_memories", args={"query": "<topic>", "limit": 10})
   ```

2. **Semantic search** — find similar content by embedding
   ```
   gana_winnowing_basket(tool="vector.search", args={"query": "<topic>", "limit": 5})
   ```

3. **Walk knowledge graph** — explore connections
   ```
   gana_winnowing_basket(tool="graph_walk", args={"start_node": "<memory_id>", "max_depth": 3})
   ```

4. **Extract entities** — build knowledge graph from findings
   ```
   gana_chariot(tool="kg.extract", args={"text": "<combined findings>"})
   ```

5. **Store synthesis** — persist the research as a new memory
   ```
   gana_neck(tool="create_memory", args={"title": "Research: <topic>", "content": "<synthesis>", "tags": ["research"]})
   ```
