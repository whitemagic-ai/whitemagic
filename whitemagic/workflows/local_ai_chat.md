# Workflow: Local AI Chat

Use WhiteMagic's Ollama integration for privacy-first local AI reasoning.

## Steps

1. **Check available models** — see what's installed locally
   ```
   gana_roof(tool="ollama.models", args={})
   ```

2. **Start agent loop** — agentic conversation with tool access
   ```
   gana_roof(tool="ollama.agent", args={"prompt": "<question>", "model": "llama3.2"})
   ```

3. **Direct chat** — simple completion without tool access
   ```
   gana_roof(tool="ollama.chat", args={"messages": [{"role": "user", "content": "<question>"}]})
   ```

4. **Store insights** — save any interesting findings
   ```
   gana_neck(tool="create_memory", args={"title": "<insight>", "content": "<details>", "tags": ["local_ai"]})
   ```
