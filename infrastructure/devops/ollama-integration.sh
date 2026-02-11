#!/bin/bash
# Ollama Integration - Local LLM setup for distributed AI compute
# Based on Gemini's architecture: LLM on Inspiron, agent logic on main

set -e

echo "🤖 Ollama Integration - Local LLM Setup"
echo "========================================"
echo ""

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

HOSTNAME=$(hostname)
echo -e "${BLUE}Setting up on: $HOSTNAME${NC}"
echo ""

# Check if Ollama is already installed
if command -v ollama &> /dev/null; then
    echo -e "${GREEN}✓ Ollama already installed${NC}"
    OLLAMA_VERSION=$(ollama --version 2>&1 | head -1)
    echo "  Version: $OLLAMA_VERSION"
else
    echo -e "${YELLOW}Installing Ollama...${NC}"
    curl -fsSL https://ollama.com/install.sh | sh
    echo -e "${GREEN}✓ Ollama installed${NC}"
fi

echo ""

# Determine role
echo "Setup type:"
echo "1. Server (Inspiron - runs LLM models)"
echo "2. Client (Main laptop - sends requests)"
echo "3. Both (standalone setup)"
echo ""
read -p "Choice: " choice

case $choice in
    1|3)
        echo ""
        echo -e "${BLUE}Setting up as LLM SERVER${NC}"
        echo ""
        
        # Start Ollama service
        echo -e "${YELLOW}Starting Ollama service...${NC}"
        sudo systemctl enable ollama
        sudo systemctl start ollama
        
        sleep 2
        
        echo -e "${GREEN}✓ Ollama service running${NC}"
        echo ""
        
        # Pull recommended models
        echo "Recommended models for your hardware:"
        echo "1. TinyLlama (1.1B) - Fastest, ~637MB"
        echo "2. Phi-3 Mini (3.8B) - Balanced, ~2.3GB"
        echo "3. Llama 3.2 (3B) - Good quality, ~2GB"
        echo "4. Qwen2 (1.5B) - Fast, ~934MB"
        echo "5. Skip for now"
        echo ""
        read -p "Pull which model? (1-5): " model_choice
        
        case $model_choice in
            1)
                MODEL="tinyllama"
                echo -e "${YELLOW}Pulling TinyLlama...${NC}"
                ollama pull tinyllama
                ;;
            2)
                MODEL="phi3"
                echo -e "${YELLOW}Pulling Phi-3 Mini...${NC}"
                ollama pull phi3
                ;;
            3)
                MODEL="llama3.2"
                echo -e "${YELLOW}Pulling Llama 3.2...${NC}"
                ollama pull llama3.2
                ;;
            4)
                MODEL="qwen2:1.5b"
                echo -e "${YELLOW}Pulling Qwen2 1.5B...${NC}"
                ollama pull qwen2:1.5b
                ;;
            5)
                MODEL="none"
                echo "Skipping model download"
                ;;
        esac
        
        if [ "$MODEL" != "none" ]; then
            echo -e "${GREEN}✓ Model ready: $MODEL${NC}"
            echo ""
            
            # Test the model
            echo -e "${YELLOW}Testing model...${NC}"
            echo "Prompt: 'Write a hello world in Python'"
            echo ""
            ollama run $MODEL "Write a hello world in Python" --verbose
            echo ""
            echo -e "${GREEN}✓ Model working!${NC}"
        fi
        
        echo ""
        echo "Server is ready at: http://$(hostname -I | awk '{print $1}'):11434"
        echo ""
        
        # Create systemd service for API server
        cat > ~/.config/systemd/user/ollama-api.service << 'EOF'
[Unit]
Description=Ollama API Server
After=network.target

[Service]
Type=simple
Environment="OLLAMA_HOST=0.0.0.0:11434"
ExecStart=/usr/bin/ollama serve
Restart=always
RestartSec=3

[Install]
WantedBy=default.target
EOF
        
        systemctl --user daemon-reload
        echo -e "${GREEN}✓ API service configured${NC}"
        ;;
esac

if [ "$choice" = "2" ] || [ "$choice" = "3" ]; then
    echo ""
    echo -e "${BLUE}Setting up CLIENT integration${NC}"
    echo ""
    
    if [ "$choice" = "2" ]; then
        read -p "Enter Ollama server address (e.g., inspiron.local): " SERVER_HOST
    else
        SERVER_HOST="localhost"
    fi
    
    # Create Python client library
    cat > ~/SharedWorkspace/scripts/ollama_client.py << 'EOF'
#!/usr/bin/env python3
"""
Ollama Client - Async Interface for local LLM inference
Integrates with task_distributor for distributed AI compute
"""

import aiohttp
import asyncio
import json
import os
import sys
from typing import Optional, Dict, Any, List

class OllamaClient:
    """Async Client for Ollama API"""
    
    def __init__(self, host: str = "localhost", port: int = 11434):
        self.base_url = f"http://{host}:{port}"
        self.api_url = f"{self.base_url}/api"
        self.session = None
        
    async def __aenter__(self):
        self.session = aiohttp.ClientSession()
        return self
        
    async def __aexit__(self, exc_type, exc, tb):
        if self.session:
            await self.session.close()
            
    async def _ensure_session(self):
        if not self.session:
            self.session = aiohttp.ClientSession()
            
    async def list_models(self) -> List[Dict]:
        """List available models asynchronously"""
        await self._ensure_session()
        try:
            async with self.session.get(f"{self.api_url}/tags") as response:
                response.raise_for_status()
                data = await response.json()
                return data.get('models', [])
        except Exception as e:
            print(f"Error listing models: {e}")
            return []
            
    async def generate(self, model: str, prompt: str, stream: bool = False) -> Optional[str]:
        """Generate text asynchronously"""
        await self._ensure_session()
        try:
            data = {
                "model": model,
                "prompt": prompt,
                "stream": stream
            }
            
            if stream:
                full_response = ""
                async with self.session.post(f"{self.api_url}/generate", json=data) as response:
                    response.raise_for_status()
                    async for line in response.content:
                        if line:
                            chunk = json.loads(line)
                            if 'response' in chunk:
                                content = chunk['response']
                                full_response += content
                                print(content, end='', flush=True)
                print()
                return full_response
            else:
                async with self.session.post(f"{self.api_url}/generate", json=data) as response:
                    response.raise_for_status()
                    result = await response.json()
                    return result.get('response', '')
                    
        except Exception as e:
            print(f"Error generating: {e}")
            return None

    async def chat(self, model: str, messages: list) -> Optional[str]:
        """Chat with model asynchronously"""
        await self._ensure_session()
        try:
            data = {
                "model": model,
                "messages": messages,
                "stream": False
            }
            
            async with self.session.post(f"{self.api_url}/chat", json=data) as response:
                response.raise_for_status()
                result = await response.json()
                return result.get('message', {}).get('content', '')
                
        except Exception as e:
            print(f"Error in chat: {e}")
            return None
    
    async def analyze_code(self, model: str, code: str, task: str = "review") -> Optional[str]:
        """Analyze code with LLM"""
        prompts = {
            "review": f"Review this code for bugs, improvements, and best practices:\\n\\n{code}",
            "optimize": f"Suggest optimizations for this code:\\n\\n{code}",
            "explain": f"Explain what this code does:\\n\\n{code}",
            "refactor": f"Suggest refactoring improvements:\\n\\n{code}"
        }
        
        prompt = prompts.get(task, f"{task}:\\n\\n{code}")
        return await self.generate(model, prompt)


# CLI interface
if __name__ == '__main__':
    import sys
    
    if len(sys.argv) < 2:
        print(\"""
Ollama Client - Local LLM Interface (Async)

Usage:
    ollama_client.py list
    ollama_client.py generate <model> <prompt>
    ollama_client.py chat <model> <message>
    ollama_client.py analyze <model> <file> [task]

Examples:
    ollama_client.py list
    ollama_client.py generate tinyllama "Write hello world in Python"
    ollama_client.py analyze phi3 main.py review
\""")
        sys.exit(1)
    
    # Get server from environment or use default
    server = os.getenv('OLLAMA_SERVER', 'localhost')
    
    async def main():
        client = OllamaClient(host=server)
        async with client:
            command = sys.argv[1]
            
            if command == 'list':
                models = await client.list_models()
                print("Available models:")
                for model in models:
                    print(f"  - {model['name']} ({model['size'] / 1e9:.1f}GB)")
            
            elif command == 'generate':
                if len(sys.argv) < 4:
                    print("Usage: generate <model> <prompt>")
                    sys.exit(1)
                
                model = sys.argv[2]
                prompt = ' '.join(sys.argv[3:])
                
                print(f"Generating with {model}...")
                await client.generate(model, prompt, stream=True)
            
            elif command == 'chat':
                if len(sys.argv) < 4:
                    print("Usage: chat <model> <message>")
                    sys.exit(1)
                
                model = sys.argv[2]
                message = ' '.join(sys.argv[3:])
                
                messages = [{"role": "user", "content": message}]
                response = await client.chat(model, messages)
                print(response)
            
            elif command == 'analyze':
                if len(sys.argv) < 4:
                    print("Usage: analyze <model> <file> [task]")
                    sys.exit(1)
                
                model = sys.argv[2]
                filepath = sys.argv[3]
                task = sys.argv[4] if len(sys.argv) > 4 else "review"
                
                with open(filepath, 'r') as f:
                    code = f.read()
                
                print(f"Analyzing {filepath} with {model}...")
                response = await client.analyze_code(model, code, task)
                print(response)

    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        pass
    except Exception as e:
        print(f"Error: {e}")
EOF
    
    chmod +x ~/SharedWorkspace/scripts/ollama_client.py
    
    echo -e "${GREEN}✓ Client library created${NC}"
    echo "  Location: ~/SharedWorkspace/scripts/ollama_client.py"
    echo ""
    
    # Add to task_distributor
    echo -e "${YELLOW}Integrating with task_distributor...${NC}"
    
    # Add AI inference task type
    cat >> ~/SharedWorkspace/scripts/.ollama_task_profile << EOF
# Add this to task_distributor.py TASK_PROFILES:

'ai_inference': {
    'target': '${SERVER_HOST}',
    'priority': 'high',
    'timeout': 300
},
'code_review_ai': {
    'target': '${SERVER_HOST}',
    'priority': 'medium',
    'timeout': 180
},
'code_generation': {
    'target': '${SERVER_HOST}',
    'priority': 'high',
    'timeout': 240
}
EOF
    
    echo -e "${GREEN}✓ Task profiles created${NC}"
    echo "  Add profiles from: ~/SharedWorkspace/scripts/.ollama_task_profile"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo -e "${GREEN}Ollama Integration Complete!${NC}"
echo ""

if [ "$choice" = "1" ] || [ "$choice" = "3" ]; then
    echo "🖥️  Server Setup:"
    echo "  - API: http://$(hostname -I | awk '{print $1}'):11434"
    echo "  - Test: curl http://localhost:11434/api/tags"
    echo ""
    echo "Available commands:"
    echo "  ollama list                    # List models"
    echo "  ollama run $MODEL 'prompt'     # Run inference"
    echo "  ollama ps                      # Show running models"
fi

if [ "$choice" = "2" ] || [ "$choice" = "3" ]; then
    echo "💻 Client Setup:"
    echo "  export OLLAMA_SERVER=$SERVER_HOST"
    echo "  python3 ~/SharedWorkspace/scripts/ollama_client.py list"
    echo ""
    echo "Usage examples:"
    echo "  # List models"
    echo "  python3 ~/SharedWorkspace/scripts/ollama_client.py list"
    echo ""
    echo "  # Generate text"
    echo "  python3 ~/SharedWorkspace/scripts/ollama_client.py generate tinyllama 'Hello'"
    echo ""
    echo "  # Analyze code"
    echo "  python3 ~/SharedWorkspace/scripts/ollama_client.py analyze phi3 main.py review"
fi

echo ""
echo "🎯 Integration with dual-laptop system:"
echo "  - Inspiron: Runs LLM models (heavy compute)"
echo "  - Main: Sends requests, gets responses (light)"
echo "  - Use with task_distributor for automatic routing"
echo ""
echo "💡 Next steps:"
echo "  1. Test: python3 ~/SharedWorkspace/scripts/ollama_client.py list"
echo "  2. Try: distributed-ai-workflow.sh (now uses local LLM!)"
echo "  3. Integrate with ai-voting-system.sh"
