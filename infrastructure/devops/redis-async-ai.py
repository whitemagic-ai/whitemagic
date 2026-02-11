#!/usr/bin/env python3
"""
Redis Async AI Communication - Message broker for parallel AI collaboration
Optimized for high-concurrency AI agent coordination using redis.asyncio
"""

import asyncio
import json
import time
import sys
import signal
from datetime import datetime
from typing import Dict, List, Callable, Optional, Any
import redis.asyncio as redis

# Configure for speed
REDIS_HOST = 'localhost'
REDIS_PORT = 6379
POOL_SIZE = 50

class AIMessageBroker:
    """True Async Redis Message Broker"""
    
    def __init__(self, host=REDIS_HOST, port=REDIS_PORT):
        self.host = host
        self.port = port
        self.redis = None
        self.pubsub = None
        self.listeners = {}
        self.running = False
        self._connect_lock = asyncio.Lock()
        
    async def connect(self):
        """Establish async connection pool"""
        async with self._connect_lock:
            if not self.redis:
                pool = redis.ConnectionPool(
                    host=self.host, 
                    port=self.port, 
                    decode_responses=True,
                    max_connections=POOL_SIZE
                )
                self.redis = redis.Redis(connection_pool=pool)
                self.pubsub = self.redis.pubsub()

    async def disconnect(self):
        """Clean shutdown"""
        self.running = False
        if self.pubsub:
            await self.pubsub.close()
        if self.redis:
            await self.redis.close()
            
    async def publish(self, channel: str, message: Dict) -> str:
        """Publish message non-blocking"""
        if not self.redis:
            await self.connect()
            
        message['timestamp'] = datetime.now().isoformat()
        msg_id = f"{channel}_{time.time()}"
        message['id'] = msg_id
        
        # Parallelize publish and history storage
        serialized = json.dumps(message)
        await asyncio.gather(
            self.redis.publish(channel, serialized),
            self.redis.lpush(f"history:{channel}", serialized)
        )
        # Fire and forget trim
        asyncio.create_task(self.redis.ltrim(f"history:{channel}", 0, 99))
        
        return msg_id

    async def subscribe(self, channel: str, callback: Callable):
        """Subscribe with async callback"""
        if not self.redis:
            await self.connect()
            
        if channel not in self.listeners:
            self.listeners[channel] = []
            await self.pubsub.subscribe(channel)
            
        self.listeners[channel].append(callback)

    async def start_listening(self):
        """Main listener loop"""
        if not self.redis:
            await self.connect()
            
        self.running = True
        print(f"🔄 Broker listening on {self.host}:{self.port} (Async)")
        
        while self.running:
            try:
                message = await self.pubsub.get_message(ignore_subscribe_messages=True, timeout=0.1)
                if message:
                    channel = message['channel']
                    if channel in self.listeners:
                        data = json.loads(message['data'])
                        # Fan out to listeners concurrently
                        callbacks = [cb(data) for cb in self.listeners[channel]]
                        if callbacks:
                            await asyncio.gather(*callbacks, return_exceptions=True)
                else:
                    await asyncio.sleep(0.01) # Yield to event loop
            except asyncio.CancelledError:
                break
            except Exception as e:
                print(f"Error in listener loop: {e}")
                await asyncio.sleep(0.1)
    
    async def stop_listening(self):
        await self.disconnect()

class AIAgent:
    """Async AI Agent Base"""
    
    def __init__(self, name: str, broker: AIMessageBroker):
        self.name = name
        self.broker = broker
        self.tasks = []
        
    async def start(self):
        await self.broker.subscribe('tasks', self.handle_task)
        await self.broker.subscribe('results', self.handle_result)
        await self.broker.subscribe(f'agent:{self.name}', self.handle_direct_message)
        print(f"🤖 Agent {self.name} connected")

    async def handle_task(self, message: Dict):
        target = message.get('assigned_to')
        if target == 'all' or target == self.name:
            print(f"[{self.name}] Received task: {message.get('task')}")
            self.tasks.append(message)
            # Don't block processing of other messages
            asyncio.create_task(self.execute_task(message))

    async def handle_result(self, message: Dict):
        print(f"[{self.name}] Result from {message.get('from')}: {message.get('status')}")

    async def handle_direct_message(self, message: Dict):
        print(f"[{self.name}] DM: {message.get('content')}")

    async def execute_task(self, task: Dict):
        raise NotImplementedError

    async def publish_result(self, task_id: str, result: Dict):
        await self.broker.publish('results', {
            'task_id': task_id,
            'from': self.name,
            'result': result,
            'status': 'complete'
        })

class ClaudeAgent(AIAgent):
    """Claude AI agent (Async)"""
    
    async def execute_task(self, task: Dict):
        """Execute task using Claude"""
        prompt = task.get('prompt', '')
        
        try:
            process = await asyncio.create_subprocess_exec(
                'claude-cli', prompt,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await asyncio.wait_for(process.communicate(), timeout=300)
            
            await self.publish_result(task['id'], {
                'output': stdout.decode(),
                'success': process.returncode == 0
            })
        except Exception as e:
            await self.publish_result(task['id'], {
                'error': str(e),
                'success': False
            })

class OllamaAgent(AIAgent):
    """Ollama (local LLM) agent (Async)"""
    
    def __init__(self, name: str, broker: AIMessageBroker, model: str = 'phi3'):
        super().__init__(name, broker)
        self.model = model
    
    async def execute_task(self, task: Dict):
        """Execute task using Ollama"""
        prompt = task.get('prompt', '')
        
        try:
            process = await asyncio.create_subprocess_exec(
                'ollama', 'run', self.model, prompt,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await asyncio.wait_for(process.communicate(), timeout=300)
            
            await self.publish_result(task['id'], {
                'output': stdout.decode(),
                'model': self.model,
                'success': process.returncode == 0
            })
        except Exception as e:
            await self.publish_result(task['id'], {
                'error': str(e),
                'success': False
            })

class TaskCoordinator:
    """Coordinates tasks across multiple AI agents (Async)"""
    
    def __init__(self, broker: AIMessageBroker):
        self.broker = broker
        self.tasks = {}
        self.results = {}
        self._result_events = {}
        
    async def start(self):
        await self.broker.subscribe('results', self.collect_result)
        
    async def collect_result(self, message: Dict):
        """Collect result from agent"""
        task_id = message.get('task_id')
        if task_id in self.tasks:
            self.results[task_id] = message
            if task_id in self._result_events:
                self._result_events[task_id].set()
    
    async def distribute_task(self, task: str, agents: List[str] = None):
        """Distribute task to agents"""
        task_id = f"task_{int(time.time() * 1000)}"
        
        message = {
            'id': task_id,
            'task': task,
            'prompt': task,
            'assigned_to': agents[0] if agents and len(agents) == 1 else 'all'
        }
        
        self.tasks[task_id] = message
        self._result_events[task_id] = asyncio.Event()
        await self.broker.publish('tasks', message)
        
        return task_id
    
    async def wait_for_results(self, task_id: str, timeout: int = 60):
        """Wait for task results"""
        if task_id not in self._result_events:
            return None
            
        try:
            await asyncio.wait_for(self._result_events[task_id].wait(), timeout=timeout)
            return self.results.get(task_id)
        except asyncio.TimeoutError:
            return None
    
    async def parallel_tasks(self, tasks: List[str], agents: List[str] = None):
        """Execute multiple tasks in parallel"""
        task_ids = []
        for task in tasks:
            task_id = await self.distribute_task(task, agents)
            task_ids.append(task_id)
        
        # Wait for all results concurrently
        async def wait_one(tid):
            return tid, await self.wait_for_results(tid)
            
        results = await asyncio.gather(*[wait_one(tid) for tid in task_ids])
        
        return {tid: res for tid, res in results if res}

async def main():
    if len(sys.argv) < 2:
        print("""
Redis Async AI Communication (Optimized)

Usage:
    redis-async-ai.py broker              # Start message broker
    redis-async-ai.py agent <name> <type> # Start AI agent
    redis-async-ai.py task <description>  # Send task
    redis-async-ai.py parallel <file>     # Run parallel tasks from file
    redis-async-ai.py monitor             # Monitor messages
""")
        sys.exit(1)
    
    command = sys.argv[1]
    broker = AIMessageBroker()
    
    try:
        if command == 'broker':
            await broker.start_listening()
            
        elif command == 'agent':
            if len(sys.argv) < 4:
                print("Usage: agent <name> <type>")
                sys.exit(1)
            
            name = sys.argv[2]
            agent_type = sys.argv[3]
            
            # Start broker listener in background
            listener_task = asyncio.create_task(broker.start_listening())
            
            if agent_type == 'claude':
                agent = ClaudeAgent(name, broker)
            elif agent_type == 'ollama':
                model = sys.argv[4] if len(sys.argv) > 4 else 'phi3'
                agent = OllamaAgent(name, broker, model)
            else:
                print(f"Unknown agent type: {agent_type}")
                sys.exit(1)
                
            await agent.start()
            print("Press Ctrl+C to stop")
            # Keep alive
            await asyncio.Future()
            
        elif command == 'task':
            if len(sys.argv) < 3:
                print("Usage: task <description>")
                sys.exit(1)
            
            task = ' '.join(sys.argv[2:])
            
            # Start listener for coordination
            listener_task = asyncio.create_task(broker.start_listening())
            coordinator = TaskCoordinator(broker)
            await coordinator.start()
            
            print(f"📋 Sending task: {task}")
            task_id = await coordinator.distribute_task(task)
            
            print(f"Task ID: {task_id}")
            print("Waiting for results...")
            
            result = await coordinator.wait_for_results(task_id, timeout=120)
            
            if result:
                print("\n✓ Result received:")
                print(json.dumps(result, indent=2))
            else:
                print("\n✗ Timeout waiting for results")
            
            await broker.disconnect()
            
        elif command == 'parallel':
            if len(sys.argv) < 3:
                print("Usage: parallel <tasks_file>")
                sys.exit(1)
            
            tasks_file = sys.argv[2]
            with open(tasks_file, 'r') as f:
                tasks = [line.strip() for line in f if line.strip()]
            
            listener_task = asyncio.create_task(broker.start_listening())
            coordinator = TaskCoordinator(broker)
            await coordinator.start()
            
            print(f"📋 Running {len(tasks)} tasks in parallel...")
            results = await coordinator.parallel_tasks(tasks)
            
            print(f"\n✓ Completed {len(results)}/{len(tasks)} tasks")
            for task_id, result in results.items():
                print(f"\nTask {task_id}:")
                print(f"  Status: {result.get('status')}")
                print(f"  From: {result.get('from')}")
                
            await broker.disconnect()
            
        elif command == 'monitor':
            async def print_message(msg):
                print(f"[{msg.get('timestamp')}] {msg.get('from', 'unknown')}: {msg.get('task', msg.get('status', 'message'))}")
            
            await broker.subscribe('tasks', print_message)
            await broker.subscribe('results', print_message)
            await broker.start_listening()
            
    except KeyboardInterrupt:
        print("\nStopping...")
    finally:
        await broker.disconnect()

if __name__ == '__main__':
    asyncio.run(main())
