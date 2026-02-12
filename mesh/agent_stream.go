// Package mesh provides distributed agent coordination for WhiteMagic.
//
// agent_stream.go implements a streaming protocol for coordinating
// multiple WhiteMagic agents across a distributed mesh. Agents can
// register, send heartbeats, distribute tasks, and stream results.
//
// This uses Go's goroutines and channels for natural concurrent
// coordination — a pattern that maps directly to agent swarm behavior.
package mesh

import (
	"encoding/json"
	"fmt"
	"sync"
	"time"
)

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

// AgentCapability describes what an agent can do.
type AgentCapability struct {
	Name        string   `json:"name"`
	Description string   `json:"description"`
	ToolAccess  []string `json:"tool_access"` // Tools this agent can invoke
	MaxLoad     int      `json:"max_load"`    // Max concurrent tasks
}

// AgentInfo describes a registered agent.
type AgentInfo struct {
	ID           string            `json:"id"`
	Name         string            `json:"name"`
	Capabilities []AgentCapability `json:"capabilities"`
	Status       AgentStatus       `json:"status"`
	CurrentLoad  int               `json:"current_load"`
	MaxLoad      int               `json:"max_load"`
	LastHeartbeat time.Time        `json:"last_heartbeat"`
	RegisteredAt time.Time         `json:"registered_at"`
	TasksComplete uint64           `json:"tasks_complete"`
	TasksFailed   uint64           `json:"tasks_failed"`
	Metadata     map[string]string `json:"metadata"`
}

// AgentStatus represents the current status of an agent.
type AgentStatus string

const (
	AgentIdle      AgentStatus = "idle"
	AgentBusy      AgentStatus = "busy"
	AgentOverloaded AgentStatus = "overloaded"
	AgentOffline   AgentStatus = "offline"
	AgentDraining  AgentStatus = "draining" // Accepting no new tasks
)

// TaskRequest is a unit of work to be distributed to agents.
type TaskRequest struct {
	ID         string         `json:"id"`
	ToolName   string         `json:"tool_name"`
	Args       map[string]any `json:"args"`
	Priority   int            `json:"priority"` // Lower = higher priority
	RequiredCaps []string     `json:"required_capabilities"`
	TimeoutMs  int64          `json:"timeout_ms"`
	SubmittedAt time.Time     `json:"submitted_at"`
	AssignedTo string         `json:"assigned_to,omitempty"`
}

// TaskResult is the outcome of a distributed task.
type TaskResult struct {
	TaskID     string         `json:"task_id"`
	AgentID    string         `json:"agent_id"`
	Status     string         `json:"status"` // "completed", "failed", "timeout"
	Result     map[string]any `json:"result,omitempty"`
	Error      string         `json:"error,omitempty"`
	DurationMs int64          `json:"duration_ms"`
	CompletedAt time.Time     `json:"completed_at"`
}

// StreamEvent is an event on the agent coordination stream.
type StreamEvent struct {
	Type      string    `json:"type"`
	AgentID   string    `json:"agent_id"`
	Timestamp time.Time `json:"timestamp"`
	Data      any       `json:"data"`
}

// ---------------------------------------------------------------------------
// AgentRegistry
// ---------------------------------------------------------------------------

// AgentRegistry manages agent registration and coordination.
type AgentRegistry struct {
	mu         sync.RWMutex
	agents     map[string]*AgentInfo
	taskQueue  chan *TaskRequest
	resultChan chan *TaskResult
	eventChan  chan *StreamEvent
	heartbeatTimeout time.Duration
}

// NewAgentRegistry creates a new agent registry with the given queue size.
func NewAgentRegistry(queueSize int) *AgentRegistry {
	return &AgentRegistry{
		agents:     make(map[string]*AgentInfo),
		taskQueue:  make(chan *TaskRequest, queueSize),
		resultChan: make(chan *TaskResult, queueSize),
		eventChan:  make(chan *StreamEvent, queueSize*2),
		heartbeatTimeout: 30 * time.Second,
	}
}

// Register adds an agent to the registry.
func (r *AgentRegistry) Register(agent *AgentInfo) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	if _, exists := r.agents[agent.ID]; exists {
		return fmt.Errorf("agent %s already registered", agent.ID)
	}

	agent.RegisteredAt = time.Now()
	agent.LastHeartbeat = time.Now()
	agent.Status = AgentIdle
	r.agents[agent.ID] = agent

	r.emitEvent("agent_registered", agent.ID, agent)
	return nil
}

// Deregister removes an agent from the registry.
func (r *AgentRegistry) Deregister(agentID string) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	if _, exists := r.agents[agentID]; !exists {
		return fmt.Errorf("agent %s not found", agentID)
	}

	delete(r.agents, agentID)
	r.emitEvent("agent_deregistered", agentID, nil)
	return nil
}

// Heartbeat updates an agent's liveness.
func (r *AgentRegistry) Heartbeat(agentID string, currentLoad int) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	agent, exists := r.agents[agentID]
	if !exists {
		return fmt.Errorf("agent %s not found", agentID)
	}

	agent.LastHeartbeat = time.Now()
	agent.CurrentLoad = currentLoad

	if currentLoad >= agent.MaxLoad {
		agent.Status = AgentOverloaded
	} else if currentLoad > 0 {
		agent.Status = AgentBusy
	} else {
		agent.Status = AgentIdle
	}

	return nil
}

// SubmitTask adds a task to the distribution queue.
func (r *AgentRegistry) SubmitTask(task *TaskRequest) error {
	task.SubmittedAt = time.Now()
	select {
	case r.taskQueue <- task:
		r.emitEvent("task_submitted", "", task)
		return nil
	default:
		return fmt.Errorf("task queue full")
	}
}

// AssignTask finds the best agent for a task and assigns it.
func (r *AgentRegistry) AssignTask(task *TaskRequest) (*AgentInfo, error) {
	r.mu.Lock()
	defer r.mu.Unlock()

	var bestAgent *AgentInfo
	var bestScore float64 = -1

	for _, agent := range r.agents {
		if agent.Status == AgentOffline || agent.Status == AgentDraining {
			continue
		}
		if agent.CurrentLoad >= agent.MaxLoad {
			continue
		}

		// Check capabilities
		if !r.hasRequiredCaps(agent, task.RequiredCaps) {
			continue
		}

		// Score: prefer agents with lower load
		loadRatio := 1.0 - float64(agent.CurrentLoad)/float64(max(agent.MaxLoad, 1))
		// Bonus for agents that have completed more tasks (experienced)
		expBonus := float64(agent.TasksComplete) / float64(max(agent.TasksComplete+agent.TasksFailed, 1))
		score := loadRatio*0.7 + expBonus*0.3

		if score > bestScore {
			bestScore = score
			bestAgent = agent
		}
	}

	if bestAgent == nil {
		return nil, fmt.Errorf("no available agent with required capabilities")
	}

	bestAgent.CurrentLoad++
	if bestAgent.CurrentLoad >= bestAgent.MaxLoad {
		bestAgent.Status = AgentOverloaded
	} else {
		bestAgent.Status = AgentBusy
	}

	task.AssignedTo = bestAgent.ID
	r.emitEvent("task_assigned", bestAgent.ID, task)

	return bestAgent, nil
}

// CompleteTask records a task completion.
func (r *AgentRegistry) CompleteTask(result *TaskResult) {
	r.mu.Lock()
	defer r.mu.Unlock()

	if agent, exists := r.agents[result.AgentID]; exists {
		agent.CurrentLoad = max(0, agent.CurrentLoad-1)
		if result.Status == "completed" {
			agent.TasksComplete++
		} else {
			agent.TasksFailed++
		}
		if agent.CurrentLoad == 0 {
			agent.Status = AgentIdle
		} else {
			agent.Status = AgentBusy
		}
	}

	result.CompletedAt = time.Now()

	select {
	case r.resultChan <- result:
	default:
		// Drop if channel full
	}

	r.emitEvent("task_completed", result.AgentID, result)
}

// ListAgents returns all registered agents.
func (r *AgentRegistry) ListAgents() []*AgentInfo {
	r.mu.RLock()
	defer r.mu.RUnlock()

	agents := make([]*AgentInfo, 0, len(r.agents))
	for _, agent := range r.agents {
		agents = append(agents, agent)
	}
	return agents
}

// GetAgent returns info about a specific agent.
func (r *AgentRegistry) GetAgent(id string) (*AgentInfo, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	agent, ok := r.agents[id]
	return agent, ok
}

// PruneOffline marks agents that haven't sent a heartbeat as offline.
func (r *AgentRegistry) PruneOffline() int {
	r.mu.Lock()
	defer r.mu.Unlock()

	pruned := 0
	cutoff := time.Now().Add(-r.heartbeatTimeout)
	for _, agent := range r.agents {
		if agent.LastHeartbeat.Before(cutoff) && agent.Status != AgentOffline {
			agent.Status = AgentOffline
			pruned++
			r.emitEvent("agent_offline", agent.ID, nil)
		}
	}
	return pruned
}

// Stats returns aggregate registry statistics.
func (r *AgentRegistry) Stats() map[string]any {
	r.mu.RLock()
	defer r.mu.RUnlock()

	total := len(r.agents)
	idle, busy, offline := 0, 0, 0
	var totalComplete, totalFailed uint64

	for _, agent := range r.agents {
		switch agent.Status {
		case AgentIdle:
			idle++
		case AgentBusy, AgentOverloaded:
			busy++
		case AgentOffline:
			offline++
		}
		totalComplete += agent.TasksComplete
		totalFailed += agent.TasksFailed
	}

	return map[string]any{
		"total_agents":   total,
		"idle":           idle,
		"busy":           busy,
		"offline":        offline,
		"tasks_complete": totalComplete,
		"tasks_failed":   totalFailed,
		"queue_depth":    len(r.taskQueue),
	}
}

// EventStream returns the event channel for streaming events.
func (r *AgentRegistry) EventStream() <-chan *StreamEvent {
	return r.eventChan
}

// ToJSON serializes registry state to JSON.
func (r *AgentRegistry) ToJSON() (string, error) {
	data := r.Stats()
	data["agents"] = r.ListAgents()
	bytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

// ---------------------------------------------------------------------------
// Internal
// ---------------------------------------------------------------------------

func (r *AgentRegistry) hasRequiredCaps(agent *AgentInfo, required []string) bool {
	if len(required) == 0 {
		return true
	}
	capNames := make(map[string]bool)
	for _, cap := range agent.Capabilities {
		capNames[cap.Name] = true
	}
	for _, req := range required {
		if !capNames[req] {
			return false
		}
	}
	return true
}

func (r *AgentRegistry) emitEvent(eventType, agentID string, data any) {
	event := &StreamEvent{
		Type:      eventType,
		AgentID:   agentID,
		Timestamp: time.Now(),
		Data:      data,
	}
	select {
	case r.eventChan <- event:
	default:
		// Drop if channel full (non-blocking)
	}
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
