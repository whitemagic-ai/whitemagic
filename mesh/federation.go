// Package mesh provides federated galaxy support for WhiteMagic.
//
// federation.go implements the Federated Galaxy Protocol — allowing
// WhiteMagic nodes to mount remote galaxies as read-only memory sources
// via the libp2p mesh network.
//
// Architecture:
//   - Each node advertises its available galaxies via gossip ANNOUNCE
//   - Peers can REQUEST specific memories from a remote galaxy
//   - Responses are streamed back as protobuf HolographicSignals
//   - Local node caches remote results with TTL (no DB merge needed)
//   - All federation traffic inherits mesh encryption (Noise + optional PSK)
//
// Use cases:
//   - Team of devs sharing a "Project Galaxy" while keeping personal galaxies local
//   - Multi-device sync: laptop mounts desktop's galaxy as read-only
//   - Hive mind: multiple agents query a shared knowledge base without DB merge
//
// Protocol messages:
//   GALAXY_ADVERTISE  — "I have galaxies: [personal, project-x, research]"
//   GALAXY_QUERY      — "Search galaxy 'project-x' for 'authentication flow'"
//   GALAXY_RESPONSE   — "Here are 5 matching memories (read-only snapshots)"
//   GALAXY_MOUNT      — "I want to subscribe to updates from 'project-x'"
//   GALAXY_UNMOUNT    — "Stop sending me updates from 'project-x'"
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

// FederationMessageType identifies federation protocol messages.
type FederationMessageType string

const (
	MsgGalaxyAdvertise FederationMessageType = "GALAXY_ADVERTISE"
	MsgGalaxyQuery     FederationMessageType = "GALAXY_QUERY"
	MsgGalaxyResponse  FederationMessageType = "GALAXY_RESPONSE"
	MsgGalaxyMount     FederationMessageType = "GALAXY_MOUNT"
	MsgGalaxyUnmount   FederationMessageType = "GALAXY_UNMOUNT"
)

// GalaxyInfo describes an available galaxy on a node.
type GalaxyInfo struct {
	Name         string `json:"name"`
	MemoryCount  int    `json:"memory_count"`
	SizeMB       float64 `json:"size_mb"`
	IsPublic     bool   `json:"is_public"`      // Advertised to mesh
	IsEncrypted  bool   `json:"is_encrypted"`   // SQLCipher protected
	LastModified int64  `json:"last_modified"`
	NodeID       string `json:"node_id"`
}

// FederationMessage is the wire format for federation protocol messages.
type FederationMessage struct {
	Type      FederationMessageType `json:"type"`
	SenderID  string                `json:"sender_id"`
	Timestamp int64                 `json:"timestamp"`
	Galaxy    string                `json:"galaxy,omitempty"`
	Query     string                `json:"query,omitempty"`
	Limit     int                   `json:"limit,omitempty"`
	Payload   any                   `json:"payload,omitempty"`
}

// RemoteMemory is a read-only snapshot of a memory from a remote galaxy.
type RemoteMemory struct {
	ID          string   `json:"id"`
	Title       string   `json:"title"`
	Content     string   `json:"content"`
	Tags        []string `json:"tags"`
	Importance  float64  `json:"importance"`
	Zone        string   `json:"zone"`
	SourceNode  string   `json:"source_node"`
	SourceGalaxy string  `json:"source_galaxy"`
	FetchedAt   int64    `json:"fetched_at"`
	TTL         int64    `json:"ttl_seconds"` // Cache expiry
}

// MountedGalaxy tracks a remote galaxy that this node is subscribed to.
type MountedGalaxy struct {
	Name       string    `json:"name"`
	NodeID     string    `json:"node_id"`
	MountedAt  time.Time `json:"mounted_at"`
	LastSync   time.Time `json:"last_sync"`
	CacheCount int       `json:"cache_count"`
	ReadOnly   bool      `json:"read_only"` // Always true for remote mounts
}

// ---------------------------------------------------------------------------
// FederationNode
// ---------------------------------------------------------------------------

// FederationNode manages federated galaxy state for a WhiteMagic instance.
type FederationNode struct {
	mu             sync.RWMutex
	nodeID         string
	localGalaxies  map[string]*GalaxyInfo    // Galaxies we own
	remoteGalaxies map[string]*GalaxyInfo    // Galaxies discovered on the mesh
	mounted        map[string]*MountedGalaxy // Remote galaxies we've mounted
	cache          map[string][]*RemoteMemory // Cached remote memories by galaxy
	cacheTTL       time.Duration
	outbox         []*FederationMessage
}

// NewFederationNode creates a new federation protocol node.
func NewFederationNode(nodeID string) *FederationNode {
	return &FederationNode{
		nodeID:         nodeID,
		localGalaxies:  make(map[string]*GalaxyInfo),
		remoteGalaxies: make(map[string]*GalaxyInfo),
		mounted:        make(map[string]*MountedGalaxy),
		cache:          make(map[string][]*RemoteMemory),
		cacheTTL:       5 * time.Minute,
	}
}

// RegisterLocalGalaxy makes a local galaxy available for federation.
func (fn *FederationNode) RegisterLocalGalaxy(name string, memoryCount int, sizeMB float64, isPublic bool) {
	fn.mu.Lock()
	defer fn.mu.Unlock()

	fn.localGalaxies[name] = &GalaxyInfo{
		Name:         name,
		MemoryCount:  memoryCount,
		SizeMB:       sizeMB,
		IsPublic:     isPublic,
		LastModified: time.Now().Unix(),
		NodeID:       fn.nodeID,
	}

	if isPublic {
		fn.outbox = append(fn.outbox, &FederationMessage{
			Type:      MsgGalaxyAdvertise,
			SenderID:  fn.nodeID,
			Timestamp: time.Now().UnixMilli(),
			Payload:   fn.publicGalaxies(),
		})
	}
}

// MountRemoteGalaxy subscribes to a remote galaxy for read-only access.
func (fn *FederationNode) MountRemoteGalaxy(galaxyName, nodeID string) error {
	fn.mu.Lock()
	defer fn.mu.Unlock()

	key := fmt.Sprintf("%s@%s", galaxyName, nodeID)

	if _, exists := fn.mounted[key]; exists {
		return fmt.Errorf("galaxy %s already mounted", key)
	}

	fn.mounted[key] = &MountedGalaxy{
		Name:      galaxyName,
		NodeID:    nodeID,
		MountedAt: time.Now(),
		ReadOnly:  true,
	}

	fn.outbox = append(fn.outbox, &FederationMessage{
		Type:      MsgGalaxyMount,
		SenderID:  fn.nodeID,
		Timestamp: time.Now().UnixMilli(),
		Galaxy:    galaxyName,
	})

	return nil
}

// UnmountRemoteGalaxy unsubscribes from a remote galaxy.
func (fn *FederationNode) UnmountRemoteGalaxy(galaxyName, nodeID string) {
	fn.mu.Lock()
	defer fn.mu.Unlock()

	key := fmt.Sprintf("%s@%s", galaxyName, nodeID)
	delete(fn.mounted, key)
	delete(fn.cache, key)

	fn.outbox = append(fn.outbox, &FederationMessage{
		Type:      MsgGalaxyUnmount,
		SenderID:  fn.nodeID,
		Timestamp: time.Now().UnixMilli(),
		Galaxy:    galaxyName,
	})
}

// QueryRemoteGalaxy sends a search query to a mounted remote galaxy.
func (fn *FederationNode) QueryRemoteGalaxy(galaxyName, nodeID, query string, limit int) {
	fn.mu.Lock()
	defer fn.mu.Unlock()

	if limit <= 0 {
		limit = 10
	}

	fn.outbox = append(fn.outbox, &FederationMessage{
		Type:      MsgGalaxyQuery,
		SenderID:  fn.nodeID,
		Timestamp: time.Now().UnixMilli(),
		Galaxy:    galaxyName,
		Query:     query,
		Limit:     limit,
	})
}

// HandleMessage processes an incoming federation message.
func (fn *FederationNode) HandleMessage(msg *FederationMessage) *FederationMessage {
	fn.mu.Lock()
	defer fn.mu.Unlock()

	switch msg.Type {
	case MsgGalaxyAdvertise:
		return fn.handleAdvertise(msg)
	case MsgGalaxyQuery:
		return fn.handleQuery(msg)
	case MsgGalaxyResponse:
		return fn.handleResponse(msg)
	case MsgGalaxyMount:
		// Acknowledgment — peer wants our galaxy updates
		return nil
	case MsgGalaxyUnmount:
		// Acknowledgment — peer no longer wants updates
		return nil
	default:
		return nil
	}
}

// DrainOutbox returns and clears pending outgoing messages.
func (fn *FederationNode) DrainOutbox() []*FederationMessage {
	fn.mu.Lock()
	defer fn.mu.Unlock()

	msgs := make([]*FederationMessage, len(fn.outbox))
	copy(msgs, fn.outbox)
	fn.outbox = fn.outbox[:0]
	return msgs
}

// ListMounted returns all mounted remote galaxies.
func (fn *FederationNode) ListMounted() []*MountedGalaxy {
	fn.mu.RLock()
	defer fn.mu.RUnlock()

	result := make([]*MountedGalaxy, 0, len(fn.mounted))
	for _, m := range fn.mounted {
		result = append(result, m)
	}
	return result
}

// ListRemoteGalaxies returns all discovered remote galaxies.
func (fn *FederationNode) ListRemoteGalaxies() []*GalaxyInfo {
	fn.mu.RLock()
	defer fn.mu.RUnlock()

	result := make([]*GalaxyInfo, 0, len(fn.remoteGalaxies))
	for _, g := range fn.remoteGalaxies {
		result = append(result, g)
	}
	return result
}

// GetCachedMemories returns cached memories from a mounted remote galaxy.
func (fn *FederationNode) GetCachedMemories(galaxyName, nodeID string) []*RemoteMemory {
	fn.mu.RLock()
	defer fn.mu.RUnlock()

	key := fmt.Sprintf("%s@%s", galaxyName, nodeID)
	now := time.Now().Unix()

	// Filter expired entries
	valid := make([]*RemoteMemory, 0)
	for _, mem := range fn.cache[key] {
		if now-mem.FetchedAt < mem.TTL {
			valid = append(valid, mem)
		}
	}
	return valid
}

// Stats returns federation statistics.
func (fn *FederationNode) Stats() map[string]any {
	fn.mu.RLock()
	defer fn.mu.RUnlock()

	totalCached := 0
	for _, mems := range fn.cache {
		totalCached += len(mems)
	}

	return map[string]any{
		"local_galaxies":    len(fn.localGalaxies),
		"remote_galaxies":   len(fn.remoteGalaxies),
		"mounted_galaxies":  len(fn.mounted),
		"cached_memories":   totalCached,
		"cache_ttl_seconds": int(fn.cacheTTL.Seconds()),
	}
}

// ToJSON serializes federation state.
func (fn *FederationNode) ToJSON() (string, error) {
	data := fn.Stats()
	data["local"] = fn.localGalaxies
	data["remote"] = fn.ListRemoteGalaxies()
	data["mounted"] = fn.ListMounted()

	bytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

// ---------------------------------------------------------------------------
// Internal handlers
// ---------------------------------------------------------------------------

func (fn *FederationNode) handleAdvertise(msg *FederationMessage) *FederationMessage {
	// Parse advertised galaxies
	if payload, ok := msg.Payload.([]any); ok {
		for _, item := range payload {
			if galaxy, ok := item.(map[string]any); ok {
				name, _ := galaxy["name"].(string)
				count, _ := galaxy["memory_count"].(float64)
				size, _ := galaxy["size_mb"].(float64)
				fn.remoteGalaxies[fmt.Sprintf("%s@%s", name, msg.SenderID)] = &GalaxyInfo{
					Name:         name,
					MemoryCount:  int(count),
					SizeMB:       size,
					IsPublic:     true,
					LastModified: msg.Timestamp,
					NodeID:       msg.SenderID,
				}
			}
		}
	}
	return nil
}

func (fn *FederationNode) handleQuery(msg *FederationMessage) *FederationMessage {
	// Check if we have the requested galaxy
	galaxy, exists := fn.localGalaxies[msg.Galaxy]
	if !exists || !galaxy.IsPublic {
		return nil // Don't have it or it's private
	}

	// Return a placeholder response — actual memory lookup is done by the
	// Python bridge that calls into SQLite. The Go layer signals that the
	// query is valid and should be forwarded to the local WhiteMagic instance.
	return &FederationMessage{
		Type:      MsgGalaxyResponse,
		SenderID:  fn.nodeID,
		Timestamp: time.Now().UnixMilli(),
		Galaxy:    msg.Galaxy,
		Query:     msg.Query,
		Payload: map[string]any{
			"status":    "forwarded_to_local",
			"galaxy":    msg.Galaxy,
			"query":     msg.Query,
			"limit":     msg.Limit,
			"requester": msg.SenderID,
		},
	}
}

func (fn *FederationNode) handleResponse(msg *FederationMessage) *FederationMessage {
	// Cache received memories
	if payload, ok := msg.Payload.(map[string]any); ok {
		if memories, ok := payload["memories"].([]any); ok {
			key := fmt.Sprintf("%s@%s", msg.Galaxy, msg.SenderID)
			for _, item := range memories {
				if mem, ok := item.(map[string]any); ok {
					id, _ := mem["id"].(string)
					title, _ := mem["title"].(string)
					content, _ := mem["content"].(string)
					importance, _ := mem["importance"].(float64)
					zone, _ := mem["zone"].(string)

					fn.cache[key] = append(fn.cache[key], &RemoteMemory{
						ID:           id,
						Title:        title,
						Content:      content,
						Importance:   importance,
						Zone:         zone,
						SourceNode:   msg.SenderID,
						SourceGalaxy: msg.Galaxy,
						FetchedAt:    time.Now().Unix(),
						TTL:          int64(fn.cacheTTL.Seconds()),
					})
				}
			}

			// Update mount stats
			if mount, exists := fn.mounted[key]; exists {
				mount.LastSync = time.Now()
				mount.CacheCount = len(fn.cache[key])
			}
		}
	}
	return nil
}

func (fn *FederationNode) publicGalaxies() []map[string]any {
	result := make([]map[string]any, 0)
	for _, g := range fn.localGalaxies {
		if g.IsPublic {
			result = append(result, map[string]any{
				"name":         g.Name,
				"memory_count": g.MemoryCount,
				"size_mb":      g.SizeMB,
			})
		}
	}
	return result
}
