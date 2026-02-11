// Package mesh provides distributed memory synchronization for WhiteMagic.
//
// gossip.go implements a gossipsub-inspired protocol for propagating
// memory updates across WhiteMagic nodes. Each node maintains a
// partial view of the global memory space and synchronizes updates
// via a gossip protocol.
//
// Protocol:
//   1. ANNOUNCE: Node broadcasts that it has new/updated memories
//   2. WANT:     Peer requests specific memory IDs it doesn't have
//   3. HAVE:     Node responds with memory metadata + content
//   4. SYNC:     Bidirectional full sync (expensive, used on join)
//   5. HEARTBEAT: Periodic liveness check with vector clock
//
// Architecture:
//   - Each node has a unique ID and maintains a peer list
//   - Gossip messages are JSON-encoded over TCP
//   - Vector clocks track causal ordering of memory updates
//   - Anti-entropy mechanism detects and repairs divergence
package mesh

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

// MessageType identifies the gossip message variant.
type MessageType string

const (
	MsgAnnounce  MessageType = "ANNOUNCE"
	MsgWant      MessageType = "WANT"
	MsgHave      MessageType = "HAVE"
	MsgSync      MessageType = "SYNC"
	MsgHeartbeat MessageType = "HEARTBEAT"
	MsgAck       MessageType = "ACK"
)

// GossipMessage is the wire format for all gossip protocol messages.
type GossipMessage struct {
	Type      MessageType    `json:"type"`
	SenderID  string         `json:"sender_id"`
	Timestamp int64          `json:"timestamp"`
	Payload   map[string]any `json:"payload"`
	VectorClock map[string]uint64 `json:"vector_clock"`
}

// MemoryDigest is a lightweight summary of a memory for sync purposes.
type MemoryDigest struct {
	ID             string  `json:"id"`
	Title          string  `json:"title"`
	Version        uint64  `json:"version"`
	GalacticDist   float64 `json:"galactic_distance"`
	Zone           string  `json:"zone"`
	LastModified   int64   `json:"last_modified"`
	ContentHash    string  `json:"content_hash"`
	RetentionScore float64 `json:"retention_score"`
}

// PeerInfo tracks a known peer in the gossip network.
type PeerInfo struct {
	ID           string    `json:"id"`
	Address      string    `json:"address"`
	LastSeen     time.Time `json:"last_seen"`
	MemoryCount  int       `json:"memory_count"`
	IsAlive      bool      `json:"is_alive"`
	VectorClock  map[string]uint64 `json:"vector_clock"`
}

// SyncStatus tracks the synchronization state.
type SyncStatus struct {
	TotalPeers      int       `json:"total_peers"`
	AlivePeers      int       `json:"alive_peers"`
	PendingAnnounce int       `json:"pending_announce"`
	PendingWant     int       `json:"pending_want"`
	LastSyncTime    time.Time `json:"last_sync_time"`
	MessagesIn      uint64    `json:"messages_in"`
	MessagesOut     uint64    `json:"messages_out"`
	Divergences     uint64    `json:"divergences_detected"`
}

// ---------------------------------------------------------------------------
// GossipNode
// ---------------------------------------------------------------------------

// GossipNode manages gossip protocol state for a single WhiteMagic instance.
type GossipNode struct {
	mu          sync.RWMutex
	id          string
	address     string
	peers       map[string]*PeerInfo
	vectorClock map[string]uint64
	digests     map[string]*MemoryDigest
	outbox      []*GossipMessage
	inbox       []*GossipMessage
	stats       SyncStatus
	fanout      int           // Number of peers to gossip to per round
	interval    time.Duration // Gossip interval
	maxPeers    int
}

// NewGossipNode creates a new gossip protocol node.
func NewGossipNode(id, address string) *GossipNode {
	return &GossipNode{
		id:          id,
		address:     address,
		peers:       make(map[string]*PeerInfo),
		vectorClock: map[string]uint64{id: 0},
		digests:     make(map[string]*MemoryDigest),
		outbox:      make([]*GossipMessage, 0),
		inbox:       make([]*GossipMessage, 0),
		fanout:      3,
		interval:    5 * time.Second,
		maxPeers:    50,
	}
}

// AddPeer registers a new peer in the gossip network.
func (n *GossipNode) AddPeer(id, address string) {
	n.mu.Lock()
	defer n.mu.Unlock()

	if id == n.id {
		return // Don't add self
	}

	n.peers[id] = &PeerInfo{
		ID:          id,
		Address:     address,
		LastSeen:    time.Now(),
		IsAlive:     true,
		VectorClock: make(map[string]uint64),
	}
}

// RemovePeer removes a peer from the network.
func (n *GossipNode) RemovePeer(id string) {
	n.mu.Lock()
	defer n.mu.Unlock()
	delete(n.peers, id)
}

// AnnounceMemory broadcasts that a memory has been created or updated.
func (n *GossipNode) AnnounceMemory(digest *MemoryDigest) {
	n.mu.Lock()
	defer n.mu.Unlock()

	// Update local state
	n.digests[digest.ID] = digest
	n.vectorClock[n.id]++

	// Create announce message
	msg := &GossipMessage{
		Type:      MsgAnnounce,
		SenderID:  n.id,
		Timestamp: time.Now().UnixMilli(),
		Payload: map[string]any{
			"memory_id":    digest.ID,
			"title":        digest.Title,
			"version":      digest.Version,
			"content_hash": digest.ContentHash,
			"zone":         digest.Zone,
		},
		VectorClock: copyVectorClock(n.vectorClock),
	}

	n.outbox = append(n.outbox, msg)
	n.stats.PendingAnnounce++
}

// RequestMemory sends a WANT message for a specific memory ID.
func (n *GossipNode) RequestMemory(memoryID string) {
	n.mu.Lock()
	defer n.mu.Unlock()

	msg := &GossipMessage{
		Type:      MsgWant,
		SenderID:  n.id,
		Timestamp: time.Now().UnixMilli(),
		Payload: map[string]any{
			"memory_id": memoryID,
		},
		VectorClock: copyVectorClock(n.vectorClock),
	}

	n.outbox = append(n.outbox, msg)
	n.stats.PendingWant++
}

// HandleMessage processes an incoming gossip message.
func (n *GossipNode) HandleMessage(msg *GossipMessage) *GossipMessage {
	n.mu.Lock()
	defer n.mu.Unlock()

	n.stats.MessagesIn++

	// Update peer liveness
	if peer, ok := n.peers[msg.SenderID]; ok {
		peer.LastSeen = time.Now()
		peer.IsAlive = true
		peer.VectorClock = msg.VectorClock
	}

	// Merge vector clocks
	n.mergeVectorClock(msg.VectorClock)

	switch msg.Type {
	case MsgAnnounce:
		return n.handleAnnounce(msg)
	case MsgWant:
		return n.handleWant(msg)
	case MsgHave:
		return n.handleHave(msg)
	case MsgHeartbeat:
		return n.handleHeartbeat(msg)
	case MsgSync:
		return n.handleSync(msg)
	default:
		return nil
	}
}

// GossipRound selects random peers and sends pending messages.
// Returns the messages to send (caller handles actual network I/O).
func (n *GossipNode) GossipRound() []*GossipMessage {
	n.mu.Lock()
	defer n.mu.Unlock()

	if len(n.outbox) == 0 {
		// Send heartbeat if nothing else
		heartbeat := &GossipMessage{
			Type:      MsgHeartbeat,
			SenderID:  n.id,
			Timestamp: time.Now().UnixMilli(),
			Payload: map[string]any{
				"memory_count": len(n.digests),
				"peer_count":   len(n.peers),
			},
			VectorClock: copyVectorClock(n.vectorClock),
		}
		return []*GossipMessage{heartbeat}
	}

	// Drain outbox
	messages := make([]*GossipMessage, len(n.outbox))
	copy(messages, n.outbox)
	n.outbox = n.outbox[:0]
	n.stats.MessagesOut += uint64(len(messages))
	n.stats.PendingAnnounce = 0
	n.stats.PendingWant = 0

	return messages
}

// SelectGossipTargets picks random peers for this gossip round.
func (n *GossipNode) SelectGossipTargets() []string {
	n.mu.RLock()
	defer n.mu.RUnlock()

	alive := make([]string, 0)
	for id, peer := range n.peers {
		if peer.IsAlive {
			alive = append(alive, id)
		}
	}

	if len(alive) <= n.fanout {
		return alive
	}

	// Random selection without replacement
	selected := make([]string, 0, n.fanout)
	perm := rand.Perm(len(alive))
	for i := 0; i < n.fanout && i < len(perm); i++ {
		selected = append(selected, alive[perm[i]])
	}
	return selected
}

// GetStatus returns the current sync status.
func (n *GossipNode) GetStatus() *SyncStatus {
	n.mu.RLock()
	defer n.mu.RUnlock()

	alive := 0
	for _, p := range n.peers {
		if p.IsAlive {
			alive++
		}
	}

	return &SyncStatus{
		TotalPeers:      len(n.peers),
		AlivePeers:      alive,
		PendingAnnounce: n.stats.PendingAnnounce,
		PendingWant:     n.stats.PendingWant,
		LastSyncTime:    n.stats.LastSyncTime,
		MessagesIn:      n.stats.MessagesIn,
		MessagesOut:     n.stats.MessagesOut,
		Divergences:     n.stats.Divergences,
	}
}

// GetDigests returns all known memory digests.
func (n *GossipNode) GetDigests() map[string]*MemoryDigest {
	n.mu.RLock()
	defer n.mu.RUnlock()

	result := make(map[string]*MemoryDigest, len(n.digests))
	for k, v := range n.digests {
		result[k] = v
	}
	return result
}

// DetectDivergence compares local vector clock with a peer's to find divergence.
func (n *GossipNode) DetectDivergence(peerClock map[string]uint64) []string {
	n.mu.RLock()
	defer n.mu.RUnlock()

	var diverged []string
	for nodeID, peerVersion := range peerClock {
		localVersion, exists := n.vectorClock[nodeID]
		if !exists || localVersion < peerVersion {
			diverged = append(diverged, nodeID)
		}
	}

	if len(diverged) > 0 {
		n.mu.RUnlock()
		n.mu.Lock()
		n.stats.Divergences += uint64(len(diverged))
		n.mu.Unlock()
		n.mu.RLock()
	}

	return diverged
}

// ToJSON serializes the node status to JSON.
func (n *GossipNode) ToJSON() (string, error) {
	n.mu.RLock()
	defer n.mu.RUnlock()

	data := map[string]any{
		"id":            n.id,
		"address":       n.address,
		"peer_count":    len(n.peers),
		"memory_count":  len(n.digests),
		"vector_clock":  n.vectorClock,
		"status":        n.GetStatus(),
	}

	bytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

// ---------------------------------------------------------------------------
// Internal message handlers
// ---------------------------------------------------------------------------

func (n *GossipNode) handleAnnounce(msg *GossipMessage) *GossipMessage {
	memoryID, _ := msg.Payload["memory_id"].(string)
	version, _ := msg.Payload["version"].(float64)
	contentHash, _ := msg.Payload["content_hash"].(string)

	existing, exists := n.digests[memoryID]
	if !exists || existing.Version < uint64(version) {
		// We need this memory — send WANT
		return &GossipMessage{
			Type:      MsgWant,
			SenderID:  n.id,
			Timestamp: time.Now().UnixMilli(),
			Payload: map[string]any{
				"memory_id":    memoryID,
				"content_hash": contentHash,
			},
			VectorClock: copyVectorClock(n.vectorClock),
		}
	}
	return nil // Already have this version
}

func (n *GossipNode) handleWant(msg *GossipMessage) *GossipMessage {
	memoryID, _ := msg.Payload["memory_id"].(string)

	digest, exists := n.digests[memoryID]
	if !exists {
		return nil // Don't have it
	}

	// Respond with HAVE
	return &GossipMessage{
		Type:      MsgHave,
		SenderID:  n.id,
		Timestamp: time.Now().UnixMilli(),
		Payload: map[string]any{
			"memory_id":       digest.ID,
			"title":           digest.Title,
			"version":         digest.Version,
			"galactic_distance": digest.GalacticDist,
			"zone":            digest.Zone,
			"content_hash":    digest.ContentHash,
			"retention_score": digest.RetentionScore,
		},
		VectorClock: copyVectorClock(n.vectorClock),
	}
}

func (n *GossipNode) handleHave(msg *GossipMessage) *GossipMessage {
	memoryID, _ := msg.Payload["memory_id"].(string)
	title, _ := msg.Payload["title"].(string)
	version, _ := msg.Payload["version"].(float64)
	galacticDist, _ := msg.Payload["galactic_distance"].(float64)
	zone, _ := msg.Payload["zone"].(string)
	contentHash, _ := msg.Payload["content_hash"].(string)
	retention, _ := msg.Payload["retention_score"].(float64)

	n.digests[memoryID] = &MemoryDigest{
		ID:             memoryID,
		Title:          title,
		Version:        uint64(version),
		GalacticDist:   galacticDist,
		Zone:           zone,
		ContentHash:    contentHash,
		RetentionScore: retention,
		LastModified:   msg.Timestamp,
	}

	n.stats.LastSyncTime = time.Now()

	return &GossipMessage{
		Type:      MsgAck,
		SenderID:  n.id,
		Timestamp: time.Now().UnixMilli(),
		Payload: map[string]any{
			"memory_id": memoryID,
			"status":    "received",
		},
		VectorClock: copyVectorClock(n.vectorClock),
	}
}

func (n *GossipNode) handleHeartbeat(msg *GossipMessage) *GossipMessage {
	// Just ACK
	return &GossipMessage{
		Type:      MsgAck,
		SenderID:  n.id,
		Timestamp: time.Now().UnixMilli(),
		Payload: map[string]any{
			"memory_count": len(n.digests),
		},
		VectorClock: copyVectorClock(n.vectorClock),
	}
}

func (n *GossipNode) handleSync(msg *GossipMessage) *GossipMessage {
	// Full sync: return all our digests
	digestList := make([]map[string]any, 0, len(n.digests))
	for _, d := range n.digests {
		digestList = append(digestList, map[string]any{
			"id":      d.ID,
			"version": d.Version,
			"hash":    d.ContentHash,
		})
	}

	n.stats.LastSyncTime = time.Now()

	return &GossipMessage{
		Type:      MsgHave,
		SenderID:  n.id,
		Timestamp: time.Now().UnixMilli(),
		Payload: map[string]any{
			"digests": digestList,
			"total":   len(digestList),
		},
		VectorClock: copyVectorClock(n.vectorClock),
	}
}

// ---------------------------------------------------------------------------
// Vector clock operations
// ---------------------------------------------------------------------------

func (n *GossipNode) mergeVectorClock(remote map[string]uint64) {
	for nodeID, remoteVersion := range remote {
		if localVersion, exists := n.vectorClock[nodeID]; !exists || remoteVersion > localVersion {
			n.vectorClock[nodeID] = remoteVersion
		}
	}
}

func copyVectorClock(vc map[string]uint64) map[string]uint64 {
	result := make(map[string]uint64, len(vc))
	for k, v := range vc {
		result[k] = v
	}
	return result
}

// ---------------------------------------------------------------------------
// Distributed lock manager (simple leader-election style)
// ---------------------------------------------------------------------------

// DistributedLock provides a simple distributed lock using gossip consensus.
type DistributedLock struct {
	mu        sync.Mutex
	Name      string
	HolderID  string
	AcquiredAt time.Time
	ExpiresAt  time.Time
	Version   uint64
}

// LockManager manages distributed locks across the gossip network.
type LockManager struct {
	mu    sync.RWMutex
	node  *GossipNode
	locks map[string]*DistributedLock
}

// NewLockManager creates a lock manager for the given gossip node.
func NewLockManager(node *GossipNode) *LockManager {
	return &LockManager{
		node:  node,
		locks: make(map[string]*DistributedLock),
	}
}

// TryAcquire attempts to acquire a named lock. Returns true if acquired.
func (lm *LockManager) TryAcquire(name string, ttl time.Duration) bool {
	lm.mu.Lock()
	defer lm.mu.Unlock()

	now := time.Now()

	if existing, ok := lm.locks[name]; ok {
		// Check if lock has expired
		if now.Before(existing.ExpiresAt) && existing.HolderID != lm.node.id {
			return false // Lock held by another node
		}
	}

	lm.locks[name] = &DistributedLock{
		Name:       name,
		HolderID:   lm.node.id,
		AcquiredAt: now,
		ExpiresAt:  now.Add(ttl),
		Version:    uint64(now.UnixNano()),
	}

	// Announce lock acquisition via gossip
	lm.node.AnnounceMemory(&MemoryDigest{
		ID:      fmt.Sprintf("lock:%s", name),
		Title:   fmt.Sprintf("Lock: %s held by %s", name, lm.node.id),
		Version: uint64(now.UnixNano()),
	})

	return true
}

// Release releases a named lock.
func (lm *LockManager) Release(name string) bool {
	lm.mu.Lock()
	defer lm.mu.Unlock()

	existing, ok := lm.locks[name]
	if !ok || existing.HolderID != lm.node.id {
		return false
	}

	delete(lm.locks, name)
	return true
}

// IsHeld checks if a lock is currently held.
func (lm *LockManager) IsHeld(name string) (bool, string) {
	lm.mu.RLock()
	defer lm.mu.RUnlock()

	lock, ok := lm.locks[name]
	if !ok {
		return false, ""
	}

	if time.Now().After(lock.ExpiresAt) {
		return false, ""
	}

	return true, lock.HolderID
}

// ListLocks returns all active locks.
func (lm *LockManager) ListLocks() []*DistributedLock {
	lm.mu.RLock()
	defer lm.mu.RUnlock()

	now := time.Now()
	result := make([]*DistributedLock, 0, len(lm.locks))
	for _, lock := range lm.locks {
		if now.Before(lock.ExpiresAt) {
			result = append(result, lock)
		}
	}
	return result
}
