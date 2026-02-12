package main

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/libp2p/go-libp2p"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	"github.com/libp2p/go-libp2p/core/host"
	"github.com/libp2p/go-libp2p/core/peer"
	"github.com/libp2p/go-libp2p/core/pnet"
	"github.com/libp2p/go-libp2p/p2p/discovery/mdns"
	libp2pquic "github.com/libp2p/go-libp2p/p2p/transport/quic"
	"github.com/libp2p/go-libp2p/p2p/transport/tcp"
	"github.com/libp2p/go-libp2p/p2p/transport/websocket"
	"github.com/redis/go-redis/v9"
	"google.golang.org/protobuf/proto"

	pb "whitemagic-go/proto"
)

const (
	DiscoveryServiceTag = "whitemagic-mesh"
	SwarmTopic          = "whitemagic-ganas-swarm"
	RedisChannel        = "ganying"
)

type discoveryNotifee struct {
	h host.Host
}

func (n *discoveryNotifee) HandlePeerFound(pi peer.AddrInfo) {
	fmt.Printf("[MESH] Found peer: %s, connecting...\n", pi.ID.String())
	err := n.h.Connect(context.Background(), pi)
	if err != nil {
		fmt.Printf("[MESH] Connection failed to %s: %s\n", pi.ID.String(), err)
	} else {
		fmt.Printf("[MESH] Successfully connected to peer %s\n", pi.ID.String())
	}
}

func setupDiscovery(h host.Host) error {
	s := mdns.NewMdnsService(h, DiscoveryServiceTag, &discoveryNotifee{h: h})
	return s.Start()
}

func main() {
	msgFlag := flag.String("msg", "", "Message content to broadcast as a HolographicSignal")
	pskFlag := flag.String("psk", "", "Pre-shared key for private mesh network (min 32 chars)")
	redisUrl := os.Getenv("REDIS_URL")
	pskEnv := os.Getenv("WM_MESH_PSK")
	flag.Parse()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	fmt.Println("🌐 WhiteMagic v15.1 Nervous System — Holographic Mesh Active")
	fmt.Println("   Transports: TCP + QUIC + WebSocket | Encryption: Noise + TLS")

	// Setup Redis
	var rdb *redis.Client
	if redisUrl != "" {
		opt, err := redis.ParseURL(redisUrl)
		if err != nil {
			log.Printf("⚠️ Invalid REDIS_URL: %v", err)
		} else {
			rdb = redis.NewClient(opt)
			if err := rdb.Ping(ctx).Err(); err != nil {
				log.Printf("⚠️ Redis connection failed: %v", err)
				rdb = nil
			} else {
				fmt.Println("✅ Connected to Redis")
			}
		}
	} else {
		rdb = redis.NewClient(&redis.Options{
			Addr: "localhost:6379",
		})
		if err := rdb.Ping(ctx).Err(); err != nil {
			log.Printf("⚠️ Redis local connection failed: %v", err)
			rdb = nil
		} else {
			fmt.Println("✅ Connected to Local Redis")
		}
	}

	// Build libp2p host with multi-transport, encryption, and optional PSK
	opts := []libp2p.Option{
		// Multi-transport: TCP + QUIC + WebSocket for maximum connectivity
		libp2p.Transport(tcp.NewTCPTransport),
		libp2p.Transport(libp2pquic.NewTransport),
		libp2p.Transport(websocket.New),
		libp2p.ListenAddrStrings(
			"/ip4/0.0.0.0/tcp/0",
			"/ip4/0.0.0.0/udp/0/quic-v1",
			"/ip4/0.0.0.0/tcp/0/ws",
		),
		// NAT traversal: UPnP + NAT-PMP for automatic port forwarding
		libp2p.NATPortMap(),
		// Enable AutoRelay for peers behind strict NATs
		libp2p.EnableRelay(),
		libp2p.EnableHolePunching(),
	}

	// Optional: Private network with pre-shared key
	psk := *pskFlag
	if psk == "" {
		psk = pskEnv
	}
	if psk != "" {
		if len(psk) < 32 {
			log.Fatal("PSK must be at least 32 characters")
		}
		// Derive 32-byte key from PSK via SHA-256
		hash := sha256.Sum256([]byte(psk))
		pnetKey := pnet.PSK(hash[:])
		opts = append(opts, libp2p.PrivateNetwork(pnetKey))
		fmt.Println("🔒 Private network mode: PSK authentication enabled")
	}

	h, err := libp2p.New(opts...)
	if err != nil {
		log.Fatal(err)
	}
	defer h.Close()

	fmt.Printf("[MESH] Listening on:\n")
	for _, addr := range h.Addrs() {
		fmt.Printf("   %s/p2p/%s\n", addr, h.ID())
	}

	fmt.Printf("[MESH] My Peer ID: %s\n", h.ID().String())

	ps, err := pubsub.NewGossipSub(ctx, h)
	if err != nil {
		log.Fatal(err)
	}

	topic, err := ps.Join(SwarmTopic)
	if err != nil {
		log.Fatal(err)
	}
	defer topic.Close()

	sub, err := topic.Subscribe()
	if err != nil {
		log.Fatal(err)
	}
	defer sub.Cancel()

	if err := setupDiscovery(h); err != nil {
		log.Fatal(err)
	}

	// Message Handler
	go func() {
		for {
			msg, err := sub.Next(ctx)
			if err != nil {
				return
			}
			if msg.ReceivedFrom == h.ID() {
				continue
			}

			// Unmarshal Protobuf
			signal := &pb.HolographicSignal{}
			if err := proto.Unmarshal(msg.Data, signal); err != nil {
				fmt.Printf("\n[SWARM] ⚠️ Received invalid data from %s: %v\n", msg.ReceivedFrom.String()[:8], err)
				continue
			}

			fmt.Printf("\n[SWARM] 📥 HolographicSignal from %s:\n", msg.ReceivedFrom.String()[:8])
			fmt.Printf("   ├─ ID: %s\n", signal.Id)
			fmt.Printf("   ├─ Coordinates: %v\n", signal.Coordinates)
			fmt.Printf("   ├─ ContentHash: %s\n", signal.ContentHash)
			fmt.Printf("   └─ Importance: %.2f\n", signal.Importance)

			// Forward to Redis
			if rdb != nil {
				event := map[string]interface{}{
					"event_type": "MESH_SIGNAL",
					"source":     "whitemagic-mesh",
					"data": map[string]interface{}{
						"peer_id":      msg.ReceivedFrom.String(),
						"signal_id":    signal.Id,
						"coordinates":  signal.Coordinates,
						"content_hash": signal.ContentHash,
						"importance":   signal.Importance,
						"timestamp":    signal.Timestamp,
					},
					"confidence": signal.Importance,
					"timestamp":  time.Now().Format(time.RFC3339),
				}
				jsonBytes, _ := json.Marshal(event)
				rdb.Publish(ctx, RedisChannel, string(jsonBytes))
			}
		}
	}()

	// If -msg is provided, broadcast a test signal
	if *msgFlag != "" {
		time.Sleep(2 * time.Second) // Wait for discovery

		testSignal := &pb.HolographicSignal{
			Id:          fmt.Sprintf("sig-%d", time.Now().Unix()),
			Coordinates: []float32{0.5, 0.5, 0.0, 1.0}, // Center of logic/micro, present time, max importance
			ContentHash: fmt.Sprintf("hash-%s", *msgFlag),
			Importance:  0.9,
			Timestamp:   time.Now().Unix(),
			SourceNode:  h.ID().String(),
		}

		data, err := proto.Marshal(testSignal)
		if err != nil {
			log.Fatal("Failed to marshal signal:", err)
		}

		fmt.Printf("[SWARM] 📤 Broadcasting Signal: %s\n", testSignal.Id)
		if err := topic.Publish(ctx, data); err != nil {
			log.Fatal(err)
		}

		// Also publish to Redis so the local Core knows we sent a signal
		if rdb != nil {
			event := map[string]interface{}{
				"event_type": "MESH_SIGNAL",
				"source":     "whitemagic-mesh",
				"data": map[string]interface{}{
					"peer_id":      h.ID().String(),
					"signal_id":    testSignal.Id,
					"coordinates":  testSignal.Coordinates,
					"content_hash": testSignal.ContentHash,
					"importance":   testSignal.Importance,
					"timestamp":    testSignal.Timestamp,
				},
				"confidence": testSignal.Importance,
				"timestamp":  time.Now().Format(time.RFC3339),
			}
			jsonBytes, _ := json.Marshal(event)
			if err := rdb.Publish(ctx, RedisChannel, string(jsonBytes)).Err(); err != nil {
				fmt.Printf("[MESH] ⚠️ Redis Publish Error: %v\n", err)
			} else {
				fmt.Println("[MESH] 📤 Signal forwarded to Redis")
			}
		}

		time.Sleep(1 * time.Second)
		return
	}

	// Periodic Heartbeat (Weak Signal)
	go func() {
		ticker := time.NewTicker(30 * time.Second)
		defer ticker.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
				heartbeat := &pb.HolographicSignal{
					Id:          fmt.Sprintf("hb-%d", time.Now().Unix()),
					Coordinates: []float32{0.0, 0.0, float32(time.Now().Unix() % 100), 0.1}, // Low importance
					ContentHash: "heartbeat",
					Importance:  0.1,
					Timestamp:   time.Now().Unix(),
					SourceNode:  h.ID().String(),
				}

				data, _ := proto.Marshal(heartbeat)
				if err := topic.Publish(ctx, data); err != nil {
					fmt.Printf("[SWARM] Publish error: %s\n", err)
				}
			}
		}
	}()

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
	<-sigCh

	fmt.Println("\n🌐 Nervous System offline.")
}
