#!/bin/bash
echo "🌐 Starting WhiteMagic Kinetic Mesh Simulation..."
CGO_ENABLED=0 go build -o whitemagic-mesh main.go

./whitemagic-mesh > node1.log 2>&1 &
PID1=$!
./whitemagic-mesh > node2.log 2>&1 &
PID2=$!
./whitemagic-mesh > node3.log 2>&1 &
PID3=$!

echo "⏳ Simulating P2P swarm for 15s..."
sleep 15

echo "--- Node 1 Output ---"
cat node1.log
echo "--- Node 2 Output ---"
cat node2.log

kill $PID1 $PID2 $PID3
rm node1.log node2.log node3.log
