#!/bin/bash
# Install Protocol Buffers Compiler (protoc)
# Run this to unblock the analysis of binary conversation logs.

if [ -f /etc/debian_version ]; then
    echo "📦 Detected Debian/Ubuntu system."
    echo "sudo apt update && sudo apt install -y protobuf-compiler"
    sudo apt update && sudo apt install -y protobuf-compiler
elif [ -f /etc/redhat-release ]; then
    echo "📦 Detected RHEL/Fedora system."
    echo "sudo dnf install -y protobuf-compiler"
    sudo dnf install -y protobuf-compiler
elif [ -f /etc/arch-release ]; then
    echo "📦 Detected Arch Linux system."
    echo "sudo pacman -S --noconfirm protobuf"
    sudo pacman -S --noconfirm protobuf
else
    echo "⚠️ Unknown OS. Please install 'protobuf-compiler' manually."
    exit 1
fi

echo "✅ protoc installed!"
protoc --version
