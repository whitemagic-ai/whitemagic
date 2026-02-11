"""Mesh module — gRPC mesh network client for distributed WhiteMagic."""

from .client import MeshClient, MeshPeer, get_mesh_client

__all__ = ["MeshClient", "MeshPeer", "get_mesh_client"]
