"""WhiteMagic Edge AI - Local inference for any platform.
Version: 4.2.0

This module provides edge AI capabilities that can run on:
- Browsers (via WebAssembly/JS)
- Mobile devices
- Embedded systems
- Legacy hardware
- IoT devices

Philosophy: AI should run ANYWHERE, not just in datacenters.
"""

from .embeddings import EmbeddingCache, LocalEmbeddings
from .export import create_standalone_package, export_to_js, export_to_json
from .federated import FederatedLearning, FederatedSync, get_federated_learning
from .inference import EdgeInference, InferenceResult, edge_infer, get_edge_inference
from .onnx_export import ONNXExporter, ONNXLoader, RuleBasedONNX
from .patterns import LearnedRule, PatternLearner

# Backward compatibility alias (docs refer to "Edge Rules")
EdgeRules = EdgeInference
EdgeRuleEngine = EdgeInference
get_edge_rules = get_edge_inference

__all__ = [
    # Inference
    "EdgeInference",
    "EdgeRules",  # Alias for EdgeInference
    "EdgeRuleEngine",  # Alias for EdgeInference
    "InferenceResult",
    "edge_infer",
    "get_edge_inference",
    "get_edge_rules",  # Alias for get_edge_inference
    # Embeddings
    "EmbeddingCache",
    "LocalEmbeddings",
    # Patterns
    "PatternLearner",
    "LearnedRule",
    # Export
    "export_to_json",
    "export_to_js",
    "create_standalone_package",
    # Federated
    "FederatedLearning",
    "FederatedSync",
    "get_federated_learning",
    # ONNX
    "ONNXExporter",
    "RuleBasedONNX",
    "ONNXLoader",
]
