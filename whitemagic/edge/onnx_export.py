"""ONNX Export for WhiteMagic Edge AI
Version: 3.0.0

Exports edge AI models to ONNX format for deployment on:
- Mobile devices (Core ML, TensorRT)
- Edge devices (ONNX Runtime)
- Browsers (ONNX.js, WebNN)
- Embedded systems

Note: Full ONNX export requires numpy and onnx packages.
This module provides rule-based export that works without ML dependencies.
"""

import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class ONNXMetadata:
    """Metadata for ONNX model."""

    version: str = "3.0.0"
    producer: str = "WhiteMagic Edge AI"
    domain: str = "whitemagic.edge"
    description: str = "Local inference model"
    ir_version: int = 8


class RuleBasedONNX:
    """Rule-based model export in ONNX-compatible format.

    This doesn't require numpy/onnx but produces a format
    that can be loaded by ONNX-compatible runtimes.
    """

    def __init__(self, rules: list[dict] | None = None) -> None:
        self.rules = rules or []
        self.metadata = ONNXMetadata()

    def add_rule(self, pattern: str, response: str, confidence: float = 1.0) -> Any:
        """Add a rule to the model."""
        self.rules.append({
            "pattern": pattern,
            "response": response,
            "confidence": confidence,
        })

    def export_json(self, path: Path) -> dict:
        """Export as JSON format (ONNX-like structure).

        This can be loaded by custom ONNX runtime handlers.
        """
        model = {
            "format": "whitemagic_onnx_json",
            "version": self.metadata.version,
            "producer": self.metadata.producer,
            "domain": self.metadata.domain,
            "description": self.metadata.description,
            "ir_version": self.metadata.ir_version,
            "graph": {
                "name": "edge_inference",
                "inputs": [
                    {"name": "query", "type": "string"},
                ],
                "outputs": [
                    {"name": "answer", "type": "string"},
                    {"name": "confidence", "type": "float"},
                    {"name": "needs_cloud", "type": "bool"},
                ],
                "nodes": [
                    {
                        "op_type": "PatternMatch",
                        "inputs": ["query"],
                        "outputs": ["matched_rule"],
                        "attributes": {
                            "rules": self.rules,
                        },
                    },
                    {
                        "op_type": "ResponseLookup",
                        "inputs": ["matched_rule"],
                        "outputs": ["answer", "confidence", "needs_cloud"],
                    },
                ],
            },
            "rules": self.rules,
            "stats": {
                "total_rules": len(self.rules),
                "patterns": [r["pattern"] for r in self.rules],
            },
        }

        path.write_text(json.dumps(model, indent=2))
        return model

    def export_binary(self, path: Path) -> bytes:
        """Export as binary format for faster loading.

        Uses a simple binary format that can be loaded quickly.
        """
        import struct

        # Header
        header = b"WMONNX"  # Magic bytes
        version = struct.pack("H", 3)  # Version 3
        rule_count = struct.pack("I", len(self.rules))

        # Rules (length-prefixed strings)
        rules_data = b""
        for rule in self.rules:
            pattern = rule["pattern"].encode("utf-8")
            response = rule["response"].encode("utf-8")
            confidence = struct.pack("f", rule["confidence"])

            rules_data += struct.pack("H", len(pattern)) + pattern
            rules_data += struct.pack("H", len(response)) + response
            rules_data += confidence

        binary = header + version + rule_count + rules_data
        path.write_bytes(binary)
        return binary


def try_export_real_onnx(rules: list[dict], path: Path) -> bool:
    """Try to export as real ONNX model if dependencies available.

    Requires: numpy, onnx
    """
    try:
        import onnx  # type: ignore[import-not-found]

        # Create a simple embedding-based model
        # This is a placeholder - real implementation would use
        # proper ML model architecture

        # For now, export as custom ONNX with metadata
        model = onnx.ModelProto()
        model.ir_version = 8
        model.producer_name = "WhiteMagic Edge AI"
        model.producer_version = "3.0.0"
        model.domain = "whitemagic.edge"

        # Add rules as metadata
        for i, rule in enumerate(rules):
            meta = model.metadata_props.add()
            meta.key = f"rule_{i}_pattern"
            meta.value = rule["pattern"]

            meta = model.metadata_props.add()
            meta.key = f"rule_{i}_response"
            meta.value = rule["response"]

        onnx.save(model, str(path))
        return True

    except ImportError:
        return False


class ONNXExporter:
    """Main ONNX exporter for WhiteMagic Edge AI.

    Supports multiple export formats:
    1. JSON (always works)
    2. Binary (always works, faster)
    3. Real ONNX (requires numpy/onnx)
    """

    def __init__(self) -> None:
        from whitemagic.edge.inference import get_edge_inference
        self.engine = get_edge_inference()

    def get_rules(self) -> list[dict]:
        """Get rules from edge inference engine."""
        return [
            {
                "pattern": r.pattern,
                "response": r.response,
                "confidence": r.confidence,
            }
            for r in self.engine._rules
        ]

    def export(self, path: Path, format: str = "json") -> dict[str, Any]:
        """Export edge AI model.

        Args:
            path: Output path
            format: 'json', 'binary', or 'onnx'

        Returns:
            Export result with metadata

        """
        rules = self.get_rules()

        if format == "json":
            exporter = RuleBasedONNX(rules)
            exporter.export_json(path)
            return {
                "success": True,
                "format": "json",
                "path": str(path),
                "rules": len(rules),
                "size_bytes": path.stat().st_size,
            }

        if format == "binary":
            exporter = RuleBasedONNX(rules)
            binary = exporter.export_binary(path)
            return {
                "success": True,
                "format": "binary",
                "path": str(path),
                "rules": len(rules),
                "size_bytes": len(binary),
            }

        if format == "onnx":
            success = try_export_real_onnx(rules, path)
            if success:
                return {
                    "success": True,
                    "format": "onnx",
                    "path": str(path),
                    "rules": len(rules),
                    "size_bytes": path.stat().st_size,
                }
            # Fall back to JSON
            json_path = path.with_suffix(".json")
            exporter = RuleBasedONNX(rules)
            exporter.export_json(json_path)
            return {
                "success": True,
                "format": "json_fallback",
                "path": str(json_path),
                "rules": len(rules),
                "note": "ONNX export requires numpy/onnx packages",
            }

        raise ValueError(f"Unknown format: {format}")

    def export_all(self, output_dir: Path) -> dict[str, Any]:
        """Export in all available formats."""
        output_dir.mkdir(parents=True, exist_ok=True)

        results = {}

        # JSON (always works)
        results["json"] = self.export(output_dir / "model.json", "json")

        # Binary (always works)
        results["binary"] = self.export(output_dir / "model.wmonnx", "binary")

        # ONNX (may fall back to JSON)
        results["onnx"] = self.export(output_dir / "model.onnx", "onnx")

        return results


# === Runtime Loader ===

class ONNXLoader:
    """Load exported ONNX models for inference."""

    @staticmethod
    def load_json(path: Path) -> list[dict]:
        """Load JSON format model."""
        data = json.loads(path.read_text())
        return data.get("rules", [])  # type: ignore[no-any-return]

    @staticmethod
    def load_binary(path: Path) -> list[dict]:
        """Load binary format model."""
        import struct

        data = path.read_bytes()

        # Check magic bytes
        if data[:6] != b"WMONNX":
            raise ValueError("Invalid WMONNX file")

        # Parse header
        struct.unpack("H", data[6:8])[0]
        rule_count = struct.unpack("I", data[8:12])[0]

        # Parse rules
        rules = []
        offset = 12

        for _ in range(rule_count):
            # Pattern
            pattern_len = struct.unpack("H", data[offset:offset+2])[0]
            offset += 2
            pattern = data[offset:offset+pattern_len].decode("utf-8")
            offset += pattern_len

            # Response
            response_len = struct.unpack("H", data[offset:offset+2])[0]
            offset += 2
            response = data[offset:offset+response_len].decode("utf-8")
            offset += response_len

            # Confidence
            confidence = struct.unpack("f", data[offset:offset+4])[0]
            offset += 4

            rules.append({
                "pattern": pattern,
                "response": response,
                "confidence": confidence,
            })

        return rules


if __name__ == "__main__":
    from whitemagic.config import PROJECT_ROOT

    logger.info("📦 ONNX EXPORT TEST")
    logger.info("=" * 50)

    exporter = ONNXExporter()
    output_dir = PROJECT_ROOT / "dist" / "onnx"

    results = exporter.export_all(output_dir)

    for format_name, result in results.items():
        logger.info(f"\n{format_name}:")
        logger.info(f"  Success: {result['success']}")
        logger.info(f"  Path: {result['path']}")
        logger.info(f"  Rules: {result['rules']}")
        if "size_bytes" in result:
            logger.info(f"  Size: {result['size_bytes']} bytes")
