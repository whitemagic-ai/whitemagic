import sys
import os
from pathlib import Path

# Add the staging path to sys.path
import sys; import os; sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))) # Auto-fixed path

from whitemagic.core.memory.sqlite_backend import SQLiteBackend
from whitemagic.core.memory.unified_types import Memory, MemoryType
from whitemagic.edge.local_llm import LocalLLM, is_url_safe
from whitemagic.inference.model_pool import is_ollama_url_safe
from whitemagic.core.memory.manager import MemoryManager

def test_json_serialization():
    print("Testing JSON serialization in SQLiteBackend...")
    db_path = Path("/tmp/test_wm_phase2.db")
    if db_path.exists():
        db_path.unlink()
    
    backend = SQLiteBackend(db_path)
    
    # Create memory with list content (the "JSON bug")
    content = ["item1", "item2", {"key": "value"}]
    mem = Memory(
        id="test_json",
        content=content,
        memory_type=MemoryType.SHORT_TERM,
        title="JSON Test"
    )
    
    backend.store(mem)
    
    # Recall and verify
    recalled = backend.recall("test_json")
    assert recalled is not None, "Recall failed"
    assert recalled.content == content, f"Content mismatch: {recalled.content} != {content}"
    assert isinstance(recalled.content, list), "Content should be a list"
    
    print("✓ JSON serialization test passed")
    return True

def test_ssrf_protection():
    print("Testing SSRF protection...")
    
    # Test helper
    assert is_url_safe("http://localhost:11434") == True
    assert is_url_safe("http://127.0.0.1:11434") == True
    assert is_url_safe("http://google.com") == False
    assert is_url_safe("http://169.254.169.254/latest/meta-data/") == False
    
    # Test and verify Ollama safety
    assert is_ollama_url_safe("http://localhost:11434") == True
    assert is_ollama_url_safe("http://internal-api.prod") == False
    
    # Test Exception handling in LocalLLM
    try:
        LocalLLM(url="http://evil.com")
        print("✗ LocalLLM failed to block unsafe URL")
        return False
    except ValueError as e:
        print(f"✓ LocalLLM correctly blocked unsafe URL: {e}")
        
    print("✓ SSRF protection tests passed")
    return True

def test_soft_delete_filtering():
    print("Testing soft-delete filtering in MemoryManager...")
    db_path = Path("/tmp/test_wm_manager_phase2.db")
    if db_path.exists():
        db_path.unlink()
        
    manager = MemoryManager(base_dir=db_path.parent)
    # Using personal DB path for this test manager instance
    manager.unified.db_path = db_path
    manager.unified.backend = SQLiteBackend(db_path)
    
    # Create 2 memories
    m1_path = manager.create_memory(title="Mem 1", content="Content 1")
    m2_path = manager.create_memory(title="Mem 2", content="Content 2")
    
    m1_id = m1_path.stem
    m2_id = m2_path.stem
    
    # Verify both exist
    recent = manager.read_recent_memories(limit=10)
    assert len(recent) == 2, f"Should have 2 memories, got {len(recent)}"
    
    # Soft delete one
    manager.delete_memory(m1_id, permanent=False)
    
    # Verify filtered
    recent_filtered = manager.read_recent_memories(limit=10, include_archived=False)
    assert len(recent_filtered) == 1, f"Should have 1 memory after soft-delete, got {len(recent_filtered)}"
    assert recent_filtered[0]["entry"]["id"] == m2_id, "Wrong memory remaining"
    
    # Verify inclusive
    recent_all = manager.read_recent_memories(limit=10, include_archived=True)
    assert len(recent_all) == 2, f"Should have 2 memories when include_archived=True, got {len(recent_all)}"
    
    print("✓ Soft-delete filtering test passed")
    return True

if __name__ == "__main__":
    success = True
    try:
        if not test_json_serialization(): success = False
        if not test_ssrf_protection(): success = False
        if not test_soft_delete_filtering(): success = False
    except Exception as e:
        print(f"Tests failed with error: {e}")
        import traceback
        traceback.print_exc()
        success = False
        
    if success:
        print("\n✨ ALL PHASE 2 TESTS PASSED ✨")
        sys.exit(0)
    else:
        print("\n❌ PHASE 2 TESTS FAILED ❌")
        sys.exit(1)
