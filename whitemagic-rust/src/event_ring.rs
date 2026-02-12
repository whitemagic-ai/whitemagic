//! EventRing — LMAX Disruptor-style Lock-Free Ring Buffer (Leap 7b)
//!
//! A pre-allocated ring of event slots for the Gan Ying resonance bus.
//! Replaces Redis pub/sub for intra-process event propagation.
//!
//! Architecture:
//!   - 65,536 slots (2^16, bitwise modulo via & MASK)
//!   - Each slot: 128 bytes (cache-line aligned)
//!     [0..8]   sequence: u64 (monotonic, set by producer)
//!     [8..16]  event_type: u64 (enum discriminant)
//!     [16..24] source_id: u64 (hash of source string)
//!     [24..32] timestamp_ns: u64
//!     [32..40] confidence_bits: u64 (f64 as bits)
//!     [40..48] data_len: u64 (length of payload in data field)
//!     [48..128] data: [u8; 80] (inline payload, no heap allocation)
//!
//!   - Atomic write cursor (producer)
//!   - Per-consumer read cursors (up to 32 consumers)
//!   - Overwrite policy: if ring is full, oldest events are lost
//!
//! Performance target: ~100ns publish, 1M+ events/sec

use pyo3::prelude::*;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

const RING_BITS: usize = 16;
const RING_SIZE: usize = 1 << RING_BITS; // 65,536
const RING_MASK: usize = RING_SIZE - 1;
const SLOT_SIZE: usize = 128;
const MAX_CONSUMERS: usize = 32;
const MAX_INLINE_DATA: usize = 80;

/// A single event slot in the ring buffer.
/// Aligned to 128 bytes to occupy exactly 2 cache lines.
#[repr(C, align(128))]
struct EventSlot {
    sequence: AtomicU64,
    event_type: AtomicU64,
    source_id: AtomicU64,
    timestamp_ns: AtomicU64,
    confidence_bits: AtomicU64,
    data_len: AtomicU64,
    // Inline payload (80 bytes). We use 10 × AtomicU64 for atomic access.
    data: [AtomicU64; 10],
}

impl EventSlot {
    const fn new() -> Self {
        Self {
            sequence: AtomicU64::new(0),
            event_type: AtomicU64::new(0),
            source_id: AtomicU64::new(0),
            timestamp_ns: AtomicU64::new(0),
            confidence_bits: AtomicU64::new(0),
            data_len: AtomicU64::new(0),
            data: [
                AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0),
                AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0),
                AtomicU64::new(0), AtomicU64::new(0),
            ],
        }
    }

    fn write_data(&self, bytes: &[u8]) {
        let len = bytes.len().min(MAX_INLINE_DATA);
        self.data_len.store(len as u64, Ordering::Relaxed);
        // Copy bytes into AtomicU64 slots
        let chunks = bytes[..len].chunks(8);
        for (i, chunk) in chunks.enumerate() {
            if i >= 10 {
                break;
            }
            let mut buf = [0u8; 8];
            buf[..chunk.len()].copy_from_slice(chunk);
            self.data[i].store(u64::from_le_bytes(buf), Ordering::Relaxed);
        }
    }

    fn read_data(&self) -> Vec<u8> {
        let len = self.data_len.load(Ordering::Relaxed) as usize;
        if len == 0 {
            return Vec::new();
        }
        let mut result = Vec::with_capacity(len);
        let full_words = len / 8;
        let remainder = len % 8;

        for i in 0..full_words {
            let word = self.data[i].load(Ordering::Relaxed);
            result.extend_from_slice(&word.to_le_bytes());
        }
        if remainder > 0 && full_words < 10 {
            let word = self.data[full_words].load(Ordering::Relaxed);
            result.extend_from_slice(&word.to_le_bytes()[..remainder]);
        }
        result
    }
}

/// The ring buffer itself. Statically allocated — no heap churn at runtime.
struct RingBuffer {
    slots: Box<[EventSlot]>,
    write_cursor: AtomicU64,
    consumer_cursors: [AtomicU64; MAX_CONSUMERS],
    active_consumers: AtomicU64,
    total_published: AtomicU64,
    total_consumed: AtomicU64,
    total_overwritten: AtomicU64,
}

impl RingBuffer {
    fn new() -> Self {
        let mut slots = Vec::with_capacity(RING_SIZE);
        for _ in 0..RING_SIZE {
            slots.push(EventSlot::new());
        }
        Self {
            slots: slots.into_boxed_slice(),
            write_cursor: AtomicU64::new(0),
            consumer_cursors: std::array::from_fn(|_| AtomicU64::new(0)),
            active_consumers: AtomicU64::new(0),
            total_published: AtomicU64::new(0),
            total_consumed: AtomicU64::new(0),
            total_overwritten: AtomicU64::new(0),
        }
    }

    /// Publish an event. Returns the sequence number.
    /// Lock-free: uses atomic fetch_add on write_cursor.
    fn publish(
        &self,
        event_type: u64,
        source_id: u64,
        confidence: f64,
        data: &[u8],
    ) -> u64 {
        // Claim a slot
        let seq = self.write_cursor.fetch_add(1, Ordering::AcqRel);
        let idx = (seq as usize) & RING_MASK;

        // Check if we're overwriting an unconsumed event
        let old_seq = self.slots[idx].sequence.load(Ordering::Relaxed);
        if old_seq > 0 && seq > old_seq + RING_SIZE as u64 {
            self.total_overwritten.fetch_add(1, Ordering::Relaxed);
        }

        // Write the event
        let slot = &self.slots[idx];
        slot.event_type.store(event_type, Ordering::Relaxed);
        slot.source_id.store(source_id, Ordering::Relaxed);
        slot.timestamp_ns.store(now_ns(), Ordering::Relaxed);
        slot.confidence_bits
            .store(confidence.to_bits(), Ordering::Relaxed);
        slot.write_data(data);

        // Sequence must be written LAST with Release ordering
        // so consumers see all fields written before the sequence advances
        slot.sequence.store(seq + 1, Ordering::Release);

        self.total_published.fetch_add(1, Ordering::Relaxed);
        seq + 1
    }

    /// Register a new consumer. Returns consumer ID (0-31).
    fn register_consumer(&self) -> Option<usize> {
        let id = self.active_consumers.fetch_add(1, Ordering::AcqRel) as usize;
        if id >= MAX_CONSUMERS {
            self.active_consumers.fetch_sub(1, Ordering::Relaxed);
            return None;
        }
        // Start from current write position
        let current = self.write_cursor.load(Ordering::Acquire);
        self.consumer_cursors[id].store(current, Ordering::Relaxed);
        Some(id)
    }

    /// Poll for new events for a consumer. Returns up to `max_events` events.
    fn poll(
        &self,
        consumer_id: usize,
        max_events: usize,
    ) -> Vec<(u64, u64, u64, u64, f64, Vec<u8>)> {
        if consumer_id >= MAX_CONSUMERS {
            return Vec::new();
        }

        let mut events = Vec::new();
        let my_cursor = self.consumer_cursors[consumer_id].load(Ordering::Acquire);
        let head = self.write_cursor.load(Ordering::Acquire);

        let mut cursor = my_cursor;
        while cursor < head && events.len() < max_events {
            let idx = (cursor as usize) & RING_MASK;
            let slot = &self.slots[idx];

            // Check that the slot has been written (sequence matches)
            let slot_seq = slot.sequence.load(Ordering::Acquire);
            if slot_seq <= cursor {
                break; // Slot not yet written
            }

            let event_type = slot.event_type.load(Ordering::Relaxed);
            let source_id = slot.source_id.load(Ordering::Relaxed);
            let timestamp = slot.timestamp_ns.load(Ordering::Relaxed);
            let confidence =
                f64::from_bits(slot.confidence_bits.load(Ordering::Relaxed));
            let data = slot.read_data();

            events.push((slot_seq, event_type, source_id, timestamp, confidence, data));
            cursor += 1;
        }

        // Advance consumer cursor
        self.consumer_cursors[consumer_id].store(cursor, Ordering::Release);
        self.total_consumed
            .fetch_add(events.len() as u64, Ordering::Relaxed);

        events
    }
}

fn now_ns() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64
}

/// FNV-1a hash for source strings (fast, deterministic)
fn hash_source(source: &str) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for b in source.bytes() {
        h ^= b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

// ---------------------------------------------------------------------------
// Global singleton
// ---------------------------------------------------------------------------

lazy_static::lazy_static! {
    static ref RING: RingBuffer = RingBuffer::new();
}

// ---------------------------------------------------------------------------
// PyO3 bindings
// ---------------------------------------------------------------------------

/// Publish an event to the ring buffer. Returns sequence number.
#[pyfunction]
pub fn ring_publish(
    event_type: u64,
    source: &str,
    confidence: f64,
    data: &[u8],
) -> PyResult<u64> {
    Ok(RING.publish(event_type, hash_source(source), confidence, data))
}

/// Register a new consumer. Returns consumer ID.
#[pyfunction]
pub fn ring_register_consumer() -> PyResult<Option<usize>> {
    Ok(RING.register_consumer())
}

/// Poll for new events. Returns list of (seq, event_type, source_id, timestamp_ns, confidence, data).
#[pyfunction]
pub fn ring_poll(consumer_id: usize, max_events: usize) -> PyResult<Vec<(u64, u64, u64, u64, f64, Vec<u8>)>> {
    Ok(RING.poll(consumer_id, max_events))
}

/// Get ring buffer statistics.
#[pyfunction]
pub fn ring_stats() -> PyResult<HashMap<String, u64>> {
    let mut stats = HashMap::new();
    stats.insert(
        "write_cursor".to_string(),
        RING.write_cursor.load(Ordering::Relaxed),
    );
    stats.insert(
        "total_published".to_string(),
        RING.total_published.load(Ordering::Relaxed),
    );
    stats.insert(
        "total_consumed".to_string(),
        RING.total_consumed.load(Ordering::Relaxed),
    );
    stats.insert(
        "total_overwritten".to_string(),
        RING.total_overwritten.load(Ordering::Relaxed),
    );
    stats.insert(
        "active_consumers".to_string(),
        RING.active_consumers.load(Ordering::Relaxed),
    );
    stats.insert("ring_size".to_string(), RING_SIZE as u64);
    stats.insert("slot_size_bytes".to_string(), SLOT_SIZE as u64);
    Ok(stats)
}

/// Reset the ring buffer (for testing).
#[pyfunction]
pub fn ring_reset() -> PyResult<()> {
    RING.write_cursor.store(0, Ordering::Relaxed);
    RING.total_published.store(0, Ordering::Relaxed);
    RING.total_consumed.store(0, Ordering::Relaxed);
    RING.total_overwritten.store(0, Ordering::Relaxed);
    RING.active_consumers.store(0, Ordering::Relaxed);
    for cursor in &RING.consumer_cursors {
        cursor.store(0, Ordering::Relaxed);
    }
    for slot in RING.slots.iter() {
        slot.sequence.store(0, Ordering::Relaxed);
        slot.event_type.store(0, Ordering::Relaxed);
        slot.source_id.store(0, Ordering::Relaxed);
        slot.timestamp_ns.store(0, Ordering::Relaxed);
        slot.confidence_bits.store(0, Ordering::Relaxed);
        slot.data_len.store(0, Ordering::Relaxed);
        for word in &slot.data {
            word.store(0, Ordering::Relaxed);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_publish_and_poll() {
        ring_reset().unwrap();
        let consumer = RING.register_consumer().unwrap();

        let seq = RING.publish(1, hash_source("test"), 0.95, b"hello");
        assert!(seq > 0);

        let events = RING.poll(consumer, 10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].1, 1); // event_type
        assert!((events[0].4 - 0.95).abs() < f64::EPSILON); // confidence
        assert_eq!(&events[0].5, b"hello"); // data
    }

    #[test]
    fn test_multiple_events() {
        ring_reset().unwrap();
        let consumer = RING.register_consumer().unwrap();

        for i in 0..100 {
            RING.publish(i, hash_source("bench"), 1.0, &[]);
        }

        let events = RING.poll(consumer, 200);
        assert_eq!(events.len(), 100);
    }

    #[test]
    fn test_consumer_isolation() {
        ring_reset().unwrap();
        let c1 = RING.register_consumer().unwrap();
        let c2 = RING.register_consumer().unwrap();

        RING.publish(42, hash_source("src"), 1.0, b"data");

        let e1 = RING.poll(c1, 10);
        let e2 = RING.poll(c2, 10);
        assert_eq!(e1.len(), 1);
        assert_eq!(e2.len(), 1);
    }

    #[test]
    fn test_hash_deterministic() {
        assert_eq!(hash_source("resonance_engine"), hash_source("resonance_engine"));
        assert_ne!(hash_source("a"), hash_source("b"));
    }

    #[test]
    fn test_inline_data_80_bytes() {
        ring_reset().unwrap();
        let consumer = RING.register_consumer().unwrap();

        let data = vec![0xAB_u8; 80]; // max inline
        RING.publish(1, 0, 1.0, &data);

        let events = RING.poll(consumer, 1);
        assert_eq!(events[0].5.len(), 80);
        assert!(events[0].5.iter().all(|&b| b == 0xAB));
    }
}
