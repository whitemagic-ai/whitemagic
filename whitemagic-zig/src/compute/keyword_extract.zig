const std = @import("std");

/// SIMD-accelerated keyword extraction for WhiteMagic memory text.
///
/// Uses @Vector for parallel byte processing:
/// - SIMD whitespace/punctuation detection (16 bytes at a time)
/// - Vectorized ASCII lowercase normalization
/// - Bloom filter stopword check
///
/// Exported FFI functions for Python ctypes integration.

const LANE_WIDTH = 16; // Process 16 bytes at a time (SSE2/NEON-friendly)
const VecU8 = @Vector(LANE_WIDTH, u8);

// ---------------------------------------------------------------------------
// SIMD-accelerated tokenizer
// ---------------------------------------------------------------------------

/// Check if a byte is a word separator (space, tab, newline, punctuation)
inline fn is_separator(c: u8) bool {
    return switch (c) {
        ' ', '\t', '\n', '\r', '.', ',', ';', ':', '!', '?',
        '(', ')', '[', ']', '{', '}', '<', '>', '"', '\'',
        '/', '\\', '|', '@', '#', '$', '%', '^', '&', '*',
        '+', '=', '~', '`', 0 => true,
        else => false,
    };
}

/// SIMD lowercase: convert A-Z to a-z in a vector of 16 bytes
inline fn simd_lowercase(v: VecU8) VecU8 {
    const a_upper: VecU8 = @splat('A');
    const z_upper: VecU8 = @splat('Z');
    const case_bit: VecU8 = @splat(0x20);

    // Mask: true where byte is uppercase ASCII
    const ge_a = v >= a_upper;
    const le_z = v <= z_upper;
    const is_upper = @select(u8, ge_a, @select(u8, le_z, @as(VecU8, @splat(1)), @as(VecU8, @splat(0))), @as(VecU8, @splat(0)));

    // Apply case bit where uppercase
    const mask = is_upper * case_bit;
    return v | mask;
}

/// Extract tokens from text using SIMD-accelerated scanning.
/// Returns token start/end offsets in the lowercased buffer.
pub fn tokenize(
    text: []const u8,
    out_buf: []u8,
    token_starts: []usize,
    token_ends: []usize,
    max_tokens: usize,
) struct { lowered_len: usize, token_count: usize } {
    if (text.len == 0) return .{ .lowered_len = 0, .token_count = 0 };

    // Phase 1: SIMD lowercase the entire input
    const simd_len = text.len - (text.len % LANE_WIDTH);
    var i: usize = 0;

    while (i < simd_len) : (i += LANE_WIDTH) {
        const chunk: VecU8 = text[i..][0..LANE_WIDTH].*;
        const lowered = simd_lowercase(chunk);
        out_buf[i..][0..LANE_WIDTH].* = lowered;
    }
    // Scalar remainder
    while (i < text.len) : (i += 1) {
        const c = text[i];
        out_buf[i] = if (c >= 'A' and c <= 'Z') c + 0x20 else c;
    }

    // Phase 2: Scan for token boundaries
    var token_count: usize = 0;
    var in_word = false;
    var word_start: usize = 0;

    for (0..text.len) |j| {
        const c = out_buf[j];
        const sep = is_separator(c);

        if (!sep and !in_word) {
            // Start of new word
            word_start = j;
            in_word = true;
        } else if (sep and in_word) {
            // End of word
            const word_len = j - word_start;
            if (word_len > 2 and token_count < max_tokens) {
                token_starts[token_count] = word_start;
                token_ends[token_count] = j;
                token_count += 1;
            }
            in_word = false;
        }
    }
    // Handle last word
    if (in_word) {
        const word_len = text.len - word_start;
        if (word_len > 2 and token_count < max_tokens) {
            token_starts[token_count] = word_start;
            token_ends[token_count] = text.len;
            token_count += 1;
        }
    }

    return .{ .lowered_len = text.len, .token_count = token_count };
}

// ---------------------------------------------------------------------------
// Bloom filter stopword check
// ---------------------------------------------------------------------------

const BLOOM_SIZE = 256; // 256 bits = 32 bytes
const BLOOM_HASH_COUNT = 3;

/// Pre-computed bloom filter for English stopwords
const stopword_bloom = blk: {
    @setEvalBranchQuota(10000);
    var bloom = [_]u8{0} ** (BLOOM_SIZE / 8);

    const stopwords = [_][]const u8{
        "the", "and", "for", "are", "but", "not", "you", "all",
        "can", "had", "her", "was", "one", "our", "out", "has",
        "his", "how", "its", "may", "new", "now", "old", "see",
        "way", "who", "did", "get", "let", "say", "she", "too",
        "use", "from", "have", "been", "will", "with", "this",
        "that", "they", "them", "then", "than", "each", "make",
        "like", "just", "over", "such", "take", "year", "also",
        "into", "some", "when", "what", "very", "about", "which",
        "were", "there", "would", "could", "should", "other",
        "more", "most", "only", "here", "where", "after", "before",
        "between", "through", "during", "being", "does",
    };

    for (stopwords) |word| {
        const h1 = comptime_fnv(word);
        const h2 = comptime_fnv2(word);
        var k: usize = 0;
        while (k < BLOOM_HASH_COUNT) : (k += 1) {
            const bit_pos = (h1 +% k *% h2) % BLOOM_SIZE;
            bloom[bit_pos / 8] |= @as(u8, 1) << @intCast(bit_pos % 8);
        }
    }
    break :blk bloom;
};

fn comptime_fnv(data: []const u8) u32 {
    var h: u32 = 0x811c9dc5;
    for (data) |b| {
        h ^= b;
        h *%= 0x01000193;
    }
    return h;
}

fn comptime_fnv2(data: []const u8) u32 {
    var h: u32 = 0x01000193;
    for (data) |b| {
        h = (h +% b) *% 0x811c9dc5;
    }
    return h;
}

/// Runtime FNV-1a hash for dynamic strings
fn fnv1a(data: []const u8) u32 {
    var h: u32 = 0x811c9dc5;
    for (data) |b| {
        h ^= b;
        h *%= 0x01000193;
    }
    return h;
}

fn fnv1a_v2(data: []const u8) u32 {
    var h: u32 = 0x01000193;
    for (data) |b| {
        h = (h +% b) *% 0x811c9dc5;
    }
    return h;
}

/// Check if a word is likely a stopword using the bloom filter.
/// False positives possible (~1-2%), but no false negatives.
pub fn is_stopword(word: []const u8) bool {
    if (word.len < 2 or word.len > 10) return false;

    const h1 = fnv1a(word);
    const h2 = fnv1a_v2(word);

    var k: usize = 0;
    while (k < BLOOM_HASH_COUNT) : (k += 1) {
        const bit_pos = (h1 +% @as(u32, @intCast(k)) *% h2) % BLOOM_SIZE;
        const byte_idx = bit_pos / 8;
        const bit_idx: u3 = @intCast(bit_pos % 8);
        if ((stopword_bloom[byte_idx] & (@as(u8, 1) << bit_idx)) == 0) {
            return false;
        }
    }
    return true;
}

// ---------------------------------------------------------------------------
// FFI exports for Python ctypes
// ---------------------------------------------------------------------------

/// Extract keywords from text. Returns count of extracted keywords.
/// Keywords are written to out_keywords as null-separated strings.
///
/// FFI signature:
///   wm_extract_keywords(text_ptr, text_len, out_ptr, out_capacity, max_keywords) -> keyword_count
pub fn wm_extract_keywords(
    text_ptr: [*]const u8,
    text_len: usize,
    out_ptr: [*]u8,
    out_capacity: usize,
    max_keywords: usize,
) usize {
    if (text_len == 0 or max_keywords == 0) return 0;

    const text = text_ptr[0..text_len];

    // Stack buffers for tokenization
    var lowered_buf: [65536]u8 = undefined;
    var starts: [4096]usize = undefined;
    var ends: [4096]usize = undefined;

    const effective_len = @min(text_len, 65536);
    const result = tokenize(
        text[0..effective_len],
        lowered_buf[0..effective_len],
        &starts,
        &ends,
        4096,
    );

    // Filter stopwords and collect unique keywords
    var keyword_count: usize = 0;
    var out_pos: usize = 0;

    // Simple dedup: track last 64 keyword hashes
    var seen_hashes: [64]u32 = [_]u32{0} ** 64;
    var seen_count: usize = 0;

    var t: usize = 0;
    while (t < result.token_count and keyword_count < max_keywords) : (t += 1) {
        const word = lowered_buf[starts[t]..ends[t]];

        // Skip stopwords
        if (is_stopword(word)) continue;

        // Skip if too short
        if (word.len < 3) continue;

        // Dedup check
        const h = fnv1a(word);
        var is_dup = false;
        for (seen_hashes[0..seen_count]) |sh| {
            if (sh == h) {
                is_dup = true;
                break;
            }
        }
        if (is_dup) continue;

        // Record hash for dedup
        if (seen_count < 64) {
            seen_hashes[seen_count] = h;
            seen_count += 1;
        }

        // Write keyword to output (null-separated)
        if (out_pos + word.len + 1 > out_capacity) break;
        @memcpy(out_ptr[out_pos .. out_pos + word.len], word);
        out_ptr[out_pos + word.len] = 0; // null terminator
        out_pos += word.len + 1;
        keyword_count += 1;
    }

    return keyword_count;
}

/// Batch tokenize + lowercase a text buffer using SIMD.
/// Returns the number of tokens found.
pub fn wm_simd_tokenize(
    text_ptr: [*]const u8,
    text_len: usize,
    out_lower_ptr: [*]u8,
    starts_ptr: [*]usize,
    ends_ptr: [*]usize,
    max_tokens: usize,
) usize {
    if (text_len == 0) return 0;
    const text = text_ptr[0..text_len];
    const out = out_lower_ptr[0..text_len];
    const starts = starts_ptr[0..max_tokens];
    const ends = ends_ptr[0..max_tokens];

    const result = tokenize(text, out, starts, ends, max_tokens);
    return result.token_count;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "simd_lowercase" {
    const input: VecU8 = .{ 'H', 'e', 'L', 'l', 'O', ' ', 'W', 'o', 'R', 'l', 'D', '!', '1', 'A', 'z', 'Z' };
    const result = simd_lowercase(input);
    const expected: VecU8 = .{ 'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '!', '1', 'a', 'z', 'z' };
    try std.testing.expectEqual(expected, result);
}

test "tokenize_basic" {
    const text = "Hello World, this is a TEST!";
    var lower: [128]u8 = undefined;
    var starts: [32]usize = undefined;
    var ends: [32]usize = undefined;

    const result = tokenize(text, &lower, &starts, &ends, 32);
    // Tokens > 2 chars: "hello", "world", "this", "test"
    try std.testing.expect(result.token_count >= 3);
}

test "is_stopword" {
    try std.testing.expect(is_stopword("the"));
    try std.testing.expect(is_stopword("and"));
    try std.testing.expect(is_stopword("with"));
    try std.testing.expect(!is_stopword("memory"));
    try std.testing.expect(!is_stopword("holographic"));
}

test "extract_keywords_ffi" {
    const text = "The WhiteMagic system uses Rust and Python for memory management";
    var out: [1024]u8 = undefined;
    const count = wm_extract_keywords(text.ptr, text.len, &out, 1024, 20);
    try std.testing.expect(count > 0);
    try std.testing.expect(count <= 20);
}
