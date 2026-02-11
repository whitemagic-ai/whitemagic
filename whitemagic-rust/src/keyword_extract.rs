//! Fast keyword extraction via PyO3 (v13.3.2)
//!
//! Replaces the Zig SIMD keyword path which was 15× slower than Python
//! due to ctypes marshaling overhead. PyO3 eliminates that overhead
//! entirely — strings are borrowed directly from Python without copying.
//!
//! Strategy:
//!   1. Lowercase + split on non-alphanumeric boundaries
//!   2. Filter stopwords via HashSet lookup (O(1))
//!   3. Frequency count, return top-N by frequency
//!   4. Return as Python set

use pyo3::prelude::*;
use std::collections::{HashMap, HashSet};

/// Common English stopwords — compiled at startup, checked via HashSet O(1)
fn stopwords() -> HashSet<&'static str> {
    [
        "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
        "of", "with", "by", "from", "is", "it", "as", "was", "are", "be",
        "been", "being", "have", "has", "had", "do", "does", "did", "will",
        "would", "could", "should", "may", "might", "shall", "can", "not",
        "no", "nor", "so", "if", "then", "than", "that", "this", "these",
        "those", "he", "she", "we", "they", "you", "me", "him", "her", "us",
        "them", "my", "your", "his", "its", "our", "their", "what", "which",
        "who", "whom", "where", "when", "why", "how", "all", "each", "every",
        "both", "few", "more", "most", "other", "some", "such", "only",
        "own", "same", "also", "just", "about", "into", "over", "after",
        "before", "between", "under", "again", "further", "once", "here",
        "there", "any", "very", "too", "much", "many", "up", "out", "off",
        "down", "now", "new", "old", "well", "way", "use", "used", "one",
        "two", "first", "last", "long", "great", "little", "right", "big",
        "high", "end", "let", "put", "set", "say", "still", "try", "ask",
        "get", "got", "make", "go", "going", "see", "look", "take", "come",
        "think", "know", "want", "give", "tell", "call", "work", "find",
        "need", "like", "back", "even", "must", "while", "because",
        "through", "during", "already", "rather", "since", "though",
        "until", "using", "etc", "via", "per", "without",
        // Common code/file noise
        "import", "def", "class", "return", "self", "none", "true", "false",
        "else", "elif", "pass", "break", "continue", "raise", "except",
        "finally", "lambda", "yield", "assert", "global", "nonlocal",
        "del", "print", "type", "str", "int", "float", "bool", "list",
        "dict", "tuple", "len", "range", "enumerate",
    ].into_iter().collect()
}

/// Extract keywords from text. Returns a set of the top `max_keywords`
/// keywords by frequency, excluding stopwords and short tokens (len <= 2).
#[pyfunction]
#[pyo3(signature = (text, max_keywords=50))]
pub fn keyword_extract(text: &str, max_keywords: usize) -> HashSet<String> {
    let stops = stopwords();
    let mut freq: HashMap<String, u32> = HashMap::with_capacity(256);

    // Tokenize: split on non-alphanumeric, lowercase inline
    let mut word_start: Option<usize> = None;
    let bytes = text.as_bytes();

    for i in 0..=bytes.len() {
        let is_alnum = if i < bytes.len() {
            bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_'
        } else {
            false
        };

        if is_alnum {
            if word_start.is_none() {
                word_start = Some(i);
            }
        } else if let Some(start) = word_start {
            let word = &text[start..i];
            if word.len() > 2 {
                let lower: String = word.chars().map(|c| c.to_ascii_lowercase()).collect();
                if !stops.contains(lower.as_str()) {
                    *freq.entry(lower).or_insert(0) += 1;
                }
            }
            word_start = None;
        }
    }

    if freq.len() <= max_keywords {
        return freq.into_keys().collect();
    }

    // Sort by frequency descending, take top N
    let mut entries: Vec<(String, u32)> = freq.into_iter().collect();
    entries.sort_unstable_by(|a, b| b.1.cmp(&a.1));
    entries.truncate(max_keywords);
    entries.into_iter().map(|(k, _)| k).collect()
}

/// Batch extract keywords from multiple texts. Returns a list of keyword sets.
#[pyfunction]
#[pyo3(signature = (texts, max_keywords=50))]
pub fn keyword_extract_batch(texts: Vec<String>, max_keywords: usize) -> Vec<HashSet<String>> {
    texts.iter().map(|t| keyword_extract(t.as_str(), max_keywords)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_extraction() {
        let text = "whitemagic memory holographic coordinate encoding system architecture";
        let kw = keyword_extract(text, 50);
        assert!(kw.contains("whitemagic"));
        assert!(kw.contains("memory"));
        assert!(kw.contains("holographic"));
        assert!(!kw.contains("the")); // stopword
    }

    #[test]
    fn test_max_keywords() {
        let text = "alpha beta gamma delta epsilon zeta eta theta iota kappa";
        let kw = keyword_extract(text, 3);
        assert_eq!(kw.len(), 3);
    }

    #[test]
    fn test_short_words_filtered() {
        let text = "a an it do go to we me he or if";
        let kw = keyword_extract(text, 50);
        assert!(kw.is_empty());
    }

    #[test]
    fn test_frequency_ordering() {
        let text = "memory memory memory holographic holographic system";
        let kw = keyword_extract(text, 2);
        assert!(kw.contains("memory"));
        assert!(kw.contains("holographic"));
        assert!(!kw.contains("system"));
    }
}
