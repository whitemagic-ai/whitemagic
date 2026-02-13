//! WebAssembly bindings for WhiteMagic Edge AI
//! 
//! Compiles to WASM for browser execution.
//! 10-100x faster than JavaScript implementation.
//!
//! Build with: wasm-pack build --target web

use wasm_bindgen::prelude::*;
use std::collections::HashMap;

/// Edge inference rule
#[wasm_bindgen]
#[derive(Clone)]
pub struct EdgeRule {
    id: String,
    pattern: String,
    response: String,
    confidence: f32,
}

#[wasm_bindgen]
impl EdgeRule {
    #[wasm_bindgen(constructor)]
    pub fn new(id: &str, pattern: &str, response: &str, confidence: f32) -> EdgeRule {
        EdgeRule {
            id: id.to_string(),
            pattern: pattern.to_string(),
            response: response.to_string(),
            confidence,
        }
    }

    #[wasm_bindgen(getter)]
    pub fn id(&self) -> String {
        self.id.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn response(&self) -> String {
        self.response.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn confidence(&self) -> f32 {
        self.confidence
    }
}

/// Inference result
#[wasm_bindgen]
pub struct InferenceResult {
    answer: String,
    confidence: f32,
    method: String,
    needs_cloud: bool,
    tokens_saved: u32,
}

#[wasm_bindgen]
impl InferenceResult {
    #[wasm_bindgen(getter)]
    pub fn answer(&self) -> String {
        self.answer.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn confidence(&self) -> f32 {
        self.confidence
    }

    #[wasm_bindgen(getter)]
    pub fn method(&self) -> String {
        self.method.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn needs_cloud(&self) -> bool {
        self.needs_cloud
    }

    #[wasm_bindgen(getter)]
    pub fn tokens_saved(&self) -> u32 {
        self.tokens_saved
    }
}

/// Edge inference engine - WASM version
#[wasm_bindgen]
pub struct EdgeEngine {
    rules: Vec<EdgeRule>,
    cache: HashMap<String, String>,
    stats_queries: u32,
    stats_local: u32,
    stats_tokens_saved: u32,
}

#[wasm_bindgen]
impl EdgeEngine {
    #[wasm_bindgen(constructor)]
    pub fn new() -> EdgeEngine {
        let mut engine = EdgeEngine {
            rules: Vec::new(),
            cache: HashMap::new(),
            stats_queries: 0,
            stats_local: 0,
            stats_tokens_saved: 0,
        };
        
        // Add default rules
        engine.add_rule(EdgeRule::new(
            "version",
            "version|what version",
            "WhiteMagic version 15.0.0",
            1.0
        ));
        engine.add_rule(EdgeRule::new(
            "gardens",
            "garden|how many garden",
            "WhiteMagic has 17 gardens: joy, love, beauty, truth, wisdom, mystery, play, wonder, connection, sangha, practice, presence, voice, dharma, courage, gratitude, patience",
            1.0
        ));
        engine.add_rule(EdgeRule::new(
            "tests",
            "test|how many test",
            "WhiteMagic has 1,955 passing tests",
            0.95
        ));
        engine.add_rule(EdgeRule::new(
            "offline",
            "offline|work offline|no internet",
            "Yes! This runs entirely locally via WebAssembly. No cloud needed.",
            1.0
        ));
        engine.add_rule(EdgeRule::new(
            "wasm",
            "wasm|webassembly|fast",
            "This is running as WebAssembly - 10-100x faster than JavaScript!",
            1.0
        ));
        
        engine
    }

    /// Add a rule to the engine
    #[wasm_bindgen]
    pub fn add_rule(&mut self, rule: EdgeRule) {
        self.rules.push(rule);
    }

    /// Run inference on a query
    #[wasm_bindgen]
    pub fn infer(&mut self, query: &str) -> InferenceResult {
        self.stats_queries += 1;
        let query_lower = query.to_lowercase();

        // Check cache first
        if let Some(cached) = self.cache.get(&query_lower) {
            self.stats_local += 1;
            self.stats_tokens_saved += 500;
            return InferenceResult {
                answer: cached.clone(),
                confidence: 1.0,
                method: "cache".to_string(),
                needs_cloud: false,
                tokens_saved: 500,
            };
        }

        // Try each rule
        for rule in &self.rules {
            let keywords: Vec<&str> = rule.pattern.split('|').collect();
            let matches = keywords.iter().any(|kw| query_lower.contains(kw.trim()));
            
            if matches {
                self.stats_local += 1;
                let tokens = (rule.response.len() / 4) as u32 + 100;
                self.stats_tokens_saved += tokens;
                
                // Cache the result
                self.cache.insert(query_lower, rule.response.clone());
                
                return InferenceResult {
                    answer: rule.response.clone(),
                    confidence: rule.confidence,
                    method: format!("rule:{}", rule.id),
                    needs_cloud: false,
                    tokens_saved: tokens,
                };
            }
        }

        // No match - needs cloud
        InferenceResult {
            answer: "I don't have a local answer. This might need cloud AI.".to_string(),
            confidence: 0.1,
            method: "no_match".to_string(),
            needs_cloud: true,
            tokens_saved: 0,
        }
    }

    /// Get statistics
    #[wasm_bindgen]
    pub fn get_stats(&self) -> String {
        format!(
            r#"{{"queries":{},"local":{},"tokens_saved":{},"rules":{},"cache_size":{}}}"#,
            self.stats_queries,
            self.stats_local,
            self.stats_tokens_saved,
            self.rules.len(),
            self.cache.len()
        )
    }

    /// Get local resolution rate
    #[wasm_bindgen]
    pub fn local_rate(&self) -> f32 {
        if self.stats_queries == 0 {
            0.0
        } else {
            self.stats_local as f32 / self.stats_queries as f32
        }
    }

    /// Reset statistics
    #[wasm_bindgen]
    pub fn reset_stats(&mut self) {
        self.stats_queries = 0;
        self.stats_local = 0;
        self.stats_tokens_saved = 0;
        self.cache.clear();
    }

    /// Get total tokens saved
    #[wasm_bindgen]
    pub fn tokens_saved(&self) -> u32 {
        self.stats_tokens_saved
    }
}

/// Quick inference function (convenience)
#[wasm_bindgen]
pub fn quick_infer(query: &str) -> String {
    let mut engine = EdgeEngine::new();
    let result = engine.infer(query);
    result.answer
}

/// Cosine similarity between two vectors (passed as JSON arrays)
#[wasm_bindgen]
pub fn cosine_similarity(a_json: &str, b_json: &str) -> f64 {
    let a: Vec<f64> = serde_json::from_str(a_json).unwrap_or_default();
    let b: Vec<f64> = serde_json::from_str(b_json).unwrap_or_default();
    if a.len() != b.len() || a.is_empty() {
        return 0.0;
    }
    let dot: f64 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let norm_a: f64 = a.iter().map(|x| x * x).sum::<f64>().sqrt();
    let norm_b: f64 = b.iter().map(|x| x * x).sum::<f64>().sqrt();
    if norm_a == 0.0 || norm_b == 0.0 {
        return 0.0;
    }
    dot / (norm_a * norm_b)
}

/// Batch cosine similarity: compare one query vector against many candidates.
/// Returns JSON array of {index, score} sorted by score descending.
#[wasm_bindgen]
pub fn batch_similarity(query_json: &str, candidates_json: &str, top_k: usize) -> String {
    let query: Vec<f64> = serde_json::from_str(query_json).unwrap_or_default();
    let candidates: Vec<Vec<f64>> = serde_json::from_str(candidates_json).unwrap_or_default();

    if query.is_empty() || candidates.is_empty() {
        return "[]".to_string();
    }

    let norm_q: f64 = query.iter().map(|x| x * x).sum::<f64>().sqrt();
    if norm_q == 0.0 {
        return "[]".to_string();
    }

    let mut scores: Vec<(usize, f64)> = candidates
        .iter()
        .enumerate()
        .map(|(i, c)| {
            if c.len() != query.len() {
                return (i, 0.0);
            }
            let dot: f64 = query.iter().zip(c.iter()).map(|(a, b)| a * b).sum();
            let norm_c: f64 = c.iter().map(|x| x * x).sum::<f64>().sqrt();
            let sim = if norm_c > 0.0 { dot / (norm_q * norm_c) } else { 0.0 };
            (i, sim)
        })
        .collect();

    scores.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    scores.truncate(top_k);

    serde_json::to_string(&scores.iter().map(|(i, s)| {
        serde_json::json!({"index": i, "score": s})
    }).collect::<Vec<_>>()).unwrap_or_else(|_| "[]".to_string())
}

/// Full-text search: find substring matches in a list of texts.
/// Returns JSON array of matching indices.
#[wasm_bindgen]
pub fn text_search(query: &str, texts_json: &str) -> String {
    let texts: Vec<String> = serde_json::from_str(texts_json).unwrap_or_default();
    let query_lower = query.to_lowercase();

    let matches: Vec<usize> = texts
        .iter()
        .enumerate()
        .filter(|(_, t)| t.to_lowercase().contains(&query_lower))
        .map(|(i, _)| i)
        .collect();

    serde_json::to_string(&matches).unwrap_or_else(|_| "[]".to_string())
}

/// Check if WASM module is loaded
#[wasm_bindgen]
pub fn wasm_ready() -> bool {
    true
}

/// Get WASM version
#[wasm_bindgen]
pub fn wasm_version() -> String {
    "15.5.0".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_inference() {
        let mut engine = EdgeEngine::new();
        let result = engine.infer("What version?");
        assert!(result.answer.contains("15.0.0"));
        assert!(!result.needs_cloud);
    }

    #[test]
    fn test_caching() {
        let mut engine = EdgeEngine::new();
        engine.infer("version");
        let result = engine.infer("version");
        assert_eq!(result.method, "cache");
    }

    #[test]
    fn test_no_match() {
        let mut engine = EdgeEngine::new();
        let result = engine.infer("random nonsense xyz");
        assert!(result.needs_cloud);
        assert!(result.confidence < 0.5);
    }

    #[test]
    fn test_stats() {
        let mut engine = EdgeEngine::new();
        engine.infer("version");
        engine.infer("gardens");
        assert_eq!(engine.stats_queries, 2);
        assert_eq!(engine.stats_local, 2);
    }

    #[test]
    fn test_cosine_similarity_identical() {
        let a = "[1.0, 0.0, 0.0]";
        let b = "[1.0, 0.0, 0.0]";
        let sim = cosine_similarity(a, b);
        assert!((sim - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_cosine_similarity_orthogonal() {
        let a = "[1.0, 0.0]";
        let b = "[0.0, 1.0]";
        let sim = cosine_similarity(a, b);
        assert!(sim.abs() < 1e-6);
    }

    #[test]
    fn test_batch_similarity() {
        let query = "[1.0, 0.0, 0.0]";
        let candidates = "[[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.5, 0.5, 0.0]]";
        let result = batch_similarity(query, candidates, 2);
        assert!(result.contains("\"index\":0"));  // First candidate should be best match
    }

    #[test]
    fn test_text_search() {
        let texts = r#"["Hello world", "Goodbye moon", "Hello again"]"#;
        let result = text_search("hello", texts);
        assert!(result.contains("0"));
        assert!(result.contains("2"));
        assert!(!result.contains("1"));
    }
}
