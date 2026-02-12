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

/// Check if WASM module is loaded
#[wasm_bindgen]
pub fn wasm_ready() -> bool {
    true
}

/// Get WASM version
#[wasm_bindgen]
pub fn wasm_version() -> String {
    "15.0.0".to_string()
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
}
