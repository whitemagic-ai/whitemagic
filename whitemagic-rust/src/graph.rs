///
/// Graph Engine - Fast Memory Linking
///
/// Uses parallel processing to build memory connections.
///

use std::collections::{HashMap, HashSet};
use rayon::prelude::*;
use regex::Regex;
use lazy_static::lazy_static;

lazy_static! {
    static ref TOKEN_REGEX: Regex = Regex::new(r"[^\w\s]").unwrap();
    static ref STOPWORDS: HashSet<&'static str> = {
        let mut s = HashSet::new();
        for w in &["the", "a", "an", "is", "are", "was", "to", "of", "in", "for", "on", "with", "and", "or", "it", "this", "that"] {
            s.insert(*w);
        }
        s
    };
}

#[derive(Debug, Clone)]
pub struct Node {
    pub id: String,
    pub content: String,
    pub tags: Vec<String>,
    pub tokens: HashSet<String>,
}

impl Node {
    pub fn new(id: String, content: String, tags: Vec<String>) -> Self {
        let tokens = tokenize(&format!("{} {:?}", content, tags));
        Node { id, content, tags, tokens }
    }
}

fn tokenize(text: &str) -> HashSet<String> {
    let lower_text = text.to_lowercase();
    let clean_text = TOKEN_REGEX.replace_all(&lower_text, " ");
    clean_text
        .split_whitespace()
        .filter(|w| w.len() > 2 && !STOPWORDS.contains(w))
        .map(|s| s.to_string())
        .collect()
}

fn calculate_similarity(n1: &Node, n2: &Node) -> f64 {
    // Tag similarity
    let tag_sim = if !n1.tags.is_empty() && !n2.tags.is_empty() {
        let s1: HashSet<_> = n1.tags.iter().collect();
        let s2: HashSet<_> = n2.tags.iter().collect();
        let intersection = s1.intersection(&s2).count();
        let union = s1.union(&s2).count();
        if union > 0 { intersection as f64 / union as f64 } else { 0.0 }
    } else {
        0.0
    };

    // Content similarity (Jaccard)
    let intersection = n1.tokens.intersection(&n2.tokens).count();
    let union = n1.tokens.union(&n2.tokens).count();
    let content_sim = if union > 0 { intersection as f64 / union as f64 } else { 0.0 };

    tag_sim * 0.3 + content_sim * 0.7
}

struct InvertedIndex {
    index: HashMap<String, Vec<usize>>,
}

impl InvertedIndex {
    fn new(nodes: &[Node]) -> Self {
        let mut index: HashMap<String, Vec<usize>> = HashMap::new();
        // Build index mapping tokens/tags to node indices
        for (i, node) in nodes.iter().enumerate() {
            for token in &node.tokens {
                index.entry(token.clone())
                    .or_default()
                    .push(i);
            }
            for tag in &node.tags {
                index.entry(tag.clone())
                    .or_default()
                    .push(i);
            }
        }
        InvertedIndex { index }
    }

    fn get_candidates(&self, node: &Node) -> HashSet<usize> {
        let mut candidates = HashSet::new();
        // Collect all nodes that share at least one token or tag
        for token in &node.tokens {
            if let Some(indices) = self.index.get(token) {
                candidates.extend(indices);
            }
        }
        for tag in &node.tags {
            if let Some(indices) = self.index.get(tag) {
                candidates.extend(indices);
            }
        }
        candidates
    }
}

pub fn find_connections(
    nodes: &[Node], 
    min_similarity: f64, 
    max_links: usize
) -> HashMap<String, Vec<(String, f64)>> {
    // 1. Build Inverted Index (O(N))
    let index = InvertedIndex::new(nodes);
    
    // 2. Find connections in parallel (O(N * K) where K << N)
    nodes.par_iter().enumerate().map(|(i, node)| {
        // Get candidates that share tokens/tags
        let candidates = index.get_candidates(node);
        
        let mut connections: Vec<(String, f64)> = candidates.iter()
            .filter(|&&other_idx| other_idx != i) // Exclude self
            .map(|&other_idx| &nodes[other_idx])
            .map(|other| (other.id.clone(), calculate_similarity(node, other)))
            .filter(|(_, sim)| *sim >= min_similarity)
            .collect();

        // Sort by similarity descending
        connections.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        connections.truncate(max_links);
        
        (node.id.clone(), connections)
    }).collect()
}

pub fn parallel_traverse(
    nodes: &[Node],
    start_id: &str,
    max_depth: usize,
    min_similarity: f64
) -> HashSet<String> {
    let mut visited = HashSet::new();
    let mut current_layer = HashSet::new();
    visited.insert(start_id.to_string());
    current_layer.insert(start_id.to_string());
    
    let node_map: HashMap<String, &Node> = nodes.iter()
        .map(|n| (n.id.clone(), n))
        .collect();
        
    for _ in 0..max_depth {
        if current_layer.is_empty() {
            break;
        }
        
        let next_layer: HashSet<String> = current_layer.par_iter()
            .filter_map(|id| node_map.get(id))
            .flat_map(|current_node| {
                // Find neighbors
                nodes.iter()
                    .filter(|other| {
                        if other.id == current_node.id { return false; }
                        let sim = calculate_similarity(current_node, other);
                        // if sim > 0.0 {
                        //    println!("DEBUG: {} -> {} = {:.3}", current_node.id, other.id, sim);
                        // }
                        sim >= min_similarity
                    })
                    .map(|other| other.id.clone())
                    .collect::<Vec<_>>()
            })
            .collect();
            
        // Add unvisited to visited and set as next layer
        current_layer.clear();
        for id in next_layer {
            if visited.insert(id.clone()) {
                current_layer.insert(id);
            }
        }
    }
    
    visited
}
