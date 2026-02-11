{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DepGraph
Description : Pure-functional Tool Dependency Graph & Planner
Copyright   : (c) WhiteMagic Contributors, 2026
License     : MIT

Directed graph of tool relationships for AI planning.
Ports the core logic from Python's @tools/dependency_graph.py@.

Edge types:
  - @requires@: hard dependency (A must run before B)
  - @suggests@: soft affinity (A often chains into B)
  - @provides@: data flow (A produces data consumed by B)

Haskell advantages:
  - Topological sort via algebraic graph theory
  - Cycle detection is a pure function
  - Plan generation via BFS with no mutable state
  - Total functions: all edge types handled exhaustively
-}

module DepGraph
    ( -- * Types
      EdgeType(..)
    , ToolEdge(..)
    , ToolGraph(..)

      -- * Construction
    , emptyGraph
    , addEdge
    , addEdges
    , fromEdgeList

      -- * Queries
    , nextSteps
    , prerequisites
    , dependencyChain
    , allTools
    , edgesFrom
    , edgesTo

      -- * Planning
    , plan
    , topologicalSort
    , hasCycle

      -- * Built-in static edges
    , staticEdges
    ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data EdgeType
    = Requires  -- ^ Hard dependency: A must run before B
    | Suggests  -- ^ Soft affinity: A often chains into B
    | Provides  -- ^ Data flow: A produces data consumed by B
    deriving (Show, Eq, Ord)

edgeTypeToStr :: EdgeType -> String
edgeTypeToStr Requires = "requires"
edgeTypeToStr Suggests = "suggests"
edgeTypeToStr Provides = "provides"

data ToolEdge = ToolEdge
    { edgeSource      :: String
    , edgeTarget      :: String
    , edgeType        :: EdgeType
    , edgeWeight      :: Double     -- 0.0-1.0 confidence
    , edgeDescription :: String
    } deriving (Show)

-- | Adjacency-list graph: forward + reverse maps for O(1) lookups.
data ToolGraph = ToolGraph
    { graphForward  :: Map String [(String, ToolEdge)]
    , graphReverse  :: Map String [(String, ToolEdge)]
    , graphAllTools :: Set String
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

emptyGraph :: ToolGraph
emptyGraph = ToolGraph Map.empty Map.empty Set.empty

addEdge :: ToolEdge -> ToolGraph -> ToolGraph
addEdge edge g = ToolGraph
    { graphForward  = Map.insertWith (++) src [(tgt, edge)] (graphForward g)
    , graphReverse  = Map.insertWith (++) tgt [(src, edge)] (graphReverse g)
    , graphAllTools = Set.insert src $ Set.insert tgt $ graphAllTools g
    }
  where
    src = edgeSource edge
    tgt = edgeTarget edge

addEdges :: [ToolEdge] -> ToolGraph -> ToolGraph
addEdges edges g = foldl' (flip addEdge) g edges

fromEdgeList :: [ToolEdge] -> ToolGraph
fromEdgeList edges = addEdges edges emptyGraph

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | What tools commonly follow this one? (outgoing edges)
nextSteps :: ToolGraph -> String -> [(String, EdgeType, Double)]
nextSteps g tool =
    map (\(tgt, e) -> (tgt, edgeType e, edgeWeight e)) $
    fromMaybe [] $ Map.lookup tool (graphForward g)

-- | What must run before this tool? (incoming 'requires' edges only)
prerequisites :: ToolGraph -> String -> [String]
prerequisites g tool =
    [ src | (src, e) <- fromMaybe [] (Map.lookup tool (graphReverse g))
          , edgeType e == Requires ]

-- | Get all edges originating from a tool.
edgesFrom :: ToolGraph -> String -> [ToolEdge]
edgesFrom g tool = map snd $ fromMaybe [] $ Map.lookup tool (graphForward g)

-- | Get all edges pointing to a tool.
edgesTo :: ToolGraph -> String -> [ToolEdge]
edgesTo g tool = map snd $ fromMaybe [] $ Map.lookup tool (graphReverse g)

-- | All known tool names.
allTools :: ToolGraph -> [String]
allTools = Set.toList . graphAllTools

-- ---------------------------------------------------------------------------
-- Dependency Chain (BFS backwards through 'requires' edges)
-- ---------------------------------------------------------------------------

-- | Find the full dependency chain to reach a goal tool.
-- Follows 'requires' edges backwards. Returns in execution order.
dependencyChain :: ToolGraph -> String -> [String]
dependencyChain g goal = reverse $ bfs [goal] Set.empty []
  where
    bfs [] _ acc = acc
    bfs (t:queue) visited acc
        | Set.member t visited = bfs queue visited acc
        | otherwise =
            let visited' = Set.insert t visited
                prereqs = prerequisites g t
                newQueue = queue ++ filter (`Set.notMember` visited') prereqs
            in bfs newQueue visited' (t : acc)

-- ---------------------------------------------------------------------------
-- Planning
-- ---------------------------------------------------------------------------

-- | Generate a plan: dependency chain + suggested follow-ups.
-- Returns (prerequisites_in_order, suggested_followups_with_weights).
plan :: ToolGraph -> String -> ([String], [(String, Double)])
plan g goal = (chain, suggestions)
  where
    chain = dependencyChain g goal
    suggestions = [ (tgt, w) | (tgt, Suggests, w) <- nextSteps g goal ]
               ++ [ (tgt, w) | (tgt, Provides, w) <- nextSteps g goal ]

-- ---------------------------------------------------------------------------
-- Topological Sort (Kahn's algorithm, pure)
-- ---------------------------------------------------------------------------

-- | Topological sort based on 'requires' edges.
-- Returns Nothing if the graph contains a cycle.
topologicalSort :: ToolGraph -> Maybe [String]
topologicalSort g = go initDegrees initQueue []
  where
    tools = allTools g

    -- Count incoming 'requires' edges per tool
    initDegrees :: Map String Int
    initDegrees = foldl' (\m t ->
        Map.insert t (length (prerequisites g t)) m) Map.empty tools

    -- Tools with zero prerequisites
    initQueue :: [String]
    initQueue = [t | t <- tools, Map.findWithDefault 0 t initDegrees == 0]

    go :: Map String Int -> [String] -> [String] -> Maybe [String]
    go _ [] sorted
        | length sorted == length tools = Just (reverse sorted)
        | otherwise                     = Nothing  -- cycle
    go degrees (t:rest) sorted =
        let sorted' = t : sorted
            -- Outgoing 'requires' edges from t
            succs = [ tgt | (tgt, e) <- fromMaybe [] (Map.lookup t (graphForward g))
                          , edgeType e == Requires ]
            -- Decrement in-degree
            degrees' = foldl' (\m s -> Map.adjust (subtract 1) s m) degrees succs
            -- Newly ready tools
            newReady = [ s | s <- succs
                           , Map.findWithDefault 1 s degrees' == 0
                           , s `notElem` rest
                           , s `notElem` sorted' ]
        in go degrees' (rest ++ newReady) sorted'

-- | Check if the graph has a cycle in its 'requires' edges.
hasCycle :: ToolGraph -> Bool
hasCycle g = case topologicalSort g of
    Nothing -> True
    Just _  -> False

-- ---------------------------------------------------------------------------
-- Built-in Static Edges (matches Python's _STATIC_EDGES)
-- ---------------------------------------------------------------------------

staticEdges :: [ToolEdge]
staticEdges =
    [ -- Memory lifecycle
      ToolEdge "create_memory" "search_memory" Suggests 0.7
        "After creating, often search to verify"
    , ToolEdge "search_memory" "get_memory" Suggests 0.8
        "Search results lead to detail fetch"
    , ToolEdge "get_memory" "update_memory" Suggests 0.5
        "Reading a memory may lead to updates"
    , ToolEdge "memory.lifecycle_sweep" "memory.lifecycle_stats" Suggests 0.9
        "Check stats after running a sweep"
    , ToolEdge "memory.consolidate" "memory.consolidation_stats" Suggests 0.9
        "Check stats after consolidation"

      -- Voting workflow
    , ToolEdge "vote.create" "vote.cast" Requires 1.0
        "Must create a session before casting votes"
    , ToolEdge "vote.cast" "vote.analyze" Suggests 0.8
        "After voting, analyze results"
    , ToolEdge "vote.analyze" "vote.record_outcome" Suggests 0.6
        "After analysis, record whether solution worked"

      -- Agent lifecycle
    , ToolEdge "agent.register" "agent.heartbeat" Suggests 0.7
        "Registered agents should send heartbeats"
    , ToolEdge "agent.register" "agent.capabilities" Suggests 0.5
        "After registration, check capabilities"

      -- Pipeline workflow
    , ToolEdge "pipeline.create" "pipeline.status" Suggests 0.9
        "Check pipeline status after creation"

      -- Galactic operations
    , ToolEdge "memory.lifecycle_sweep" "memory.consolidate" Suggests 0.6
        "Sweep often followed by consolidation"
    , ToolEdge "memory.consolidate" "memory.lifecycle_sweep" Suggests 0.4
        "Consolidation may trigger another sweep"

      -- Dharma + Governance
    , ToolEdge "dharma.reload" "maturity.assess" Suggests 0.5
        "After reloading dharma rules, check maturity"
    , ToolEdge "homeostasis.check" "homeostasis.status" Suggests 0.9
        "Check status after running homeostasis"

      -- Dream cycle
    , ToolEdge "dream_start" "dream_status" Suggests 0.9
        "Check dream status after starting"
    , ToolEdge "dream_stop" "dream_status" Suggests 0.8
        "Verify dream stopped"

      -- Introspection chains
    , ToolEdge "gnosis" "salience.spotlight" Suggests 0.6
        "After gnosis snapshot, spotlight salient items"
    , ToolEdge "salience.spotlight" "reasoning.bicameral" Suggests 0.5
        "Spotlight leads to deeper reasoning"
    , ToolEdge "tool.graph" "tool.graph_full" Suggests 0.7
        "After quick graph view, get full graph"
    ]
