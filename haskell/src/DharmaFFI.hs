{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DharmaFFI
Description : C FFI surface for DharmaRules + DepGraph modules
Copyright   : (c) WhiteMagic Contributors, 2026
License     : MIT

JSON-based C FFI for Python integration. Functions accept and return
C strings containing JSON, which Python parses via ctypes + json.

Exported symbols:
  - @c_dharma_evaluate@: Evaluate an action against a rule profile
  - @c_dharma_evaluate_all@: Return all matching rules
  - @c_depgraph_plan@: Plan a tool execution chain
  - @c_depgraph_next_steps@: Get suggested next tools
  - @c_depgraph_topo_sort@: Topological sort of tool graph
  - @c_free_string@: Free a Haskell-allocated string
-}

module DharmaFFI
    ( c_dharma_evaluate
    , c_dharma_evaluate_all
    , c_depgraph_plan
    , c_depgraph_next_steps
    , c_depgraph_topo_sort
    , c_free_string
    ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr
import Data.Char (toLower)

import DharmaRules
import DepGraph

-- ---------------------------------------------------------------------------
-- String helpers
-- ---------------------------------------------------------------------------

-- | Allocate a new C string from a Haskell string (caller must free).
returnCString :: String -> IO CString
returnCString = newCString

-- ---------------------------------------------------------------------------
-- Dharma Rules FFI
-- ---------------------------------------------------------------------------

-- | Evaluate an action against rules for a given profile.
-- Input: tool_name, description, safety_level, profile ("default"|"creative"|"secure")
-- Output: JSON string with decision {action, severity, explain, rule}
foreign export ccall c_dharma_evaluate
    :: CString -> CString -> CString -> CString -> IO CString

c_dharma_evaluate :: CString -> CString -> CString -> CString -> IO CString
c_dharma_evaluate cTool cDesc cSafety cProfile = do
    tool    <- peekCString cTool
    desc    <- peekCString cDesc
    safety  <- peekCString cSafety
    profile <- peekCString cProfile

    let ctx = ActionContext tool desc safety
        rules = rulesForProfile (parseProfile profile)
        decision = evaluate rules ctx

    returnCString $ decisionToJson decision

-- | Evaluate all rules, return ALL matching decisions as JSON array.
foreign export ccall c_dharma_evaluate_all
    :: CString -> CString -> CString -> CString -> IO CString

c_dharma_evaluate_all :: CString -> CString -> CString -> CString -> IO CString
c_dharma_evaluate_all cTool cDesc cSafety cProfile = do
    tool    <- peekCString cTool
    desc    <- peekCString cDesc
    safety  <- peekCString cSafety
    profile <- peekCString cProfile

    let ctx = ActionContext tool desc safety
        rules = rulesForProfile (parseProfile profile)
        decisions = evaluateAll rules ctx

    returnCString $ "[" ++ commaJoin (map decisionToJson decisions) ++ "]"

-- ---------------------------------------------------------------------------
-- DepGraph FFI
-- ---------------------------------------------------------------------------

-- | Plan execution chain for a goal tool.
-- Output: JSON {chain: [tools_in_order], suggestions: [{tool, weight}]}
foreign export ccall c_depgraph_plan :: CString -> IO CString

c_depgraph_plan :: CString -> IO CString
c_depgraph_plan cGoal = do
    goal <- peekCString cGoal
    let g = fromEdgeList staticEdges
        (chain, suggs) = plan g goal
    returnCString $ planToJson chain suggs

-- | Get suggested next tools after a given tool.
-- Output: JSON array [{target, edge_type, weight}]
foreign export ccall c_depgraph_next_steps :: CString -> IO CString

c_depgraph_next_steps :: CString -> IO CString
c_depgraph_next_steps cTool = do
    tool <- peekCString cTool
    let g = fromEdgeList staticEdges
        steps = nextSteps g tool
    returnCString $ "[" ++ commaJoin (map stepToJson steps) ++ "]"

-- | Topological sort of all tools.
-- Output: JSON array of tool names, or {"error": "cycle detected"}
foreign export ccall c_depgraph_topo_sort :: IO CString

c_depgraph_topo_sort :: IO CString
c_depgraph_topo_sort = do
    let g = fromEdgeList staticEdges
    case topologicalSort g of
        Just sorted -> returnCString $ "[" ++ commaJoin (map jsonStr sorted) ++ "]"
        Nothing     -> returnCString "{\"error\": \"cycle detected in requires edges\"}"

-- | Free a Haskell-allocated C string.
foreign export ccall c_free_string :: CString -> IO ()

c_free_string :: CString -> IO ()
c_free_string = free

-- ---------------------------------------------------------------------------
-- JSON serialization helpers (no dependency needed for simple structures)
-- ---------------------------------------------------------------------------

parseProfile :: String -> DharmaProfile
parseProfile s = case map toLower s of
    "creative" -> ProfileCreative
    "secure"   -> ProfileSecure
    _          -> ProfileDefault

decisionToJson :: DharmaDecision -> String
decisionToJson d = concat
    [ "{\"action\": ", jsonStr (actionStr (decAction d))
    , ", \"severity\": ", show (decSeverity d)
    , ", \"explain\": ", jsonStr (decExplain d)
    , ", \"rule\": ", jsonStr (decRule d)
    , "}"
    ]

actionStr :: DharmaAction -> String
actionStr ActionLog      = "log"
actionStr ActionTag      = "tag"
actionStr ActionWarn     = "warn"
actionStr ActionThrottle = "throttle"
actionStr ActionBlock    = "block"

planToJson :: [String] -> [(String, Double)] -> String
planToJson chain suggs = concat
    [ "{\"chain\": [", commaJoin (map jsonStr chain)
    , "], \"suggestions\": [", commaJoin (map suggToJson suggs)
    , "]}"
    ]

suggToJson :: (String, Double) -> String
suggToJson (tool, w) = concat
    ["{\"tool\": ", jsonStr tool, ", \"weight\": ", show w, "}"]

stepToJson :: (String, EdgeType, Double) -> String
stepToJson (tgt, et, w) = concat
    [ "{\"target\": ", jsonStr tgt
    , ", \"edge_type\": ", jsonStr (etStr et)
    , ", \"weight\": ", show w, "}"
    ]

etStr :: EdgeType -> String
etStr Requires = "requires"
etStr Suggests = "suggests"
etStr Provides = "provides"

jsonStr :: String -> String
jsonStr s = "\"" ++ escapeJson s ++ "\""

escapeJson :: String -> String
escapeJson [] = []
escapeJson ('"':cs) = '\\' : '"' : escapeJson cs
escapeJson ('\\':cs) = '\\' : '\\' : escapeJson cs
escapeJson ('\n':cs) = '\\' : 'n' : escapeJson cs
escapeJson (c:cs) = c : escapeJson cs

commaJoin :: [String] -> String
commaJoin [] = ""
commaJoin [x] = x
commaJoin (x:xs) = x ++ ", " ++ commaJoin xs
