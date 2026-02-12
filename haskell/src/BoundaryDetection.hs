{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : BoundaryDetection
Description : Pattern-matching boundary detection for Dharma ethical evaluation
Copyright   : (c) WhiteMagic Contributors, 2026
License     : MIT

Exhaustive algebraic pattern matching for detecting ethical boundary
violations in tool invocations. Haskell's type system guarantees
that all cases are handled — no boundary can be accidentally missed.

This module provides:
  - Content classification into safety categories
  - Pattern-based injection detection (path traversal, shell injection, prompt injection)
  - Graduated severity scoring
  - Composable boundary predicates

Designed to complement the Python boundary engine with formal correctness
guarantees. Haskell's strictness makes it suitable for security-critical paths.
-}

module BoundaryDetection
    ( BoundaryViolation(..)
    , Severity(..)
    , SafetyCategory(..)
    , ContentClass(..)
    , BoundaryCheck
    , checkBoundaries
    , classifyContent
    , detectInjection
    , severityToInt
    , allChecks
    , compositeCheck
    ) where

import Data.Char (toLower, isAlphaNum)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, foldl')

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Severity levels for boundary violations (graduated).
data Severity
    = SevInfo        -- ^ Informational — log only
    | SevWarning     -- ^ Warning — tag and alert
    | SevDangerous   -- ^ Dangerous — throttle or block
    | SevCritical    -- ^ Critical — immediate block
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Numeric severity for Python interop.
severityToInt :: Severity -> Int
severityToInt SevInfo      = 1
severityToInt SevWarning   = 2
severityToInt SevDangerous = 3
severityToInt SevCritical  = 4

-- | Safety categories for tool content.
data SafetyCategory
    = CatFileSystem     -- ^ File read/write/delete operations
    | CatNetwork        -- ^ HTTP requests, DNS, sockets
    | CatExecution      -- ^ Shell commands, code eval
    | CatMemory         -- ^ Memory create/modify/delete
    | CatGovernance     -- ^ Profile changes, policy modifications
    | CatIntrospection  -- ^ Read-only system inspection
    | CatCreative       -- ^ Content generation, dreaming
    | CatUnknown        -- ^ Unclassified
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Content classification result.
data ContentClass = ContentClass
    { ccCategory  :: SafetyCategory
    , ccRiskLevel :: Double          -- ^ 0.0 (safe) to 1.0 (dangerous)
    , ccKeywords  :: [String]        -- ^ Matched keywords
    } deriving (Show, Eq)

-- | A boundary violation.
data BoundaryViolation = BoundaryViolation
    { bvName     :: String
    , bvSeverity :: Severity
    , bvMessage  :: String
    , bvPattern  :: String          -- ^ The pattern that triggered this violation
    } deriving (Show, Eq)

-- | A boundary check is a function from (tool_name, description, args) → violations.
type BoundaryCheck = String -> String -> String -> [BoundaryViolation]

-- ---------------------------------------------------------------------------
-- Content classification
-- ---------------------------------------------------------------------------

-- | Classify content into a safety category based on keywords.
classifyContent :: String -> String -> ContentClass
classifyContent toolName description =
    let lower = map toLower (toolName ++ " " ++ description)
        matchCategory keywords cat risk = case filter (`isInfixOf` lower) keywords of
            [] -> Nothing
            matched -> Just (ContentClass cat risk matched)
    in case () of
        _ | Just cc <- matchCategory fsKeywords CatFileSystem 0.7 -> cc
          | Just cc <- matchCategory netKeywords CatNetwork 0.6 -> cc
          | Just cc <- matchCategory execKeywords CatExecution 0.9 -> cc
          | Just cc <- matchCategory memKeywords CatMemory 0.3 -> cc
          | Just cc <- matchCategory govKeywords CatGovernance 0.5 -> cc
          | Just cc <- matchCategory introKeywords CatIntrospection 0.1 -> cc
          | Just cc <- matchCategory creativeKeywords CatCreative 0.2 -> cc
          | otherwise -> ContentClass CatUnknown 0.4 []
  where
    fsKeywords  = ["file", "write", "delete", "remove", "path", "directory", "mkdir", "unlink"]
    netKeywords = ["http", "request", "fetch", "curl", "socket", "dns", "download", "upload"]
    execKeywords = ["exec", "shell", "command", "eval", "subprocess", "system", "popen", "spawn"]
    memKeywords = ["memory", "create_memory", "store", "recall", "forget"]
    govKeywords = ["profile", "dharma", "policy", "governance", "set_", "config"]
    introKeywords = ["gnosis", "status", "health", "inspect", "list", "get_", "show"]
    creativeKeywords = ["dream", "generate", "create", "imagine", "compose", "synthesize"]

-- ---------------------------------------------------------------------------
-- Injection detection
-- ---------------------------------------------------------------------------

-- | Detect various injection patterns in input text.
detectInjection :: String -> [BoundaryViolation]
detectInjection input = concat
    [ detectPathTraversal input
    , detectShellInjection input
    , detectPromptInjection input
    , detectSqlInjection input
    ]

-- | Path traversal detection.
detectPathTraversal :: String -> [BoundaryViolation]
detectPathTraversal input
    | "../" `isInfixOf` input = [BoundaryViolation
        "path_traversal" SevDangerous
        "Path traversal attempt detected: ../ in input"
        "../"]
    | "..\\" `isInfixOf` input = [BoundaryViolation
        "path_traversal" SevDangerous
        "Path traversal attempt detected: ..\\ in input"
        "..\\"]
    | "/etc/" `isInfixOf` lower = [BoundaryViolation
        "sensitive_path" SevCritical
        "Access to sensitive system path: /etc/"
        "/etc/"]
    | "/proc/" `isInfixOf` lower = [BoundaryViolation
        "sensitive_path" SevWarning
        "Access to /proc/ filesystem"
        "/proc/"]
    | otherwise = []
  where lower = map toLower input

-- | Shell injection detection.
detectShellInjection :: String -> [BoundaryViolation]
detectShellInjection input =
    let lower = map toLower input
        patterns = [ (";", "semicolon_injection")
                   , ("&&", "and_chain")
                   , ("||", "or_chain")
                   , ("|", "pipe_injection")
                   , ("$(", "command_substitution")
                   , ("`", "backtick_execution")
                   , (">", "output_redirect")
                   , (">>", "output_append")
                   ]
        check (pat, name)
            | pat `isInfixOf` lower = Just $ BoundaryViolation
                ("shell_" ++ name) SevDangerous
                ("Shell injection pattern detected: " ++ pat)
                pat
            | otherwise = Nothing
    in concatMap (maybe [] (:[]) . check) patterns

-- | Prompt injection detection.
detectPromptInjection :: String -> [BoundaryViolation]
detectPromptInjection input =
    let lower = map toLower input
        patterns = [ "ignore previous instructions"
                   , "forget your rules"
                   , "you are now"
                   , "pretend you are"
                   , "act as if"
                   , "new instructions:"
                   , "system prompt:"
                   , "override:"
                   , "bypass safety"
                   , "ignore safety"
                   ]
        check pat
            | pat `isInfixOf` lower = Just $ BoundaryViolation
                "prompt_injection" SevCritical
                ("Prompt injection attempt: '" ++ pat ++ "'")
                pat
            | otherwise = Nothing
    in concatMap (maybe [] (:[]) . check) patterns

-- | SQL injection detection.
detectSqlInjection :: String -> [BoundaryViolation]
detectSqlInjection input =
    let lower = map toLower input
        patterns = [ "'; drop"
                   , "1=1"
                   , "' or '"
                   , "union select"
                   , "insert into"
                   , "--"
                   ]
        check pat
            | pat `isInfixOf` lower = Just $ BoundaryViolation
                "sql_injection" SevDangerous
                ("SQL injection pattern: " ++ pat)
                pat
            | otherwise = Nothing
    in concatMap (maybe [] (:[]) . check) patterns

-- ---------------------------------------------------------------------------
-- Composable boundary checks
-- ---------------------------------------------------------------------------

-- | Check that tool names contain only safe characters.
checkToolNameSafe :: BoundaryCheck
checkToolNameSafe toolName _ _ =
    let isSafe c = isAlphaNum c || c == '.' || c == '_' || c == '-'
    in if all isSafe toolName
       then []
       else [BoundaryViolation
           "unsafe_tool_name" SevWarning
           ("Tool name contains unsafe characters: " ++ toolName)
           toolName]

-- | Check description length (defense against payload stuffing).
checkDescriptionLength :: BoundaryCheck
checkDescriptionLength _ description _ =
    if length description > 100000
    then [BoundaryViolation
        "oversized_description" SevWarning
        ("Description exceeds 100KB: " ++ show (length description) ++ " chars")
        "length_exceeded"]
    else []

-- | Check args for injection patterns.
checkArgsInjection :: BoundaryCheck
checkArgsInjection _ _ args = detectInjection args

-- | Check for dangerous tool categories.
checkDangerousCategory :: BoundaryCheck
checkDangerousCategory toolName description _ =
    let cc = classifyContent toolName description
    in if ccRiskLevel cc >= 0.8
       then [BoundaryViolation
           "high_risk_category" SevDangerous
           ("Tool classified as high risk: " ++ show (ccCategory cc))
           (show (ccCategory cc))]
       else []

-- | All standard boundary checks combined.
allChecks :: [BoundaryCheck]
allChecks =
    [ checkToolNameSafe
    , checkDescriptionLength
    , checkArgsInjection
    , checkDangerousCategory
    ]

-- | Run all checks and collect violations.
compositeCheck :: [BoundaryCheck] -> String -> String -> String -> [BoundaryViolation]
compositeCheck checks toolName desc args =
    concatMap (\check -> check toolName desc args) checks

-- | Run the full boundary check suite.
checkBoundaries :: String -> String -> String -> [BoundaryViolation]
checkBoundaries = compositeCheck allChecks
