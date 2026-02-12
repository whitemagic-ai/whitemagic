{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : DharmaRules
Description : Pure-functional Dharma Rules Engine
Copyright   : (c) WhiteMagic Contributors, 2026
License     : MIT

Type-safe, declarative ethical policy evaluation.
Ports the core logic from Python's @dharma/rules.py@ into Haskell,
gaining:

  * Exhaustive pattern matching on the action spectrum
  * Total functions (no runtime crashes from unhandled cases)
  * Referential transparency (same input → same output, always)
  * Composable rule evaluation via monadic chaining

The graduated action spectrum (Gevurah → Chesed):

  @LOG → TAG → WARN → THROTTLE → BLOCK@

Rules match against structured action dicts via:
  - Tool name glob patterns
  - Keyword substring matching
  - Safety level matching
  - Regex patterns on full action text
-}

module DharmaRules
    ( -- * Types
      DharmaAction(..)
    , DharmaRule(..)
    , DharmaDecision(..)
    , DharmaProfile(..)
    , ActionContext(..)

      -- * Evaluation
    , evaluate
    , evaluateAll
    , worstAction

      -- * Built-in Rules
    , defaultRules
    , creativeRules
    , secureRules
    , rulesForProfile

      -- * Utilities
    , actionSeverityOrd
    , isBlocking
    , isWarningOrWorse
    ) where

import Data.Char (toLower)
import Data.List (isInfixOf, foldl', sortBy)
import Data.Ord (Down(..))

-- ---------------------------------------------------------------------------
-- Action Spectrum (Gevurah → Chesed)
-- ---------------------------------------------------------------------------

-- | Graduated action spectrum from least to most restrictive.
data DharmaAction
    = ActionLog       -- ^ Record silently
    | ActionTag       -- ^ Apply a dharma tag
    | ActionWarn      -- ^ Proceed with warning
    | ActionThrottle  -- ^ Rate-limit the action
    | ActionBlock     -- ^ Deny entirely
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert action to string representation.
actionToStr :: DharmaAction -> String
actionToStr ActionLog      = "log"
actionToStr ActionTag      = "tag"
actionToStr ActionWarn     = "warn"
actionToStr ActionThrottle = "throttle"
actionToStr ActionBlock    = "block"

-- | Parse action from string.
actionFromStr :: String -> Maybe DharmaAction
actionFromStr s = case map toLower s of
    "log"      -> Just ActionLog
    "tag"      -> Just ActionTag
    "warn"     -> Just ActionWarn
    "throttle" -> Just ActionThrottle
    "block"    -> Just ActionBlock
    _          -> Nothing

-- | Numeric severity ordering (higher = more severe).
actionSeverityOrd :: DharmaAction -> Int
actionSeverityOrd ActionLog      = 0
actionSeverityOrd ActionTag      = 1
actionSeverityOrd ActionWarn     = 2
actionSeverityOrd ActionThrottle = 3
actionSeverityOrd ActionBlock    = 4

-- | Is this action blocking?
isBlocking :: DharmaAction -> Bool
isBlocking ActionBlock = True
isBlocking _           = False

-- | Is this warning-level or worse?
isWarningOrWorse :: DharmaAction -> Bool
isWarningOrWorse a = actionSeverityOrd a >= actionSeverityOrd ActionWarn

-- ---------------------------------------------------------------------------
-- Profiles
-- ---------------------------------------------------------------------------

data DharmaProfile
    = ProfileDefault   -- ^ Balanced governance
    | ProfileCreative  -- ^ Relaxed for creative work
    | ProfileSecure    -- ^ Strict for production safety
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Rule Definition
-- ---------------------------------------------------------------------------

data DharmaRule = DharmaRule
    { ruleName         :: String
    , ruleDescription  :: String
    , ruleAction       :: DharmaAction
    , ruleSeverity     :: Double          -- 0.0 – 1.0
    , ruleExplain      :: String          -- Human-readable reason
    , ruleProfile      :: DharmaProfile
    , ruleToolPatterns :: [String]        -- Glob patterns on tool name
    , ruleKeywords     :: [String]        -- Substring match on description
    , ruleSafetyLevels :: [String]        -- "WRITE", "DELETE", etc.
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Action Context (input to evaluation)
-- ---------------------------------------------------------------------------

data ActionContext = ActionContext
    { actTool        :: String   -- ^ Tool name being invoked
    , actDescription :: String   -- ^ Description/intent
    , actSafety      :: String   -- ^ Declared safety level
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Decision (output of evaluation)
-- ---------------------------------------------------------------------------

data DharmaDecision = DharmaDecision
    { decAction   :: DharmaAction
    , decSeverity :: Double
    , decExplain  :: String
    , decRule     :: String       -- ^ Name of the rule that matched
    } deriving (Show)

-- | The "all clear" decision when no rules match.
allClear :: DharmaDecision
allClear = DharmaDecision ActionLog 0.0 "No rules triggered" "none"

-- ---------------------------------------------------------------------------
-- Pattern Matching
-- ---------------------------------------------------------------------------

-- | Simple glob matching (supports * and ?).
globMatch :: String -> String -> Bool
globMatch []     []     = True
globMatch ('*':ps) s    = any (globMatch ps) (tails s)
  where tails []     = [[]]
        tails xs@(_:rest) = xs : tails rest
globMatch ('?':ps) (_:ss) = globMatch ps ss
globMatch (p:ps)   (s:ss) = toLower p == toLower s && globMatch ps ss
globMatch _        _      = False

-- | Check if a rule matches a given action context.
ruleMatches :: DharmaRule -> ActionContext -> Bool
ruleMatches rule ctx =
    matchesToolPattern || matchesKeyword || matchesSafety
  where
    toolLower = map toLower (actTool ctx)
    descLower = map toLower (actDescription ctx)
    safetyUpper = map toLower (actSafety ctx)

    matchesToolPattern = any (\pat -> globMatch (map toLower pat) toolLower)
                             (ruleToolPatterns rule)

    matchesKeyword = any (\kw -> map toLower kw `isInfixOf` descLower)
                         (ruleKeywords rule)

    matchesSafety = any (\sl -> map toLower sl == safetyUpper)
                        (ruleSafetyLevels rule)

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a single rule against an action context.
evaluateRule :: DharmaRule -> ActionContext -> Maybe DharmaDecision
evaluateRule rule ctx
    | ruleMatches rule ctx = Just DharmaDecision
        { decAction   = ruleAction rule
        , decSeverity = ruleSeverity rule
        , decExplain  = ruleExplain rule
        , decRule     = ruleName rule
        }
    | otherwise = Nothing

-- | Evaluate all rules and return the most severe matching decision.
evaluate :: [DharmaRule] -> ActionContext -> DharmaDecision
evaluate rules ctx =
    case decisions of
        [] -> allClear
        ds -> foldl' worstDecision (head ds) (tail ds)
  where
    decisions = [d | rule <- rules, Just d <- [evaluateRule rule ctx]]

-- | Evaluate all rules and return ALL matching decisions (sorted by severity).
evaluateAll :: [DharmaRule] -> ActionContext -> [DharmaDecision]
evaluateAll rules ctx =
    sortBy (\a b -> compare (Down (decSeverity a)) (Down (decSeverity b))) decisions
  where
    decisions = [d | rule <- rules, Just d <- [evaluateRule rule ctx]]

-- | Pick the more severe of two decisions.
worstDecision :: DharmaDecision -> DharmaDecision -> DharmaDecision
worstDecision a b
    | actionSeverityOrd (decAction a) >= actionSeverityOrd (decAction b) = a
    | otherwise = b

-- | Pick the worst action from a list.
worstAction :: [DharmaAction] -> DharmaAction
worstAction [] = ActionLog
worstAction as = foldl' (\a b -> if actionSeverityOrd a >= actionSeverityOrd b then a else b)
                        ActionLog as

-- ---------------------------------------------------------------------------
-- Built-in Rules
-- ---------------------------------------------------------------------------

-- | Default profile rules (balanced).
defaultRules :: [DharmaRule]
defaultRules =
    [ DharmaRule "destructive_ops" "Block destructive operations without consent"
        ActionBlock 0.9 "Destructive operations require explicit consent"
        ProfileDefault
        ["*delete*", "*destroy*", "*purge*"] ["delete", "destroy", "purge", "wipe"] ["DELETE"]

    , DharmaRule "write_awareness" "Tag write operations for karma tracking"
        ActionTag 0.3 "Write operations tracked in karma ledger"
        ProfileDefault
        [] [] ["WRITE"]

    , DharmaRule "system_modification" "Warn on system configuration changes"
        ActionWarn 0.6 "System modifications need careful review"
        ProfileDefault
        ["set_dharma_profile", "governor_set_goal"] ["system", "config", "modify"] []

    , DharmaRule "external_requests" "Throttle external network requests"
        ActionThrottle 0.5 "External requests are rate-limited for safety"
        ProfileDefault
        ["*external*", "*network*", "*http*"] ["external", "fetch", "download"] []

    , DharmaRule "sensitive_data" "Warn on access to sensitive data"
        ActionWarn 0.7 "Sensitive data access requires awareness"
        ProfileDefault
        ["*secret*", "*credential*", "*password*"] ["secret", "credential", "password", "token"] []
    ]

-- | Creative profile rules (relaxed).
creativeRules :: [DharmaRule]
creativeRules =
    [ DharmaRule "creative_destructive" "Warn instead of block for destructive ops"
        ActionWarn 0.5 "Creative mode: destructive ops allowed with warning"
        ProfileCreative
        ["*delete*", "*destroy*"] ["delete", "destroy"] ["DELETE"]

    , DharmaRule "creative_write" "Log writes silently in creative mode"
        ActionLog 0.1 "Creative mode: writes logged silently"
        ProfileCreative
        [] [] ["WRITE"]
    ]

-- | Secure profile rules (strict).
secureRules :: [DharmaRule]
secureRules =
    [ DharmaRule "secure_all_writes" "Warn on ALL write operations"
        ActionWarn 0.7 "Secure mode: all writes require review"
        ProfileSecure
        [] [] ["WRITE"]

    , DharmaRule "secure_destructive" "Block ALL destructive operations"
        ActionBlock 1.0 "Secure mode: destructive operations forbidden"
        ProfileSecure
        ["*delete*", "*destroy*", "*purge*", "*modify*"]
        ["delete", "destroy", "purge", "wipe", "remove", "modify"]
        ["DELETE"]

    , DharmaRule "secure_external" "Block external requests"
        ActionBlock 0.8 "Secure mode: external requests blocked"
        ProfileSecure
        ["*external*", "*network*", "*http*"] ["external", "fetch", "download", "upload"] []
    ]

-- | Get rules for a given profile.
rulesForProfile :: DharmaProfile -> [DharmaRule]
rulesForProfile ProfileDefault  = defaultRules
rulesForProfile ProfileCreative = creativeRules
rulesForProfile ProfileSecure   = secureRules ++ defaultRules
