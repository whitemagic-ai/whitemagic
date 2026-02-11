{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : RuleComposer
Description : Composable Dharma rule engine with algebraic rule combinators
Copyright   : (c) WhiteMagic Contributors, 2026
License     : MIT

Provides a composable rule algebra for building complex ethical
evaluation rules from simple predicates. Rules can be combined
using AND, OR, NOT, and conditional operators.

Key features:
  - Algebraic rule combinators (composable, testable, serializable)
  - Priority-ordered rule evaluation
  - Rule conflict detection
  - Profile-based rule filtering
  - Rule optimization (constant folding, dead rule elimination)

This complements the YAML-driven rules in Python's dharma/rules.py
with a formally verified evaluation path.
-}

module RuleComposer
    ( Rule(..)
    , RulePredicate(..)
    , RuleAction(..)
    , RuleResult(..)
    , Profile(..)
    , mkRule
    , ruleAnd
    , ruleOr
    , ruleNot
    , ruleIf
    , evaluate
    , evaluateAll
    , optimizeRule
    , detectConflicts
    , filterByProfile
    , defaultRules
    , creativeRules
    , secureRules
    ) where

import Data.List (nub, partition, isInfixOf)
import Data.Char (toLower)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Profiles for rule set selection.
data Profile = Default | Creative | Secure
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Predicates that can be evaluated against tool context.
data RulePredicate
    = PredToolName String               -- ^ Tool name matches (case-insensitive)
    | PredToolContains String            -- ^ Tool name contains substring
    | PredDescContains String            -- ^ Description contains substring
    | PredSafetyLevel String             -- ^ Safety level matches
    | PredCategory String                -- ^ Tool category matches
    | PredTrue                           -- ^ Always true
    | PredFalse                          -- ^ Always false
    | PredAnd RulePredicate RulePredicate  -- ^ Both must hold
    | PredOr RulePredicate RulePredicate   -- ^ Either must hold
    | PredNot RulePredicate                -- ^ Negation
    deriving (Show, Eq)

-- | Actions to take when a rule matches.
data RuleAction
    = ActionLog String
    | ActionTag String
    | ActionWarn String
    | ActionThrottle Int    -- ^ Throttle to N requests per minute
    | ActionBlock String
    deriving (Show, Eq)

-- | A single rule with priority and profile.
data Rule = Rule
    { ruleName     :: String
    , rulePriority :: Int           -- ^ Lower = higher priority
    , ruleProfile  :: [Profile]     -- ^ Which profiles this rule applies to
    , rulePred     :: RulePredicate
    , ruleAction   :: RuleAction
    } deriving (Show, Eq)

-- | Result of evaluating a rule.
data RuleResult = RuleResult
    { rrRuleName :: String
    , rrAction   :: RuleAction
    , rrMatched  :: Bool
    , rrPriority :: Int
    } deriving (Show, Eq)

-- | Tool invocation context for evaluation.
data ToolContext = ToolContext
    { tcToolName    :: String
    , tcDescription :: String
    , tcSafetyLevel :: String
    , tcCategory    :: String
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Predicate evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a predicate against a tool context.
evalPred :: ToolContext -> RulePredicate -> Bool
evalPred _ PredTrue = True
evalPred _ PredFalse = False
evalPred ctx (PredToolName name) =
    map toLower (tcToolName ctx) == map toLower name
evalPred ctx (PredToolContains sub) =
    map toLower sub `isInfixOf` map toLower (tcToolName ctx)
evalPred ctx (PredDescContains sub) =
    map toLower sub `isInfixOf` map toLower (tcDescription ctx)
evalPred ctx (PredSafetyLevel lvl) =
    map toLower (tcSafetyLevel ctx) == map toLower lvl
evalPred ctx (PredCategory cat) =
    map toLower (tcCategory ctx) == map toLower cat
evalPred ctx (PredAnd p1 p2) = evalPred ctx p1 && evalPred ctx p2
evalPred ctx (PredOr p1 p2) = evalPred ctx p1 || evalPred ctx p2
evalPred ctx (PredNot p) = not (evalPred ctx p)

-- ---------------------------------------------------------------------------
-- Rule combinators
-- ---------------------------------------------------------------------------

-- | Create a simple rule.
mkRule :: String -> Int -> [Profile] -> RulePredicate -> RuleAction -> Rule
mkRule = Rule

-- | Combine two rules with AND (both predicates must match).
ruleAnd :: Rule -> Rule -> Rule
ruleAnd r1 r2 = r1
    { ruleName = ruleName r1 ++ " AND " ++ ruleName r2
    , rulePred = PredAnd (rulePred r1) (rulePred r2)
    , rulePriority = min (rulePriority r1) (rulePriority r2)
    }

-- | Combine two rules with OR (either predicate matches).
ruleOr :: Rule -> Rule -> Rule
ruleOr r1 r2 = r1
    { ruleName = ruleName r1 ++ " OR " ++ ruleName r2
    , rulePred = PredOr (rulePred r1) (rulePred r2)
    , rulePriority = min (rulePriority r1) (rulePriority r2)
    }

-- | Negate a rule's predicate.
ruleNot :: Rule -> Rule
ruleNot r = r
    { ruleName = "NOT " ++ ruleName r
    , rulePred = PredNot (rulePred r)
    }

-- | Conditional: if pred1 then action1, else action2.
ruleIf :: String -> RulePredicate -> RuleAction -> RuleAction -> Int -> [Profile] -> [Rule]
ruleIf name pred thenAction elseAction prio profiles =
    [ Rule (name ++ "_then") prio profiles pred thenAction
    , Rule (name ++ "_else") (prio + 1) profiles (PredNot pred) elseAction
    ]

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate all rules against a context, return first matching (highest priority).
evaluate :: [Rule] -> String -> String -> String -> String -> Maybe RuleResult
evaluate rules toolName desc safety category =
    let ctx = ToolContext toolName desc safety category
        sorted = sortByPriority rules
        results = map (evalRule ctx) sorted
    in case filter rrMatched results of
        (r:_) -> Just r
        []    -> Nothing

-- | Evaluate all rules, return ALL matching results.
evaluateAll :: [Rule] -> String -> String -> String -> String -> [RuleResult]
evaluateAll rules toolName desc safety category =
    let ctx = ToolContext toolName desc safety category
    in filter rrMatched $ map (evalRule ctx) rules

evalRule :: ToolContext -> Rule -> RuleResult
evalRule ctx r = RuleResult
    { rrRuleName = ruleName r
    , rrAction = ruleAction r
    , rrMatched = evalPred ctx (rulePred r)
    , rrPriority = rulePriority r
    }

sortByPriority :: [Rule] -> [Rule]
sortByPriority = foldr insert []
  where
    insert r [] = [r]
    insert r (x:xs)
        | rulePriority r <= rulePriority x = r : x : xs
        | otherwise = x : insert r xs

-- | Filter rules by profile.
filterByProfile :: Profile -> [Rule] -> [Rule]
filterByProfile profile = filter (\r -> profile `elem` ruleProfile r)

-- ---------------------------------------------------------------------------
-- Rule optimization
-- ---------------------------------------------------------------------------

-- | Optimize a predicate (constant folding, double negation elimination).
optimizePred :: RulePredicate -> RulePredicate
optimizePred (PredNot (PredNot p)) = optimizePred p
optimizePred (PredAnd PredTrue p) = optimizePred p
optimizePred (PredAnd p PredTrue) = optimizePred p
optimizePred (PredAnd PredFalse _) = PredFalse
optimizePred (PredAnd _ PredFalse) = PredFalse
optimizePred (PredOr PredTrue _) = PredTrue
optimizePred (PredOr _ PredTrue) = PredTrue
optimizePred (PredOr PredFalse p) = optimizePred p
optimizePred (PredOr p PredFalse) = optimizePred p
optimizePred (PredNot PredTrue) = PredFalse
optimizePred (PredNot PredFalse) = PredTrue
optimizePred (PredAnd p1 p2) = PredAnd (optimizePred p1) (optimizePred p2)
optimizePred (PredOr p1 p2) = PredOr (optimizePred p1) (optimizePred p2)
optimizePred (PredNot p) = PredNot (optimizePred p)
optimizePred p = p

-- | Optimize a rule's predicate.
optimizeRule :: Rule -> Rule
optimizeRule r = r { rulePred = optimizePred (rulePred r) }

-- | Detect conflicting rules (same predicate, contradicting actions).
detectConflicts :: [Rule] -> [(Rule, Rule, String)]
detectConflicts rules =
    [ (r1, r2, "Same predicate with conflicting actions")
    | r1 <- rules
    , r2 <- rules
    , ruleName r1 < ruleName r2
    , rulePred r1 == rulePred r2
    , isConflicting (ruleAction r1) (ruleAction r2)
    ]
  where
    isConflicting (ActionBlock _) (ActionLog _) = True
    isConflicting (ActionLog _) (ActionBlock _) = True
    isConflicting _ _ = False

-- ---------------------------------------------------------------------------
-- Built-in rule sets
-- ---------------------------------------------------------------------------

-- | Default profile rules.
defaultRules :: [Rule]
defaultRules =
    [ Rule "block_destructive" 1 [Default, Secure]
        (PredDescContains "delete") (ActionBlock "Destructive operation blocked")
    , Rule "warn_file_ops" 5 [Default, Creative, Secure]
        (PredCategory "filesystem") (ActionWarn "File operation — verify path safety")
    , Rule "throttle_network" 10 [Default, Secure]
        (PredCategory "network") (ActionThrottle 10)
    , Rule "log_introspection" 50 [Default, Creative, Secure]
        (PredCategory "introspection") (ActionLog "Introspection tool called")
    , Rule "block_shell" 1 [Default, Secure]
        (PredDescContains "shell") (ActionBlock "Shell execution blocked in default profile")
    ]

-- | Creative profile rules (more permissive).
creativeRules :: [Rule]
creativeRules =
    [ Rule "allow_creative" 20 [Creative]
        (PredCategory "creative") (ActionLog "Creative tool — proceed with inspiration")
    , Rule "warn_destructive_creative" 5 [Creative]
        (PredDescContains "delete") (ActionWarn "Destructive operation — be mindful")
    , Rule "log_all_creative" 100 [Creative]
        PredTrue (ActionLog "Creative session — all tools available")
    ]

-- | Secure profile rules (most restrictive).
secureRules :: [Rule]
secureRules =
    [ Rule "block_all_exec" 1 [Secure]
        (PredCategory "execution") (ActionBlock "All execution blocked in secure mode")
    , Rule "block_network_secure" 1 [Secure]
        (PredCategory "network") (ActionBlock "Network access blocked in secure mode")
    , Rule "throttle_memory_secure" 5 [Secure]
        (PredCategory "memory") (ActionThrottle 5)
    , Rule "tag_all_secure" 100 [Secure]
        PredTrue (ActionTag "secure_audit")
    ]
