{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : MaturityGate
Description : Algebraic state machine for staged capability unlocking
Copyright   : (c) WhiteMagic Contributors, 2026
License     : MIT

Implements the WhiteMagic maturity gate system as a Haskell state machine.
The system progresses through 6 developmental stages, each unlocking
additional tool categories:

  SEED       → Basic introspection only
  BICAMERAL  → Memory + reasoning tools
  REFLECTIVE → Governance + ethical tools
  RADIANT    → Creative + synthesis tools
  COLLECTIVE → Agent coordination + distributed tools
  LOGOS      → Full unrestricted access

Transitions are guarded by invariants that must hold before advancing.
Haskell's algebraic types guarantee exhaustive case coverage.
-}

module MaturityGate
    ( MaturityStage(..)
    , MaturityState(..)
    , ToolCategory(..)
    , TransitionResult(..)
    , mkInitialState
    , currentStage
    , canAccessTool
    , allowedCategories
    , tryAdvance
    , tryRegress
    , stageToInt
    , intToStage
    , stageRequirements
    , assessReadiness
    ) where

import Data.List (nub)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Developmental stages (ordered by maturity).
data MaturityStage
    = Seed
    | Bicameral
    | Reflective
    | Radiant
    | Collective
    | Logos
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Tool categories gated by maturity.
data ToolCategory
    = TCIntrospection  -- ^ gnosis, status, health
    | TCMemory         -- ^ create, recall, search, consolidate
    | TCReasoning      -- ^ bicameral, ensemble, salience
    | TCGovernance     -- ^ dharma, karma, harmony, circuit breaker
    | TCCreative       -- ^ dream, grimoire, synthesis
    | TCAgent          -- ^ register, coordinate, distribute
    | TCDangerous      -- ^ shell execution, file delete, config override
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The full maturity state.
data MaturityState = MaturityState
    { msStage            :: MaturityStage
    , msToolsExecuted    :: Int
    , msSessionCount     :: Int
    , msDharmaScore      :: Double    -- ^ 0.0-1.0, ethical compliance
    , msHarmonyScore     :: Double    -- ^ 0.0-1.0, system health
    , msConsolidations   :: Int       -- ^ Memory consolidations performed
    , msAgentsRegistered :: Int
    , msErrorRate        :: Double    -- ^ 0.0-1.0, recent error frequency
    } deriving (Show, Eq)

-- | Result of a transition attempt.
data TransitionResult
    = Advanced MaturityStage MaturityStage String   -- ^ from, to, reason
    | Regressed MaturityStage MaturityStage String  -- ^ from, to, reason
    | Blocked MaturityStage String                  -- ^ current, reason
    | AlreadyAtMax
    | AlreadyAtMin
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Stage → integer mapping (for FFI)
-- ---------------------------------------------------------------------------

stageToInt :: MaturityStage -> Int
stageToInt Seed       = 0
stageToInt Bicameral  = 1
stageToInt Reflective = 2
stageToInt Radiant    = 3
stageToInt Collective = 4
stageToInt Logos      = 5

intToStage :: Int -> Maybe MaturityStage
intToStage 0 = Just Seed
intToStage 1 = Just Bicameral
intToStage 2 = Just Reflective
intToStage 3 = Just Radiant
intToStage 4 = Just Collective
intToStage 5 = Just Logos
intToStage _ = Nothing

-- ---------------------------------------------------------------------------
-- Category access control
-- ---------------------------------------------------------------------------

-- | Which tool categories are unlocked at each stage.
allowedCategories :: MaturityStage -> [ToolCategory]
allowedCategories Seed       = [TCIntrospection]
allowedCategories Bicameral  = [TCIntrospection, TCMemory, TCReasoning]
allowedCategories Reflective = [TCIntrospection, TCMemory, TCReasoning, TCGovernance]
allowedCategories Radiant    = [TCIntrospection, TCMemory, TCReasoning, TCGovernance, TCCreative]
allowedCategories Collective = [TCIntrospection, TCMemory, TCReasoning, TCGovernance, TCCreative, TCAgent]
allowedCategories Logos      = [TCIntrospection, TCMemory, TCReasoning, TCGovernance, TCCreative, TCAgent, TCDangerous]

-- | Check if a tool category is accessible at the current maturity.
canAccessTool :: MaturityState -> ToolCategory -> Bool
canAccessTool state cat = cat `elem` allowedCategories (msStage state)

-- ---------------------------------------------------------------------------
-- Stage transition requirements
-- ---------------------------------------------------------------------------

-- | Requirements to advance to a given stage.
data StageRequirement = StageRequirement
    { srMinTools        :: Int
    , srMinSessions     :: Int
    , srMinDharma       :: Double
    , srMinHarmony      :: Double
    , srMinConsolidations :: Int
    , srMinAgents       :: Int
    , srMaxErrorRate    :: Double
    , srDescription     :: String
    } deriving (Show, Eq)

-- | Get requirements for advancing TO a given stage.
stageRequirements :: MaturityStage -> StageRequirement
stageRequirements Seed = StageRequirement 0 0 0.0 0.0 0 0 1.0 "Initial stage — no requirements"
stageRequirements Bicameral = StageRequirement
    10 1 0.3 0.3 0 0 0.5
    "Execute 10+ tools, 1+ session, basic ethical/health scores"
stageRequirements Reflective = StageRequirement
    50 3 0.5 0.5 1 0 0.3
    "Execute 50+ tools, 3+ sessions, moderate ethics, 1+ consolidation"
stageRequirements Radiant = StageRequirement
    200 10 0.7 0.6 5 0 0.2
    "Execute 200+ tools, 10+ sessions, high ethics, 5+ consolidations"
stageRequirements Collective = StageRequirement
    500 25 0.8 0.7 10 1 0.15
    "Execute 500+ tools, 25+ sessions, very high ethics, agents registered"
stageRequirements Logos = StageRequirement
    1000 50 0.9 0.8 25 3 0.1
    "Execute 1000+ tools, 50+ sessions, near-perfect ethics and health"

-- | Check if the state meets the requirements for a target stage.
meetsRequirements :: MaturityState -> StageRequirement -> Bool
meetsRequirements MaturityState{..} StageRequirement{..} =
    msToolsExecuted >= srMinTools &&
    msSessionCount >= srMinSessions &&
    msDharmaScore >= srMinDharma &&
    msHarmonyScore >= srMinHarmony &&
    msConsolidations >= srMinConsolidations &&
    msAgentsRegistered >= srMinAgents &&
    msErrorRate <= srMaxErrorRate

-- | Assess readiness for the next stage. Returns (ready, unmet requirements).
assessReadiness :: MaturityState -> (Bool, [String])
assessReadiness state
    | msStage state == Logos = (False, ["Already at maximum stage"])
    | otherwise =
        let nextStage = succ (msStage state)
            req = stageRequirements nextStage
            unmet = concat
                [ ["Need " ++ show (srMinTools req) ++ " tools (have " ++ show (msToolsExecuted state) ++ ")"
                  | msToolsExecuted state < srMinTools req]
                , ["Need " ++ show (srMinSessions req) ++ " sessions (have " ++ show (msSessionCount state) ++ ")"
                  | msSessionCount state < srMinSessions req]
                , ["Need dharma >= " ++ show (srMinDharma req) ++ " (have " ++ show (msDharmaScore state) ++ ")"
                  | msDharmaScore state < srMinDharma req]
                , ["Need harmony >= " ++ show (srMinHarmony req) ++ " (have " ++ show (msHarmonyScore state) ++ ")"
                  | msHarmonyScore state < srMinHarmony req]
                , ["Need " ++ show (srMinConsolidations req) ++ " consolidations (have " ++ show (msConsolidations state) ++ ")"
                  | msConsolidations state < srMinConsolidations req]
                , ["Need error rate <= " ++ show (srMaxErrorRate req) ++ " (have " ++ show (msErrorRate state) ++ ")"
                  | msErrorRate state > srMaxErrorRate req]
                ]
        in (null unmet, unmet)

-- ---------------------------------------------------------------------------
-- Transition logic
-- ---------------------------------------------------------------------------

-- | Create initial state at Seed stage.
mkInitialState :: MaturityState
mkInitialState = MaturityState
    { msStage = Seed
    , msToolsExecuted = 0
    , msSessionCount = 0
    , msDharmaScore = 0.5
    , msHarmonyScore = 0.5
    , msConsolidations = 0
    , msAgentsRegistered = 0
    , msErrorRate = 0.0
    }

-- | Get the current stage.
currentStage :: MaturityState -> MaturityStage
currentStage = msStage

-- | Try to advance to the next stage.
tryAdvance :: MaturityState -> (TransitionResult, MaturityState)
tryAdvance state
    | msStage state == Logos = (AlreadyAtMax, state)
    | otherwise =
        let nextStage = succ (msStage state)
            req = stageRequirements nextStage
        in if meetsRequirements state req
           then let newState = state { msStage = nextStage }
                    msg = "Advanced from " ++ show (msStage state) ++ " to " ++ show nextStage
                in (Advanced (msStage state) nextStage msg, newState)
           else let (_, unmet) = assessReadiness state
                    msg = "Cannot advance: " ++ unwords unmet
                in (Blocked (msStage state) msg, state)

-- | Regress to the previous stage (e.g., due to ethical violations).
tryRegress :: MaturityState -> String -> (TransitionResult, MaturityState)
tryRegress state reason
    | msStage state == Seed = (AlreadyAtMin, state)
    | otherwise =
        let prevStage = pred (msStage state)
            newState = state { msStage = prevStage }
            msg = "Regressed from " ++ show (msStage state) ++ " to " ++ show prevStage ++ ": " ++ reason
        in (Regressed (msStage state) prevStage msg, newState)
