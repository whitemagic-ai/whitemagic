{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : WuXing
Description : The Five Elements (Wu Xing) cycle — generation, overcoming, and balance
Copyright   : (c) WhiteMagic Contributors, 2025
License     : MIT

Wu Xing models system dynamics as a cycle of five elemental phases.
Each element generates the next and overcomes another, forming two
interlocking pentagons of influence. This module provides:

  * The five-element state vector (all values 0.0–1.0)
  * Generation and overcoming cycle queries
  * Balance scoring and rebalancing
  * Elemental advice for a given SystemState

Philosophy: Every system has a dominant element and a deficient one.
Health is not equality—it is dynamic flow through the cycle.
-}

module WuXing
    ( -- * Core type
      Element(..)
    , WuXingState(..)

      -- * Cycle relationships
    , generates      -- ^ Which element does this one generate?
    , overcomes      -- ^ Which element does this one overcome?
    , generatedBy    -- ^ Which element generates this one?
    , overcomBy      -- ^ Which element overcomes this one?

      -- * State queries
    , dominant       -- ^ Element with the highest value
    , deficient      -- ^ Element with the lowest value
    , balance        -- ^ Balance score 0.0 (perfectly equal) .. 1.0 (maximally skewed)
    , isHealthy      -- ^ True when no element is below the threshold

      -- * Adjustment
    , rebalance      -- ^ Nudge all elements toward equilibrium
    , boost          -- ^ Increase a single element
    , deplete        -- ^ Decrease a single element

      -- * Elemental advice
    , advice         -- ^ Human-readable guidance based on current imbalance

      -- * Deep Dharma (Phase 14.5)
    , trigramElement -- ^ Map a Trigram to its corresponding Element
    , hexagramElement -- ^ Map a Hexagram to its governing Element
    , resonanceScore -- ^ Calculate how well a Hexagram resonates with a WuXingState
    ) where

import Data.List (minimumBy, maximumBy)
import Data.Ord  (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import IChing (Trigram(..), Hexagram(..), Line(..))

-- | The five classical elements
data Element = Wood | Fire | Earth | Metal | Water
    deriving (Eq, Show, Enum, Bounded)

-- | State vector: each element in [0, 1]
data WuXingState = WuXingState
    { wood  :: Double
    , fire  :: Double
    , earth :: Double
    , metal :: Double
    , water :: Double
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Cycle relationships (Generation: Wood→Fire→Earth→Metal→Water→Wood)
--                     (Overcoming:  Wood→Earth→Water→Fire→Metal→Wood)
-- ---------------------------------------------------------------------------

-- | The element this one generates (productive cycle)
generates :: Element -> Element
generates Wood  = Fire
generates Fire  = Earth
generates Earth = Metal
generates Metal = Water
generates Water = Wood

-- | The element this one overcomes (destructive cycle)
overcomes :: Element -> Element
overcomes Wood  = Earth
overcomes Fire  = Metal
overcomes Earth = Water
overcomes Metal = Wood
overcomes Water = Fire

-- | Inverse: who generates this element
generatedBy :: Element -> Element
generatedBy Fire  = Wood
generatedBy Earth = Fire
generatedBy Metal = Earth
generatedBy Water = Metal
generatedBy Wood  = Water

-- | Inverse: who overcomes this element
overcomBy :: Element -> Element
overcomBy Earth  = Wood
overcomBy Metal  = Fire
overcomBy Water  = Earth
overcomBy Wood   = Metal
overcomBy Fire   = Water

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

valueOf :: Element -> WuXingState -> Double
valueOf Wood  = wood
valueOf Fire  = fire
valueOf Earth = earth
valueOf Metal = metal
valueOf Water = water

allElements :: [Element]
allElements = [minBound .. maxBound]

-- ---------------------------------------------------------------------------
-- State queries
-- ---------------------------------------------------------------------------

-- | Element with the highest value
dominant :: WuXingState -> Element
dominant st = maximumBy (comparing (`valueOf` st)) allElements

-- | Element with the lowest value
deficient :: WuXingState -> Element
deficient st = minimumBy (comparing (`valueOf` st)) allElements

-- | Balance score: 0.0 = perfect equilibrium, 1.0 = maximally skewed.
-- Computed as the standard deviation of the five values, normalised to [0,1].
balance :: WuXingState -> Double
balance st =
    let vals = map (`valueOf` st) allElements
        mean = sum vals / 5.0
        variance = sum (map (\v -> (v - mean) ^ (2 :: Int)) vals) / 5.0
        stddev = sqrt variance
    in min 1.0 (stddev * sqrt 5.0)  -- normalise: max stddev when one=1, rest=0

-- | Healthy when no element drops below a minimum threshold (default 0.15)
isHealthy :: WuXingState -> Bool
isHealthy st = all (\e -> valueOf e st >= 0.15) allElements

-- ---------------------------------------------------------------------------
-- Adjustment
-- ---------------------------------------------------------------------------

clamp :: Double -> Double
clamp = max 0.0 . min 1.0

-- | Nudge all five elements toward 0.2 (equilibrium) by a fraction of the gap.
--   A single call moves ~25 % of the distance to equilibrium.
rebalance :: WuXingState -> WuXingState
rebalance st = WuXingState
    { wood  = lerp (wood  st) 0.2
    , fire  = lerp (fire  st) 0.2
    , earth = lerp (earth st) 0.2
    , metal = lerp (metal st) 0.2
    , water = lerp (water st) 0.2
    }
  where
    lerp current target = current + 0.25 * (target - current)

-- | Increase one element by `amount`, clamped to [0, 1].
boost :: Element -> Double -> WuXingState -> WuXingState
boost e amount st = setElement e (clamp (valueOf e st + amount)) st

-- | Decrease one element by `amount`, clamped to [0, 1].
deplete :: Element -> Double -> WuXingState -> WuXingState
deplete e amount st = setElement e (clamp (valueOf e st - amount)) st

setElement :: Element -> Double -> WuXingState -> WuXingState
setElement Wood  v st = st { wood  = v }
setElement Fire  v st = st { fire  = v }
setElement Earth v st = st { earth = v }
setElement Metal v st = st { metal = v }
setElement Water v st = st { water = v }

-- ---------------------------------------------------------------------------
-- Elemental advice
-- ---------------------------------------------------------------------------

-- | Concise advice based on dominant / deficient analysis
advice :: WuXingState -> Text
advice st
    | isHealthy st = T.concat
        [ "Balance is good (score ", showF (balance st), "). "
        , "Continue nurturing ", showE (generates (dominant st))
        , " through ", showE (dominant st), "."
        ]
    | otherwise = T.concat
        [ showE def, " is depleted. "
        , "Strengthen it by nurturing ", showE (generatedBy def), " first, "
        , "and be cautious of ", showE (overcomBy def), " (which overcomes it). "
        , "Overall imbalance: ", showF (balance st), "."
        ]
  where
    def = deficient st

showE :: Element -> Text
showE Wood  = "Wood"
showE Fire  = "Fire"
showE Earth = "Earth"
showE Metal = "Metal"
showE Water = "Water"

showF :: Double -> Text
showF x = T.pack $ show (round (x * 100) :: Int)

-- ---------------------------------------------------------------------------
-- Deep Dharma (Phase 14.5)
-- ---------------------------------------------------------------------------

-- | Map a Trigram to its corresponding Element (Later Heaven Sequence)
trigramElement :: Trigram -> Element
trigramElement (Trigram Yang Yang Yang) = Metal -- Qian (Heaven) -> Metal/Gold
trigramElement (Trigram Yin Yin Yin)    = Earth -- Kun (Earth) -> Earth
trigramElement (Trigram Yin Yang Yin)   = Water -- Kan (Water) -> Water
trigramElement (Trigram Yang Yin Yang)  = Fire  -- Li (Fire) -> Fire
trigramElement (Trigram Yin Yin Yang)   = Wood  -- Zhen (Thunder) -> Wood
trigramElement (Trigram Yang Yang Yin)  = Metal -- Dui (Lake) -> Metal
trigramElement (Trigram Yang Yin Yin)   = Earth -- Gen (Mountain) -> Earth
trigramElement (Trigram Yin Yang Yang)  = Wood  -- Xun (Wind) -> Wood

-- | Map a Hexagram to its governing Element (simplified approach)
-- In deep theory, this considers moving lines, nuclear trigrams, etc.
-- For v13.2, we use the Upper Trigram as the heavenly governor.
hexagramElement :: Hexagram -> Element
hexagramElement (Hexagram upper _) = trigramElement upper

-- | Calculate how well a Hexagram resonates with a WuXingState.
-- A Hexagram which is governed by an element that the system is *deficient* in
-- will have HIGH resonance (it fills the void).
-- A Hexagram governed by an element the system has in *excess*
-- will have LOW resonance (it exacerbates imbalance).
resonanceScore :: Hexagram -> WuXingState -> Double
resonanceScore hex st =
    let govElement = hexagramElement hex
        currentVal = valueOf govElement st
        
        -- If current value is low (< 0.2), resonance is high (needs boosting)
        -- If current value is high (> 0.2), resonance is low (avoid boosting)
        -- Normalised around equilibrium of 0.2.
        
        rawScore = 1.0 - currentVal
        
        -- Boost score if the hexagram's element generates the deficient element
        def = deficient st
        genBonus = if generates govElement == def then 0.2 else 0.0
        
    in clamp (rawScore + genBonus)
