{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : IChing
Description : I Ching 64-hexagram state machine for memory system states
Copyright   : (c) WhiteMagic Contributors, 2025
License     : MIT
Maintainer  : whitemagic@example.com

The I Ching provides a complete state space (2^6 = 64 hexagrams) for
modeling system states. Each hexagram represents a unique configuration
of six binary lines (yin/yang), mapping perfectly to system state dimensions.

Philosophy: Ancient wisdom meets modern type theory. Pure functional state
transitions with compile-time correctness guarantees.
-}

module IChing
    ( -- * Core Types
      Line(..)
    , Trigram(..)
    , Hexagram(..)
    , HexagramName(..)
    
      -- * State Transitions
    , transition
    , oppositeHexagram
    , complementaryHexagram
    
      -- * Hexagram Construction
    , hexagram
    , fromNumber
    , toNumber
    
      -- * Queries
    , isBalanced
    , yinYangRatio
    , dominantForce
    
      -- * All Hexagrams
    , allHexagrams
    ) where

import GHC.Generics (Generic)
import Data.List (foldl')

-- | Yin (broken) or Yang (solid) line
data Line = Yin | Yang
    deriving (Eq, Show, Generic, Enum, Bounded)

-- | Three lines forming a trigram (8 total combinations)
data Trigram = Trigram Line Line Line
    deriving (Eq, Show, Generic)

-- | Six lines forming a hexagram (64 total combinations)
-- Upper trigram (Heaven) and Lower trigram (Earth)
data Hexagram = Hexagram 
    { upper :: Trigram  -- Heaven (top 3 lines)
    , lower :: Trigram  -- Earth (bottom 3 lines)
    } deriving (Eq, Show, Generic)

-- | Traditional I Ching hexagram names (King Wen sequence)
data HexagramName
    = TheCreative          -- 1. ䷀ Qián (乾)
    | TheReceptive         -- 2. ䷁ Kūn (坤)  
    | DifficultyAtBeginning -- 3. ䷂ Zhūn (屯)
    | YouthfulFolly        -- 4. ䷃ Méng (蒙)
    | Waiting              -- 5. ䷄ Xū (需)
    | Conflict             -- 6. ䷅ Sòng (訟)
    -- ... (all 64 hexagrams would be here in full implementation)
    | Custom Int           -- Placeholder for remaining hexagrams
    deriving (Eq, Show, Generic)

-- | Convert line to binary (0 or 1)
lineToBit :: Line -> Int
lineToBit Yin = 0
lineToBit Yang = 1

-- | Convert binary to line
bitToLine :: Int -> Line
bitToLine 0 = Yin
bitToLine _ = Yang

-- | Opposite line (yin ↔ yang)
oppositeLine :: Line -> Line
oppositeLine Yin = Yang
oppositeLine Yang = Yin

-- | Create hexagram from six lines (bottom to top)
hexagram :: Line -> Line -> Line -> Line -> Line -> Line -> Hexagram
hexagram l1 l2 l3 l4 l5 l6 = Hexagram
    { upper = Trigram l4 l5 l6  -- Top 3 lines
    , lower = Trigram l1 l2 l3  -- Bottom 3 lines
    }

-- | Convert hexagram to number (1-64)
toNumber :: Hexagram -> Int
toNumber (Hexagram (Trigram u1 u2 u3) (Trigram l1 l2 l3)) =
    let bits = map lineToBit [l1, l2, l3, u1, u2, u3]
        binary = foldl' (\acc b -> acc * 2 + b) 0 bits
    in binary + 1  -- I Ching numbers start at 1

-- | Convert number (1-64) to hexagram
fromNumber :: Int -> Maybe Hexagram
fromNumber n 
    | n < 1 || n > 64 = Nothing
    | otherwise = Just $ numToHex (n - 1)
  where
    numToHex :: Int -> Hexagram
    numToHex num =
        case map bitToLine $ reverse $ take 6 $ toBinary num ++ repeat 0 of
            [l1, l2, l3, u1, u2, u3] -> hexagram l1 l2 l3 u1 u2 u3
            _                        -> hexagram Yin Yin Yin Yin Yin Yin  -- unreachable
    
    toBinary :: Int -> [Int]
    toBinary 0 = []
    toBinary n' = toBinary (n' `div` 2) ++ [n' `mod` 2]

-- | State transition: change one or more lines
-- This models how system state evolves
transition :: [Int]  -- ^ Line positions to change (1-6, bottom to top)
           -> Hexagram
           -> Hexagram
transition positions (Hexagram (Trigram u1 u2 u3) (Trigram l1 l2 l3)) =
    let allLines = [l1, l2, l3, u1, u2, u3]
        changed = zipWith (\i line -> if i `elem` positions
                                       then oppositeLine line
                                       else line) [1..6] allLines
    in case changed of
        [nl1, nl2, nl3, nu1, nu2, nu3] -> hexagram nl1 nl2 nl3 nu1 nu2 nu3
        _                              -> hexagram l1 l2 l3 u1 u2 u3  -- unreachable

-- | Opposite hexagram (all lines reversed)
oppositeHexagram :: Hexagram -> Hexagram
oppositeHexagram = transition [1,2,3,4,5,6]

-- | Complementary hexagram (rotated 180°)
-- In I Ching, this represents the inverse situation
complementaryHexagram :: Hexagram -> Hexagram
complementaryHexagram (Hexagram upper' lower') = Hexagram lower' upper'

-- | Check if hexagram is balanced (3 yin, 3 yang)
isBalanced :: Hexagram -> Bool
isBalanced hex = yinYangRatio hex == (3, 3)

-- | Count yin and yang lines
yinYangRatio :: Hexagram -> (Int, Int)
yinYangRatio (Hexagram (Trigram u1 u2 u3) (Trigram l1 l2 l3)) =
    let allLines = [l1, l2, l3, u1, u2, u3]
        yinCount = length $ filter (== Yin) allLines
        yangCount = 6 - yinCount
    in (yinCount, yangCount)

-- | Determine dominant force
data Force = YinDominant | YangDominant | Balanced
    deriving (Eq, Show)

dominantForce :: Hexagram -> Force
dominantForce hex =
    case yinYangRatio hex of
        (y, ya) | y > ya    -> YinDominant
                | ya > y    -> YangDominant
                | otherwise -> Balanced

-- | All 64 hexagrams
allHexagrams :: [Hexagram]
allHexagrams = [hexagram l1 l2 l3 l4 l5 l6 
               | l1 <- [Yin, Yang]
               , l2 <- [Yin, Yang]
               , l3 <- [Yin, Yang]
               , l4 <- [Yin, Yang]
               , l5 <- [Yin, Yang]
               , l6 <- [Yin, Yang]
               ]

