{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Casting
    ( Context(..)
    , SystemState(..)
    , cast
    , castWithEntropy
    , interpret
    , changingLines
    , transform
    ) where

import IChing
import HexagramData
import WuXing (WuXingState(..), fire, earth)
import Data.List (foldl')
import Data.Text (Text)

-- | Context for casting
data Context = Context
    { keywords :: [Text]
    , sentiment :: Double  -- -1.0 (Yin) to 1.0 (Yang)
    , urgency :: Double    -- 0.0 (calm) to 1.0 (urgent)
    } deriving (Show, Eq)

-- | Current system state
data SystemState = SystemState
    { wuXingBalance :: WuXingState
    , memoryConfidence :: Double
    , recentPatterns :: [Text]
    } deriving (Show, Eq)

-- | Pure functional hexagram casting
cast :: Context -> SystemState -> Hexagram
cast ctx state =
    let line1 = determineLine Foundation ctx state
        line2 = determineLine Resources ctx state
        line3 = determineLine Action ctx state
        line4 = determineLine Response ctx state
        line5 = determineLine Vision ctx state
        line6 = determineLine Transcendence ctx state
    in hexagram line1 line2 line3 line4 line5 line6

-- | Cast with added entropy (for mystery)
castWithEntropy :: Context -> SystemState -> Int -> Hexagram
castWithEntropy ctx state seed =
    let baseHex = cast ctx state
        entropy = fromIntegral (seed `mod` 10) / (100.0 :: Double)
    in if entropy < (0.1 :: Double)
       then baseHex  -- 90% deterministic
       else flipLine (seed `mod` 6 + 1) baseHex  -- 10% add mystery

-- | Determine single line based on position
determineLine :: Position -> Context -> SystemState -> Line
determineLine Foundation _ctx state
    | earth (wuXingBalance state) > 0.5 = Yang
    | otherwise = Yin
determineLine Resources _ctx state
    | memoryConfidence state > 0.7 = Yang
    | otherwise = Yin
determineLine Action ctx _state
    | urgency ctx > 0.6 || hasKeyword "speed" ctx = Yang
    | otherwise = Yin
determineLine Response ctx _state
    | sentiment ctx > 0.0 = Yang
    | otherwise = Yin
determineLine Vision _ctx state
    | fire (wuXingBalance state) > 0.5 = Yang
    | otherwise = Yin
determineLine Transcendence _ctx state
    | length (recentPatterns state) > 10 = Yang
    | otherwise = Yin

-- | Line positions
data Position = Foundation | Resources | Action | Response | Vision | Transcendence
    deriving (Show, Eq, Enum)

-- | Check if context has keyword
hasKeyword :: Text -> Context -> Bool
hasKeyword word ctx = word `elem` keywords ctx

-- | Interpret hexagram
interpret :: Hexagram -> Text
interpret hex =
    case getHexagramInfo (toNumber hex) of
        Just info -> judgment info
        Nothing -> "Unknown hexagram"

-- | Find changing lines (lines with strong energy)
changingLines :: Hexagram -> [Int]
changingLines hex =
    let yinCount = countLines Yin hex
        yangCount = 6 - yinCount
        imbalance = abs (yinCount - yangCount)
    in if imbalance >= 4
       then [1..6]  -- Strong imbalance = many changing lines
       else []      -- Balanced = stable

-- | Transform hexagram via changing lines
transform :: Hexagram -> Hexagram
transform hex =
    foldl' flipLineAt hex (changingLines hex)
  where
    flipLineAt h pos = modifyLine pos opposite h

-- | Count lines of a type
countLines :: Line -> Hexagram -> Int
countLines lineType hex = length $ filter (== lineType) $ linesOf hex

-- Helper to get lines from hexagram (bottom to top: lower then upper)
linesOf :: Hexagram -> [Line]
linesOf (Hexagram (Trigram u1 u2 u3) (Trigram l1 l2 l3)) = [l1, l2, l3, u1, u2, u3]

-- | Opposite line
opposite :: Line -> Line
opposite Yin = Yang
opposite Yang = Yin

-- | Modify line at position (1=bottom .. 6=top)
modifyLine :: Int -> (Line -> Line) -> Hexagram -> Hexagram
modifyLine pos f (Hexagram (Trigram u1 u2 u3) (Trigram l1 l2 l3)) = case pos of
    1 -> hexagram (f l1) l2 l3 u1 u2 u3
    2 -> hexagram l1 (f l2) l3 u1 u2 u3
    3 -> hexagram l1 l2 (f l3) u1 u2 u3
    4 -> hexagram l1 l2 l3 (f u1) u2 u3
    5 -> hexagram l1 l2 l3 u1 (f u2) u3
    6 -> hexagram l1 l2 l3 u1 u2 (f u3)
    _ -> hexagram l1 l2 l3 u1 u2 u3  -- Invalid position, no change

-- | Flip a line
flipLine :: Int -> Hexagram -> Hexagram
flipLine pos = modifyLine pos opposite
