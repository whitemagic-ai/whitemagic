module Main (main) where

import IChing
import Casting
import WuXing
import HexagramData
import Transform (Memory(..), MemoryType(..))
import Query (hasTag, and', execute)
import Data.Time (getCurrentTime)
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "WhiteMagic Haskell Divination Layer"
    putStrLn "===================================="
    putStrLn ""

    -- ---------------------------------------------------------------
    -- I Ching basics
    -- ---------------------------------------------------------------
    putStrLn "--- I Ching ---"
    putStrLn $ "  Total hexagrams : " ++ show (length allHexagrams)
    let creative   = hexagram Yang Yang Yang Yang Yang Yang
        receptive  = hexagram Yin  Yin  Yin  Yin  Yin  Yin
    putStrLn $ "  The Creative (#): " ++ show (toNumber creative)
    putStrLn $ "  The Receptive (#): " ++ show (toNumber receptive)
    putStrLn $ "  Creative balanced: " ++ show (isBalanced creative)
    putStrLn $ "  Receptive balanced: " ++ show (isBalanced receptive)
    putStrLn ""

    -- ---------------------------------------------------------------
    -- HexagramData lookups
    -- ---------------------------------------------------------------
    putStrLn "--- Hexagram Judgments ---"
    mapM_ (\n -> case getHexagramInfo n of
        Just info -> putStrLn $ "  #" ++ show n ++ " " ++ T.unpack (hexName info)
                              ++ " (" ++ T.unpack (hexPinyin info) ++ "): "
                              ++ take 60 (T.unpack (judgment info)) ++ "..."
        Nothing   -> return ()
        ) [1, 2, 11, 12, 29, 64]
    putStrLn ""

    -- ---------------------------------------------------------------
    -- Wu Xing casting
    -- ---------------------------------------------------------------
    putStrLn "--- Wu Xing ---"
    let wxState = WuXingState
            { wood  = 0.6
            , fire  = 0.3
            , earth = 0.2
            , metal = 0.5
            , water = 0.1
            }
    putStrLn $ "  State      : " ++ show wxState
    putStrLn $ "  Dominant   : " ++ show (dominant wxState)
    putStrLn $ "  Deficient  : " ++ show (deficient wxState)
    putStrLn $ "  Balance    : " ++ show (WuXing.balance wxState)
    putStrLn $ "  Healthy?   : " ++ show (isHealthy wxState)
    putStrLn $ "  Advice     : " ++ T.unpack (advice wxState)
    let wxRebalanced = rebalance wxState
    putStrLn $ "  Rebalanced : " ++ show wxRebalanced
    putStrLn ""

    -- ---------------------------------------------------------------
    -- Casting (Context + SystemState → Hexagram + interpretation)
    -- ---------------------------------------------------------------
    putStrLn "--- Casting ---"
    let ctx = Context
            { keywords  = [T.pack "speed", T.pack "clarity"]
            , sentiment = 0.7
            , urgency   = 0.8
            }
        sysState = SystemState
            { wuXingBalance    = wxState
            , memoryConfidence = 0.85
            , recentPatterns   = map T.pack ["pattern1","pattern2","pattern3","pattern4","pattern5"]
            }
        hex    = cast ctx sysState
        hexNum = toNumber hex
        hexInt = interpret hex
    putStrLn $ "  Cast result   : " ++ show hex
    putStrLn $ "  King Wen (#)  : " ++ show hexNum
    putStrLn $ "  Interpretation: " ++ T.unpack hexInt
    putStrLn ""

    -- ---------------------------------------------------------------
    -- Transition + changing lines + transform
    -- ---------------------------------------------------------------
    putStrLn "--- Transition ---"
    let changed    = Casting.transform hex
        changedNum = toNumber changed
    putStrLn $ "  Original  : #" ++ show hexNum
    putStrLn $ "  Changing  : " ++ show (changingLines hex)
    putStrLn $ "  Transformed: #" ++ show changedNum
    putStrLn ""

    -- ---------------------------------------------------------------
    -- Query DSL demo
    -- ---------------------------------------------------------------
    putStrLn "--- Query DSL ---"
    now <- getCurrentTime
    let memories =
            [ Memory "m1" "System Health" "All checks passed"    ["health","important"] now ShortTerm
            , Memory "m2" "Bug Report"    "Null pointer in cast" ["bug"]               now ShortTerm
            , Memory "m3" "Design Doc"    "Wu Xing integration"  ["important","design"] now LongTerm
            ]
        query  = hasTag "important" `and'` hasTag "design"
        hits   = execute query memories
    putStrLn $ "  Memories      : " ++ show (length memories)
    putStrLn $ "  Query hits    : " ++ show (length hits)
    mapM_ (\m -> putStrLn $ "    - " ++ memoryTitle m) hits
    putStrLn ""

    putStrLn "All layers initialised successfully."
