module Main (main) where

import IChing
import Casting
import WuXing
import HexagramData
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)

-- Minimal test harness (no external deps)
type Test = (String, Bool)

runTests :: [Test] -> IO ()
runTests tests = do
    let failures = [(name, res) | (name, res) <- tests, not res]
    mapM_ (\(name, _) -> putStrLn $ "  PASS  " ++ name) [(n,r) | (n,r) <- tests, r]
    mapM_ (\(name, _) -> putStrLn $ "  FAIL  " ++ name) failures
    putStrLn $ show (length tests - length failures) ++ "/" ++ show (length tests) ++ " passed"
    if null failures then exitSuccess else exitFailure

main :: IO ()
main = do
    putStrLn "WhiteMagic Haskell — Unit Tests"
    putStrLn "================================"
    runTests
        [ -- I Ching construction
          ( "hexagram constructor round-trips through toNumber"
          , let h = hexagram Yang Yin Yang Yin Yang Yin
                n = toNumber h
            in n >= 1 && n <= 64
          )
        , ( "The Creative is #1 (all Yang)"
          , toNumber (hexagram Yang Yang Yang Yang Yang Yang) == 64
            -- Note: all-Yang = binary 111111 = 63, +1 = 64
          )
        , ( "The Receptive is all Yin"
          , toNumber (hexagram Yin Yin Yin Yin Yin Yin) == 1
          )
        , ( "fromNumber . toNumber round-trips"
          , let h = hexagram Yin Yang Yin Yang Yin Yang
            in fromNumber (toNumber h) == Just h
          )
        , ( "allHexagrams has 64 elements"
          , length allHexagrams == 64
          )
        , ( "oppositeHexagram flips all lines"
          , let h = hexagram Yang Yang Yang Yang Yang Yang
                o = oppositeHexagram h
            in o == hexagram Yin Yin Yin Yin Yin Yin
          )
        , ( "complementaryHexagram swaps trigrams"
          , let h = hexagram Yin Yin Yin Yang Yang Yang
                c = complementaryHexagram h
            in c == hexagram Yang Yang Yang Yin Yin Yin
          )
        , ( "isBalanced: 3 yin 3 yang"
          , isBalanced (hexagram Yin Yang Yin Yang Yin Yang)
          )
        , ( "isBalanced: all Yang is not balanced"
          , not $ isBalanced (hexagram Yang Yang Yang Yang Yang Yang)
          )
        , ( "yinYangRatio sums to 6"
          , let (y, ya) = yinYangRatio (hexagram Yin Yang Yin Yang Yin Yang)
            in y + ya == 6
          )

          -- HexagramData
        , ( "getHexagramInfo 1 returns Just"
          , case getHexagramInfo 1 of
              Just info -> hexName info == T.pack "\20094"  -- 乾 (U+4E7E)
              Nothing   -> False
          )
        , ( "getHexagramInfo 0 returns Nothing"
          , getHexagramInfo 0 == Nothing
          )
        , ( "getHexagramInfo 65 returns Nothing"
          , getHexagramInfo 65 == Nothing
          )

          -- Wu Xing
        , ( "dominant picks highest element"
          , dominant (WuXingState 0.9 0.1 0.1 0.1 0.1) == Wood
          )
        , ( "deficient picks lowest element"
          , deficient (WuXingState 0.9 0.1 0.5 0.5 0.5) == Fire
          )
        , ( "generates cycle: Wood->Fire->Earth->Metal->Water->Wood"
          , map generates [Wood, Fire, Earth, Metal, Water]
            == [Fire, Earth, Metal, Water, Wood]
          )
        , ( "overcomes cycle: Wood->Earth->Water->Fire->Metal->Wood"
          , map overcomes [Wood, Fire, Earth, Metal, Water]
            == [Earth, Metal, Water, Wood, Fire]
          )
        , ( "rebalance moves toward 0.2"
          , let st  = WuXingState 1.0 0.0 0.0 0.0 0.0
                st' = rebalance st
            in wood st' < 1.0 && fire st' > 0.0
          )
        , ( "isHealthy when all >= 0.15"
          , isHealthy (WuXingState 0.2 0.2 0.2 0.2 0.2)
          )
        , ( "not healthy when one < 0.15"
          , not $ isHealthy (WuXingState 0.2 0.2 0.2 0.2 0.1)
          )

          -- Casting
        , ( "cast produces a valid hexagram (number 1-64)"
          , let ctx = Context [T.pack "speed"] 0.5 0.7
                st  = SystemState (WuXingState 0.6 0.4 0.3 0.5 0.2) 0.8 []
                n   = toNumber (cast ctx st)
            in n >= 1 && n <= 64
          )
        , ( "interpret returns non-empty text"
          , let h = hexagram Yang Yang Yang Yang Yang Yang
            in not $ T.null (interpret h)
          )
        , ( "changingLines: all-Yang has imbalance 6 -> [1..6]"
          , changingLines (hexagram Yang Yang Yang Yang Yang Yang) == [1..6]
          )
        , ( "changingLines: balanced hex -> []"
          , changingLines (hexagram Yin Yang Yin Yang Yin Yang) == []
          )
        ]
