{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HexagramData
Description : Traditional I Ching hexagram names and judgments (King Wen sequence)
Copyright   : (c) WhiteMagic Contributors, 2025
License     : MIT

All 64 hexagrams with their traditional Chinese names, pinyin, and judgment
text. Data sourced from the classical King Wen ordering used in standard
I Ching translations.
-}

module HexagramData
    ( HexagramInfo(..)
    , getHexagramInfo
    ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

-- | Metadata for a single hexagram
data HexagramInfo = HexagramInfo
    { hexName    :: Text   -- ^ Traditional Chinese name
    , hexPinyin  :: Text   -- ^ Pinyin romanization
    , judgment   :: Text   -- ^ Classical judgment / core teaching
    } deriving (Show, Eq)

-- | Lookup hexagram info by King Wen number (1-64)
getHexagramInfo :: Int -> Maybe HexagramInfo
getHexagramInfo n = Map.lookup n hexagramTable

-- | Full table: King Wen number -> HexagramInfo
hexagramTable :: Map.Map Int HexagramInfo
hexagramTable = Map.fromList
    [ ( 1, HexagramInfo "乾" "Qian"  "The Creative. Heaven above heaven. Great success through perseverance and inner truth.")
    , ( 2, HexagramInfo "坤" "Kun"   "The Receptive. Earth above earth. Devoted service and quiet strength bring success.")
    , ( 3, HexagramInfo "屯" "Zhun"  "Difficulty at the Beginning. Water above thunder. Perseverance through initial hardship yields eventual success.")
    , ( 4, HexagramInfo "蒙" "Meng"  "Youthful Folly. Mountain above water. Accept instruction with humility; the teacher finds the pupil.")
    , ( 5, HexagramInfo "需" "Xu"    "Waiting. Water above heaven. Patience and readiness; do not force action.")
    , ( 6, HexagramInfo "訟" "Song"  "Conflict. Heaven above water. Avoid prolonged dispute; seek compromise before engaging others.")
    , ( 7, HexagramInfo "師" "Shi"   "The Army. Earth above water. Leadership and discipline bring collective success.")
    , ( 8, HexagramInfo "比" "Bi"    "Holding Together. Water above earth. Unity and mutual support create lasting bonds.")
    , ( 9, HexagramInfo "小畜" "Xiao Xu" "The Power of the Small. Wind above heaven. Gentle persistence accumulates quiet influence.")
    , (10, HexagramInfo "履" "Lu"    "Treading. Heaven above lake. Walk carefully and with awareness; respect boundaries.")
    , (11, HexagramInfo "泰" "Tai"   "Peace. Earth above heaven. Harmony between heaven and earth; the right moment for action.")
    , (12, HexagramInfo "否" "Pi"    "Standstill. Heaven above earth. Stagnation and disconnection; withdraw and conserve energy.")
    , (13, HexagramInfo "同人" "Tong Ren" "Fellowship of Men. Heaven above fire. Harmonious cooperation with others toward shared purpose.")
    , (14, HexagramInfo "大畜" "Da Xu" "Great Accumulation. Mountain above heaven. Vast resources gathered through patience and merit.")
    , (15, HexagramInfo "謙" "Qian"  "Modesty. Earth above mountain. Humility is the highest virtue; it brings lasting respect.")
    , (16, HexagramInfo "豫" "Yu"    "Enthusiasm. Thunder above earth. Joyful preparation; inspire others through genuine delight.")
    , (17, HexagramInfo "隨" "Sui"   "Following. Lake above thunder. Adapt to circumstances with open awareness; yielding is strength.")
    , (18, HexagramInfo "蠱" "Gu"    "Work on the Decayed. Mountain above wind. Address what has decayed; renewal requires honest effort.")
    , (19, HexagramInfo "臨" "Lin"   "Approach. Earth above lake. The time of coming and engagement; welcome opportunity with openness.")
    , (20, HexagramInfo "觀" "Guan"  "Contemplation. Wind above earth. Observe carefully before acting; perception precedes understanding.")
    , (21, HexagramInfo "噬嗑" "She He" "Biting Through. Fire above thunder. Resolve obstacles decisively; clarity comes through direct action.")
    , (22, HexagramInfo "賁" "Bi"    "Grace. Fire above mountain. Beauty and refinement add meaning; outer form reflects inner truth.")
    , (23, HexagramInfo "剥" "Bo"    "Splitting Apart. Mountain above earth. Decline is natural; do not resist what must fall away.")
    , (24, HexagramInfo "復" "Fu"    "Return. Earth above thunder. The cycle turns; light returns after darkness. Rest and renew.")
    , (25, HexagramInfo "無妨" "Wu Wang" "Innocence. Heaven above thunder. Act from pure intention; sincerity brings unexpected fortune.")
    , (26, HexagramInfo "大畜" "Da Xu" "Great Accumulation. Mountain above heaven. Contain and nourish power; the mountain holds the sky.")
    , (27, HexagramInfo "頌" "Yi"    "Nourishment. Mountain above thunder. Attend to what you feed: body, mind, and others.")
    , (28, HexagramInfo "大過" "Da Guo" "Preponderance of the Great. Lake above wind. The weight of the situation demands courage and decisiveness.")
    , (29, HexagramInfo "坎" "Kan"   "The Abysmal. Water above water. Navigate danger with steadiness and inner light; do not panic.")
    , (30, HexagramInfo "離" "Li"    "The Clinging. Fire above fire. Clarity and awareness illuminate the path; cling to truth.")
    , (31, HexagramInfo "咸" "Xian"  "Influence. Lake above mountain. Mutual attraction and sensitivity; receptivity creates connection.")
    , (32, HexagramInfo "恒" "Heng"  "Duration. Wind above thunder. Endurance and consistency; commit fully to what you begin.")
    , (33, HexagramInfo "遁" "Dun"   "Retreat. Heaven above mountain. Strategic withdrawal preserves strength; know when to step back.")
    , (34, HexagramInfo "大壯" "Da Zhuang" "The Power of the Great. Thunder above heaven. Great strength must be tempered with wisdom.")
    , (35, HexagramInfo "進" "Jin"   "Progress. Fire above earth. Steady advancement into the light; conditions favor forward movement.")
    , (36, HexagramInfo "明夷" "Ming Yi" "Darkening of the Light. Earth above fire. Protect your inner light during difficult times; maintain integrity.")
    , (37, HexagramInfo "家人" "Jia Ren" "The Family. Wind above fire. Harmonious roles within family create lasting stability.")
    , (38, HexagramInfo "睽" "Kui"   "Opposition. Fire above lake. Differences can be bridged; understanding lies beneath apparent conflict.")
    , (39, HexagramInfo "蹇" "Jian"  "Obstruction. Water above mountain. Difficulty ahead; seek guidance and do not travel alone.")
    , (40, HexagramInfo "解" "Jie"   "Deliverance. Thunder above water. Release and liberation; forgive and move forward freely.")
    , (41, HexagramInfo "損" "Sun"   "Decrease. Mountain above lake. Let go of excess; simplification brings clarity and renewal.")
    , (42, HexagramInfo "益" "Yi"    "Increase. Wind above thunder. The time for generous giving and mutual benefit has arrived.")
    , (43, HexagramInfo "夬" "Gua"   "Breakthrough. Lake above heaven. Decisive action resolves long-standing tension; speak truth.")
    , (44, HexagramInfo "姤" "Gou"   "Coming Together. Wind above heaven. Unexpected encounter; be discerning about what you welcome.")
    , (45, HexagramInfo "萃" "Cui"   "Gathering. Lake above earth. Assemble resources and allies; collective strength serves the cause.")
    , (46, HexagramInfo "升" "Sheng" "Pushing Upward. Wind above earth. Gradual rise through steady effort; the time is right.")
    , (47, HexagramInfo "困" "Kun"   "Oppression. Lake above water. Exhaustion and constraint; maintain inner peace through difficulty.")
    , (48, HexagramInfo "井" "Jing"  "The Well. Water above wind. Nourishment is always available; draw from the deep source.")
    , (49, HexagramInfo "革" "Ge"    "Revolution. Lake above fire. Transformation is needed; change comes when old forms can no longer serve.")
    , (50, HexagramInfo "鼎" "Ding"  "The Cauldron. Fire above wind. The vessel of civilization; cooking and transformation nourish all.")
    , (51, HexagramInfo "震" "Zhen"  "The Arousing. Thunder above thunder. Shock awakens; after the storm, clarity and fresh beginning.")
    , (52, HexagramInfo "艮" "Gen"   "Keeping Still. Mountain above mountain. Rest and meditation; stillness reveals what motion obscures.")
    , (53, HexagramInfo "漸" "Jian"  "Development. Wind above mountain. Gradual progress like a tree growing; patience with the process.")
    , (54, HexagramInfo "歸妹" "Gui Mei" "The Marrying Maiden. Thunder above lake. Understand your true position; act from a place of clarity.")
    , (55, HexagramInfo "豐" "Feng"  "Abundance. Thunder above fire. Fullness and prosperity; enjoy the peak, knowing it is transient.")
    , (56, HexagramInfo "旅" "Lu"    "The Wanderer. Fire above mountain. Travel light and with respect; the stranger finds kindness in humility.")
    , (57, HexagramInfo "巽" "Xun"   "The Gentle. Wind above wind. Gentle penetration and influence; persistence without force achieves results.")
    , (58, HexagramInfo "兌" "Dui"   "The Joyous. Lake above lake. Joy shared is joy multiplied; openness and receptivity invite delight.")
    , (59, HexagramInfo "渙" "Huan"  "Dispersion. Wind above water. Dissolve what is rigid; flow and trust in the process of release.")
    , (60, HexagramInfo "節" "Jie"   "Limitation. Water above lake. Boundaries create structure; accept limits as the framework for growth.")
    , (61, HexagramInfo "中孚" "Zhong Fu" "Inner Truth. Wind above lake. Sincerity penetrates all barriers; truth spoken from the heart.")
    , (62, HexagramInfo "小過" "Xiao Guo" "Preponderance of the Small. Thunder above mountain. Small actions matter now; do not attempt the grand gesture.")
    , (63, HexagramInfo "既濟" "Ji Ji"  "After Completion. Water above fire. The task is done; maintain awareness, for disorder follows order.")
    , (64, HexagramInfo "未濟" "Wei Ji" "Before Completion. Fire above water. The task is not yet finished; perseverance will see it through.")
    ]
