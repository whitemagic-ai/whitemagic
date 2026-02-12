"""Complete 64 I Ching Hexagrams - Traditional Interpretations

Binary perfection: 8 trigrams × 8 = 64 (matching DNA codons)
Each hexagram represents a unique pattern of change and wisdom.
"""

COMPLETE_HEXAGRAMS = [
    # 1-10
    (1, "The Creative", "乾 Qián", "The Creative works sublime success, furthering through perseverance",
     "Heaven's movement is full of power", "☰☰", "Continue with confidence and strength"),

    (2, "The Receptive", "坤 Kūn", "The Receptive brings supreme success through the perseverance of a mare",
     "Earth's disposition is receptive devotion", "☷☷", "Be receptive, nurturing, and yielding"),

    (3, "Difficulty at Beginning", "屯 Zhūn", "Difficulty at the beginning works supreme success through perseverance",
     "Clouds and thunder emerge", "☵☳", "Persist through chaos - order will emerge"),

    (4, "Youthful Folly", "蒙 Méng", "Inexperience seeks teaching; I do not seek the young fool, the young fool seeks me",
     "Spring at foot of mountain", "☶☵", "Embrace learning with humility"),

    (5, "Waiting", "需 Xū", "Waiting with sincerity brings success; perseverance brings good fortune",
     "Clouds rise to heaven", "☵☰", "Patient waiting with inner confidence"),

    (6, "Conflict", "訟 Sòng", "Conflict requires meeting halfway; going through brings danger",
     "Heaven and water move apart", "☰☵", "Seek compromise, avoid escalation"),

    (7, "The Army", "師 Shī", "The army needs experienced leadership; this brings good fortune",
     "Water beneath earth", "☷☵", "Discipline and order under wise command"),

    (8, "Holding Together", "比 Bǐ", "Union brings good fortune; inquire of the oracle again",
     "Water upon earth", "☵☷", "Build alliances with sincerity"),

    (9, "Small Taming", "小畜 Xiǎo Chù", "Gentle restraint brings success",
     "Wind moves across heaven", "☴☰", "Accumulate power through gentle persistence"),

    (10, "Treading", "履 Lǚ", "Treading carefully on the tiger's tail; it does not bite",
     "Heaven above, lake below", "☰☱", "Proceed with caution and respect"),

    # 11-20
    (11, "Peace", "泰 Tài", "Peace; the small departs, the great approaches",
     "Earth above, heaven below", "☷☰", "Harmony between heaven and earth"),

    (12, "Standstill", "否 Pǐ", "Standstill; the great departs, the small approaches",
     "Heaven above, earth below", "☰☷", "Withdraw and cultivate inner resources"),

    (13, "Fellowship", "同人 Tóng Rén", "Fellowship with others in the open brings success",
     "Heaven together with fire", "☰☲", "Unite with others for common purpose"),

    (14, "Great Possession", "大有 Dà Yǒu", "Possession in great measure; supreme success",
     "Fire above heaven", "☲☰", "Abundance through virtue and sharing"),

    (15, "Modesty", "謙 Qiān", "Modesty creates success; the superior person carries things through",
     "Earth above mountain", "☷☶", "Humility brings respect and advancement"),

    (16, "Enthusiasm", "豫 Yù", "Enthusiasm brings advantage; appoint helpers",
     "Thunder from earth", "☳☷", "Move forward with infectious joy"),

    (17, "Following", "隨 Suí", "Following has supreme success; perseverance furthers",
     "Thunder within lake", "☱☳", "Adapt and follow the time"),

    (18, "Work on the Decayed", "蠱 Gǔ", "Work on what has been spoiled brings supreme success",
     "Wind below mountain", "☶☴", "Repair corruption at its source"),

    (19, "Approach", "臨 Lín", "Approach brings supreme success; perseverance furthers",
     "Earth above lake", "☷☱", "Leadership through joyful accessibility"),

    (20, "Contemplation", "觀 Guān", "Contemplation; ablution made but not yet the offering",
     "Wind above earth", "☴☷", "Observe deeply before acting"),

    # 21-30
    (21, "Biting Through", "噬嗑 Shì Kè", "Biting through brings success; it is favorable to let justice be administered",
     "Fire and thunder", "☲☳", "Remove obstacles decisively"),

    (22, "Grace", "賁 Bì", "Grace brings success in small matters",
     "Fire below mountain", "☶☲", "Beauty and form enhance substance"),

    (23, "Splitting Apart", "剝 Bō", "Splitting apart; it does not further one to go anywhere",
     "Mountain above earth", "☶☷", "Yield when foundation crumbles"),

    (24, "Return", "復 Fù", "Return brings success; going out and coming in without error",
     "Thunder within earth", "☷☳", "Renewal after the turning point"),

    (25, "Innocence", "無妄 Wú Wàng", "Innocence brings supreme success; perseverance furthers",
     "Heaven beneath thunder", "☳☰", "Act from genuine spontaneity"),

    (26, "Great Taming", "大畜 Dà Chù", "Great taming brings perseverance; not eating at home brings good fortune",
     "Mountain above heaven", "☶☰", "Accumulate virtue and knowledge"),

    (27, "Nourishment", "頤 Yí", "Nourishment brings good fortune when seeking nourishment properly",
     "Mountain above thunder", "☶☳", "Be mindful of what you take in"),

    (28, "Great Excess", "大過 Dà Guò", "Preponderance of the great; the ridgepole sags",
     "Lake above wind", "☱☴", "Extraordinary times require extraordinary measures"),

    (29, "The Abysmal", "坎 Kǎn", "The Abysmal repeated brings danger but sincerity brings success",
     "Water doubled", "☵☵", "Flow through danger with constancy"),

    (30, "The Clinging", "離 Lí", "The Clinging; perseverance furthers; it brings success",
     "Fire doubled", "☲☲", "Cling to what is luminous and correct"),

    # 31-40
    (31, "Influence", "咸 Xián", "Influence brings success; perseverance furthers; taking a maiden brings good fortune",
     "Lake above mountain", "☱☶", "Mutual attraction and receptivity"),

    (32, "Duration", "恆 Héng", "Duration brings success without blame; perseverance furthers",
     "Thunder above wind", "☳☴", "Constancy and endurance prevail"),

    (33, "Retreat", "遯 Dùn", "Retreat brings success in small matters",
     "Heaven above mountain", "☰☶", "Strategic withdrawal preserves strength"),

    (34, "Great Power", "大壯 Dà Zhuàng", "Great power; perseverance brings good fortune",
     "Thunder above heaven", "☳☰", "Strength tempered by righteousness"),

    (35, "Progress", "晉 Jìn", "Progress like the sun rising over earth",
     "Fire above earth", "☲☷", "Advancement through clarity"),

    (36, "Darkening of Light", "明夷 Míng Yí", "Darkening of the light; perseverance in adversity furthers",
     "Earth above fire", "☷☲", "Hide brilliance in dark times"),

    (37, "The Family", "家人 Jiā Rén", "The family; perseverance of the woman brings good fortune",
     "Wind above fire", "☴☲", "Establish order within the home"),

    (38, "Opposition", "睽 Kuí", "Opposition; in small matters, good fortune",
     "Fire above lake", "☲☱", "Honor differences while seeking common ground"),

    (39, "Obstruction", "蹇 Jiǎn", "Obstruction; the southwest furthers, the northeast does not",
     "Water above mountain", "☵☶", "Face difficulty; seek help from allies"),

    (40, "Deliverance", "解 Xiè", "Deliverance; the southwest furthers; if nothing more, return brings good fortune",
     "Thunder above water", "☳☵", "Release from tension brings relief"),

    # 41-50
    (41, "Decrease", "損 Sǔn", "Decrease combined with sincerity brings supreme good fortune",
     "Mountain above lake", "☶☱", "Simplify and decrease to increase essence"),

    (42, "Increase", "益 Yì", "Increase; it furthers one to undertake something",
     "Wind above thunder", "☴☳", "Augment the good; share abundance"),

    (43, "Breakthrough", "夬 Guài", "Breakthrough; one must resolutely make the matter known",
     "Lake above heaven", "☱☰", "Decisive action ends the old"),

    (44, "Coming to Meet", "姤 Gòu", "Coming to meet; the maiden is powerful",
     "Heaven above wind", "☰☴", "Unexpected encounter requires caution"),

    (45, "Gathering Together", "萃 Cuì", "Gathering together brings success; the king approaches his temple",
     "Lake above earth", "☱☷", "Assemble forces for common purpose"),

    (46, "Pushing Upward", "升 Shēng", "Pushing upward brings supreme success; see the great person",
     "Earth above wind", "☷☴", "Steady growth through effort"),

    (47, "Oppression", "困 Kùn", "Oppression brings success to the persevering great person",
     "Lake above water", "☱☵", "Maintain integrity under constraint"),

    (48, "The Well", "井 Jǐng", "The well; the town may be changed but not the well",
     "Water above wind", "☵☴", "Inexhaustible source of nourishment"),

    (49, "Revolution", "革 Gé", "Revolution; supreme success through firm correctness",
     "Lake above fire", "☱☲", "Transform when the time is ripe"),

    (50, "The Caldron", "鼎 Dǐng", "The caldron brings supreme good fortune and success",
     "Fire above wind", "☲☴", "Nourishment and transformation through culture"),

    # 51-60
    (51, "The Arousing", "震 Zhèn", "The arousing brings success; thunder comes with shock",
     "Thunder doubled", "☳☳", "Shock awakens; maintain composure"),

    (52, "Keeping Still", "艮 Gèn", "Keeping still; meditate until movement ceases",
     "Mountain doubled", "☶☶", "Stillness and contemplation bring clarity"),

    (53, "Development", "漸 Jiàn", "Development; gradual progress brings good fortune",
     "Wind above mountain", "☴☶", "Patient development like tree growth"),

    (54, "The Marrying Maiden", "歸妹 Guī Mèi", "The marrying maiden; undertakings bring misfortune",
     "Thunder above lake", "☳☱", "Act with propriety in relationships"),

    (55, "Abundance", "豐 Fēng", "Abundance brings success; the king attains it",
     "Thunder above fire", "☳☲", "Fullness at its peak; prepare for decline"),

    (56, "The Wanderer", "旅 Lǚ", "The wanderer; success in small matters through perseverance",
     "Fire above mountain", "☲☶", "Travel light and remain adaptable"),

    (57, "The Gentle", "巽 Xùn", "The gentle penetrates; success in small matters",
     "Wind doubled", "☴☴", "Gentle penetration achieves results"),

    (58, "The Joyous", "兌 Duì", "The joyous brings success; perseverance is favorable",
     "Lake doubled", "☱☱", "Joy shared with others multiplies"),

    (59, "Dispersion", "渙 Huàn", "Dispersion brings success; the king approaches his temple",
     "Wind above water", "☴☵", "Dissolve rigidity and separateness"),

    (60, "Limitation", "節 Jié", "Limitation brings success; bitter limitation brings misfortune",
     "Water above lake", "☵☱", "Accept necessary limits with grace"),

    # 61-64
    (61, "Inner Truth", "中孚 Zhōng Fú", "Inner truth; it furthers one to cross the great water",
     "Wind above lake", "☴☱", "Sincerity penetrates to the core"),

    (62, "Small Exceeding", "小過 Xiǎo Guò", "Preponderance of the small brings success",
     "Thunder above mountain", "☳☶", "Attend to details; avoid grand ventures"),

    (63, "After Completion", "既濟 Jì Jì", "After completion brings success in small matters",
     "Water above fire", "☵☲", "Maintain vigilance at success"),

    (64, "Before Completion", "未濟 Wèi Jì", "Before completion brings success; careful perseverance furthers",
     "Fire above water", "☲☵", "Almost complete; take final steps with care"),
]
