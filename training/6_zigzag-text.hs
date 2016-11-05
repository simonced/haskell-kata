-- I don't remember where did I see that exercice,
-- maybe on leetcode, I'll add the link if I find that again.
--
-- The goal is to encode an input Text into another text in
-- the following pattern of a certain height

-- sample: PAYPALISHIRING (that was the sample text provided) with height 3
-- pattern: P A H N
--          APLSIIG
--          Y I R
-- result string: PAHNAPLSIIGYIR

-- to work the algorightm out, I wrote the same text in the zigzag pattern
-- of different heights
-- the following is the results of my observations:
-- the letters are numbered from 0
-- the 2 first result letters are 0 [start] and [phase], actually [start
-- + phase]
-- with phase = (height-1)*2
-- the next letters are applied with the following:
-- concat all letters from [start] to [start + phase] with 
-- [start + phase - level*2]
-- where level = the level of the letter in the zigzag.
-- In the previous zigzag sample, the first letter P is at level 0 
-- and the second letter A is at level 1, etc...


word = "PAYPALISHIRING"

getPhase :: Int -> Int
getPhase height_ = (height_ - 1) * 2


zigzagline :: [Char] -> Int -> Int -> [Char]
zigzagline word_ height_ start_ = do
        if start_ >= length word_
            then []
            else
                word_ !! start_ : middle ++ next
        where
            phase = getPhase height_
            middle = checkBefore word_ height_ start_
            next = zigzagline word_ height_ $ start_+phase


checkBefore :: [Char] -> Int -> Int -> [Char]
checkBefore word_ height_ start_
    | level == 0 || level == height_-1 = []
    | before < length word_ = [word_ !! before]
    | otherwise = []
    where
        before = start_ + phase - level * 2
        phase = getPhase height_
        level = findLevel height_ start_


-- finding the row level at which is the start position
findLevel :: Int -> Int -> Int
findLevel height_ start_
    | start_ < height_ = start_
    | otherwise = findLevel height_ $ start_ - getPhase height_


-- main function processing the whole word
zigzagword :: [Char] -> Int -> [Char]
zigzagword word_ height_ = 
        concat ([zigzagline word_ height_ x | x <- [0..height_-1]])
