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

zigzagline :: [Char] -> Int -> Int -> [Char]
zigzagline word_ height_ start_ = do
        if start_ >= length word_
            then []
            else
                word_ !! start_ : end ++ next
        where
            phase = (height_ - 1) * 2
            end = checkLast word_ $ start_ + phase
            next = zigzagline word_ height_ (start_+phase*2)


-- checking if we are out of bound
checkLast :: [Char] -> Int -> [Char]
checkLast word_ position_
    | position_ < length word_ = [word_ !! position_]
    | otherwise = []


-- main function doing the whole word
zigzagword :: [Char] -> Int -> [Char]
zigzagword word_ height_ = 
        map (zigzagline word_ height_) [0..height_]
        -- that doesn't compile, I feel curry and/or zip should be useful
        -- here
