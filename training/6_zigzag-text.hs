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
                word_ !! start_ : last ++ next
        where
            phase = (height_ - 1) * 2
            last = checkLast word_ $ start_ + phase
            next = zigzagline word_ height_ (start_+phase*2)
            -- TODO
            -- add middle, a character sometimes between start and
            -- last


-- checking if we are out of bound
checkLast :: [Char] -> Int -> [Char]
checkLast word_ position_
    | position_ < length word_ = [word_ !! position_]
    | otherwise = []


-- trying my own higher order function
zigzagline' (word_, height_, start_) = zigzagline word_ height_ start_


-- main function doing the whole word
zigzagword :: [Char] -> Int -> [Char]
zigzagword word_ height_ = 
        concat (map zigzagline' $ [(word_, height_, x) | x <- [0..height_-1]])
        -- feels cheap but works X_X
        -- === !IDEA! ===
        -- I might not need to map, I could apply the function inside the
        -- list itself I feel...
