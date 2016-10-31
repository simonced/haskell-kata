
-- find different letter
-- king of my second attempt...

findAdded :: Char -> [Char] -> Bool
findAdded letter [] = True
findAdded letter (first:rest)
    | letter == first = False
    | otherwise = findAdded letter rest


-- a is supposed to be the longer list
-- each of its charactesrs will be compared
-- to all characters in b
compareLists :: [Char] -> [Char] -> Maybe Char
compareLists [] b = Nothing
compareLists (a:rest) b
    | findAdded a b = Just a
    | otherwise = compareLists rest b

-- I wonder how should I deal with lists of different sizes.
-- well, it's not explained in the problem so it's not that important I tink.
    
