import qualified Data.Map as M

dico = M.fromList [(1, "abc")
                  ,(2, "xyz")]

-- try of the update function
-- that removes an entry if it has a certain value
f1 x = if x == "bbb" then Nothing else Just $ reverse x
test1 = M.update f1 1 dico
test2 = M.update f1 2 dico

-- making an each_chunks function like in ruby?
eachChunks :: Int -> [a] -> [[a]]
eachChunks n list 
    | length list >= n = (take n list) : eachChunks n (tail list)
    | otherwise = []
