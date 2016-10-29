
-- problem description:
-- 2 inputs A and B
-- 2 outputs A and B
-- each input data has to be copied to the corresponding output

-- inputs
inA = [4, 5, -1, 9, 8, 5, 6]::[Int]
inB = [4, -6, 8, 7, 11, 43, 1]::[Int]

-- expected outputs
outA = [4, 5, -1, 9, 8, 5, 6]::[Int]
outB = [4, -6, 8, 7, 11, 43, 1]::[Int]
-- for that simple problem, they are identical

-- process function
process :: Int -> Int
process x = x
-- simply returns the same parameter

-- output check function
check :: Int -> Int -> (Int, Int, Bool)
check i o = (i, result, o == result)
    where result = process i

checkAll :: [Int] -> [Int] -> [(Int, Int, Bool)]
checkAll [] [] = []
checkAll (i:is) (o:os) = (check i o):checkAll is os

-- another way to generate the list comprehension instead of checkAll
-- could be:
-- [check i o | (i, o) <- zip inA outA]

-- testing the whole thing
run = do
    print (checkAll inA outA)
    print (checkAll inB outB)

