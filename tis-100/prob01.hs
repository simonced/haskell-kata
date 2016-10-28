-- problem description:
-- 2 inputs A and B
-- 2 outputs A and B
-- each input data has to be copied to the corresponding output

-- inputs
inA = [4, 5, -1, 9, 8, 5, 6]
inB = [2, -6, 8, 7, 11, 43, 1]

-- expected outputs
outA = [4, 5, -1, 9, 8, 5, 6]
outB = [2, -6, 8, 7, 11, 43, 1]
-- for that simple problem, they are identical

-- process function
process :: Integer -> Integer
process x = x
-- simply returns the same parameter

-- output check function
check :: Integer -> Integer -> (Integer, Integer, Bool)
check i o = (i, process i, o == process i)

checkAll :: [Integer] -> [Integer] -> [(Integer, Integer, Bool)]
checkAll [] [] = []
checkAll (i:is) (o:os) = [check i o] ++ checkAll is os

-- then testing with 
-- checkAll inA ouA
-- will display all tests and resuts

-- simple way to have tuples of inputs and outputs
-- zip outA (map process inA)
-- or
-- [(i, process i) | i <- inA]
