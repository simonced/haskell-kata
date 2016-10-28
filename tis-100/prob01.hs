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
process :: Int -> Int
process x = x
-- simply returns the same parameter

-- output check function
check :: Int -> Int -> Bool
check i o = o == process i

-- simple way to have tuples of inputs and outputs
-- zip outA (map process inA)
-- or
-- [(i, process i) | i <- inA]
