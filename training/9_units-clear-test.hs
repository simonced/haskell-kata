-- Test in Haskell

-- Simply testing a simple function for summing up total clear units progression.\\
-- We have a list of daily cleared units
-- and we want a progression graph giving the total in the current month

unitsClear = [1,0,4,5,2,0,0,5]
--unitsClear = [1]
--unitsClear = []

unitsClearDays :: Int -> [Int] -> [Int]
unitsClearDays _ [] = []
unitsClearDays t [x] = t + x : []
unitsClearDays t (uc:ucs) = a : (unitsClearDays a ucs)
    where a = t + uc

-- to test the function
run = unitsClearDays 0 unitsClear


