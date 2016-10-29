-- kata location:
-- https://leetcode.com/problems/two-sum/
--
-- goal: find the 2 indices of integers in a list that sum results in the target
-- ie:
-- list = [2, 7, 9, 11] and target = 9
-- solution is [0,1]
-- because 7 + 2 = 9
--
-- nothing is said about the order of the elements in the list (sorted or not)
-- but only ONE solution is possible
-- also, I suppose that all elements in the list are unique

-- libs
import Data.List

-- Entries
list :: [Int]
list = [2, 7, 11, 15]

target :: Int
target = 9

-- algo
-- I think I need a recursive thing to simply try all combinations
-- and stop when I found the target sum

-- here is a one liner ;)
solution = [(x `elemIndex` list, y `elemIndex` list)
    | x <- list, y <- tail list, x+y==target]

-- lets turn that into a function and let's try a bunch of patterns
process :: Int -> [Int] -> (Maybe Int, Maybe Int)
process t l = head  [(x `elemIndex` l, y `elemIndex` l)
        | x <- l, y <- tail l, x + y == t]

-- preparing a bunch of lists and targets to test
list1 :: [Int]
list1 = [1, 4, 6, 3, 8, 10, 14]

list2 :: [Int]
list2 = [20, 8, 9, 4, 7, 13, 2]

list3 :: [Int]
list3 = [1..30]

-- test a list with no solution
list4 :: [Int]
list4 = [1,2,3,4]

-- let's run that
run = do
    print (process target list)
    print (process target list1)
    print (process target list2)
    print (process target list3)
    --print (process target list4)
