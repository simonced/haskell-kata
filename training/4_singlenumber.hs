import Data.List
import Data.

-- problem url:
-- https://leetcode.com/problems/single-number/
--
-- Given an array of integers, every element appears twice except for one.
-- Find that single one.

numbers :: [Int]
numbers = [1, 3, 5, 6, 1, 5, 8, 6, 2, 3]

-- I could simply sort the list and check numbers 2 by 2, so easy!
findTheOne :: [Int] -> Maybe Int
-- casual case with more than 2 numbers to test
findTheOne list
    | length list == 1 = Just $ head list
    | list !! 0 /= list !! 1 = Just $ list !! 0
    | otherwise = findTheOne (tail ( tail list))

-- easy testing :
-- findTheOne numbers
-- returns: Just 1

-- now a specific case
numbers' :: [Int]
numbers' = [1]
