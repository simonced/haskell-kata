import Data.List (sort)

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
findTheOne [] = Nothing
findTheOne (x:[]) = Just x
findTheOne (x:y:list)
    | x /= y = Just x
    | otherwise = findTheOne list

-- easy testing :
-- findTheOne $ sort numbers
-- returns: Just 2

-- now a specific case
numbers' :: [Int]
numbers' = [1]
