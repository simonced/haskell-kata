-- url: https://leetcode.com/problems/intersection-of-two-arrays/

-- Given two arrays, write a function to compute their intersection.

-- Example:
-- Given nums1 = [1, 2, 2, 1], nums2 = [2, 2], return [2].

-- Note:
-- Each element in the result must be unique.
-- The result can be in any order.

list1 :: [Int]
list1 = [1,2,3,4,5,6,7]

list2 :: [Int]
list2 = [7,8,9,2,8,7, 2]

-- only 2 is common tho the 2 lists

isInList :: Int -> [Int] -> Bool
isInList _ [] = False
isInList needle_ (x:list)
    | needle_ == x = True
    | otherwise = isInList needle_ list


intersecLists :: [Int] -> [Int] -> [Int] -> [Int]
intersecLists [] _ res = res
intersecLists (l:l1) l2 res
    | isInList l l2 = l: (intersecLists l1 l2 res)
    | otherwise = intersecLists l1 l2 res
