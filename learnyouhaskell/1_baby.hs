doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x * 2

-- memos on lists

-- append to lists
-- ---------------
-- [1,2,3] ++ [4,5,6] concatenates lists together
-- so as "aa" ++ "bb" gives "aabb" that is like ['a','a','b','b'] in fact
-- but that concatenates at the end, which could take some process time 
-- with long lists
-- so appending on top of a list like so
-- 1:2:[3,4] will give [1,2,3,4] is way faster
-- but it can only concatenate scalars to a list

-- removing elements
-- -----------------
-- ['a','b','c'] !! 1
-- identical to "abc" !! 1
-- will return 2 (index 1 in the list)

-- also, some useful functions on lists
-- ------------------------------------
-- head returns the first element
-- tail returns the elements after the head
-- last returns the last element
-- init returns the elements before the last

-- ======================================================================

-- further reading:
-- http://learnyouahaskell.com/starting-out
-- I'm a list comprehension
