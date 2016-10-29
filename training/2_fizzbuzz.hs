-- exercice address:
-- https://leetcode.com/problems/fizz-buzz/

-- my great implementation!
fizzbuzz :: Int -> String
fizzbuzz i
    | i `mod` 3 == 0 && i `mod` 5 == 0 = "FizzBuzz"
    | i `mod` 3 == 0 = "Fizz"
    | i `mod` 5 == 0 = "Buzz"
    | otherwise = show i

-- simple run loop
run = do
    map fizzbuzz [1..15]
