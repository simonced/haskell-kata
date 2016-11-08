
-- simple problem:
-- input string like "abcde"
-- and output like:
-- ["", "a", "ab", "abc", "abcd", "abcde"]


-- main function (has to be called)
buildSteps :: [Char] -> [[Char]]
buildSteps str = [] : take (length str) (nextStep str 1)


-- Careful! this is an infinite loop!
-- it has to be used with a take call for example
nextStep :: [Char] -> Int -> [[Char]]
nextStep str i = (take i str) : nextStep str (i+1)

