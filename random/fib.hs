

fib :: Integer -> Integer -> Int -> Integer
fib prev _ 0 = prev
fib _ next 1 = next
fib prev next n = fib next (prev+next) (n-1)

-- call: fib 0 1 x (x being the depth of the sequence wanted)