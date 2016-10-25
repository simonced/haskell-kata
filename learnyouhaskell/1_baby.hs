doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x * 2

-- further reading:
-- http://learnyouahaskell.com/starting-out
-- An intro to lists

-- vim: set expandtab:
