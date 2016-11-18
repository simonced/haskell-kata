module Main where

import Lib

-- modules I need
--import Regex.Posix
--import System.Directory


main :: IO ()
main = do
        emails <- makeEmailsList "emails"
        results <- parseEmails emails
        print results
