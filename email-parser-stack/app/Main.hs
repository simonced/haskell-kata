module Main where

import Lib

-- modules I need
--import Regex.Posix
--import System.Directory


main :: IO ()
main = do
        emails <- makeEmailsList "../email-parser/emails"
        results <- parseEmails emails
        print results
