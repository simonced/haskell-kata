module Main where

import Lib

-- modules I need
--import Regex.Posix
--import System.Directory


-- simple tools to test in ghci
-- this function simply allows me to get the MailData I need
-- I'll use it to test other functions
run :: IO [MailData]
run = do
        emails <- makeEmailsList "emails"
        results <- parseEmails emails
        return (results)


main :: IO ()
main = do
        d <- run
        print d
