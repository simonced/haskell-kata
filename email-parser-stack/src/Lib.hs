module Lib
    --(
    --    makeEmailsList
    --   ,parseEmails
    --) 
    where

-- ======================================================================

-- will turn into a module once it's working
--
-- package: http://hackage.haskell.org/package/regex-posix
-- $ cabal install regex-posix
--
-- also, I need directory package for files listing:
-- $ cabal install directory
--
-- I might need to use stack and play with dependencies,
-- since I need libraries for that program...

import Text.Regex.Posix
import System.Directory


--  ____        _
-- |  _ \  __ _| |_ __ _
-- | | | |/ _` | __/ _` |
-- | |_| | (_| | || (_| |
-- |____/ \__,_|\__\__,_|


--  _____                      
-- |_   _|   _ _ __   ___  ___ 
--   | || | | | '_ \ / _ \/ __|
--   | || |_| | |_) |  __/\__ \
--   |_| \__, | .__/ \___||___/
--       |___/|_|              

type Email = String


-- basic data structure
data MailData = MailData {
            fromEmail :: Email
            ,subject :: String
            ,spamScore :: Float
            ,spamFlag :: Bool
            } deriving (Show)


data SpamFromData = SpamFromData ( String, Int ) deriving (Show)
-- the string is the "matched" from entry
-- and Int is the count of occurences of that string
-- intended to be calculated by groupAndCountUniques below


--  __  __       _ _    __                  _   _
-- |  \/  | __ _(_) |  / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
-- | |\/| |/ _` | | | | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- | |  | | (_| | | | |  _| |_| | | | | (__| |_| | (_) | | | \__ \
-- |_|  |_|\__,_|_|_| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/


-- this should open the file and extract the data we need
parseEmailContent :: String -> MailData
parseEmailContent filecontent = MailData {
            fromEmail = searchFrom filecontent
            ,subject = searchSubject filecontent
            ,spamScore = searchSpamScore filecontent
            ,spamFlag = searchSpamFlag filecontent
            }


searchFrom :: String -> String
searchFrom content = head(content =~ "^From:.*<(.*)>" :: [[String]]) !! 1


searchSubject :: String -> String
searchSubject content = head (content =~ "^Subject: (.*)" :: [[String]]) !! 1
-- TODO deal with encoded subjects


searchSpamScore :: String -> Float
searchSpamScore content = read (head (content =~ "score=([0-9.]+)" :: [[String]]) !! 1) :: Float


searchSpamFlag :: String -> Bool
searchSpamFlag content = content =~ "^X-Spam-Flag: YES" :: Bool



--            _            _       _   _                 
--   ___ __ _| | ___ _   _| | __ _| |_(_) ___  _ __  ___ 
--  / __/ _` | |/ __| | | | |/ _` | __| |/ _ \| '_ \/ __|
-- | (_| (_| | | (__| |_| | | (_| | |_| | (_) | | | \__ \
--  \___\__,_|_|\___|\__,_|_|\__,_|\__|_|\___/|_| |_|___/
--                                                      

avgSpamRate :: [MailData] -> Float
avgSpamRate list_ = sum list / elems
    where
        list = [(spamScore x)|x <- list_]
        elems = fromIntegral (length list_) -- to return a Float

-- simple test in stack ghci:
-- test <- run
-- avgSpamRate test


-- a simple idea is to compare an email to all the others with compareEmailTail
-- and only keep the longer "part in common" found if any
compareEmails :: Email -> [Email] -> [String]
compareEmails _ [] = []
compareEmails baseEmail (fst:emails) = (compareEmailTail baseEmail fst) : (compareEmails baseEmail emails)
-- this should build a list of all possible common tails between the list of emails

-- ghci simple testing function
-- for now, only does it with the first email against all the others
compareAllEmails :: [MailData] -> [String]
compareAllEmails emails = compareEmails (head emailsList) (tail emailsList)
    where
    emailsList = [fromEmail x | x <- emails]
-- TODO group by longer occurence 
-- and maybe only take the first
-- something like groupAndCountUniques bellow?


-- TODO
groupAndCountUniques :: [String] -> [SpamFromData]
groupAndCountUniques email = undefined


-- I nedd to compare the tails of strings... >>>

-- compare from head, to compare from tail, strings will have to bve reversed first
-- will return the number of characters that matches from head
-- no better idea for now
commonPart :: String -> String -> Int -> Int
commonPart (m1:m1s) (m2:m2s) counter
  | m1 == m2 = 1 + commonPart m1s m2s counter
  | otherwise = counter


compareEmailTail :: Email -> Email -> String
compareEmailTail email1 email2 = reverse $ take common remail1
  where
    remail1 = reverse email1
    remail2 = reverse email2
    common = commonPart remail1 remail2 0

-- 2 simple tests
--test = compareEmailTail "test@example.org" "toto@email.com"
--test = compareEmailTail "test@example.org" "toto@email.org"
-- <<<

--  _     _     _   _             
-- | |   (_)___| |_(_)_ __   __ _ 
-- | |   | / __| __| | '_ \ / _` |
-- | |___| \__ \ |_| | | | | (_| |
-- |_____|_|___/\__|_|_| |_|\__, |
--                          |___/ 
--   __                  _   _                 
--  / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
-- | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- |  _| |_| | | | | (__| |_| | (_) | | | \__ \
-- |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/


-- first, simple array of the results of all mails we analysed
makeEmailsList :: FilePath -> IO [FilePath]
makeEmailsList dir = do
    list <- listDirectory dir
    return [dir ++ "/" ++ file | file <- list]


-- takes a list of emails and returns a list of corresponding MailData
parseEmails :: [FilePath] -> IO [MailData]
parseEmails [] = return []
parseEmails (file:files) = do
    filecontent <- readFile file
    rest <- parseEmails files
    return (parseEmailContent filecontent : rest)
