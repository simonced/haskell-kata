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

-- basic data structure
data MailData = MailData {
            fromEmail :: String
            ,subject :: String
            ,spamScore :: Float
            ,spamFlag :: Bool
            } deriving (Show)


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
        list = [(spamScore x)|x<- list_]
        elems = fromIntegral (length list_) -- to return a Float

-- simple test in stack ghci:
-- test <- run
-- avgSpamRate test


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
