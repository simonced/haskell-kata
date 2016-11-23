module Lib
    --(
    --    makeEmailsList
    --   ,parseEmails
    --) 
    where
-- currently exporting everything while testing in ghci

-- ======================================================================

-- packages I need:
-- directory
-- regex-posix


import           Text.Regex.Posix
import           System.Directory
import           Data.Maybe -- needed for MaybeMap
import qualified Data.Map as M


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
-- > test <- run
-- > avgSpamRate test
-- run is a simple shortcut to launch main function with directory to parse as parameter


-- I nedd to compare the tails of strings... >>>
-- compare from head, to compare from tail, strings will have to bve reversed first
-- will return the number of characters that matches from head
-- no better idea for now
commonPart :: String -> String -> Int -> Int
commonPart (m1:m1s) (m2:m2s) counter
  | m1 == m2 = 1 + commonPart m1s m2s counter
  | otherwise = counter


-- compares 2 strings (emails) and returns the common part if any
compareEmailTail :: Email -> Email -> Maybe String
compareEmailTail email1 email2
  | common > 0 = Just $ reverse $ take common remail1
  | otherwise = Nothing
  where
    remail1 = reverse email1
    remail2 = reverse email2
    common = commonPart remail1 remail2 0
-- <<<


compareOneToList :: Email -> [Email] -> [String]
compareOneToList email list = mapMaybe (compareEmailTail email) list


compareAllList :: [Email] -> [String]
compareAllList [x] = [] -- if one value is left, we have nothing to compare
compareAllList (email:list) = compareOneToList email list ++ compareAllList list
-- this is not a perfect calculation yet, but it'll do for now
-- FIXME do a better algorithm


groupUniques :: [String] -> M.Map String Int
groupUniques [] = M.empty
groupUniques (x:xs) = M.insertWith (\ new old -> new + old) x 1 map
    where map = groupUniques xs
-- easy way to test this:
-- list <- test
-- groupUniques . compareAllList $ list


-- testing function
test = do
  emailsList <- makeEmailsList "emails"
  emailsData <- parseEmails emailsList
  return [fromEmail x | x <- emailsData]
  -- let groups = compareAllEmails emailsData
  -- return groups


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
