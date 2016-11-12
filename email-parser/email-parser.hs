-- will turn into a module once it's working

-- package: http://hackage.haskell.org/package/regex-posix
-- installation intructions "cabal install regex-posix"
-- if failed but finally worked. problem with firewall!

import Text.Regex.Posix

-- basic data structure
data MailData = MailData {
            fromEmail :: String
            ,subject :: String
            ,spamScore :: Float
            ,spamFlag :: Bool
            } deriving (Show)


-- this should open the file and extract the data we need
parseEmail :: String -> IO MailData
parseEmail mailfile = do
        filecontent <- readFile mailfile
        return MailData {
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


-- for now, we try our function
main = parseEmail "emails/email1.eml"
-- TODO parse all emails in folder and return a summary

