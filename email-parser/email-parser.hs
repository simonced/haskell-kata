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
            fromEmail = searchFromIn filecontent
            ,subject = ""
            ,spamScore = searchSpamScore filecontent
            ,spamFlag = False
            }
-- TODO subject and spamFlag


searchFromIn :: String -> String
searchFromIn content = head(content =~ "^From:.*<(.*)>" :: [[String]]) !! 1


searchSpamScore :: String -> Float
searchSpamScore content = read (head (content =~ "score=([0-9.]+)" :: [[String]]) !! 1) :: Float


-- for now, we try our function
main = parseEmail "emails\\All-Sport Cutting-Edge Headlight.  Great Holiday Gift. 75% OFF TODAY..eml"


