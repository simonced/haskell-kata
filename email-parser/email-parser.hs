-- will turn into a module once it's working

-- package: http://hackage.haskell.org/package/regex-posix
-- installation intructions "cabal install regex-posix"
-- if failed but finally worked. problem with firewall!

import Text.Regex.Posix

-- basic data structure
data MailData = MailData {
            fromEmail :: String
            ,title :: String
            ,spamScore :: Float
            ,spamFlag :: Bool
            } deriving (Show)


-- this should open the file and extract the data we need
parseEmail :: String -> IO MailData
parseEmail mailfile = do
        filecontent <- readFile mailfile
        return MailData {
            fromEmail = searchFromIn filecontent
            ,title = ""
            ,spamScore = 4.3
            ,spamFlag = False
            }


searchFromIn :: String -> String
searchFromIn content = content =~ "From:.*<(.*)>" :: String
-- I found a good solution to get only the email:
-- head ("From: longstringofdoom! <simon@test.com>" =~ "From:.*<(.*)>" :: [[String]]) !! 1

-- for now, we try our function
main = do

    parseEmail "emails\\Javaプログラマーに朗報！プログラミングスキル定着に最適「プログラマー早期育成ドリル」_ウェアラブル機器活用最前線【DIP記事】.eml"


