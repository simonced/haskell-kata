-- will turn into a module once it's working

-- basic data structure
data MailData = MailData {
            fromEmail :: String
            ,title :: String
            ,spamScore :: Float
            ,spamFlag :: Bool
            } deriving (Show)


-- this should open the file and extract the data we need
parseEmail :: String -> MailData
parseEmail mailfile = MailData {
            fromEmail = "fromemail@todo"
            ,title = ""
            ,spamScore = 4.3
            ,spamFlag = False
            }
            -- TODO real logic


-- for now, we try our function
main = do

    let mymaildata = parseEmail "emails\\Javaプログラマーに朗報！プログラミングスキル定着に最適「プログラマー早期育成ドリル」_ウェアラブル機器活用最前線【DIP記事】.eml"

    -- for testing
    print mymaildata

