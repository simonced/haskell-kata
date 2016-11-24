module Main where

import Lib

lookupFolder = "emails"

-- ======================================================================


--                  _             _ _              __                      
--   ___ ___  _ __ | |_ _ __ ___ | | | ___ _ __   / _|_   _ _ __   ___ ___ 
--  / __/ _ \| '_ \| __| '__/ _ \| | |/ _ \ '__| | |_| | | | '_ \ / __/ __|
-- | (_| (_) | | | | |_| | | (_) | | |  __/ |    |  _| |_| | | | | (__\__ \
--  \___\___/|_| |_|\__|_|  \___/|_|_|\___|_|    |_|  \__,_|_| |_|\___|___/
--                                                                         


displayFromStats :: [MailData] -> IO ()
displayFromStats emailsData_ = do
        -- TODO make a better formated listing
        print $ groupUniques . compareAllList $ [fromEmail x | x <- emailsData_]



--                  _       
--  _ __ ___   __ _(_)_ __  
-- | '_ ` _ \ / _` | | '_ \ 
-- | | | | | | (_| | | | | |
-- |_| |_| |_|\__,_|_|_| |_|
--                          


main :: IO ()
main = do
        emailsFiles <- makeEmailsList lookupFolder
        emailsData <- parseEmails emailsFiles
        displayFromStats emailsData
