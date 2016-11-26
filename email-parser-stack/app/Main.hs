module Main where

import Lib
import qualified Data.Map as M

lookupFolder = "emails"

-- ======================================================================


--                  _             _ _              __                      
--   ___ ___  _ __ | |_ _ __ ___ | | | ___ _ __   / _|_   _ _ __   ___ ___ 
--  / __/ _ \| '_ \| __| '__/ _ \| | |/ _ \ '__| | |_| | | | '_ \ / __/ __|
-- | (_| (_) | | | | |_| | | (_) | | |  __/ |    |  _| |_| | | | | (__\__ \
--  \___\___/|_| |_|\__|_|  \___/|_|_|\___|_|    |_|  \__,_|_| |_|\___|___/
--                                                                         


displayFromStats :: [Email] -> IO ()
displayFromStats list = do
        let elements = groupUniques $ compareAllList list
        print elements
        --print list


displayEmailsOnly :: [Email] -> IO ()
displayEmailsOnly list = mapM_ print (map (padLeft (maximum (map length list))) list)


-- testing to pad display of emails
-- left is the size to match by adding spaces
-- to the left of the provided string
padLeft :: Int -> String -> String
padLeft size text
    | missing > 0 = (replicate missing ' ') ++ text
    | otherwise = text
    where missing = size - (length text)


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
        let emailsOnly = [fromEmail x | x <- emailsData]
        displayEmailsOnly emailsOnly
        displayFromStats emailsOnly
