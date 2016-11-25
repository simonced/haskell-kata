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


displayFromStats :: [MailData] -> IO ()
displayFromStats emailsData_ = do
        let elements = groupUniques . compareAllList $ [fromEmail x | x <- emailsData_]
        print elements
        -- TODO make a better formated listing
        -- attempts
        -- print $ (M.toList elements) !! 1



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
        displayFromStats emailsData
