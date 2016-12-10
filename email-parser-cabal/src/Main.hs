module Main where

import EmailParser.Lib

import qualified Data.Map as M

import Control.Monad (void)
import Data.Maybe
import Text.Printf
import Safe          (readMay)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

lookupFolder = "../emails"

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



run = do
        emailsFiles <- makeEmailsList lookupFolder
        emailsData <- parseEmails emailsFiles
        let emailsOnly = [fromEmail x | x <- emailsData]
        displayEmailsOnly emailsOnly
        displayFromStats emailsOnly


readEmails = do
    emailsFiles <- makeEmailsList lookupFolder
    emailsData <- parseEmails emailsFiles
    return emailsData

--                  _       
--  _ __ ___   __ _(_)_ __  
-- | '_ ` _ \ / _` | | '_ \ 
-- | | | | | | (_| | | | | |
-- |_| |_| |_|\__,_|_|_| |_|
--                          

main :: IO ()
-- main = run

-- threepenny version (start)
main = do
    startGUI defaultConfig
        {
            jsStatic = Just "static"
        } setup

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

-- for now, copy-pasted sample
setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Emails data"

    -- main settings for the client
    UI.addStyleSheet window "styles.css"

    --let emailsData = do liftIO $ readEmails
    list <- liftIO $ readEmails

    let gridData = grid $ [string "Email", string "spam score"] : [[string $ fromEmail d, string $ show $ spamScore d] | d <- list]

    getBody window #+ [
            mkElement "h1" # set text "Current emails analysed"
            ,column [
                gridData #. "table table-list"
            ]
            ,mkElement "h2" # set text "Statistics"
            ,string "TODO" #. "alert alert-info"
        ]


    -- good repos for inspiration:
    -- https://bitbucket.org/duplode/stunts-cartography/overview
