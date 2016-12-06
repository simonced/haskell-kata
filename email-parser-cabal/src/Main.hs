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


--                  _       
--  _ __ ___   __ _(_)_ __  
-- | '_ ` _ \ / _` | | '_ \ 
-- | | | | | | (_| | | | | |
-- |_| |_| |_|\__,_|_|_| |_|
--                          


main :: IO ()
-- main = run

-- threepenny version (start)
main = startGUI defaultConfig setup

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

-- for now, copy-pasted sample
setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Currency Converter"

    dollar <- UI.input
    euro   <- UI.input
    
    getBody window #+ [
            column [
                grid [[string "Dollar:", element dollar]
                     ,[string "Euro:"  , element euro  ]]
            , string "Amounts update while typing."
            ]]

    euroIn   <- stepper "0" $ UI.valueChange euro
    dollarIn <- stepper "0" $ UI.valueChange dollar
    let
        rate = 0.7 :: Double
        withString f = maybe "-" (printf "%.2f") . fmap f . readMay
    
        dollarOut = withString (/ rate) <$> euroIn
        euroOut   = withString (* rate) <$> dollarIn
    
    element euro   # sink value euroOut
    element dollar # sink value dollarOut
