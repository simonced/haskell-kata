module Main where

import Text.XML.Light
import Text.XML.Light.Types

dictFile = "dict/JMdict_e-sample.xml"


type Reading = String
type Meaning = String


-- returns a tuple (reading, meaning)
readingAndMeaning :: Element -> (Maybe Reading, Maybe Meaning)
readingAndMeaning entry = (r, m)
    where 
        r = fmap strContent $ findElement readtag entry
        m = fmap strContent $ findElement meaningtag entry
        readtag = QName "reb" Nothing Nothing
        meaningtag = QName "gloss" Nothing Nothing


-- shortcut especially useful while testing
getEntries :: IO [Element]
getEntries = do
    source <- readFile dictFile
    let contents = parseXML source
        tag = QName "entry" Nothing Nothing
        entries  = concatMap (findElements $ tag) (onlyElems contents)
    return entries


main :: IO ()
main = do
    putStrLn $ "File to parse: " ++ dictFile
    
    entries <- getEntries    
    
    putStrLn $ "Number of entries: " ++ (show (length entries))

    putStrLn "Sample entry n.20: " 
    print $ readingAndMeaning $ entries !! 20

    return ()