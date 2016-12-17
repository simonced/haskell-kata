module Main where

import Text.XML.Light
import Text.XML.Light.Types

dictFile = "dict/JMdict_e-sample.xml" -- sample to test algo
--dictFile = "dict/JMdict_e.xml"


type Kanji   = String
type Reading = String
type Meaning = String

data Entry = Entry {
    entryKanji :: Kanji,
    entryReading :: Reading,
    entryMeanings :: [Meaning]
} deriving (Show)


-- test sample function >>>
-- returns a tuple (reading, meaning)
readingAndMeaning :: Element -> (Maybe Reading, Maybe Meaning)
readingAndMeaning entry = (r, m)
    where 
        r = fmap strContent $ findElement readtag entry
        m = fmap strContent $ findElement meaningtag entry
        readtag    = QName "reb" Nothing Nothing
        meaningtag = QName "gloss" Nothing Nothing
-- <<<


parseDictEntry :: Element -> Entry
parseDictEntry entry = Entry {
     entryKanji = k
    ,entryReading = r
    ,entryMeanings = m ++ [] } -- TODO get all the meanings if present!
    where 
        k = case fmap strContent $ findElement kanjitag entry of
            Just t  -> t
            Nothing -> "no kanji"
        r = case fmap strContent $ findElement readtag entry of
            Just t  -> t
            Nothing -> "no reading"
        m = case fmap strContent $ findElement meaningtag entry of
            Just t  -> t : []
            Nothing -> []
        kanjitag   = QName "keb" Nothing Nothing
        readtag    = QName "reb" Nothing Nothing
        meaningtag = QName "gloss" Nothing Nothing


-- shortcut especially useful while testing
getEntries :: IO [Element]
getEntries = do
    source <- readFile dictFile
    let contents = parseXML source
        tag      = QName "entry" Nothing Nothing
        entries  = concatMap (findElements $ tag) (onlyElems contents)
    return entries


-- ======================================================================
-- Main
-- ======================================================================


main :: IO ()
main = do
    putStrLn $ "File to parse: " ++ dictFile
    
    entries <- getEntries    
    
    --putStrLn $ "Number of entries: " ++ (show (length entries))

    -- for our tests
    let sampleNumber = 20
    putStrLn $ "Sample entry n.: " ++ (show sampleNumber)

    let sampleEntry = parseDictEntry $ entries !! sampleNumber

    putStrLn $ "kanji:    " ++ entryKanji sampleEntry
    putStrLn $ "reading:  " ++ entryReading sampleEntry
    putStrLn $ "meanings: " ++ (unlines $ entryMeanings sampleEntry)

    {-
    let testEntry = readingAndMeaning $ entries !! sampleNumber
    -- reading
    case fst testEntry of
        Just t  -> putStrLn $ "Reading: " ++ t
        Nothing -> putStrLn "no reading"
    -- meaning
    case snd testEntry of
        Just t  -> putStrLn $ "Meaning: " ++ t
        Nothing -> putStrLn "no meaning"
    -}

    return ()
