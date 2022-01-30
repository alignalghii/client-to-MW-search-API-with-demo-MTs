{-# LANGUAGE OverloadedStrings #-}

module WebServiceConsultation (runSearch) where

import InterpretJSON (JSONResponseObject, SearchResult, extractFoundTitlesAndContinuationControl)

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.Aeson (decode)

-- import Data.Text (Text)
import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (pack, unpack, putStrLn)


runSearch :: String -> IO ()
runSearch searchedWord = do
    putStr "Searchphrase: "
    putStrLn $ pack searchedWord
    putStrLn ""
    maybeJsonObject <- searchService searchedWord
    maybe (putStrLn "Error") (presentSearchResult . extractFoundTitlesAndContinuationControl) maybeJsonObject

searchService :: String -> IO (Maybe JSONResponseObject)
searchService searchedWord = do
    resp <- get $ searchURL searchedWord
    let rawBody = resp ^. responseBody
    let maybeJsonObject = decode rawBody
    return maybeJsonObject

presentSearchResult :: SearchResult -> IO ()
presentSearchResult (maybeTitles, maybeContinuationObject) = print maybeTitles >> print maybeContinuationObject

searchURL :: String -> String
searchURL = (++) "https://en.wikipedia.org/w/api.php?action=query&format=json&list=search&utf8=1&srsearch="
