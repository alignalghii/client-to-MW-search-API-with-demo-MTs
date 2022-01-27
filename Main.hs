{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wreq (get, responseBody)

import Control.Lens ((^.))
import Data.Aeson.Lens (_String, key)
import Data.Aeson (Value, decode, encode)
import Data.Aeson.Types (parseMaybe, (.:), Parser, Object, Array)

import Data.Text (Text)
import Control.Monad ((<=<))
import Control.Arrow((&&&))

import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (pack, unpack, putStrLn)

import Data.Maybe as Myb (mapMaybe, listToMaybe)

import Test.Hspec (hspec)
import qualified Effectless.PaginationConceptSeriesSpec as EffectlessSpecs (spec)
import qualified Effectful.PaginationConceptSeriesSpec  as EffectfulSpecs  (spec)

import System.Environment (getArgs)


main, runTests :: IO ()
main = fmap listToMaybe getArgs >>= maybe runTests runSearch
runTests = do
    putStrLn "No searchphrase provided, so I am running unit tests instead"
    hspec $ EffectlessSpecs.spec >> EffectfulSpecs.spec

runSearch :: String -> IO ()
runSearch searchedWord = do
    putStr "Searchphrase: "
    putStrLn $ pack searchedWord
    putStrLn ""
    maybeJsonObject <- searchService searchedWord
    maybe (putStrLn "Error") (presentSearchResult . extractFoundTitlesAndContinuationControl) maybeJsonObject

type JSONResponseObject = Object
type Title = String
type SearchResult = (Maybe [Title], Maybe ContinuationControl)
type ContinuationControl = Object

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

extractFoundTitlesAndContinuationControl :: JSONResponseObject -> SearchResult
extractFoundTitlesAndContinuationControl = maybeTitlesInRootObject &&& maybeContinuationObjectInRootObject

maybeTitlesInRootObject :: JSONResponseObject -> Maybe [Title]
maybeTitlesInRootObject = fmap selectTitlesInSearchArray . (maybeSearchArrayInQueryObject <=< maybeQueryObjectInRootObject)

maybeContinuationObjectInRootObject, maybeQueryObjectInRootObject :: Object -> Maybe Object
maybeContinuationObjectInRootObject = parseMaybe (.: "continue")
maybeQueryObjectInRootObject        = parseMaybe (.: "query")

maybeSearchArrayInQueryObject :: Object -> Maybe [Object]
maybeSearchArrayInQueryObject = parseMaybe (.: "search")

selectTitlesInSearchArray :: [Object] -> [String]
selectTitlesInSearchArray = Myb.mapMaybe maybeTitleInSearchArrayItemObject

maybeTitleInSearchArrayItemObject :: Object -> Maybe String
maybeTitleInSearchArrayItemObject = parseMaybe (.: "title")
