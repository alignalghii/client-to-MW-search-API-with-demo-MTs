{-# LANGUAGE OverloadedStrings #-}

module InterpretJSON (JSONResponseObject, Title, SearchResult, ContinuationControl, extractFoundTitlesAndContinuationControl) where

import Data.Aeson (decode)
import Data.Aeson.Types (parseMaybe, (.:), Parser, Object, Array)
import qualified Data.Maybe as Myb (mapMaybe) -- Aeson overwrites `mapMaybe`, but we need the original (Prelude) variant!

import Control.Monad ((<=<))
import Control.Arrow ((&&&))


type JSONResponseObject = Object
type Title = String
type SearchResult = (Maybe [Title], Maybe ContinuationControl)
type ContinuationControl = Object

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
