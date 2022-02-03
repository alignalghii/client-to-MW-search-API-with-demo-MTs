{-# LANGUAGE OverloadedStrings #-}

module Service.InterpretJSON (JSONResponseObject, Title, Sroffset, extractFoundTitlesAndSroffset) where

import Service.SearchResult (SearchResult, Title, Sroffset)

import Data.Aeson.Types (parseMaybe, (.:), Object, FromJSON)
import qualified Data.Maybe as Myb (mapMaybe) -- Aeson overwrites `mapMaybe`, but we need the original (Prelude) variant!
import Data.Text (Text)

import Control.Monad ((<=<))
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)


type JSONResponseObject = Object

extractFoundTitlesAndSroffset :: JSONResponseObject -> ([Title], Maybe Sroffset)
extractFoundTitlesAndSroffset = titlesInRootObject &&& maybeSroffsetInRootObject

titlesInRootObject :: JSONResponseObject -> [Title]
titlesInRootObject = maybe [] (multiselect "title") . (select "search" <=< select "query")

maybeSroffsetInRootObject :: Object -> Maybe Sroffset
maybeSroffsetInRootObject = select "sroffset" <=< select "continue"

-- maybeSelectPath :: FromJSON b => [Text] -> Object -> Maybe b
-- maybeSelectPath = foldr (\lbl f -> per lbl <=< f) Just

select :: FromJSON b => Text -> Object -> Maybe b
select = parseMaybe . flip (.:)

multiselect :: FromJSON b => Text -> [Object] -> [b]
multiselect = Myb.mapMaybe . select
