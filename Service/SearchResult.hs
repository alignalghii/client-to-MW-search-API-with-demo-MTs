module Service.SearchResult (SearchResult, Title, Sroffset, showSearchResult) where

import Control.Arrow ((***))


type SearchResult = ([Title], Maybe Sroffset)

type Title = String
type Sroffset = Int

showSearchResult :: SearchResult -> String
showSearchResult =  uncurry (++) . (showTitles *** showMaybeSroffset)

showTitles :: [Title] -> String
showTitles = unlines . map ((++) " - ")

showMaybeSroffset :: Maybe Sroffset -> String
showMaybeSroffset = maybe "No more search results" ((++) "There are more results, repeat seach with &sroffset=" . show)
