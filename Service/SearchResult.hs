module Service.SearchResult (SearchResult, Title, Sroffset, showSearchResult, actSearchResult) where

import Control.Arrow ((***))
import Control.Monad (void, (=<<), liftM2)


type SearchResult = ([Title], Maybe Sroffset)

type Title = String
type Sroffset = Int

showSearchResult :: SearchResult -> String
showSearchResult =  uncurry (++) . (showTitles *** showMaybeSroffset)

showTitles :: [Title] -> String
showTitles = unlines . map ((++) " - ")

showMaybeSroffset :: Maybe Sroffset -> String
showMaybeSroffset = maybe "No more search results" ((++) "There are more results, repeat seach with &sroffset=" . show)

actSearchResult :: SearchResult -> IO SearchResult
actSearchResult =  uncurry (liftM2 (,)) . (printAndPassTitles *** actContinuationIfAny)

printAndPassTitles :: [Title] -> IO [Title]
printAndPassTitles titles = mapM_ (putStrLn . (++) " - ") titles >> return titles

actContinuationIfAny :: Maybe Sroffset -> IO (Maybe Sroffset)
actContinuationIfAny ms = maybe (putStrLn "No more items." >> return Nothing) confirmContinuationToken ms

confirmContinuationToken :: Sroffset -> IO (Maybe Sroffset)
confirmContinuationToken tok = putStrLn ("Press ENTER to continue (continuation token: " ++ show tok ++ ")...") >>  getLine >>= (return . flip interpretContinuation tok)

interpretContinuation :: String -> Sroffset -> Maybe Sroffset
interpretContinuation "n"   = const Nothing
interpretContinuation "no"  = const Nothing
interpretContinuation ""    = Just
interpretContinuation "y"   = Just
interpretContinuation "yes" = Just
interpretContinuation _     = Just
