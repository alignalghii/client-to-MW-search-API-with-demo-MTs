module Service.WebServiceConsultation (runSearchFirstPage, runSearchPaged, runSearchInteract) where

import Service.Service (Service, callService)
import Service.SearchResult (showSearchResult)
import Service.InterpretJSON (JSONResponseObject, Title, Sroffset, extractFoundTitlesAndSroffset)
import Service.Url (searchURL)

import Control.Pagination (PaginationEffect)
import Effectful.PaginationConceptSeries (pagination_noMT)


runSearchFirstPage :: String -> IO ()
runSearchFirstPage searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    wrapper callService searchphrase Nothing >>= (putStrLn . showSearchResult)

wrapper :: Service -> String -> PaginationEffect Sroffset IO [Title]
wrapper service url maybeSroffset = maybe ([], Nothing) extractFoundTitlesAndSroffset <$> service (searchURL url maybeSroffset)
-- Code smell: don't hide service call errors! Redesign, or at least extend the monad transformer stack.


runSearchPaged :: String -> IO ()
runSearchPaged = runSearchPagedWith (\phrase -> "Service: paginated search result for searchphase `" ++ phrase ++ "'") getLine -- (\ a -> getLine >> print a)

runSearchPagedWith :: (String -> String) -> IO a -> String -> IO ()
runSearchPagedWith info delay searchphrase = do
    putStrLn $ info searchphrase
    pagination_noMT (wrapper (flip postdelay delay . callService) searchphrase) >>= print

postdelay :: Monad m => m a -> m b -> m a
postdelay action delay = action >>= ((>>) delay . return)

runSearchInteract :: String -> IO ()
runSearchInteract = undefined
