module Service.WebServiceConsultation (runSearchFirstPage, runSearchPaged, runSearchPaged', runSearchInteract) where

import Service.Service (Service, callService)
import Service.SearchResult (showSearchResult, actSearchResult)
import Service.InterpretJSON (JSONResponseObject, Title, Sroffset, extractFoundTitlesAndSroffset)
import Service.Url (searchURL)

import Control.Pagination (PaginationEffect)
import Effectful.PaginationConceptSeries (pagination_noMT)

import Data.Maybe (isJust)
import Control.Monad (when, void)


runSearchFirstPage :: String -> IO ()
runSearchFirstPage searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    wrapper callService searchphrase Nothing >>= (putStrLn . showSearchResult)

wrapper :: Service -> String -> PaginationEffect Sroffset IO [Title]
wrapper service searchphrase maybeSroffset = build <$> service (searchURL searchphrase maybeSroffset)


runSearchPaged :: String -> IO ()
runSearchPaged = runSearchPagedWith (\phrase -> "Service: paginated search result for searchphase `" ++ phrase ++ "'") getLine -- (\ a -> getLine >> print a)

runSearchPagedWith :: (String -> String) -> IO a -> String -> IO ()
runSearchPagedWith info delay searchphrase = do
    putStrLn $ info searchphrase
    pagination_noMT (wrapper (flip postdelay delay . callService) searchphrase) >>= print

runSearchPaged' :: String -> IO ()
runSearchPaged' searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    pagination_noMT $ wrapper' callService searchphrase
    return ()

wrapper' :: Service -> String -> PaginationEffect Sroffset IO [Title]
wrapper' service searchphrase maybeSroffset = (actSearchResult . build) =<< service (searchURL searchphrase maybeSroffset)

build :: Maybe JSONResponseObject -> ([Title], Maybe Sroffset)
build = maybe ([], Nothing) extractFoundTitlesAndSroffset
-- Code smell: don't hide service call errors! Redesign, or at least extend the monad transformer stack.

{--
runSearchPagedWith' :: (String -> String) -> (Maybe JSONResponseObject -> IO ()) -> String -> IO ()
runSearchPagedWith' info delay searchphrase = do
    putStrLn $ info searchphrase
    pagination_noMT $ wrapper' (\url -> callService url >>= delay) searchphrase
    return ()

--wrapper' :: (String -> IO a) -> String -> PaginationEffect Sroffset IO [Title]
wrapper' service url maybeSroffset = service (searchURL url maybeSroffset)
-- Code smell: don't hide service call errors! Redesign, or at least extend the monad transformer stack.
--}

{--
sandwich :: Monad m => m a -> m b -> m c -> m c
sandwich info delay action = info >> postdelay action delay
--}

postdelay :: Monad m => m a -> m b -> m a
postdelay action delay = action >>= ((>>) delay . return)

runSearchInteract :: String -> IO ()
runSearchInteract = undefined
