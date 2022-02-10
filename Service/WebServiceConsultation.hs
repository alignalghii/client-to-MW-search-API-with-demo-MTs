module Service.WebServiceConsultation (searchMessage, runSearchFirstPage, runSearchFirstPage', runSearchPaged', runSearchSlideshow) where


import Service.ServiceHigh (printHigh, showHigh, abstractFromLowRepresentation, abstractFromLowRepresentation_withMockBase)
import Service.ServiceLow (ServiceLow, theServiceLow, ServiceLow', theServiceLow', serviceFormatErrorMsg)
import Service.SearchResult (showSearchResult, actSearchResult)
import Service.InterpretJSON (JSONResponseObject, Title, Sroffset, extractFoundTitlesAndSroffset)
import Service.Url (URL, searchURL)

import Control.Pagination (PaginationEffect)
import PaginationStateMachines.Effectful.PaginationConceptSeries (pagination_noMT)
import PaginationStateMachines.Effectful.PaginationConcept_lazy (pagination_MT_lazy)
import Control.Monad.State.Lazy (StateT (StateT))

import Data.Maybe (isJust)
import Control.Monad (when, void)
import Control.Concurrent (threadDelay)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)


searchMessage :: URL -> String -> IO String
searchMessage mockBaseURL searchphrase = showHigh <$> (runMaybeT $ abstractFromLowRepresentation_withMockBase mockBaseURL searchphrase Nothing)


runSearchSlideshow :: String -> IO ()
runSearchSlideshow searchphrase = do
    x <- pagination_MT_lazy $ StateT $ wrapperD theServiceLow searchphrase
    print x


runSearchFirstPage :: String -> IO ()
runSearchFirstPage searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    wrapper theServiceLow searchphrase Nothing >>= (putStrLn . showSearchResult)

wrapper, wrapperD :: ServiceLow -> String -> PaginationEffect Sroffset IO [Title]
wrapper  service searchphrase maybeSroffset = build <$> service (searchURL searchphrase maybeSroffset)
wrapperD service searchphrase maybeSroffset = do
    x <- build <$> service (searchURL searchphrase maybeSroffset)
    threadDelay 5000000
    return x

runSearchFirstPage' :: String -> IO ()
runSearchFirstPage' searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    printHigh $ abstractFromLowRepresentation searchphrase



runSearchPaged :: String -> IO ()
runSearchPaged = runSearchPagedWith (\phrase -> "Service: paginated search result for searchphase `" ++ phrase ++ "'") getLine -- (\ a -> getLine >> print a)

runSearchPagedWith :: (String -> String) -> IO a -> String -> IO ()
runSearchPagedWith info delay searchphrase = do
    putStrLn $ info searchphrase
    pagination_noMT (wrapper (flip postdelay delay . theServiceLow) searchphrase) >>= print

runSearchPaged' :: String -> IO ()
runSearchPaged' searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    pagination_noMT $ wrapper' theServiceLow searchphrase
    return ()

wrapper' :: ServiceLow -> String -> PaginationEffect Sroffset IO [Title]
wrapper' service searchphrase maybeSroffset = (actSearchResult . build) =<< service (searchURL searchphrase maybeSroffset)

build :: Maybe JSONResponseObject -> ([Title], Maybe Sroffset)
build = maybe ([], Nothing) extractFoundTitlesAndSroffset

{--
runSearchPaged_MT_lazy :: String -> IO ()
runSearchPaged_MT_lazy searchphrase = do
    pagination_MT_lazy $ wrapper' theServiceLow searchphrase
--}

postdelay :: Monad m => m a -> m b -> m a
postdelay action delay = action >>= ((>>) delay . return)