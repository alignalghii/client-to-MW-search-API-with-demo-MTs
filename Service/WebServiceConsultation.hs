module Service.WebServiceConsultation (searchMessage, runSearchFirstPage, runSearchPaged) where

import Service.ServiceHigh (printHigh, showHigh, serviceHigh, serviceHigh_withMockBase)
import Service.SearchResult (SearchResult, actSearchResult)
import Service.Url (SearchPhrase, URL)

import PaginationStateMachines.Effectful.PaginationConceptSeries (pagination_noMT)

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)


searchMessage :: URL -> SearchPhrase -> IO String
searchMessage mockBaseURL searchphrase = showHigh <$> (runMaybeT $ serviceHigh_withMockBase mockBaseURL searchphrase Nothing)


runSearchFirstPage :: SearchPhrase -> IO ()
runSearchFirstPage searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    printHigh $ serviceHigh searchphrase Nothing


runSearchPaged :: SearchPhrase -> IO ()
runSearchPaged searchphrase = void $ do
    putStrLn $ "Service: paginated search result for searchphase `" ++ searchphrase ++ "'"
    runMaybeT $ pagination_noMT $ (mActSearchResult =<<) . serviceHigh searchphrase

mActSearchResult :: SearchResult -> MaybeT IO SearchResult
mActSearchResult = lift . actSearchResult
