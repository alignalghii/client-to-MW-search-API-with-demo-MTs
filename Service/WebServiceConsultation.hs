module Service.WebServiceConsultation (searchMessage, runSearchFirstPage', runSearchPaged') where


import Service.ServiceHigh (printHigh, showHigh, serviceHigh, serviceHigh_withMockBase)
import Service.ServiceLow (ServiceLow_lengthy, theServiceLow_lengthy, ServiceLow, theServiceLow, serviceFormatErrorMsg)
import Service.SearchResult (showSearchResult, actSearchResult)
import Service.InterpretJSON (JSONResponseObject, Title, Sroffset, extractFoundTitlesAndSroffset)
import Service.Url (SearchPhrase, URL, searchURL)

import Control.Pagination (PaginationEffect)
import PaginationStateMachines.Effectful.PaginationConceptSeries (pagination_noMT)

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)


searchMessage :: URL -> SearchPhrase -> IO String
searchMessage mockBaseURL searchphrase = showHigh <$> (runMaybeT $ serviceHigh_withMockBase mockBaseURL searchphrase Nothing)


runSearchFirstPage' :: SearchPhrase -> IO ()
runSearchFirstPage' searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    printHigh $ serviceHigh searchphrase Nothing


runSearchPaged' :: SearchPhrase -> IO ()
runSearchPaged' searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    pagination_noMT $ wrapper' theServiceLow_lengthy searchphrase
    return ()

wrapper' :: ServiceLow_lengthy -> SearchPhrase -> PaginationEffect Sroffset IO [Title]
wrapper' service searchphrase maybeSroffset = (actSearchResult . build) =<< service (searchURL searchphrase maybeSroffset)

build :: Maybe JSONResponseObject -> ([Title], Maybe Sroffset)
build = maybe ([], Nothing) extractFoundTitlesAndSroffset
