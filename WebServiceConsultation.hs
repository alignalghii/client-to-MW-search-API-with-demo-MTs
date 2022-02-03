module WebServiceConsultation (runSearchFirstPage) where

import Service (Service, callService)
import SearchResult (SearchResult, showSearchResult)
import InterpretJSON (JSONResponseObject, Title, Sroffset, extractFoundTitlesAndSroffset)
import Url (searchURL)


runSearchFirstPage :: String -> IO ()
runSearchFirstPage searchphrase = do
    putStrLn $ "Service: first-page of search result for searchphase `" ++ searchphrase ++ "'"
    wrapper callService searchphrase Nothing >>= (putStrLn . showSearchResult)

wrapper :: Service -> String -> Maybe Sroffset -> IO SearchResult
wrapper service url maybeSroffset = maybe ([], Nothing) extractFoundTitlesAndSroffset <$> service (searchURL url maybeSroffset)
-- Code smell: don't hide service call errors! Redesign, or at least extend the monad transformer stack.
