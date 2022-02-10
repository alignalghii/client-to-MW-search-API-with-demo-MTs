module Service.ServiceHigh (printHigh, showHigh, abstractFromLowRepresentation, abstractFromLowRepresentation_withMockBase) where

import Control.Pagination (PaginationEffect)
import Service.ServiceLow (ServiceLow, theServiceLow, serviceFormatErrorMsg)
import Service.Url (URL, searchURL, searchURL')
import Service.SearchResult (SearchResult, showSearchResult, Title, Sroffset)
import Service.InterpretJSON (extractFoundTitlesAndSroffset)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)


printHigh :: MaybeT IO SearchResult -> IO ()
printHigh = (=<<) (putStrLn . showHigh) . runMaybeT

showHigh :: Maybe SearchResult -> String
showHigh = maybe serviceFormatErrorMsg showSearchResult

abstractFromLowRepresentation :: String -> MaybeT IO SearchResult
abstractFromLowRepresentation searchphrase = abstractFromLowRepresentation' theServiceLow searchphrase Nothing

abstractFromLowRepresentation' :: ServiceLow -> String -> PaginationEffect Sroffset (MaybeT IO) [Title]
abstractFromLowRepresentation' service' searchphrase = fmap extractFoundTitlesAndSroffset . service' . searchURL searchphrase

abstractFromLowRepresentation_withMockBase :: URL -> String -> PaginationEffect Sroffset (MaybeT IO) [Title]
abstractFromLowRepresentation_withMockBase mockBaseURL searchphrase = fmap extractFoundTitlesAndSroffset . theServiceLow . searchURL' mockBaseURL searchphrase
