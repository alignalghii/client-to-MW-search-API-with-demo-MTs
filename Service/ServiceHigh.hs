module Service.ServiceHigh (printHigh, showHigh, serviceHigh, serviceHigh', serviceHigh_withMockBase) where

import Control.Pagination (PaginationEffect)
import Service.ServiceLow (ServiceLow, theServiceLow, serviceFormatErrorMsg)
import Service.Url (SearchPhrase, URL, searchURL', baseURL)
import Service.SearchResult (SearchResult, showSearchResult, Title, Sroffset)
import Service.InterpretJSON (extractFoundTitlesAndSroffset)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)


printHigh :: MaybeT IO SearchResult -> IO ()
printHigh = (=<<) (putStrLn . showHigh) . runMaybeT

showHigh :: Maybe SearchResult -> String
showHigh = maybe serviceFormatErrorMsg showSearchResult

serviceHigh :: SearchPhrase -> MaybeT IO SearchResult
serviceHigh = flip serviceHigh' Nothing

serviceHigh' :: SearchPhrase -> PaginationEffect Sroffset (MaybeT IO) [Title]
serviceHigh' = serviceHigh_withMockBase baseURL

serviceHigh_withMockBase :: URL -> SearchPhrase -> PaginationEffect Sroffset (MaybeT IO) [Title]
serviceHigh_withMockBase mockBaseURL searchphrase = fmap extractFoundTitlesAndSroffset . theServiceLow . searchURL' mockBaseURL searchphrase
